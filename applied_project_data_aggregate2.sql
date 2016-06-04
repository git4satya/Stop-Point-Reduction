DELETE FROM applied_project.`total_orders_increment`
WHERE build_type IN ('RMA-RFB', 'RMA-MWR', '');

-- ------------------------------------
-- KEEP ALL ORDERS DATA IN MASTER
-- FOR CLUSTERING ON CUSTOMER AND TEAM
-- ------------------------------------
DELETE FROM applied_project.`total_orders_master`
WHERE scn in
	( SELECT scn FROM applied_project.`total_orders_increment` GROUP BY 1) ;

INSERT INTO applied_project.`total_orders_master`
SELECT * FROM applied_project.`total_orders_increment`;

-- ---------------------------
-- AGGREGATE SHIPMENT DATA 
-- ---------------------------
-- -----------------------------------------
-- MASTER AGGREGATION FOR COMPLEXITY_SCALE 
-- DERIVATION
-- -----------------------------------------
DELETE FROM applied_project.`wrk_shipped_orders_agg_master`;
insert into applied_project.`wrk_shipped_orders_agg_master`
select sd.cust_no, sd.bill_to_customer, sd.scn, sd.team, sd.build_type
		-- , bd.bi, sd.no_of_prd, sd.tot_time
        , NULL as bi, sd.no_of_prd, sd.tot_time
		, sd.tot_units, sd.tot_resale, sd.tot_integ_cost
        , round(sd.tot_integ_cost/(tot_time/60.0),1) as integ_cost_per_hr
        , round(sd.tot_integ_cost/sd.tot_units,1) integ_cost_per_unit
        , sd.Fiscal_Quarter
        , sd.Fiscal_Month
        , sd.BU
from 
(
	select Cust_No, Bill_To_Customer, SCN, Team, Build_Type, Fiscal_Quarter, Fiscal_Month, BU, 
			count(distinct prdno) no_of_prd, sum(Sch_Touch_Time) tot_time, sum(Units) tot_units, 
			sum(Resale) tot_resale, sum(Integ_Cost) tot_integ_cost
	from applied_project.`total_orders_master`
	group by 1,2,3,4,5,6,7,8
) sd -- shipment details
where (sd.tot_time is not null and sd.tot_time > 0.0) -- ignore records with missing tot_time & time value < 0.0
	and (sd.tot_units is not null and sd.tot_units > 0.0) -- ignore records with missing tot_units & units value < 0.0
;

delete from applied_project.`wrk_shipped_orders_agg1`;
insert into applied_project.`wrk_shipped_orders_agg1`
select sd.cust_no, sd.bill_to_customer, sd.scn, sd.team, sd.build_type
		-- , bd.bi, sd.no_of_prd, sd.tot_time
        , NULL as bi, sd.no_of_prd, sd.tot_time
		, sd.tot_units, sd.tot_resale, sd.tot_integ_cost
        , round(sd.tot_integ_cost/(tot_time/60.0),1) as integ_cost_per_hr
        , round(sd.tot_integ_cost/sd.tot_units,1) integ_cost_per_unit
        , sd.Fiscal_Quarter
        , sd.Fiscal_Month
        , sd.BU
from 
(
	select Cust_No, Bill_To_Customer, SCN, Team, Build_Type, Fiscal_Quarter, Fiscal_Month, BU, 
			count(distinct prdno) no_of_prd, sum(Sch_Touch_Time) tot_time, sum(Units) tot_units, 
			sum(Resale) tot_resale, sum(Integ_Cost) tot_integ_cost
	from applied_project.`total_orders_increment`
	group by 1,2,3,4,5,6,7,8
) sd -- shipment details
where (sd.tot_time is not null and sd.tot_time > 0.0) -- ignore records with missing tot_time & time value < 0.0
	and (sd.tot_units is not null and sd.tot_units > 0.0) -- ignore records with missing tot_units & units value < 0.0
;

-- ------------------------------------------------------
-- CALCULATE 80, 60, 40 & 20 PERCENTILE VALUES
-- THIS WILL BE USED FOR DERIVATION OF COMPLEXITY_SCALE
-- ------------------------------------------------------

select count(1) into @rec_count from applied_project.`wrk_shipped_orders_agg_master`;

-- GET 80% PERCENTILE
SET @row_number:=0;
select integ_cost_per_hr into @icph_80 from
(
select integ_cost_per_hr, @row_number:=@row_number+1 as rn
from applied_project.`wrk_shipped_orders_agg_master` order by integ_cost_per_hr asc
) Y
where rn = (select ceil(@rec_count * .80));

-- GET 60% PERCENTILE
SET @row_number:=0;
select integ_cost_per_hr into @icph_60 from
(
select integ_cost_per_hr, @row_number:=@row_number+1 as rn
from applied_project.`wrk_shipped_orders_agg_master` order by integ_cost_per_hr asc
) Y
where rn = (select ceil(@rec_count * .60));

-- GET 40% PERCENTILE
SET @row_number:=0;
select integ_cost_per_hr into @icph_40 from
(
select integ_cost_per_hr, @row_number:=@row_number+1 as rn
from applied_project.`wrk_shipped_orders_agg_master` order by integ_cost_per_hr asc
) Y
where rn = (select ceil(@rec_count * .40));

-- GET 20% PERCENTILE
SET @row_number:=0;
select integ_cost_per_hr into @icph_20 from
(
select integ_cost_per_hr, @row_number:=@row_number+1 as rn
from applied_project.`wrk_shipped_orders_agg_master` order by integ_cost_per_hr asc
) Y
where rn = (select ceil(@rec_count * .20));

-- -------------------------------------------------------
-- FINAL DENORMALIZED TABLE WITH "STOP POINT" REASON CODE
-- + WHICH IS USED AS CLASS LABLE IN THE TRAINING SET
-- + THIS DATA WILL BE USED FOR TRAINING THE MODEL
-- --------------------------------------------------------
DELETE FROM applied_project.`wrk_shipped_orders_agg2`;
insert into applied_project.`wrk_shipped_orders_agg2`
select wso.cust_no
		, wso.bill_to_customer
        , wso.scn
        , wso.team
        , wso.build_type
        , wso.bi
        , wso.no_of_prd
        , wso.tot_time
        , wso.tot_units
        , wso.tot_resale
        , wso.tot_integ_cost
        , wso.integ_cost_per_hr
        , wso.integ_cost_per_unit
        , case 
			when wso.integ_cost_per_hr > @icph_80 then 'high complex'
            when wso.integ_cost_per_hr > @icph_60 then 'complex'
            when wso.integ_cost_per_hr > @icph_40  then 'medium complex'
            when wso.integ_cost_per_hr > @icph_20  then 'simple'
            else 'very simple' end as complexity_scale
         , wso.fiscal_month
         , wso.fiscal_qtr
         , -9.9 as total_days_hold
         , -9.9 as workdays_hold
         , 'do-not-use' as new_reason
         , wso.bu
         , NULL as project_cat -- ignoring project_cat as BI is not found to be a good predictor
from
	applied_project.`wrk_shipped_orders_agg1` wso 
group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21
;

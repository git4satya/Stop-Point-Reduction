#! /bin/ksh

mysql -u root -p$(cat .mysqlpass) -h localhost applied_project <<! >> ./${$}_applied_project.log 2>&1
source load_total_orders;
source load_stop_orders;
source applied_project_data_aggregate1.sql;
source export_bill_to_cust_cluster;
source export_team_cluster;

!
## mv to move the exported .csv files to source path
sudo mv /tmp/*.csv .
cat cluster_team.R | R --no-save >> ./${$}_applied_project.log 2>&1
cat cluster_btc.R | R --no-save >> ./${$}_applied_project.log 2>&1 

mysql -u root -p$(cat .mysqlpass) -h localhost applied_project <<! >> ./${$}_applied_project.log 2>&1
source load_cluster_btc;
source load_cluster_team;
source export_wrk_shipped_orders_agg2;
!
## mv to move the exported .csv files to source path
sudo mv /tmp/*.csv .

sed 's/"//g' wrk_shipped_orders6.csv > wrk_shipped_orders6.csv.new


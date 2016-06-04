set.seed(100)
setwd("/Users/admin/Downloads/avnet_csvs/final")

## read train data
trn_data <- read.csv("wrk_shipped_orders6.csv.new",head=T,quote = "", row.names = NULL)
## read test data
tst_data <- read.csv("wrk_shipped_orders_agg2.csv.new",head=T,quote = "", row.names = NULL)

filter_zscore = c("no_of_prd","tot_time","tot_units", "tot_resale", "tot_integ_cost", "integ_cost_per_hr", "integ_cost_per_unit")
filter_others = c("build_type", "complexity_scale", "fiscal_qtr", "bu", "cust_group", "team_group", "new_reason")
filter_others_tst = c("build_type", "complexity_scale", "fiscal_qtr", "bu", "cust_group", "team_group", "new_reason", "scn")

## normalize numeric predictor - training set
v_scaled_trn <- scale(trn_data[filter_zscore], center = T, scale = T)
subset_trn <- cbind(v_scaled_trn, trn_data[filter_others])

## normalize numeric predictor - test set
v_scaled_tst <- scale(tst_data[filter_zscore], center = T, scale = T)
subset_tst <- cbind(v_scaled_tst, tst_data[filter_others_tst])

nrow(subset_trn)
nrow(subset_tst)
#######################################
## Predictors
## 1. build_type (factor)
## 2. no_of_prd
## 3. tot_time
## 4. tot_units
## 5. tot_resale
## 6. tot_integ_cost
## 7. integ_cost_per_hr
## 8. integ_cost_per_unit
## 9. complexity_scale
## 10. fiscal_qtr (factor)
## 11. bu (factor)
## 12. cust_group (factor)
## 13. team_group (factor)
#######################################

## convert categorical values to factor - training set
subset_trn$build_type <- as.factor(subset_trn$build_type)
subset_trn$fiscal_qtr <- as.factor(subset_trn$fiscal_qtr)
subset_trn$bu <- as.factor(subset_trn$bu)
subset_trn$cust_group <- as.factor(subset_trn$cust_group)
subset_trn$team_group <- as.factor(subset_trn$team_group)

## convert categorical values to factor - training set
subset_tst$build_type <- as.factor(subset_tst$build_type)
subset_tst$fiscal_qtr <- as.factor(subset_tst$fiscal_qtr)
subset_tst$bu <- as.factor(subset_tst$bu)
subset_tst$cust_group <- as.factor(subset_tst$cust_group)
subset_tst$team_group <- as.factor(subset_tst$team_group)

#######################################
## Error Fixing                     ###
## Error Desc:                      ###
## Type of predictors in new data do 
## not match that of the training data
########################################
levels(subset_tst$build_type) <- levels(subset_trn$build_type)
levels(subset_tst$fiscal_qtr) <- levels(subset_trn$fiscal_qtr)
levels(subset_tst$bu) <- levels(subset_trn$bu)
levels(subset_tst$cust_group) <- levels(subset_trn$cust_group)
levels(subset_tst$team_group) <- levels(subset_trn$team_group)


## randomForest
library(randomForest)
library(caret)
rfm1 <- randomForest(new_reason ~ ., subset_trn, ntree=1000)
importance(rfm1)

## using caret
varImp(rfm1)
dev.new(width=10, height=8)
varImpPlot(rfm1, main="Feature Importance by Decresing Impurity")
library(gplots)
library(RColorBrewer)
rfm2 <- randomForest(new_reason ~ ., subset_trn, ntree=1000, importance=T)
dev.new(width=10, height=8)
heatmap.2(t(importance(rfm2)[,1:6]), col=brewer.pal(9, "Blues"), dend="none"
          , trace = "none", key=F, margins = c(10,10), main = "Variable Importance")

##################################################
##  Below figure shows mean decrease in node 
##+ impurity. Meaning higher value => greater
##+ decrease in impurity hence higher importance
##################################################
## Overall
## build_type            41.4234
## no_of_prd            174.8786
## tot_time             817.1339
## tot_units            343.3693
## tot_resale           817.3488
## tot_integ_cost       891.9446
## integ_cost_per_hr    766.1196
## integ_cost_per_unit 1036.0619
## complexity_scale     197.5031
## fiscal_qtr           417.5616
## bu                   331.8319
## cust_group           240.1877
## team_group           230.4322
predict1 <- predict(rfm2, newdata = subset_tst, type="prob")
predict2 <- predict(rfm2, newdata = subset_tst, predict.all = T )

n = dim(subset_tst)[1]

predicted_class = rep(0,n)
## predicted membership with maximum probabilities from all trees
for (i in 1:n) {
  tmp = table(predict2$individual[i,])
  predicted_class[i] = names(tmp)[which.max(tmp)]
}

table(subset_tst$new_reason, predicted_class)
CrossTable(subset_tst$new_reason, predicted_class)

## overall accuracy with data
(overall_acc <- (1+14+8783)/9400) ## 0.9359574

## accuracy with of other classes except 'none'
(other_acc <- (1+14)/(153+64+289+74+24)) ## 0.02483444

output_res <- cbind(tst_data, predicted_class)
output_res_with_prob <- cbind(tst_data, predict1)

write.csv(output_res,"output_res.csv",row.names = F, quote = F)
write.csv(output_res_with_prob,"output_res_with_prob.csv",row.names = F, quote = F)
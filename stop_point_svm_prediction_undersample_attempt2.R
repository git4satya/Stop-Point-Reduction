set.seed(100)
library(gmodels)
library(randomForest)
library(caret)
library(gplots)
library(RColorBrewer)

library(e1071)
library(kernlab)

## read train data
setwd("/Users/admin/Downloads/avnet_csvs/final")
trn_data.raw <- read.csv("wrk_shipped_orders_agg2.csv",head=T,quote = "", row.names = NULL)
colnames(trn_data.raw)

## trn_data <- trn_data.raw[trn_data.raw[,16] == "none",]
trn_data <- trn_data.raw
# trn_data <- rbind(trn_data.raw[trn_data.raw[,16] == "nomad",], trn_data.raw[trn_data.raw[,16] == "integration",]
#               , trn_data.raw[trn_data.raw[,16] == "ipf",], trn_data.raw[trn_data.raw[,16] == "none",])
# 
# sales_nr <- trn_data.raw[trn_data.raw[,16] == 'sales',]
# head(sales_nr[1])


filter_zscore = c("no_of_prd","tot_time","tot_units", "tot_resale", "tot_integ_cost", "integ_cost_per_hr", "integ_cost_per_unit")
filter_others = c("build_type", "complexity_scale", "fiscal_qtr", "bu", "cust_group", "team_group", "new_reason", "scn")
filter_others_tst = c("build_type", "complexity_scale", "fiscal_qtr", "bu", "cust_group", "team_group", "new_reason", "scn")

## normalize numeric predictor - training set
v_scaled_trn <- scale(trn_data[filter_zscore], center = T, scale = T)
subset_trn <- cbind(v_scaled_trn, trn_data[filter_others])

## normalize numeric predictor - test set
# v_scaled_tst <- scale(tst_data[filter_zscore], center = T, scale = T)
## subset_tst <- cbind(v_scaled_tst, tst_data[filter_others_tst])

## write count of training set & test set
nrow(subset_trn)
## nrow(subset_tst)

## convert categorical values to factor - training set
subset_trn$build_type <- as.factor(subset_trn$build_type)
subset_trn$fiscal_qtr <- as.factor(subset_trn$fiscal_qtr)
subset_trn$bu <- as.factor(subset_trn$bu)
subset_trn$cust_group <- as.factor(subset_trn$cust_group)
subset_trn$team_group <- as.factor(subset_trn$team_group)
subset_trn$new_reason <- as.factor(subset_trn$new_reason)

## convert categorical values to factor - training set
## subset_tst$build_type <- as.factor(subset_tst$build_type)
## subset_tst$fiscal_qtr <- as.factor(subset_tst$fiscal_qtr)
## subset_tst$bu <- as.factor(subset_tst$bu)
## subset_tst$cust_group <- as.factor(subset_tst$cust_group)
## subset_tst$team_group <- as.factor(subset_tst$team_group)

#######################################
## Error Fixing                     ###
## Error Desc:                      ###
## Type of predictors in new data do 
## not match that of the training data
########################################
## levels(subset_tst$build_type) <- levels(subset_trn$build_type)
## levels(subset_tst$fiscal_qtr) <- levels(subset_trn$fiscal_qtr)
## levels(subset_tst$bu) <- levels(subset_trn$bu)
## levels(subset_tst$cust_group) <- levels(subset_trn$cust_group)
## levels(subset_tst$team_group) <- levels(subset_trn$team_group)

# split the training set to training & cross validation ##
train.prop = .75
train.filter <- sample(nrow(subset_trn), nrow(subset_trn)*train.prop)
t_subset_trn <- subset_trn[train.filter,]
cv_subset_trn <- subset_trn[-train.filter,]

table(trn_data.raw[,16])
table(t_subset_trn[,14])
table(cv_subset_trn[,14])

table(t_subset_trn[,14])
under.sample.prop = 1
none <- t_subset_trn[t_subset_trn[,14] == "none",]
us.filter <- sample(nrow(none), nrow(none)*under.sample.prop)
t_subset_trn_us <- none[us.filter,]
not_none <- t_subset_trn[t_subset_trn[,14] != "none",]
t_subset_trn_us <- rbind(t_subset_trn_us, not_none)

## table(t_subset_trn_us[,14])

## table(none[,14])
## table(t_subset_trn_us)

##  dynamically calculate misclassification cost based on
##+ class distribution
correction = 0.001  ## correction to avoid division by zero error
p <- prop.table(table(t_subset_trn_us[,14])) + correction
weights <- round(1/p)
## weights <- c(10,15,5, 1, 20,25)
weights <- c(10,15,0, 1, 20,25)
names(weights) <- names(p)

## oversample "nomad"
nomad <- t_subset_trn_us[t_subset_trn_us[,14] == "nomad",]
if (nrow(nomad)!=0) 
  for (i in 1:weights["nomad"]) 
    t_subset_trn_us <- rbind(t_subset_trn_us, nomad) 

## oversample "integration"
integration <- t_subset_trn_us[t_subset_trn_us[,14] == "integration",]
## for (i in 1:weights["integration"]) t_subset_trn_us <- rbind(t_subset_trn_us, integration) 
if (nrow(integration)!=0) 
  for (i in 1:weights["integration"]) 
    t_subset_trn_us <- rbind(t_subset_trn_us, integration)

## oversample "ipf"
ipf <- t_subset_trn_us[t_subset_trn_us[,14] == "ipf",]
if (nrow(ipf)!=0) 
  for (i in 1:weights["ipf"]) 
    t_subset_trn_us <- rbind(t_subset_trn_us, ipf) 

## oversample "sales"
sales <- t_subset_trn_us[t_subset_trn_us[,14] == "sales",]
if (nrow(sales)!=0) 
  for (i in 1:weights["sales"]) 
    t_subset_trn_us <- rbind(t_subset_trn_us, sales) 

## oversample "warehouse"
warehouse <- t_subset_trn_us[t_subset_trn_us[,14] == "warehouse",]
if (nrow(warehouse)!=0) 
  for (i in 1:weights["warehouse"]) 
    t_subset_trn_us <- rbind(t_subset_trn_us, warehouse) 

# oversample "engineering"
# engineering <- t_subset_trn_us[t_subset_trn_us[,14] == "engineering",]
# if (nrow(engineering)!=0) 
#   for (i in 1:weights["engineering"]) 
#     t_subset_trn_us <- rbind(t_subset_trn_us, engineering) 
rbf <- rbfdot(sigma = 0.1)
rbf

svp1 <- ksvm(new_reason ~ integ_cost_per_unit+tot_integ_cost+tot_time+tot_resale+integ_cost_per_hr+fiscal_qtr+bu+complexity_scale+team_group+cust_group
             , data = t_subset_trn_us, kernel=rbf, C=5, prob.model=T, cross = 5 )

pred1 <- predict(svp1, cv_subset_trn[,-14], type="probabilities")
pred2 <- predict(svp1, cv_subset_trn[,-14])

fitted(svp1)

table(cv_subset_trn$new_reason)
(ct1=table(cv_subset_trn$new_reason, pred2))
(recall1 = diag(round(prop.table(ct1,1),2)))
(precision1 = diag(round(prop.table(ct1,2),2)))
## CrossTable(cv_subset_trn$new_reason, predicted_class0)
CrossTable(subset_trn$new_reason, pred2, prop.r=T, expected = F, prop.c = F, prop.t = F, prop.chisq = F, total.c = F)

## randomForest
# rfm1 <- randomForest(new_reason ~ integ_cost_per_unit+tot_integ_cost+tot_time+tot_resale+integ_cost_per_hr+fiscal_qtr+bu+complexity_scale+team_group+cust_group
#                      , t_subset_trn_us, ntree=1000, nodesize = 1, maxnodes = 40, replace = T)
## preint feature importance
importance(rfm1)

## using caret plot feature importance chart
varImp(rfm1)
dev.new(width=10, height=8)
varImpPlot(rfm1, main="Feature Importance by Decresing Impurity")

rfm2 <- randomForest(new_reason ~ integ_cost_per_unit+tot_integ_cost+tot_time+tot_resale+integ_cost_per_hr+fiscal_qtr+bu+complexity_scale+team_group+cust_group
                     , t_subset_trn_us, ntree=1000, importance=T, nodesize = 1, maxnodes = 40, replace = T)

## plot the heatmap in a new window
dev.new(width=10, height=8)
heatmap.2(t(importance(rfm2)[,1:6]), col=brewer.pal(9, "Blues"), dend="none"
          , trace = "none", key=F, margins = c(10,10), main = "Variable Importance")

## predict the accuracy in the validation set
predict0 <- predict(rfm2, newdata = cv_subset_trn, predict.all = T)

## predict for actual test data
## predict1 <- predict(rfm2, newdata = subset_tst, type="prob")
## predict2 <- predict(rfm2, newdata = subset_tst, predict.all = T )

## print the result of cross-validation ##
n0 = dim(cv_subset_trn)[1]
predicted_class0 = rep(0,n0)
## predicted membership with maximum probabilities from all trees
for (i in 1:n0) {
  tmp1 = table(predict0$individual[i,])
  predicted_class0[i] = names(tmp1)[which.max(tmp1)]
}

(ct=table(cv_subset_trn$new_reason, predicted_class0))
(recall1 = diag(round(prop.table(ct,1),2)))
(precision1 = diag(round(prop.table(ct,2),2)))
## CrossTable(cv_subset_trn$new_reason, predicted_class0)
CrossTable(cv_subset_trn$new_reason, predicted_class0, prop.r=T, expected = F, prop.c = F, prop.t = F, prop.chisq = F, total.c = F)


## get actual result on test data ##
### n = dim(subset_tst)[1]

### predicted_class = rep(0,n)
## predicted membership with maximum probabilities from all trees
### for (i in 1:n) {
### tmp = table(predict2$individual[i,])
### predicted_class[i] = names(tmp)[which.max(tmp)]
### }

output_res <- cbind(cv_subset_trn, predicted_class0)
output_res_with_prob <- cbind(cv_subset_trn, predict0)

write.csv(output_res,"output_res_us_os.us.csv",row.names = F, quote = F)
write.csv(output_res_with_prob,"output_res_with_prob_us_os.us.csv",row.names = F, quote = F)
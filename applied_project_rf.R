set.seed(100)
setwd("/Users/admin/Downloads/avnet_csvs/final")
t_data <- read.csv("wrk_shipped_orders6.csv.new",head=T,quote = "", row.names = NULL)

filter1 = c("build_type", "no_of_prd","tot_time","tot_units", "tot_resale", "tot_integ_cost", "integ_cost_per_hr"
            , "integ_cost_per_unit", "complexity_scale", "fiscal_qtr", "new_reason", "bu", "cust_group", "team_group")
subset0 <- t_data[filter1]
head(subset0)

filter_zscore = c("no_of_prd","tot_time","tot_units", "tot_resale", "tot_integ_cost", "integ_cost_per_hr", "integ_cost_per_unit")
filter_others = c("build_type", "complexity_scale", "fiscal_qtr", "bu", "cust_group", "team_group", "new_reason")

v_scaled <- scale(subset0[filter_zscore], center = T, scale = T)
subset1 <- cbind(v_scaled, subset0[filter_others])
head(subset1)
## sapply(t_data, function(x) sum(is.na(x)))
## sapply(t_data, function(y) length(unique(y)))
## sub_data <- subset(training.data.raw, select = c(2,4,13,14,19))
## nrow(sub_data)
#### train <- sub_data[1:68884,]
#### test <- sub_data[68885:86105,]
nrow(subset1)
## trying to train with smaller data set as it was taking too long
train.prop = .75
train.filter <- sample(nrow(subset1), nrow(subset1)*train.prop)
train_data <- subset1[train.filter,]
test_data <- subset1[-train.filter,]
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
train_data$build_type <- as.factor(train_data$build_type)
train_data$fiscal_qtr <- as.factor(train_data$fiscal_qtr)
train_data$bu <- as.factor(train_data$bu)
train_data$cust_group <- as.factor(train_data$cust_group)
train_data$team_group <- as.factor(train_data$team_group)

## load nnet package
## library(nnet)
## train model
## trained_m <- multinom(new_reason ~ ., data = train_data, model = T)
## above was not working trying glm instead
## train$y <- ifelse(train$new_reason == 'nomad', 1, 0)
## trained_m <- glm(y ~ bill_to_customer + team + integ_cost_per_unit + complexity_scale , data = train, family = binomial)
## summary(trained_m)
## anova(trained_m, test = "Chisq")

## library(VGAM)
## trained_m <- vglm(new_reason ~ bill_to_customer + team + bi, data = train, multinomial) 
## summary(trained_m)


## randomForest
library(randomForest)
library(caret)
rfm1 <- randomForest(new_reason ~ ., train_data, ntree=500)
summary(rfm1)
importance(rfm1)

## using caret
varImp(rfm1)
dev.new(width=10, height=8)
varImpPlot(rfm1, main="Feature Importance by Decresing Impurity")
library(gplots)
library(RColorBrewer)
rfm2 <- randomForest(new_reason ~ ., train_data, ntree=500, importance=T)
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
predict1 <- predict(rfm1, newdata = test_data, type="prob")
predict2 <- predict(rfm1, newdata = test_data, predict.all = T )

n = dim(test_data)[1]

predicted_class = rep(0,n)
## predicted membership with maximum probabilities from all trees
for (i in 1:n) {
  tmp = table(predict2$individual[i,])
  predicted_class[i] = names(tmp)[which.max(tmp)]
}

table(test_data$new_reason, predicted_class)
CrossTable(test_data$new_reason, predicted_class)

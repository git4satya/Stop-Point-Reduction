library(datasets)
data <- airquality
data[4:10, 3] <- rep(NA,7)
data[1:5,4] <- NA

colnames(data)

head(data <- data[-c(5,6)])
summary(data)

pMiss <- function(x) {sum(is.na(x))/length(x) * 100}
apply(data, 2, pMiss)
apply(data, 1, pMiss)

library(mice)
md.pattern(data)

install.packages("VIM")
library(VIM)

aggr_plot <- aggr(data, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

colnames(data)
marginplot(data[c(1,2)])
## if MCAR assumption is correct both red & blue plots should look similar
## as in this case
tempData <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 50)
summary(tempData)
tempData$imp$Ozone

head(data,25)

tempData$method

## complete the dataset using mice imputation
completedData <- complete(tempData,1)
head(completedData)

## scatter plot to check the distribution
xyplot(tempData, Ozone ~ Wind + Temp + Solar.R, pch=18, cex=1)

## density plot
densityplot(tempData)

### stripplot
stripplot(tempData, pch = 20, cex = 1.2)

## use imputed data set for modeling and pool the result
modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))

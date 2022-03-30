## 2.1.3.

library(randomForest)
library(data.table)
library(readxl)

setwd(paste(getwd(),'/Data',sep=""))

source("../helperFns.R")

df <- fread("uci_online_retail_cleaned_CLV.csv")
#View(df)
## Drop the first column
df <- df[,-1]

## Check the type of each column and factorise as necessary
sapply(df,class)
df$InvoiceNo <- factor(df$InvoiceNo)
df$StockCode <- factor(df$StockCode)
df$CustomerID <- factor(df$CustomerID)
df$Description <- factor(df$Description)
df$Country <- factor(df$Country)
df$InvoiceDate <- as.POSIXct(df$InvoiceDate,format="%Y-%m-%d %H:%M:%S",tz="	Europe/London")
df$InvoiceDate_DayofWeek = factor(weekdays(df$InvoiceDate))
df$InvoiceDate_DayofMonth = as.numeric(format(df$InvoiceDate, format = "%d"))
df$InvoiceDate_MonthPeriod = cut(df$InvoiceDate_DayofMonth, breaks=c(0,10,20,31), labels=c("Beginning of Month", "Middle of Month", "End of Month"))
df$InvoiceDate_MonthName = factor(months(df$InvoiceDate))
df$InvoiceDate_HourofDay = as.numeric(format(df$InvoiceDate, format = "%H"))
df$InvoiceDate_DayPeriod = cut(df$InvoiceDate_HourofDay, breaks=c(-1,6,12,18,24), labels=c("Midnight", "Morning", "Afternoon", "Evening"))


## Calculate CLV by simply multiplying all 3 variables & then normalising it between 0 and 1
df$clv <- df$FREQUENCY*df$MONEY*(df$`NEW RECENCY`/60/24)

## Clustering
## K-Means Clustering to create 3 segments
library(factoextra)
library(cluster)
library(dplyr)
library(tidyverse)

## Find out optimal number of clusters using elbow method
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = df$clv, centers = k,nstart=25)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)

## Optimal Clusters is at the elbow, so clusters 3 or 4 
## https://hastie.su.domains/ISLR2/ISLRv2_website.pdf
# https://stackoverflow.com/questions/39906180/consistent-cluster-order-with-kmeans-in-r
## To minimise within-cluster sum of squares using the nstart function
## Recommend to use 20 or 50 for nstart otherwise an undesirable local optimum may be obtained
set.seed(2014)
kmCenters <- kmeans(df$clv,centers=4,nstart=25)$centers
kmCenters = sort(kmCenters)
km = kmeans(df$clv,centers=kmCenters,nstart=25)
km
# Cluster Size: 1. 357326 | 2. 24634 | 3. 9602 | 4. 2709
# Cluster centers: 1. 244063664 | 2. 4955208900 | 3. 16425619899 | 4. 55324774302
## Visualise the clusters
par(mfrow=c(1,2))
plot(df$clv,col=(km$cluster+1),main = "K-Means Clustering Results with K=4",xlab = "",ylab="",pch=20,cex=2)
abline(h = km$centers, col = 1:2, pch = 8,cex = 2)
## Analyse within-cluster sum of squares & total within-cluster sum of squares
km$withinss ## [1] 6.833492e+22 5.642723e+22 9.335332e+22 1.686347e+19
km$tot.withinss ## [1] 2.181323e+23

## K-Means is a multi-variate clustering method, thus might not be suitable for 1-d or 1 variable data
## Test a second model on ckmeans.1d.dp, an optimal 1-d kmeans clustering
# ckmeans.1d.dp is a one-dimensional example with a two-component Gaussian mixture model
library(Ckmeans.1d.dp)
set.seed(2014)
ckm <- Ckmeans.1d.dp(df$clv, 4) ## Not required to specify nstart since it will auto-optimise
ckm
# Cluster Size: 1. 358032 | 2. 24865 | 3. 8665 | 4. 2709
# Cluster centers: 1. 248945129 | 2. 5242455908 | 3. 17023857173 | 4. 55324774302
# Slight difference from above km$centers
## Visualise the clusters
par(mfrow=c(1,2))
plot(df$clv,col=(ckm$cluster+1),main = "K-Means Clustering Results with K=4",xlab = "",ylab="",pch=20,cex=2)
abline(h = ckm$centers, col = 1:2, pch = 8,cex = 2)
ckm$withinss ## [1] 7.265291e+22 8.388719e+22 6.157451e+22 1.686347e+19
ckm$tot.withinss ## [1] 2.181315e+23
# Total within-cluster sum of squares is the same
# Individual within-cluster sum of squares differs

km
ckm



df$cluster = factor(km$cluster)
summary(df$cluster)

for (k in 1:10) {
  test = kmeans(df$clv,centers=k,nstart=25)
  print(test$tot.withinss)
}


###########################################################################################################










## Parse the cluster coefficients back to original dataframe


## Predicting CLV cluster with Logistic Regression
library(nnet)
## Train-Test Split
generateTrainTest(df,0.7)
summary(train)
summary(test)

logreg <- multinom(cluster~ Quantity+InvoiceDate+UnitPrice+Country+ProductVariations, data=train)
summary(logreg)

## Odds Ratio
OR.DelStatus <- exp(coef(logreg))
OR.DelStatus

# 95% Confidence interval
OR.DelStatus.CI <- exp(confint(logreg))
OR.DelStatus.CI

## Predict on trainset
train.logreg.predict <- predict(logreg, newdata=train, type="class")
results.train <- data.frame(train$cluster,train.logreg.predict)
results.train
accuracy.train <- mean(results.train$train.cluster == results.train$train.logreg.predict)
accuracy.train

## Predict on testset
test.logreg.predict <- predict(logreg, newdata=test, type="class")
results.test <- data.frame(test$cluster,test.logreg.predict)
results.test
accuracy.test <- mean(results.test$test.cluster ==results$test.logreg.predict)
accuracy.test

## Accuracy
accuracy <- data.frame(accuracy.train,accuracy.test)
print(accuracy)

## Confusion Matrix
cm <- table(test$cluster,test.logreg.predict,deparse.level=2)
cm

'
## Calculation of accuracy, precision, recall (WITH REGARDS TO OBSERVATIONS OF LATE DELIVERY)
LogReg.accuracy <- (LogReg.Confusion.matrix[1,1]+LogReg.Confusion.matrix[2,2]+
                      LogReg.Confusion.matrix[3,3]+LogReg.Confusion.matrix[4,4])/sum(LogReg.Confusion.matrix)
LogReg.precision <- LogReg.Confusion.matrix[3,3]/sum(LogReg.Confusion.matrix[c(1:4),3])
LogReg.recall <- LogReg.Confusion.matrix[3,3]/sum(LogReg.Confusion.matrix[3,c(1:4)])

c(LogReg.accuracy, LogReg.precision, LogReg.recall)
'

## MARS
library(earth)
mars <- earth(clv_normalized~Quantity+InvoiceDate+UnitPrice+Country+ProductVariations,degree=1,data=train)
summary(mars)
mars.predict <- predict(mars,newdata=test)
mars.predict
RMSE.mars <- round(sqrt(mean((df$clv_normalized-mars.predict)^2))) ## Error
RMSE.mars ## ????
varimpt <- evimp(mars)
print(varimpt)

## Random Forest
library(randomForest)
rf <- randomForest(cluster~Quantity+InvoiceDate+UnitPrice+Country+ProductVariations, data=train, importance=T)
rf



'

# MARS on the 4 main variables degree 1 ----------------------------------------------
m.mars1 <- earth(resale_price ~ 
                   floor_area_sqm + 
                   remaining_lease_years + 
                   town + 
                   storey_range , degree=1, data=data1)

summary(m.mars1)

m.mars1.yhat <- predict(m.mars1)

RMSE.mars1 <- round(sqrt(mean((data1$resale_price - m.mars1.yhat)^2)))


m.mars2 <- earth(resale_price ~ 
                   floor_area_sqm + 
                   remaining_lease_years + 
                   town + 
                   storey_range , degree=2, data=data1)

summary(m.mars2)

m.mars2.yhat <- predict(m.mars2)

RMSE.mars2 <- round(sqrt(mean((data1$resale_price - m.mars2.yhat)^2)))

# MARS Prediction for Flat in Clementi, 100 sq metres, 19-21 storey, 80 yrs lease remaining --
testcase <- data.frame(town = "CLEMENTI",
                       floor_area_sqm = 100,
                       storey_range = "19 TO 21",
                       remaining_lease_years = 80)

m.mars1.yhat.test <-  predict(m.mars1, newdata = testcase)

m.mars2.yhat.test <-  predict(m.mars2, newdata = testcase)


# Estimated Variable Importance in degree 2 MARS
varimpt <- evimp(m.mars2)
print(varimpt)
## Floor Area is relatively most impt, followed by remaining lease.

'
'









## Random Forest
library(randomForest)
rf <- randomForest(cluster~Quantity+InvoiceDate+UnitPrice+Country+ProductVariations, data=train, importance=T)
rf

## OOB MSE = 1611604542 ==> OOB RMSE = $40,145

plot(m.RF1)
## Confirms error stablised before 500 trees.

m.RF1.yhat <- predict(m.RF1, newdata = testset)

RMSE.test.RF1 <- round(sqrt(mean((testset$resale_price - m.RF1.yhat)^2)))

var.impt.RF <- importance(m.RF1)

varImpPlot(m.RF1, type = 1)

'


'
B <- c(25, 25, 25, 100, 100, 100, 500, 500, 500)

# Num of X variables in dataset
m <- ncol(heart.df)-1

RSF <- rep.int(c(1, floor(sqrt(m)), m), times=3)

OOB.error <- seq(1:9)

set.seed(1)  # for Bootstrap sampling & RSF selection.

for (i in 1:length(B)) {
  m.RF <- randomForest(AHD ~ . , data = heart.df,
                       mtry = RSF[i],
                       ntree = B[i],
                       na.action = na.omit)
  OOB.error[i] <- m.RF$err.rate[m.RF$ntree, 1]
}
## OOB Error across all trees stored in the last row in err.rate.

results <- data.frame(B, RSF, OOB.error)
## trying different seeds, OOB error is relatively low for B = 500, RSF = 3.
## these are default values in randomForest() function.

m.RF.final <- randomForest(AHD ~ . , data = heart.df, na.action = na.omit, importance = T)

m.RF.final  ## Confirms defaults are B = 500, RSF = int(sqrt(m)) = 3

var.impt <- importance(m.RF.final)

varImpPlot(m.RF.final, type = 1)

'








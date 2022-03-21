## 2.1.3.

library(randomForest)
library(data.table)
library(readxl)

setwd(paste(getwd(),'/Data',sep=""))

source("../helperFns.R")

df <- fread("uci_online_retail_cleaned_CLV.csv")
View(df)
## Drop the first column
df <- df[,-1]

## Check the type of each column and factorise as necessary
sapply(df,class)
df$InvoiceNo <- factor(df$InvoiceNo)
df$StockCode <- factor(df$StockCode)
df$CustomerID <- factor(df$CustomerID)
df$Description <- factor(df$Description)
df$Country <- factor(df$Country)

## Calculate CLV by simply multiplying all 3 variables & then normalising it between 0 and 1
df$clv <- df$FREQUENCY*df$MONEY*df$`NEW RECENCY`
df$clv_normalized <- (df$clv-min(df$clv))/(max(df$clv)-min(df$clv))

## Clustering
## Clustering to create 3 segments
library(factoextra)
library(cluster)
library(dplyr)
## Subset the cluster to remove all categorical variables
cluster <- select(df,clv_normalized)
km <- kmeans(cluster,centers=3,nstart=25)
km


## Thresholds
'Cluster means:
  clv_normalized
1    0.006084544
2    0.999195326
3    0.189244313'

## Parse the cluster coefficients back to original dataframe
df$cluster = km$cluster
df$cluster <- factor(df$cluster)

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

## Random Forest






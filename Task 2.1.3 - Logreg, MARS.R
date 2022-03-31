## 2.1.3.

library(randomForest)
library(data.table)
library(readxl)

tryCatch(setwd(paste(getwd(),'/Data',sep="")), error = function(e) {    # set working directory to 
    paste('Directory is:', getwd())                                     # the 'Data' folder in the
})                                                                      # group project.

source("../helperFns.R")    # import list of helper functions we've written separately

## UCI Dataset -----------------------------------------------------
df <- fread("uci_online_retail_cleaned_CLV.csv")
#View(df)

## Check the type of each column and factorise as necessary
sapply(df,class)
df$InvoiceNo <- factor(df$InvoiceNo)
df$StockCode <- factor(df$StockCode)
df$CustomerID <- factor(df$CustomerID)
df$Description <- factor(df$Description)
df$Country <- factor(df$Country)

df$InvoiceDate <- as.POSIXct(df$InvoiceDate,format="%Y-%m-%d %H:%M:%S",tz="Europe/London")
df$InvoiceDate_DayofWeek = factor(weekdays(df$InvoiceDate))
df$InvoiceDate_DayofMonth = as.numeric(format(df$InvoiceDate, format = "%d"))
df$InvoiceDate_MonthPeriod = cut(df$InvoiceDate_DayofMonth, breaks=c(0,10,20,31), labels=c("Beginning of Month", "Middle of Month", "End of Month"))
df$InvoiceDate_MonthName = factor(months(df$InvoiceDate))
df$InvoiceDate_HourofDay = as.numeric(format(df$InvoiceDate, format = "%H"))
df$InvoiceDate_DayPeriod = cut(df$InvoiceDate_HourofDay, breaks=c(-1,6,12,18,24), labels=c("Midnight", "Morning", "Afternoon", "Evening"))


## Calculate CLV by simply multiplying all 3 variables & then normalising it between 0 and 1
df$clv <- df$FREQUENCY_normalised*df$MONEY_normalised*df$RECENCY_normalised

## Clustering -----------------------------------------------------
# K-Means Clustering to create 3 segments
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

## Optimal Clusters is at the elbow, so clusters 2 or 3 
## https://hastie.su.domains/ISLR2/ISLRv2_website.pdf
# https://stackoverflow.com/questions/39906180/consistent-cluster-order-with-kmeans-in-r
## To minimise within-cluster sum of squares using the nstart function
## Recommend to use 20-50 for nstart otherwise an undesirable local optimum may be obtained
set.seed(2014)
kmCenters <- kmeans(df$clv,centers=3,nstart=25)$centers
kmCenters = sort(kmCenters)
km = kmeans(df$clv,centers=kmCenters,nstart=25)
km
# Cluster Size: 1. 287737 | 2. 78847 | 3. 27687
# Cluster centers: 1. 3.137792 | 2. 7.885030 | 3. 37.624167
## Visualise the clusters
par(mfrow=c(1,2))
plot(df$clv,col=(km$cluster+1),main = "K-Means Clustering Results",xlab = "",ylab="",pch=20,cex=2)
abline(h = km$centers, col = 1:2, pch = 8,cex = 2)
## Analyse within-cluster sum of squares & total within-cluster sum of squares
km$withinss ## [1] 460984.1  854637.4 3751597.6
km$tot.withinss ## [1] 5067219

## K-Means is a multi-variate clustering method, thus might not be suitable for 1-d or 1 variable data
## Test a second model on ckmeans.1d.dp, an optimal 1-d kmeans clustering
# ckmeans.1d.dp is a one-dimensional example with a two-component Gaussian mixture model
library(Ckmeans.1d.dp)
set.seed(2014)
ckm <- Ckmeans.1d.dp(df$clv, 3) ## Not required to specify nstart since it will auto-optimise
ckm
# Cluster Size: 1. 364207 | 2. 22456 | 3. 7608
# Cluster centers: 1. 4.052515 | 2. 29.905174 | 3. 55.042750
# Slight difference from above km$centers
## Visualise the clusters
par(mfrow=c(1,2))
plot(df$clv,col=(ckm$cluster+1),main = "K-Means Clustering Results",xlab = "",ylab="",pch=20,cex=2)
abline(h = ckm$centers, col = 1:2, pch = 8,cex = 2)
ckm$withinss ## [1] 2071400.31  759478.47   50486.51
ckm$tot.withinss ## [1] 2881365
# Total within-cluster sum of squares is the same
# Individual within-cluster sum of squares differs
## Parse the cluster coefficients back to original dataframe
df$cluster = factor(ckm$cluster)
summary(df$cluster)


## Train-Test Split
generateTrainTest(df,0.7)
summary(train)
summary(test)


## Logistic Regression: Original Trainset -----------------------------------------------------
library(nnet)
logreg <- multinom(cluster~ Quantity+UnitPrice+ProductVariations, data=train)
summary(logreg)

## Odds Ratio
OR.logreg <- exp(coef(logreg))
OR.logreg

# 95% Confidence interval
OR.logreg.CI <- exp(confint(logreg))
OR.logreg.CI

# p-value
z <- summary(logreg)$coefficients/summary(logreg)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue


logreg.step <- step(logreg)
logreg.step

## Predict on trainset
predict.cluster.train <- predict(logreg.step)
predict.cluster.train

logreg.cm.train <- table(`Trainset Actuals` = train$cluster, `Model Prediction` = predict.cluster.train, deparse.level = 2)
logreg.cm.train

accuracy.logreg.train <- mean(predict.cluster.train == train$cluster)
accuracy.logreg.train

## Predict on testset
predict.cluster.test <- predict(logreg.step, newdata=test)
predict.cluster.test

logreg.cm.test <- table(`Testset Actuals` = test$cluster, `Model Prediction` = predict.cluster.test, deparse.level = 2)
logreg.cm.test

accuracy.logreg.test <- mean(predict.cluster.test == test$cluster)
accuracy.logreg.test

## MARS: Original Trainset -----------------------------------------------------
library(earth)
set.seed(2014)
mars <- earth(cluster~Quantity+UnitPrice+Country+ProductVariations,degree=2,data=train)
summary(mars)
mars.predict <- predict(mars)
mars.predict
df1 <- as.data.frame(mars.predict)
View(df1)

?earth



mars.predict <- predict(mars,newdata=test)
mars.predict
RMSE.mars <- round(sqrt(mean((df$cluster-mars.predict)^2))) ## Error
RMSE.mars ## ????
varimpt <- evimp(mars)
print(varimpt)


## Data is skewed towards cluster 1, thus attempt to create balanced dataset to train the model
# Create balanced trainset  --------------------------------
# Random sample from majority class Default = No and combine with Default = Yes to form new trainset
majority <- train[cluster == 1]
middle <- train[cluster == 2]
minority <- train[cluster == 3] 
# Randomly sample the row numbers to be in trainset. Same sample size as minority cases. 
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority)) 
chosen2 <- sample(seq(1:nrow(middle)), size = nrow(minority))
# Subset the original trainset based on randomly chosen row numbers. 
majority.chosen <- majority[chosen] 
middle.chosen <- middle[chosen2]
# Combine two data tables by appending the rows 
train.bal <- rbind(majority.chosen,middle.chosen, minority) 
summary(train.bal) 

## Logistic Regression: Balanced Trainset -----------------------------------------------------
set.seed(2014)
logreg.bal <- multinom(cluster~ Quantity+UnitPrice+ProductVariations, data=train.bal)
summary(logreg.bal)

## Odds Ratio
OR.logreg.bal <- exp(coef(logreg.bal))
OR.logreg.bal

# 95% Confidence interval
OR.logreg.CI.bal <- exp(confint(logreg.bal))
OR.logreg.CI.bal

# p-value
z <- summary(logreg.bal)$coefficients/summary(logreg.bal)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue

logreg.step.bal <- step(logreg.bal)
logreg.step.bal

## Predict on trainset
predict.cluster.train.bal <- predict(logreg.step.bal)
predict.cluster.train.bal

logreg.cm.train.bal <- table(`Trainset Actuals` = train.bal$cluster, `Model Prediction` = predict.cluster.train.bal, deparse.level = 2)
logreg.cm.train.bal

accuracy.logreg.train.bal <- mean(predict.cluster.train.bal == train.bal$cluster)
accuracy.logreg.train.bal ## [1] 0.3929632

## Predict on testset
predict.cluster.test.bal <- predict(logreg.step.bal, newdata=test)
predict.cluster.test.bal

logreg.cm.test.bal <- table(`Testset Actuals` = test$cluster, `Model Prediction` = predict.cluster.test.bal, deparse.level = 2)
logreg.cm.test.bal

accuracy.logreg.test.bal <- mean(predict.cluster.test.bal == test$cluster)
accuracy.logreg.test.bal ## [1] 0.6791143


## MARS: Balanced Trainset -----------------------------------------------------








## Olist Dataset -----------------------------------------------------
df1 <- fread("Orders_merged_CLV.csv")
summary(df1)
## Calculate CLV by simply multiplying all 3 variables & then normalising it between 0 and 1
df1$clv <- df1$FREQUENCY_normalised*df1$MONEY_normalised*df1$RECENCY_normalised

## Clustering -----------------------------------------------------
# K-Means Clustering to create 3 segments
## Find out optimal number of clusters using elbow method
tot_withinss1 <- map_dbl(1:10,  function(k){
  model <- kmeans(x = df1$clv, centers = k,nstart=25)
  model$tot.withinss
})

elbow_df1 <- data.frame(
  k = 1:10,
  tot_withinss1 = tot_withinss1
)

ggplot(elbow_df1, aes(x = k, y = tot_withinss1)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)

## Optimal Clusters is at the elbow, so clusters 3 or 4 
## https://hastie.su.domains/ISLR2/ISLRv2_website.pdf
# https://stackoverflow.com/questions/39906180/consistent-cluster-order-with-kmeans-in-r
## To minimise within-cluster sum of squares using the nstart function
## Recommend to use 20-50 for nstart otherwise an undesirable local optimum may be obtained
set.seed(2014)
kmCenters1 <- kmeans(df1$clv,centers=3,nstart=25)$centers
kmCenters1 = sort(kmCenters1)
km1 = kmeans(df1$clv,centers=kmCenters1,nstart=25)
km1
# Cluster Size: 1. 94327 | 2. 12766 | 3. 730
# Cluster centers: 1. 1.121630 | 2. 1.813204 | 3. 4.103957
## Visualise the clusters
par(mfrow=c(1,2))
plot(df1$clv,col=(km1$cluster+1),main = "K-Means Clustering Results",xlab = "",ylab="",pch=20,cex=2)
abline(h = km1$centers, col = 1:2, pch = 8,cex = 2)
## Analyse within-cluster sum of squares & total within-cluster sum of squares
km1$withinss ## [1] 1159.946 1326.407 1372.275
km1$tot.withinss ## [1] 3858.628

## K-Means is a multi-variate clustering method, thus might not be suitable for 1-d or 1 variable data
## Test a second model on ckmeans.1d.dp, an optimal 1-d kmeans clustering
# ckmeans.1d.dp is a one-dimensional example with a two-component Gaussian mixture model
set.seed(2014)
ckm1 <- Ckmeans.1d.dp(df1$clv, 3) ## Not required to specify nstart since it will auto-optimise
ckm1
ckm1$size
# Cluster Size: 1. 94327 | 2. 12766 | 3. 730
# Cluster centers: 1. 1.121630 | 2. 1.813204 | 3. 4.103957
# Slight difference from above km$centers
## Visualise the clusters
par(mfrow=c(1,2))
plot(df1$clv,col=(ckm1$cluster+1),main = "CK-Means Clustering Results",xlab = "",ylab="",pch=20,cex=2)
abline(h = ckm1$centers, col = 1:2, pch = 8,cex = 2)
ckm1$withinss ## [1] 1159.946 1326.407 1372.275
ckm1$tot.withinss ## [1] 3858.628
# Total within-cluster sum of squares is the same
# Individual within-cluster sum of squares is the same as well
## Parse the cluster coefficients back to original dataframe
df1$cluster = factor(ckm1$cluster)
summary(df1$cluster)





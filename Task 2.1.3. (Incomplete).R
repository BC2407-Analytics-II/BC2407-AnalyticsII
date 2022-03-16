## 2.1.3.

library(randomForest)
library(data.table)
library(readxl)

setwd("C:/Users/Alvin Lim/Documents/GitHub/BC2407-AnalyticsII/Data")

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

## Predicting FREQUENCY and MONEY with Linear Regression
set.seed(2014)
lm.frequency <- lm(FREQUENCY ~ Quantity + InvoiceDate + UnitPrice + ProductVariations + Country, data = df)
summary(lm1)
lm.money <- lm(MONEY ~ Quantity + InvoiceDate + UnitPrice + ProductVariations + Country, data = df)
summary(lm1)

RMSE.frequency <- sqrt(mean(residuals(lm.frequency)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(lm.frequency)))  # Check Min Abs Error and Max Abs Error.

RMSE.money <- sqrt(mean(residuals(lm.money)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(lm.money)))  # Check Min Abs Error and Max Abs Error.
c(RMSE.frequency,RMSE.money)


## To calculate CLV before clustering

df$clv <- df$FREQUENCY*df$MONEY
df$clv_normalized <- (df$clv-min(df$clv))/(max(df$clv)-min(df$clv))




## Clustering to create 3 segments
library(factoextra)
library(cluster)
library(dplyr)
## Subset the cluster to remove all categorical variables
cluster.frequency <- select(df,Quantity, UnitPrice,ProductVariations,FREQUENCY,MONEY)
km.frequency <- kmeans(cluster.frequency,centers=3,nstart=25)
km.frequency

cluster.money <- select(df,Quantity, UnitPrice,ProductVariations,MONEY)
km.money <- kmeans(cluster.money,centers=3,nstart=25)
km.money

## fviz_cluster(km, data = cluster.df)

"
Cluster means:
  Quantity UnitPrice ProductVariations FREQUENCY      MONEY
1 10.05518  2.836167          1.175319   270.044   3189.716
2 23.89974  2.711036          1.183323  3760.538  31365.914
3 29.49101  3.115258          1.217397  2733.040  69443.534
4 53.20808  3.196216          1.115061  3963.844 173386.379
"

## Parse the cluster coefficients back to original dataframe
df$cluster = km$cluster












#################################  B C 2 4 0 7   S E M I N A R   1  #################################
###########################################  T E A M   7  ###########################################
################################### Instructor: Prof Neumann Chew ###################################

# <-------- This R script is formatted to fit on a window of width specified by this line --------> #

# set working directory to the relative path of the '2. Datasets' folder of the grp project
tryCatch(setwd(paste(getwd(),'./../2. Datasets',sep="")), error = function(e) {   
    paste('Directory is:', getwd())                                               
})                                                                                

# import list of helper functions we've written separately
source("./../3. Analysis Scripts/helperFns.R")

library(randomForest)       # for random forest
library(data.table)         # for data manipulation 1
library(dplyr)              # for data manipulation 2
library(tidyverse)          # for data manipulation 3
library(earth)              # for MARS
library(factoextra)         # for kmeans clustering
library(cluster)            # for Kmeans clustering
library(Ckmeans.1d.dp)      # for Kmeans clustering
library(nnet)               # for multinomial logistic regression

#####################################################################################################
#######                                   DATA PREPROCESSING                                  #######

df <- fread("uci_online_retail_cleaned_CLV.csv")
#View(df)

## Check the type of each column and factorise as necessary
sapply(df,class)
df$CustomerID <- factor(df$CustomerID)
df$Description <- factor(df$Description)
df$Country <- factor(df$Country)

df$InvoiceDate <- as.POSIXct(df$InvoiceDate,format="%Y-%m-%d %H:%M:%S",tz="Europe/London")
df$InvoiceDate_DayofWeek = factor(weekdays(df$InvoiceDate))
df$InvoiceDate_DayofMonth = as.numeric(format(df$InvoiceDate, format = "%d"))
df$InvoiceDate_MonthPeriod = cut(df$InvoiceDate_DayofMonth, breaks=c(0,10,20,31), labels=
                                     c("Beginning of Month", "Middle of Month", "End of Month"))
df$InvoiceDate_MonthName = factor(months(df$InvoiceDate))
df$InvoiceDate_HourofDay = as.numeric(format(df$InvoiceDate, format = "%H"))
df$InvoiceDate_DayPeriod = cut(df$InvoiceDate_HourofDay, breaks=c(-1,6,12,18,24), labels=
                                   c("Midnight", "Morning", "Afternoon", "Evening"))

## Calculate CLV by simply multiplying all 3 variables & then normalising it between 0 and 1
df$clv <- df$FREQUENCY_normalised*df$MONEY_normalised*df$RECENCY_normalised

df[ ,c('RECENCY',
        'FREQUENCY',
        'MONEY',
        'RECENCY_normalised',
        'FREQUENCY_normalised',
        'MONEY_normalised',
        'Description',
        'InvoiceDate',
        'CustomerID',
       'OrderDetails',
       'InvoiceNo',
       'StockCode'
):=NULL]

#####################################################################################################
#######                                       CLUSTERING                                      #######
# K-Means Clustering to create 3 segments

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

## K-Means is a multi-variate clustering method, thus might not be suitable for 1-d or 1 var data
## Test a second model on ckmeans.1d.dp, an optimal 1-d kmeans clustering
# ckmeans.1d.dp is a one-dimensional example with a two-component Gaussian mixture model
set.seed(2014)
ckm <- Ckmeans.1d.dp(df$clv, 3) ## Not required to specify nstart since it will auto-optimise
ckm
# Cluster Size: 1. 364207 | 2. 22456 | 3. 7608
# Cluster centers: 1. 4.052515 | 2. 29.905174 | 3. 55.042750
# Slight difference from above km$centers
## Visualise the clusters
plot(df$clv,col=(ckm$cluster+1),main = "K-Means Clustering Results",xlab = "",ylab="",pch=20,cex=2)
abline(h = ckm$centers, col = 1:2, pch = 8,cex = 2)
ckm$withinss ## [1] 2071400.31  759478.47   50486.51
ckm$tot.withinss ## [1] 2881365

table.cluster <- data.frame("Model"=c("K-Means", "Ckmeans.1d.dp"),"Total Within-Cluster Sum of Squares" = c(km$tot.withinss,ckm$tot.withinss))

# Total within-cluster sum of squares is lower for Ckmeans.1d.dp
## Parse the cluster coefficients back to original dataframe
df$cluster = factor(ckm$cluster)
summary(df$cluster)

df[ ,c('clv'
):=NULL]

#####################################################################################################
#######                                    TRAIN-TEST SPLIT                                   #######
generateTrainTest(df,0.7)
summary(train)
summary(test)

#####################################################################################################
#################################    RANDOM FOREST, ORIGINAL DATA   #################################

## Random Forest: Train on Original Trainset ##
#stackoverflow.com/questions/49161802/random-forest-with-r-cannot-allocate-vector-of-size-7-5-gb
set.seed(2014)
memory.limit(100000)
rf = randomForest(cluster ~ . , data = train, importance = TRUE)

## Random Forest: Get Model Stats ##
rf
# error rate = 3.23%

par(mfrow=c(1,1))
plot(rf, main = 'Random Forest Error Rate on Original UCI Dataset')
# Confirms error stabilised before 500 trees.

## Random Forest: Predict on Trainset ##
rf.pred.train <- predict(rf)

rf.train.confMat <- table(`Trainset Actuals` = train$cluster, `Model Prediction` = 
                              rf.pred.train, deparse.level = 2)
rf.train.confMat
# (7657+1219+29+6)/275989 = 3.23% or 96.8% accuracy

## Random Forest: Predict on Testset ##
rf.pred.test <- predict(rf, newdata=test)
rf.test.confMat <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                             rf.pred.test, deparse.level = 2)
rf.test.confMat
#(3345+490+16+5)/118282 = 3.26% or 96.7% accuracy

var.impt.RF <- importance(rf)

varImpPlot(rf, type = 1, main='Variable Importance on Original UCI Dataset')
# Country is the most important by a long shot

## Random Forest: Optimise B and RSF ##
#################################################################
##################### FUNCTION: calculateRF #####################
#################################################################
### This function takes in a dataframe, the random forest equation, a list of B values, and a list
### of RSF values to test
### and returns a matrix summarising random forest performance for each combination of B and RSF
calculateRF <- function(data, eqn, rows, cols) {
    #generates matrix for the specified rows and cols
    mat = matrix(rep(0,length(cols)*length(rows)), nrow = length(cols), dimnames = list(rows, cols))
    #iterate through the values
    for (row in 1:nrow(mat)) {
        for (col in 1:ncol(mat)) {
            set.seed(1) #Bootstrap + RSF
            #run random forest with the specified B / ntree / row and RSF / mtry / col
            #get the value in the row and column of the matrix and convert from string to int.
            #these are the B and RSF values that we want
            Bval = strtoi(rownames(mat)[row], base=0L)
            RSFval = strtoi(colnames(mat)[col], base=0L)
            tempModel = randomForest(eqn, data, importance = T,
                                     ntree = Bval, #B
                                     mtry = RSFval, #RSF size
            )$err.rate #get the error rate
            result = tempModel[nrow(tempModel),1] #select the last row, first value. 
            # this is the OOB error
            mat[row, col] = result #assign to matrix
            cat('running RF with B =', Bval, 'and RSF =', RSFval, 'gave us error =', result, '\n')
        }
    }
    return(mat)
}
#################### END FUNCTION DEFINITION ####################
#################################################################

set.seed(2014)
mat = calculateRF(test, cluster ~ ., c(25,100,500), c(1, floor(sqrt(ncol(test)-1)), ncol(test)-1))
mat


# in this case, increasing B and RSF each will result in a decrease in error rate.
# however, improvement from B = 100 to 500 decreases error rate only by a marginal amount.
# choose B = 100
# RSF = 11 yields the lowest error rate. however, given the observed dominance of the country
# variable, this is a stable model and hence bagging will become not useful due to the dominant X.
# different bootstrap samples will produce the same or almost the same model.
# therefore, we will just use RSF = sqrt(variables).

#####################################################################################################
##############################    LOGISTIC REGRESSION, ORIGINAL DATA   ##############################

## Logistic Regression: Train on Original Trainset

logreg <- multinom(cluster ~ ., data=train)

summary(logreg)

## Odds Ratio
OR.logreg <- exp(coef(logreg))
OR.logreg

# 95% Confidence interval
OR.logreg.CI <- exp(confint(logreg))
OR.logreg.CI

confint <- as.data.frame(OR.logreg.CI)

# p-value
z <- summary(logreg)$coefficients/summary(logreg)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue

logreg.step <- step(logreg)
logreg.step

## Odds Ratio
OR.logreg.step <- exp(coef(logreg.step))
OR.logreg.step

# 95% Confidence interval
OR.logreg.CI.step <- exp(confint(logreg.step))
OR.logreg.CI.step

# p-value
z <- summary(logreg.step)$coefficients/summary(logreg.step)$standard.errors
pvalue.step <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue.step

# has the same number of variables
logreg$coefnames
logreg.step$coefnames

## Logistic Regression: Predict on Original Trainset
predict.cluster.train <- predict(logreg.step)
predict.cluster.train


logreg.cm.train <- table(`Trainset Actuals` = train$cluster, `Model Prediction` = 
                             predict.cluster.train, deparse.level = 2)
logreg.cm.train

logreg.cm.train <- table(`Trainset Actuals` = train$cluster, `Model Prediction` = predict.cluster.train, deparse.level = 2)


accuracy.logreg.train <- mean(predict.cluster.train == train$cluster)
accuracy.logreg.train #0.9391751

## Logistic Regression: Predict on Original Testset
predict.cluster.test <- predict(logreg.step, newdata=test)
predict.cluster.test


logreg.cm.test <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                            predict.cluster.test, deparse.level = 2)
logreg.cm.test

logreg.cm.test <- table(`Testset Actuals` = test$cluster, `Model Prediction` = predict.cluster.test, deparse.level = 2)

accuracy.logreg.test <- mean(predict.cluster.test == test$cluster)
accuracy.logreg.test #0.9379618

c(accuracy.logreg.train,accuracy.logreg.test)

#####################################################################################################
######################################    MARS, ORIGINAL DATA   #####################################

## MARS: Train on Original Trainset
set.seed(2014)
#UnitPrice+InvoiceDate+Country+ProductVariations
mars <- earth(cluster~.,degree=2,glm = list(family=binomial),trace =1, data=train)
summary(mars)

str(mars)

mars.predict.train <- predict(mars)
mars.predict.train
mars.predict.train <- as.data.frame(mars.predict.train)
mars.predict.train$`predicted cluster` <- ifelse(
    mars.predict.train$`1`>mars.predict.train$`2`&mars.predict.train$`1`>mars.predict.train$`3`,"1",
                                          ifelse(
    mars.predict.train$`2`>mars.predict.train$`1`&mars.predict.train$`2`>mars.predict.train$`3`,"2",
                                          ifelse(
    mars.predict.train$`3`>mars.predict.train$`1`&mars.predict.train$`3`>mars.predict.train$`2`,"3",
    "NA")))

mars.cm.train <- table(`Trainset Actuals` = train$cluster, `Model Prediction` = 
                           mars.predict.train$`predicted cluster`, deparse.level = 2)
mars.cm.train

accuracy.mars.train <- mean(mars.predict.train$`predicted cluster` == train$cluster)
accuracy.mars.train

varimpt <- evimp(mars)
print(varimpt)

## MARS: Predict on Testset
mars.predict.test <- predict(mars, newdata=test)
mars.predict.test
mars.predict.test <- as.data.frame(mars.predict.test)
mars.predict.test$`predicted cluster` <- ifelse(
    mars.predict.test$`1`>mars.predict.test$`2` & mars.predict.test$`1`>mars.predict.test$`3`,"1",
                                         ifelse(
    mars.predict.test$`2`>mars.predict.test$`1` & mars.predict.test$`2`>mars.predict.test$`3`,"2",
                                         ifelse(
    mars.predict.test$`3`>mars.predict.test$`1` & mars.predict.test$`3`>mars.predict.test$`2`,"3",
    "NA")))

mars.cm.test <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                          mars.predict.test$`predicted cluster`, deparse.level = 2)
mars.cm.test

accuracy.mars.test<- mean(mars.predict.test$`predicted cluster` == test$cluster)
accuracy.mars.test

c(accuracy.mars.train,accuracy.mars.test)

varimpt <- evimp(mars)
print(varimpt)

## Data is skewed towards cluster 1, thus attempt to create balanced dataset to train the model


#####################################################################################################
#######                                     BALANCING DATA                                    #######

## Random sample from majority class Default = No and combine with Default = Yes to form new trainset
majority <- train[cluster == 1]
middle <- train[cluster == 2]
minority <- train[cluster == 3] 
## Randomly sample the row numbers to be in trainset. Same sample size as minority cases. 
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority)) 
chosen2 <- sample(seq(1:nrow(middle)), size = nrow(minority))
## Subset the original trainset based on randomly chosen row numbers. 
majority.chosen <- majority[chosen] 
middle.chosen <- middle[chosen2]
## Combine two data tables by appending the rows 
train.bal <- rbind(majority.chosen,middle.chosen, minority) 
summary(train.bal) 

#####################################################################################################
#################################    RANDOM FOREST, BALANCED DATA   #################################

set.seed(2014)
memory.limit(100000)
rf.bal = randomForest(cluster ~ . , data = train.bal, importance = TRUE)

## Random Forest: Get Model Stats ##
rf.bal
# error rate = 8.12%

par(mfrow=c(1,1))
plot(rf.bal, main = 'Random Forest Error Rate on Balanced UCI Dataset')
# Error needed more trees to stabilise, but still stabilised at about 200 trees.

## Random Forest: Predict on Trainset ##
rf.bal.pred.train <- predict(rf.bal)

rf.bal.train.confMat <- table(`Trainset Actuals` = train.bal$cluster, `Model Prediction` = 
                                  rf.bal.pred.train, deparse.level = 2)
rf.bal.train.confMat
# (680+15+591)/15831 = 8.12% or 91.9% accuracy

## Random Forest: Predict on Testset ##
rf.bal.pred.test <- predict(rf.bal, newdata=test)
rf.bal.test.confMat <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                                 rf.bal.pred.test, deparse.level = 2)
rf.bal.test.confMat
#(13957+496+778)/118282 = 12.9% or 87.1% accuracy

var.impt.RF.bal <- importance(rf.bal)

varImpPlot(rf.bal, type = 1, main = 'Variable Importance on Balanced UCI Dataset')
# Country became even more important

set.seed(2014)
mat.bal <- calculateRF(train.bal, cluster ~ ., c(25,100,500), c(1, floor(sqrt(ncol(train.bal)-1)), ncol(train.bal)-1))
mat.bal
# unlike in the original dataset, the default of B=500 and RSF=sqrt(variable) provides the most
# accurate measure. thus, we will stick to it.


#####################################################################################################
##############################    LOGISTIC REGRESSION, BALANCED DATA   ##############################

## Logistic Regression: Train on Balanced Trainset
set.seed(2014)
logreg.bal <- multinom(cluster~ ., data=train.bal)
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

## Logistic Regression: Predict on Balanced Trainset
predict.cluster.train.bal <- predict(logreg.step.bal)
predict.cluster.train.bal

logreg.cm.train.bal <- table(`Trainset Actuals` = train.bal$cluster, `Model Prediction` = 
                                 predict.cluster.train.bal, deparse.level = 2)
logreg.cm.train.bal

accuracy.logreg.train.bal <- mean(predict.cluster.train.bal == train.bal$cluster)
accuracy.logreg.train.bal ## [1] 0.3929632

## Logistic Regression: Predict on Balanced Testset
predict.cluster.test.bal <- predict(logreg.step.bal, newdata=test)
predict.cluster.test.bal

logreg.cm.test.bal <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                                predict.cluster.test.bal, deparse.level = 2)
logreg.cm.test.bal

accuracy.logreg.test.bal <- mean(predict.cluster.test.bal == test$cluster)
accuracy.logreg.test.bal ## [1] 0.6791143

c(accuracy.logreg.train.bal,accuracy.logreg.test.bal)

#####################################################################################################
######################################    MARS, BALANCED DATA   #####################################

## MARS: Train on Balanced Trainset
set.seed(2014)
mars.bal <- earth(cluster~.,degree=2,glm=list(family=binomial),data=train.bal)
summary(mars.bal)
mars.predict.train.bal <- predict(mars.bal)
mars.predict.train.bal
mars.predict.train.bal <- as.data.frame(mars.predict.train.bal)
mars.predict.train.bal$`predicted cluster` <- ifelse(
    mars.predict.train.bal$`1`>mars.predict.train.bal$`2`&mars.predict.train.bal$`1`>
        mars.predict.train.bal$`3`,"1",
                                              ifelse(
    mars.predict.train.bal$`2`>mars.predict.train.bal$`1`&mars.predict.train.bal$`2`>
        mars.predict.train.bal$`3`,"2",
                                              ifelse(
    mars.predict.train.bal$`3`>mars.predict.train.bal$`1`&mars.predict.train.bal$`3`>
        mars.predict.train.bal$`2`,"3","NA")))

mars.cm.train.bal <- table(`Trainset Actuals` = train.bal$cluster, `Model Prediction` = 
                               mars.predict.train.bal$`predicted cluster`, deparse.level = 2)
mars.cm.train.bal

accuracy.mars.train.bal <- mean(mars.predict.train.bal$`predicted cluster` == train.bal$cluster)
accuracy.mars.train.bal

varimpt <- evimp(mars.bal)
print(varimpt)

## MARS: Predict on Balanced Testset
mars.predict.test.bal <- predict(mars.bal, newdata=test)
mars.predict.test.bal
mars.predict.test.bal <- as.data.frame(mars.predict.test.bal)
mars.predict.test.bal$`predicted cluster` <- ifelse(
    mars.predict.test.bal$`1`>mars.predict.test.bal$`2`&mars.predict.test.bal$`1`>
        mars.predict.test.bal$`3`,"1",
                                             ifelse(
    mars.predict.test.bal$`2`>mars.predict.test.bal$`1`&mars.predict.test.bal$`2`>
        mars.predict.test.bal$`3`,"2",
                                             ifelse(
    mars.predict.test.bal$`3`>mars.predict.test.bal$`1` & mars.predict.test.bal$`3`>
        mars.predict.test.bal$`2`,"3","NA")))

mars.cm.test.bal <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                              mars.predict.test.bal$`predicted cluster`, deparse.level = 2)
mars.cm.test.bal

accuracy.mars.test.bal <- mean(mars.predict.test.bal$`predicted cluster` == test$cluster)
accuracy.mars.test.bal 

c(accuracy.mars.train.bal,accuracy.mars.test.bal)

varimpt <- evimp(mars.bal)
print(varimpt)

## Degree 2 has higher accuracy for train, lower accuracy for test
## Degree 1 has higher accuracy for test, lower accuracy for train
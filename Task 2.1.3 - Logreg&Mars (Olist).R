#################################  B C 2 4 0 7   S E M I N A R   1  #################################
###########################################  T E A M   7  ###########################################
################################### Instructor: Prof Neumann Chew ###################################

# <-------- This R script is formatted to fit on a window of width specified by this line --------> #

tryCatch(setwd(paste(getwd(),'/Data',sep="")), error = function(e) {    # set working directory to 
    paste('Directory is:', getwd())                                     # the 'Data' folder in the
})                                                                      # group project.

source("../helperFns.R")    # import list of helper functions we've written separately

library(randomForest)       # for random forest
library(data.table)         # for data manipulation 1
library(dplyr)              # for data manipulation 2
library(tidyverse)          # for data manipulation 3
library(readxl)             # 
library(earth)              # for MARS
library(factoextra)         #
library(cluster)            # for Kmeans clustering
library(Ckmeans.1d.dp)      # for Kmeans clustering
library(nnet)               # for neural networks?
library(stringr)            # to split strings

#####################################################################################################
#######                                   DATA PREPROCESSING                                  #######

df1 <- fread("Orders_merged_CLV.csv")
summary(df1)

## Calculate CLV by simply multiplying all 3 variables & then normalising it between 0 and 1
df1$clv <- df1$FREQUENCY_normalised*df1$MONEY_normalised*df1$RECENCY_normalised

## Remove columns deemed useless
df1[ ,c('product_id',
        'V1',
        'seller_id',
        'order_id',
        'customer_id',
        'order_purchase_timestamp',
        'order_approved_at',
        'order_delivered_carrier_date',
        'order_delivered_customer_date',
        'order_estimated_delivery_date',
        'customer_unique_id',
        'customer_zip_code_prefix',
        'review_id',
        'review_comment_title',
        'review_comment_message',
        'review_creation_date',
        'review_answer_timestamp',
        'order_item_id',
        'shipping_limit_date',
        'seller_zip_code_prefix',
        'product_name_lenght',
        'product_description_lenght',
        'product_photos_qty',
        'product_weight_g',
        'product_length_cm',
        'product_height_cm',
        'product_width_cm',
        'product_category_name',
        'order_status',
        'customer_city',
        'seller_city'
):=NULL]
sum(is.na(df1))
df1[!complete.cases(df1), ]
df1 <- na.omit(df1)

## Convert to categorical
cat <- c("order_status","customer_state","payment_type",
         "seller_state","product_category_name_english")
df1 <- df1 %>%
  mutate_at(cat, list(~factor(.)))
sapply(df1,class)

#####################################################################################################
#######                                       CLUSTERING                                      #######
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

## K-Means is a multi-variate clustering method, thus might not be suitable for 1-d/1 variable data
## Test a second model on ckmeans.1d.dp, an optimal 1-d kmeans clustering
# ckmeans.1d.dp is a one-dimensional example with a two-component Gaussian mixture model
set.seed(2014)
ckm1 <- Ckmeans.1d.dp(df1$clv, 3) ## Not required to specify nstart since it will auto-optimise
ckm1
ckm1$size
# Cluster Size: 1. 94327 | 2. 12766 | 3. 730
# Cluster centers: 1. 1. 121630 | 2. 1.813204 | 3. 4.103957
# Slight difference from above km$centers
## Visualise the clusters
plot(df1$clv,col=(ckm1$cluster+1),main ="CK-Means Clustering Results",xlab = "",ylab="",pch=20,cex=2)
abline(h = ckm1$centers, col = 1:2, pch = 8,cex = 2)
ckm1$withinss ## [1] 1159.946 1326.407 1372.275
ckm1$tot.withinss ## [1] 3858.628
# Total within-cluster sum of squares is the same
# Individual within-cluster sum of squares is the same as well
## Parse the cluster coefficients back to original dataframe
df1$cluster = factor(ckm1$cluster)
summary(df1$cluster)

df1[ ,c('RECENCY',
       'FREQUENCY',
       'MONEY',
       'RECENCY_normalised',
       'FREQUENCY_normalised',
       'MONEY_normalised',
       'clv'
):=NULL]

#####################################################################################################
#######                                    TRAIN-TEST SPLIT                                   #######

generateTrainTest(df1,0.7)
summary(train)
summary(test)

#####################################################################################################
##############################    LOGISTIC REGRESSION, ORIGINAL DATA   ##############################

## Logistic Regression: Train on Original Trainset
## https://stackoverflow.com/questions/36303404/too-many-weights-in-multinomial-logistic-regression-and-the-code-is-running-for
logreg1 <- multinom(cluster~customer_state+review_score+payment_sequential+
                        payment_type+payment_installments+payment_value+price+freight_value
                        +seller_state+product_category_name_english, data=train, family="multinomial",MaxNWts=20000)
summary(logreg1)

str(df1)
## Odds Ratio
OR.logreg1 <- exp(coef(logreg1))
OR.logreg1

# 95% Confidence interval
OR.logreg1.CI <- exp(confint(logreg1))
OR.logreg1.CI

# p-value
z <- summary(logreg1)$coefficients/summary(logreg1)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue

logreg1.step <- step(logreg1)
logreg1.step

## Logistic Regression: Predict on Original Trainset
predict.cluster.train1 <- predict(logreg1.step)
predict.cluster.train1

logreg1.cm.train <- table(`Trainset Actuals` = train$cluster, `Model Prediction` = 
                              predict.cluster.train1, deparse.level = 2)
logreg1.cm.train

accuracy.logreg1.train <- mean(predict.cluster.train1 == train$cluster)
accuracy.logreg1.train

## Logistic Regression: Predict on Original Testset
predict.cluster.test1 <- predict(logreg1.step, newdata=test)
predict.cluster.test1

logreg1.cm.test <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                             predict.cluster.test1, deparse.level = 2)
logreg1.cm.test

accuracy.logreg1.test <- mean(predict.cluster.test1 == test$cluster)
accuracy.logreg1.test

#####################################################################################################
######################################    MARS, ORIGINAL DATA   #####################################

## MARS: Train on Original Trainset
set.seed(2014)
mars1 <- earth(cluster~customer_state+review_score+payment_sequential+
                   payment_type+payment_installments+payment_value+price+freight_value+
                   seller_state+product_category_name_english,degree=2,data=train)
summary(mars1)
mars1.predict.train <- predict(mars1)
mars1.predict.train
mars1.predict.train <- as.data.frame(mars1.predict.train)
mars1.predict.train$`predicted cluster` <- ifelse(
    mars1.predict.train$`1`>mars1.predict.train$`2`&mars1.predict.train$`1`>mars1.predict.train$`3`,"1",
                                           ifelse(
    mars1.predict.train$`2`>mars1.predict.train$`1`& mars1.predict.train$`2`>mars1.predict.train$`3`,"2",
                                           ifelse(
    mars1.predict.train$`3`>mars1.predict.train$`1`& mars1.predict.train$`3`>mars1.predict.train$`2`,"3",
    "NA")))

mars1.cm.train <- table(`Testset Actuals` = train$cluster, `Model Prediction` = 
                            mars1.predict.train$`predicted cluster`, deparse.level = 2)
mars1.cm.train

accuracy.mars1.train <- mean(mars1.predict.train$`predicted cluster` == train$cluster)
accuracy.mars1.train

varimpt1 <- evimp(mars1)
print(varimpt1)

## MARS: Predict on Testset
mars1.predict.test <- predict(mars, newdata=test)
mars1.predict.test
mars1.predict.test <- as.data.frame(mars1.predict.test)
mars1.predict.test$`predicted cluster` <- ifelse(
    mars1.predict.test$`1`>mars1.predict.test$`2`&mars1.predict.test$`1`>mars1.predict.test$`3`,"1",
                                          ifelse(
    mars1.predict.test$`2`>mars1.predict.test$`1`&mars1.predict.test$`2`>mars1.predict.test$`3`,"2",
                                          ifelse(
    mars1.predict.test$`3`>mars1.predict.test$`1`&mars1.predict.test$`3`>mars1.predict.test$`2`,"3",
    "NA")))

mars1.cm.test <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                           mars1.predict.test$`predicted cluster`, deparse.level = 2)
mars1.cm.test

accuracy.mars1.test<- mean(mars1.predict.test$`predicted cluster` == test$cluster)
accuracy.mars1.test

varimpt1 <- evimp(mars1)
print(varimpt1)

#####################################################################################################
#################################    RANDOM FOREST, ORIGINAL DATA   #################################

## Random Forest: Train on Original Trainset ##
#stackoverflow.com/questions/49161802/random-forest-with-r-cannot-allocate-vector-of-size-7-5-gb
set.seed(2014)
memory.limit(100000)
rf = randomForest(cluster ~ . , data = train, importance = TRUE)

## Random Forest: Cannot run on more than 53 categories ##
df1.copy <- df1
str(df1.copy)
#strip product_category_name_english to its base form
levels(df1.copy$product_category_name_english)
df1.copy$product_category_name_english <- sub("_", " ", df1.copy$product_category_name_english)
df1.copy$product_category_name_english <- word(df1.copy$product_category_name_english, 1)
df1.copy$product_category_name_english <- sub("fashio", "fashion", df1.copy$product_category_name_english)
df1.copy$product_category_name_english = as.factor(df1.copy$product_category_name_english)
str(df1.copy)
#remove seller_city, customer_city for now
df1.copy.noCity = df1.copy
df1.copy.noCity[ ,c(
        'customer_city',
        'seller_city',
        'payment_value'
):=NULL]

## Train-test split ##
set.seed(3)
sample <- sample.int(n = nrow(df1.copy.noCity), size = floor(0.7*nrow(df1.copy.noCity)), replace = F)
train2 <- df1.copy.noCity[sample, ]
test2  <- df1.copy.noCity[-sample, ]

## Random Forest: Train on Original Trainset ##
set.seed(2014)
memory.limit(100000)
rf = randomForest(cluster ~ . , data = train2, importance = TRUE)

## Random Forest: Get Model Stats ##
rf
# error rate = 9.1%

par(mfrow=c(1,1))
plot(rf)
# Confirms error stabilised before 500 trees.

## Random Forest: Predict on Trainset ##
rf.pred.train <- predict(rf)

rf.train.confMat <- table(`Trainset Actuals` = train2$cluster, `Model Prediction` = 
                              rf.pred.train, deparse.level = 2)
rf.train.confMat
#(408+8+6228+216+7)/75474 = 9.1%

## Random Forest: Predict on Testset ##
rf.pred.test <- predict(rf, newdata=test2)
rf.test.confMat <- table(`Testset Actuals` = test2$cluster, `Model Prediction` = 
                             rf.pred.test, deparse.level = 2)
rf.test.confMat
#(171+2+2551+5+92+1)/32346 = 8.7% or 91.3% accuracy

var.impt.RF <- importance(rf)

varImpPlot(rf, type = 1)

library(beepr)
beep()

## Data is skewed towards cluster 1, thus attempt to create balanced dataset to train the model

#####################################################################################################
########################################    BALANCING DATA   ########################################

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
##############################    LOGISTIC REGRESSION, BALANCED DATA   ##############################

## Logistic Regression: Train on Balanced Trainset
set.seed(2014)
logreg.bal <- multinom(cluster~ccustomer_state+review_score+payment_sequential+
                           payment_type+payment_installments+payment_value+price+freight_value+
                           seller_state+product_category_name_english, data=train.bal,MaxNWts=20000)
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

#####################################################################################################
######################################    MARS, BALANCED DATA   #####################################

## MARS: Train on Balanced Trainset
set.seed(2014)
mars.bal <- earth(cluster~customer_state+review_score+payment_sequential+
                      payment_type+payment_installments+payment_value+price+freight_value+
                      seller_state+product_category_name_english,degree=1,data=train.bal)
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
    mars.predict.train.bal$`3`>mars.predict.train.bal$`1` & mars.predict.train.bal$`3`>
        mars.predict.train.bal$`2`,"3",
    "NA")))

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
    mars.predict.test.bal$`3`>mars.predict.test.bal$`1`&mars.predict.test.bal$`3`>
        mars.predict.test.bal$`2`,"3",
    "NA")))

mars.cm.test.bal <- table(`Testset Actuals` = test$cluster, `Model Prediction` = 
                              mars.predict.test.bal$`predicted cluster`, deparse.level = 2)
mars.cm.test.bal

accuracy.mars.test.bal <- mean(mars.predict.test.bal$`predicted cluster` == test$cluster)
accuracy.mars.test.bal 

varimpt <- evimp(mars.bal)
print(varimpt)

#####################################################################################################
#################################    RANDOM FOREST, BALANCED DATA   #################################

set.seed(2014)
memory.limit(100000)
rf.bal = randomForest(cluster ~ . , data = train.bal, importance = TRUE)

## Random Forest: Get Model Stats ##
rf.bal
# error rate = 8.12%

par(mfrow=c(1,1))
plot(rf.bal)
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
#(13961+496+780)/118282 = 12.9% or 87.1% accuracy

var.impt.RF.bal <- importance(rf.bal)

varImpPlot(rf.bal, type = 1)
# Country became even more important

set.seed(2014)
mat.bal <- calculateRF(train.bal, cluster ~ ., c(25,100,500), c(1, floor(sqrt(ncol(train.bal)-1)), ncol(train.bal)-1))
mat.bal
# unlike in the original dataset, the default of B=500 and RSF=sqrt(variable) provides the most
# accurate measure. thus, we will stick to it.

library(beepr)
beep()

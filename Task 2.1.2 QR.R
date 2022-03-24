library(dplyr)
library(quantreg)
library(lubridate)

setwd(paste(getwd(),'/Data',sep="")) 
#set working directory to the dataset folder (works locally on every computer)

source("../helperFns.R") 
#import the helper functions

orders_all <- read.csv("Orders_merged.csv")
orders_all$order_purchase_timestamp <-
  as.POSIXct(orders_all$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_delivered_customer_date<-
  as.POSIXct(orders_all$order_delivered_customer_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_approved_at<-
  as.POSIXct(orders_all$order_approved_at,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_estimated_delivery_date<-
  as.POSIXct(orders_all$order_estimated_delivery_date,format="%Y-%m-%d",tz="America/Sao_Paulo")
orders_all$shipping_limit_date<-
  as.POSIXct(orders_all$shipping_limit_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")



####Preparing data for ML models####
###Selecting and dropping variables###
drops <- c("product_id","seller_id","order_id","customer_id","order_status",
           "order_purchase_timestamp","order_approved_at","order_delivered_carrier_date",
           "order_delivered_customer_date","order_estimated_delivery_date",
           "customer_unique_id","customer_zip_code_prefix","customer_city",
           "customer_state","review_id", "review_comment_title","review_comment_message",
           "review_creation_date","review_answer_timestamp","payment_sequential",
           "order_item_id","shipping_limit_date","seller_zip_code_prefix","seller_city","seller_state",
           "product_category_name","product_weight_g","product_length_cm",
           "product_height_cm","product_width_cm","product_category_name_english")

orders_all_1 <- orders_all[ , !(names(orders_all) %in% drops)]
orders_all_1 <- na.omit(orders_all_1) 
orders_all_1$payment_type <- as.factor(orders_all_1$payment_type)

###Finding correlation matrix and scatterplots###
cor_mat <- cor(subset(orders_all_1, select=-payment_type)) #no meaningful correlation

# par(mfrow=c(8,8))
# par(mar=c(1,1,1,1))
# pairs(orders_all_1, col="blue", main="Scatterplots") #useless coz categorical?
# par(mfrow=c(1,1))

###Train-test split###
generateTrainTest(orders_all_1, 0.7)


####ML Models####
### Fit 50th Percentile Line (i.e. Median) ###
fit.p.5 <- rq(review_score ~ . , tau=.5, data = train) #tau: percentile level. 0.5 is the 50th percentile (aka median).
#abline(fit.p.5, col="blue")

fit.p.5
# summary(fit.p.5)
# summary(fit.p.5, se = "nid")

###Prediction and classification###
calculateAccuracy = function(predictive_model, test){
    predictions=predict(predictive_model, newdata = test)
    summary(predictions)
    predictions[predictions<=1.4]=1 
    predictions[(predictions>1.4)&(predictions<=2.4)]=2
    predictions[(predictions>2.4)&(predictions<=3.4)]=3
    predictions[(predictions>3.4)&(predictions<=4.4)]=4
    predictions[predictions>4.4]=5
    print('rating - predictions:')
    print(table(predictions))
    print('rating - actual:')
    print(table(test$review_score))
    table(predictions, test$review_score)
    table(predictions == test$review_score)
    mean(predictions == test$review_score)
}
calculateAccuracy(fit.p.5, test)
summary(orders_all_1$review_score) #Accuracy is 0.58 -> very low

###Finding derivative variables to find improvement###
orders_all_2 <- orders_all
orders_all_2$del_time <- difftime(orders_all$order_delivered_customer_date,
                                orders_all$order_approved_at,
                                units="days")
orders_all_2$est_del_time<- difftime(orders_all$order_estimated_delivery_date,
                                     orders_all$order_approved_at,
                                     units="days")
orders_all_2$delta_time<- difftime(orders_all$order_estimated_delivery_date,
                                   orders_all$order_delivered_customer_date,
                                   units="days")
orders_all_2$Late <- as.factor(ifelse(orders_all_2$delta_time<0,1,0))
orders_all_2$total_price <- orders_all_2$price+orders_all_2$freight_value
orders_all_2$freight_ratio <- orders_all_2$freight_value/orders_all_2$price
orders_all_2$purchase_day_of_week <- wday(orders_all_2$order_approved_at)
orders_all_2 <- orders_all_2[ , !(names(orders_all_2) %in% drops)]
orders_all_2$payment_type <- as.factor(orders_all_2$payment_type)
orders_all_2$del_time <- as.numeric(orders_all_2$del_time)
orders_all_2$est_del_time <- as.numeric(orders_all_2$est_del_time)
orders_all_2$delta_time <- as.numeric(orders_all_2$delta_time)
orders_all_2 <- na.omit(orders_all_2)
orders_all_2 <- orders_all_2[ , -which(names(orders_all_2) %in% c("price"))]

#dropping payment_type because of singular design matrix error
#orders_all_2 <- subset(orders_all_2, select=-payment_type)

###Train-test split###
generateTrainTest(orders_all_2, 0.7)

### Fit 50th Percentile Line (i.e. Median) ###
train.jitter = train
train.jitter$Late <- jitter(train.jitter$Late)
train.jitter$product_photos_qty <- jitter(train.jitter$product_photos_qty)
train.jitter$purchase_day_of_week <- jitter(train.jitter$purchase_day_of_week)
fit.p.5.2 <- rq(review_score ~ . , tau=.5, data = train.jitter) #tau: percentile level. 0.5 is the 50th percentile (aka median).

###################################################

print('singular design matrix error. why?')
sapply(train.jitter, class)
model.matrix(~train.jitter$review_score+train.jitter$payment_type) #this is the design matrix - matrix that gets passed into the function
#'singular' matrix means the determinant is zero, e.g. the diagonals of the matrix are all zero.
print('https://cran.r-project.org/web/packages/quantreg/quantreg.pdf page 68 tells us to use sfn method for sparse matrices')
fit.p.5.2 <- rq(review_score ~ . , tau=.5, data = train.jitter, method='sfn')
print('error: increase tmpmax. lets use traceback() in the console to see the error')
#9: stop(mess)
#8: .local(x, ...)
#7: chol(a, ...)
#6: chol(a, ...)
#5: solve(e, ao %*% y, tmpmax = tmpmax, nnzlmax = nnzlmax, nsubmax = nsubmax)
#4: solve(e, ao %*% y, tmpmax = tmpmax, nnzlmax = nnzlmax, nsubmax = nsubmax)
#3: rq.fit.sfn(x, y, tau = tau, ...)
#2: rq.fit(X, Y, tau = tau, method, ...)
#1: rq(review_score ~ ., tau = 0.5, data = train.jitter, method = "sfn")
print('according to documentation in https://stat.ethz.ch/pipermail/r-help/2008-October/178523.html, error originates from rq.fit.sfn')
print('lets type rq.fit.sfn in console to view the source code https://mail.rfaqs.com/source-code-of-r-method/')
#    ctrl <- sfn.control()
#if (!missing(control)) {
#    control <- as.list(control)
#    ctrl[names(control)] <- control
#}
#nsubmax <- ctrl$nsubmax
#tmpmax <- ctrl$tmpmax
print('sfn.control() is what we need to increase tmpmax https://www.rdocumentation.org/packages/quantreg/versions/5.88/topics/sfn.control')
sfn.control(tmpmax = Inf)
fit.p.5.2 <- rq(review_score ~ . , tau=.5, data = train.jitter, method='sfn')
print('doesnt work, we might need to run rq.fit.sfn with tmpmax inside ourselves')
sfn.Y = train.jitter$review_score
sfn.X = train.jitter
sfn.X$review_score = NULL
sfn.X.mat = as.matrix(sfn.X)
sfn.sX = as.matrix.csr(sfn.X.mat)
print('error: everything in the matrix must be a number, i.e. we need to one-hot encode the categorical ourselves')
rm(sfn.X, sfn.X.mat, sfn.Y)

library(mltools)
library(data.table)
train.jitter.1hot = one_hot(as.data.table(train.jitter))
fit.p.5.2 <- rq(review_score ~ . , tau=.5, data = train.jitter.1hot) #still gives us the error

sfn.Y = train.jitter.1hot$review_score
sfn.X = train.jitter.1hot
sfn.X$review_score = NULL
sfn.X.mat = as.matrix(sfn.X)
sfn.sX = as.matrix.csr(sfn.X.mat)

rq.fit.sfn(sfn.sX, sfn.Y, control = list(tmpmax=999999) )
rm(sfn.X, sfn.X.mat, sfn.Y, sfn.sX, train.jitter, train.jitter.1hot)

#back to the drawing board
#https://stats.stackexchange.com/questions/70899/what-correlation-makes-a-matrix-singular-and-what-are-implications-of-singularit/70910#70910
print('hypothesis: del_time, est_del_time and delta_time linear independances is causing this')
orders_all_2.2 = orders_all_2
orders_all_2.2$del_time = NULL
orders_all_2.2$est_del_time = NULL
#orders_all_2.1$delta_time = -orders_all_2.1$delta_time
generateTrainTest(orders_all_2.2, 0.7)
fit.p.5.2 <- rq(review_score ~ . , tau=.5, data = train)
summary(fit.p.5.2)
calculateAccuracy(fit.p.5.2, test)
#https://stat.ethz.ch/pipermail/r-help/2006-July/109821.html
#0.561 accuracy. pain
print('hypothesis is correct: linear independances between the time columns (deriatives of each other) caused the singular matrix problem')

orders_all_2.3 = orders_all_2
orders_all_2.3$delta_time_percentage = -orders_all_2.3$delta_time/orders_all_2.3$est_del_time
orders_all_2.3$del_time = NULL
orders_all_2.3$est_del_time = NULL
orders_all_2.3$delta_time = NULL
generateTrainTest(orders_all_2.3, 0.7)
fit.p.5.3 <- rq(review_score ~ . , tau=.5, data = train)
calculateAccuracy(fit.p.5.3, test)
summary(fit.p.5.3)

###################################################

#From fit.p.5.2 and fit.p.5.3 -> only payment_value, Late, and total_price is important
#Eliminating other variables
fit.p.5.4 <- rq(review_score ~ payment_type+Late+total_price, tau=.5, data = train)
calculateAccuracy(fit.p.5.4, test) #Accuracy improves a little to 0.5627706
summary(fit.p.5.4) #All variables now become not significant


# # 5th, 10th, 25th, 75th, 90th, 95th percentiles.
taus <- c(.05, .1, .25, .75, .90, .95)

table <- data.frame("Tau"=1:6,"Accuracy"=1:6,"beta0"=1:6,"beta_payment_credit"=1:6,"beta_payment_debit"=1:6,
                    "beta_payment_voucher"=1:6,"beta_Late"=1:6,"beta_totalprice"=1:6)

calculateAccuracy2 = function(predictive_model, test){
  predictions=predict(predictive_model, newdata = test)
  summary(predictions)
  predictions[predictions<=1.4]=1 
  predictions[(predictions>1.4)&(predictions<=2.4)]=2
  predictions[(predictions>2.4)&(predictions<=3.4)]=3
  predictions[(predictions>3.4)&(predictions<=4.4)]=4
  predictions[predictions>4.4]=5
  return(mean(predictions == test$review_score))
}

for( i in 1:length(taus)){
  fit <- rq(review_score ~ payment_type+Late+total_price, tau=taus[i], data = train)
  coef <- coef(fit)
  table$Tau[i]=taus[i]
  table$Accuracy[i]= calculateAccuracy2(fit,test)
  table$beta0[i]=coef[1]
  table$beta_payment_credit[i]=coef[2]
  table$beta_payment_debit[i]=coef[3]
  table$beta_payment_voucher[i]=coef[4]
  table$beta_Late[i]=coef[5]
  table$beta_totalprice[i]=coef[6]
}

#Best accuracy is 0.57452072 from tau = 0.75 onwards -> still pretty bad

#------------------------------MARS Implementation------------------------------#

library(earth)

#MARS Degree 1
set.seed(22)

reviews_mars <- earth(review_score ~ . , degree = 1, data = train)
summary(reviews_mars)

calculateAccuracy(reviews_mars, test) #accuracy = 0.2841064

#MARS Degree 2
set.seed(22)

reviews_mars2 <- earth(review_score ~ . , degree = 2, data = train)
summary(reviews_mars2)

calculateAccuracy(reviews_mars2, test) #best accuracy = 0.2867347

#MARS with 10 fold cross validation
set.seed(22)

earth(review_score ~ . , degree = 2, trace = 3, data = train)
reviews_mars_10fold <- earth(review_score ~ . , pmethod = "cv", degree = 1, nfold = 10, ncross = 1, data = train)
summary(reviews_mars_10fold)

calculateAccuracy(reviews_mars_10fold, test) #accuracy = 0.2841064

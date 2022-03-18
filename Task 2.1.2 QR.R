library(dplyr)
library(quantreg)
library(lubridate)

setwd(paste(getwd(),'/Data',sep=""))

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
set.seed(3)
sample <- sample.int(n = nrow(orders_all_1), size = floor(.7*nrow(orders_all_1)), replace = F)
train <- orders_all_1[sample, ]
test  <- orders_all_1[-sample, ]

####ML Models####
### Fit 50th Percentile Line (i.e. Median) ###
fit.p.5 <- rq(review_score ~ . , tau=.5, data = train) #tau: percentile level. 0.5 is the 50th percentile (aka median).
#abline(fit.p.5, col="blue")

fit.p.5
# summary(fit.p.5)
# summary(fit.p.5, se = "nid")

###Prediction and classification###
fit.5.pred=predict(fit.p.5, newdata = test)
summary(fit.5.pred)
fit.5.pred[fit.5.pred<=1.4]=1 
fit.5.pred[(fit.5.pred>1.4)&(fit.5.pred<=2.4)]=2
fit.5.pred[(fit.5.pred>2.4)&(fit.5.pred<=3.4)]=3
fit.5.pred[(fit.5.pred>3.4)&(fit.5.pred<=4.4)]=4
fit.5.pred[fit.5.pred>4.4]=5
table(fit.5.pred)
table(test$review_score)
table(fit.5.pred, test$review_score)
table(fit.5.pred == test$review_score)
mean(fit.5.pred == test$review_score)
summary(orders_all_1$review_score) #Accuracy is 0.58 -> very low

###Finding derivative variables to find improvement###
orders_all_2 <- orders_all
orders_all_2$del_time <- difftime(orders_all$order_delivered_customer_date,
                                orders_all$order_purchase_timestamp,
                                units="days")
orders_all_2$est_del_time<- difftime(orders_all$order_estimated_delivery_date,
                                     orders_all$order_approved_at,
                                     units="days")
orders_all_2$delta_time<- orders_all_2$est_del_time-orders_all_2$del_time
orders_all_2$Late <- ifelse(orders_all_2$delta_time<0,1,0)
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
set.seed(3)
sample <- sample.int(n = nrow(orders_all_2), size = floor(.7*nrow(orders_all_2)), replace = F)
train <- orders_all_2[sample, ]
test  <- orders_all_2[-sample, ]

### Fit 50th Percentile Line (i.e. Median) ###
train$Late <- jitter(train$Late)
train$product_photos_qty <- jitter(train$product_photos_qty)
train$purchase_day_of_week <- jitter(train$purchase_day_of_week)
fit.p.5.2 <- rq(review_score ~ . , tau=.5, data = train) #tau: percentile level. 0.5 is the 50th percentile (aka median).
#abline(fit.p.5, col="blue")

# fit.p.5.2
# # summary(fit.p.5)
# # summary(fit.p.5, se = "nid")
# 
# ###Prediction and classification###
# fit.5.pred=predict(fit.p.5, newdata = test)
# summary(fit.5.pred)
# fit.5.pred[fit.5.pred<=1.4]=1 
# fit.5.pred[(fit.5.pred>1.4)&(fit.5.pred<=2.4)]=2
# fit.5.pred[(fit.5.pred>2.4)&(fit.5.pred<=3.4)]=3
# fit.5.pred[(fit.5.pred>3.4)&(fit.5.pred<=4.4)]=4
# fit.5.pred[fit.5.pred>4.4]=5
# table(fit.5.pred)
# table(test$review_score)
# table(fit.5.pred, test$review_score)
# table(fit.5.pred == test$review_score)
# mean(fit.5.pred == test$review_score)
# summary(orders_all_1$review_score) #Accuracy is 0.58 -> very low
# 
# # 5th, 10th, 25th, 75th, 90th, 95th percentiles.
# taus <- c(.05, .1, .25, .75, .90, .95)
# 
# table <- data.frame("Tau"=1:6,"beta0"=1:6,"beta_Income"=1:6)
# 
# for( i in 1:length(taus)){
#   fit <- rq(engel$foodexp~engel$income,tau=taus[i])
#   coef <- coef(fit)
#   table$Tau[i]=taus[i]
#   table$beta0[i]=coef[1]
#   table$beta_Income[i]=coef[2]
#   abline(fit,col="grey")
# } 
# 
# #classification
# # glm.fit = glm(Direction ~ Lag1+Lag2, data=Smarket, family=binomial, subset=train)
# # glm.probs=predict(glm.fit, Smarket.2005, type="response")
# # glm.pred=rep("Down", 252)
# # glm.pred[glm.probs>0.5]="Up"
# # table(glm.pred, Direction.2005)
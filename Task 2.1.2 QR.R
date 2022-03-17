library(dplyr)

setwd(paste(getwd(),'/Data',sep=""))

orders_all <- read.csv("Orders_merged.csv")
orders_all$order_purchase_timestamp <-
  as.POSIXct(orders_all$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_delivered_customer_date<-
  as.POSIXct(orders_all$order_delivered_customer_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_approved_at<-
  as.POSIXct(orders_all$order_approved_at,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_estimated_delivery_date<-
  as.POSIXct(orders_all$order_estimated_delivery_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
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

###Finding correlation matrix###
cor_mat <- cor(subset(orders_all_1, select=-payment_type)) #no meaningful correlation

###Train-test split###
set.seed(3)
sample <- sample.int(n = nrow(orders_all_1), size = floor(.7*nrow(orders_all_1)), replace = F)
train <- orders_all_1[sample, ]
test  <- orders_all_1[-sample, ]



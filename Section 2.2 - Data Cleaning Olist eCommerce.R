tryCatch(setwd(paste(getwd(),'/Data',sep="")), error = function(e) {    # set working directory to 
    paste('Directory is:', getwd())                                     # the 'Data' folder in the
})                                                                      # group project.

orders <- read.csv("Raw Data/olist_orders_dataset.csv",header = T)
customers <- read.csv("Raw Data/olist_customers_dataset.csv",header = T)
order_reviews <- read.csv("Raw Data/olist_order_reviews_dataset.csv",header = T)
order_payments <- read.csv("Raw Data/olist_order_payments_dataset.csv",header = T)
order_items_details <- read.csv("Raw Data/olist_order_items_dataset.csv",header = T)
sellers <- read.csv("Raw Data/olist_sellers_dataset.csv",header = T)
geolocation <- read.csv("Raw Data/olist_geolocation_dataset.csv",header = T)
products <- read.csv("Raw Data/olist_products_dataset.csv",header = T)
products_translation <- read.csv("Raw Data/olist_product_category_name_translation.csv",header = T, fileEncoding="UTF-8-BOM")
#names(products_translation)[names(products_translation) == "product_category_name_portugese"] <- 'product_category_name'

####Data preparation####
###Merging datasets###
table(orders$order_status)
#Selecting only delivered orders
orders <- orders[orders$order_status=="delivered",]
orders_all <- merge(orders,customers,by="customer_id",all.x=T)
orders_all$order_purchase_timestamp <-
  as.POSIXct(orders_all$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_delivered_customer_date<-
  as.POSIXct(orders_all$order_delivered_customer_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_approved_at<-
  as.POSIXct(orders_all$order_approved_at,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all$order_estimated_delivery_date<-
  as.POSIXct(orders_all$order_estimated_delivery_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
orders_all <- merge(orders_all,order_reviews,by="order_id")
orders_all <- orders_all[!duplicated(orders_all$order_id),]
orders_all <- merge(orders_all,order_payments,by="order_id",all.x = T)
orders_all <- orders_all[!duplicated(orders_all$order_id),]
orders_all <- merge(orders_all,order_items_details,by="order_id",all.X=T)
orders_all <- merge(orders_all,sellers,by="seller_id",all.X=T)
products_all <- merge(products,products_translation,by="product_category_name",all.X=T)
products_all <- products_all[!duplicated(products_all$product_id),]
orders_all <- merge(orders_all,products_all,by="product_id",all.X=T)
rm(list=setdiff(ls(), "orders_all"))
write.csv(orders_all,file="Orders_merged.csv",row.names = F)

# orders_all$order_delivered_month <-
#   format(as.Date(orders_all$order_estimated_delivery_date),"%Y-%m")
# orders_all$del_time <- difftime(orders_all$order_delivered_customer_date,
#                                 orders_all$order_purchase_timestamp,
#                                 units="days")
#orders_all$del_time[is.na(orders_all$del_time)] <- 0

setwd(paste(getwd(),'/Data',sep=""))
library(arules)
library(tidyr)
library(dplyr)

data <- read.csv('Orders_merged.csv')

data$order_purchase_timestamp <-
  as.POSIXct(data$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
data$order_delivered_customer_date<-
  as.POSIXct(data$order_delivered_customer_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
data$order_approved_at<-
  as.POSIXct(data$order_approved_at,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
data$order_estimated_delivery_date<-
  as.POSIXct(data$order_estimated_delivery_date,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")

data$'year/quarter' <- paste(format(data$order_purchase_timestamp, "%y/"), 0, 
      sub( "Q", "", quarters(data$order_purchase_timestamp) ), sep = "")
data$'customerUID_year/quarter' <- paste(data$customer_unique_id, as.character(data$'year/quarter'), sep = "-")
data$customer_unique_id <- as.factor(data$customer_unique_id)
data$'customerUID_year/quarter' <- as.factor(data$'customerUID_year/quarter')

#####################################################################################
# 1 customer (within same quarter) 1 basket - items -> categories (6 rules)

# dataframe with customer UID and product category
customerUID_productCat_long <- data[c('customerUID_year/quarter','product_category_name_english')]
customerUID_productCat_long$product_category_name_english <- as.factor(customerUID_productCat_long$product_category_name_english)

# convert to wide format
customerUID_productCat_wide <- customerUID_productCat_long %>%
  select(`customerUID_year/quarter`, product_category_name_english) %>% 
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_category_name_english, value, fill = 0)

# convert to transaction object
cat_transaction  <- as(as.matrix(customerUID_productCat_wide[, -1]), "transactions")
cat_transaction_df <- as(cat_transaction, "data.frame") # for viewing

# get frequent product categories
frequent.cats <- eclat(cat_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.cats)
itemFrequencyPlot(cat_transaction, topN=10, type="relative", main="Top 10 Categories Frequency")

# get rules (min. count = 2)
cat_rules <- apriori (cat_transaction, parameter = list(supp = 0.00002, conf = 0.1, minlen = 2))
inspect(cat_rules)
cat_rules.df <- as(cat_rules, "data.frame")
cat_subRules<-cat_rules[quality(cat_rules)$lift > 1] # get those with lift > 1 
cat_subRules.df <- as(cat_subRules, "data.frame")

#####################################################################################
# 1 customer 1 basket - items -> seller_id (16 rules)

# dataframe with customer UID and seller ID
customerUID_sellerID_long <- data[c('customer_unique_id','seller_id')]
customerUID_sellerID_long$seller_id <- as.factor(customerUID_sellerID_long$seller_id)

# convert to wide format
customerUID_sellerID_wide <- customerUID_sellerID_long %>%
  select(customer_unique_id, seller_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(seller_id, value, fill = 0)

# convert to transaction object
seller_transaction  <- as(as.matrix(customerUID_sellerID_wide[, -1]), "transactions")
seller_transaction_df <- as(seller_transaction, "data.frame")

# get frequent sellers
frequent.sellers <- eclat(seller_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.sellers)
itemFrequencyPlot(seller_transaction, topN=10, type="relative", main="Top 10 Sellers Frequency")

# get rules (min. count = 5) sufficient??
seller_rules <- apriori (seller_transaction, parameter = list(supp = 0.00005, conf = 0.05, minlen = 2))
inspect(seller_rules)
seller_rules.df <- as(seller_rules, "data.frame")
seller_subRules<-seller_rules[quality(seller_rules)$lift > 1] # get those with lift > 1 
seller_subRules.df <- as(seller_subRules, "data.frame")

#####################################################################################
# 1 customer (within same quarter) 1 basket - items (29 rules)

# dataframe with customer UID and product ID
customerUID_productID_long <- data[c('customerUID_year/quarter','product_id')]
customerUID_productID_long$product_id <- as.factor(customerUID_productID_long$product_id)

# convert to transaction object
product_transaction <- transactions(customerUID_productID_long, format = "long")
product_transaction_df <- as(product_transaction,"data.frame")

# get frequent products
frequent.products <- eclat(product_transaction, parameter = list(supp = 0.001, maxlen = 10))
inspect(frequent.products)
itemFrequencyPlot(product_transaction, topN=10, type="relative", main="Top 10 Products Frequency")

# get rules (min. count = 5) sufficient??
product_rules <- apriori (product_transaction, parameter = list(supp = 0.00005, conf = 0.1, minlen = 2))
inspect(product_rules)
product_rules.df <- as(product_rules, "data.frame")
product_subRules<-product_rules[quality(product_rules)$lift > 1] # get those with lift > 1 
product_subRules.df <- as(product_subRules, "data.frame")

write.csv(product_subRules.df,file="subRules olist.csv",row.names = F)
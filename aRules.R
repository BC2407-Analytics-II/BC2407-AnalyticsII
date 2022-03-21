setwd(paste(getwd(),'/Data',sep=""))
library(arules)
library(tidyr)
library(dplyr)

data <- read.csv('Orders_merged.csv')
data$customer_unique_id = as.factor(data$customer_unique_id)

#####################################################################################
# 1 customer 1 basket - items -> categories (18 rules)
customerUID_productCat_long <- data[c('customer_unique_id','product_category_name_english')]
customerUID_productCat_long$product_category_name_english <- as.factor(customerUID_productCat_long$product_category_name_english)

customerUID_productCat_long <- customerUID_productCat_long %>%
  select(customer_unique_id, product_category_name_english) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_category_name_english, value, fill = 0)

cat_transaction  <- as(as.matrix(customerUID_productCat_long[, -1]), "transactions")
cat_transaction_df <- as(cat_transaction, "data.frame")

frequent.cats <- eclat(cat_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.cats)
itemFrequencyPlot(cat_transaction, topN=10, type="relative", main="Top 10 Categories Frequency")

cat_rules <- apriori (cat_transaction, parameter = list(supp = 0.00002, conf = 0.1, minlen = 2))
inspect(cat_rules)
cat_rules.df <- as(cat_rules, "data.frame")
cat_subRules<-cat_rules[quality(cat_rules)$lift > 1]
cat_subRules.df <- as(cat_subRules, "data.frame")

#####################################################################################
# 1 customer 1 basket - items -> seller_id (16 rules)
customerUID_sellerID_long <- data[c('customer_unique_id','seller_id')]
customerUID_sellerID_long$seller_id <- as.factor(customerUID_sellerID_long$seller_id)

customerUID_sellerID_wide <- customerUID_sellerID_long %>%
  select(customer_unique_id, seller_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(seller_id, value, fill = 0)

seller_transaction  <- as(as.matrix(customerUID_sellerID_wide[, -1]), "transactions")
seller_transaction_df <- as(seller_transaction, "data.frame")

frequent.sellers <- eclat(seller_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.sellers)
itemFrequencyPlot(seller_transaction, topN=10, type="relative", main="Top 10 Sellers Frequency")

# support threshold = 0.00002 -> 464 observations
# support threshold = 0.00005 -> min count = 5 -> reasonable enough?
seller_rules <- apriori (seller_transaction, parameter = list(supp = 0.00005, conf = 0.05, minlen = 2))
inspect(seller_rules)
seller_rules.df <- as(seller_rules, "data.frame")
seller_subRules<-seller_rules[quality(seller_rules)$lift > 1]
seller_subRules.df <- as(seller_subRules, "data.frame")

#####################################################################################
# 1 customer 1 basket - items -> product_id, can't run, way too big
customerUID_productID_long <- data[c('customer_unique_id','product_id')]
customerUID_productID_long$product_id <- as.factor(customerUID_productID_long$product_id)

product_transaction  <- as(split(customerUID_productID_long[,"product_id"], customerUID_productID_long[,"customer_unique_id"]), "transactions")
###STOP###
set.seed(1)
# 1 customer 1 basket - items -> product_id, w/ sampling (8 rules) but changes a lot based on sampling :'))
sample_data <- customerUID_productID_long[customerUID_productID_long$customer_unique_id %in% sample(customerUID_productID_long$customer_unique_id,10000),]
customerUID_productID_wide <- sample_data %>%
  select(customer_unique_id, product_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_id, value, fill = 0)

product_transaction  <- as(as.matrix(customerUID_productID_wide[, -1]), "transactions")
product_transaction_df <- as(product_transaction, "data.frame")

frequent.products <- eclat(product_transaction, parameter = list(supp = 0.001, maxlen = 10))
inspect(frequent.products)
itemFrequencyPlot(product_transaction, topN=10, type="relative", main="Top 10 Products Frequency")

product_rules <- apriori (product_transaction, parameter = list(supp = 0.0005, conf = 0.05, minlen = 2))
inspect(product_rules)
product_rules.df <- as(product_rules, "data.frame")
product_subRules<-product_rules[quality(product_rules)$lift > 1]
product_subRules.df <- as(product_subRules, "data.frame")


setwd(paste(getwd(),'/Data',sep=""))
library(arules)
library(tidyr)
library(dplyr)

data <- read.csv('Orders_merged.csv')

#####################################################################################
# 1 customer 1 basket - items -> categories (18 rules)
customerUID_productCat_long <- data[c('customer_unique_id','product_category_name_english')]
customerUID_productCat_long$customer_unique_id <- as.factor(customerUID_productCat_long$customer_unique_id)
customerUID_productCat_long$product_category_name_english <- as.factor(customerUID_productCat_long$product_category_name_english)

customerUID_productCat_long <- customerUID_productCat_long %>%
  select(customer_unique_id, product_category_name_english) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_category_name_english, value, fill = 0)

transaction  <- as(as.matrix(customerUID_productCat_long[, -1]), "transactions")
transaction_df <- as(transaction, "data.frame")

frequent.items <- eclat(transaction, parameter = list(supp = 0.05, maxlen = 10))
inspect(frequent.items)
itemFrequencyPlot(transaction, topN=10, type="relative", main="Top 10 Item Frequency")

rules <- apriori (transaction, parameter = list(supp = 0.00002, conf = 0.1, minlen = 2))
inspect(rules)
rules.df <- as(rules, "data.frame")
subRules<-rules[quality(rules)$lift > 1]
subRules.df <- as(subRules, "data.frame")

#####################################################################################
# 1 customer 1 basket - items -> seller_id (16 rules)
customerUID_sellerID_long <- data[c('customer_unique_id','seller_id')]
customerUID_sellerID_long$customer_unique_id <- as.factor(customerUID_sellerID_long$customer_unique_id)
customerUID_sellerID_long$seller_id <- as.factor(customerUID_sellerID_long$seller_id)

customerUID_sellerID_wide <- customerUID_sellerID_long %>%
  select(customer_unique_id, seller_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(seller_id, value, fill = 0)

transaction  <- as(as.matrix(customerUID_sellerID_wide[, -1]), "transactions")
transaction_df <- as(transaction, "data.frame")

frequent.items <- eclat(transaction, parameter = list(supp = 0.05, maxlen = 10))
inspect(frequent.items)
itemFrequencyPlot(transaction, topN=10, type="relative", main="Top 10 Item Frequency")

# support threshold = 0.00002 -> 464 observations
# support threshold = 0.00005 -> min count = 5 -> reasonable enough?
rules <- apriori (transaction, parameter = list(supp = 0.00005, conf = 0.05, minlen = 2))
inspect(rules)
rules.df <- as(rules, "data.frame")
subRules<-rules[quality(rules)$lift > 1]
subRules.df <- as(subRules, "data.frame")

#####################################################################################
# 1 customer 1 basket - items -> product_id, can't run, way too big
customerUID_product_ID_long <- data[c('customer_unique_id','product_id')]
customerUID_product_ID_long$customer_unique_id <- as.factor(customerUID_product_ID_long$customer_unique_id)
customerUID_product_ID_long$product_id <- as.factor(customerUID_product_ID_long$product_id)

transaction  <- as(split(customerUID_product_ID_long[,"product_id"], customerUID_product_ID_long[,"customer_unique_id"]), "transactions")
###STOP###
set.seed(1)
# 1 customer 1 basket - items -> product_id, w/ sampling (8 rules) but changes a lot based on sampling :'))
sample_data <- customerUID_product_ID_long[customerUID_product_ID_long$customer_unique_id %in% sample(customerUID_product_ID_long$customer_unique_id,10000),]
customerUID_productID_wide <- sample_data %>%
  select(customer_unique_id, product_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_id, value, fill = 0)

transaction  <- as(as.matrix(customerUID_productID_wide[, -1]), "transactions")
transaction_df <- as(transaction, "data.frame")

frequent.items <- eclat(transaction, parameter = list(supp = 0.05, maxlen = 10))
inspect(frequent.items)
itemFrequencyPlot(transaction, topN=10, type="relative", main="Top 10 Item Frequency")

rules <- apriori (transaction, parameter = list(supp = 0.0005, conf = 0.05, minlen = 2))
inspect(rules)
rules.df <- as(rules, "data.frame")
subRules<-rules[quality(rules)$lift > 1]
subRules.df <- as(subRules, "data.frame")


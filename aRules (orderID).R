setwd(paste(getwd(),'/Data',sep=""))
library(arules)
library(tidyr)
library(dplyr)

data <- read.csv('Orders_merged.csv')
data$order_id = as.factor(data$order_id)

#####################################################################################
# 1 order 1 basket - items -> categories (2 rules)

# dataframe with order ID and product category
order_ID_productCat_long <- data[c('order_id','product_category_name_english')]
order_ID_productCat_long$product_category_name_english <- as.factor(order_ID_productCat_long$product_category_name_english)

# convert to wide format
order_ID_productCat_wide <- order_ID_productCat_long %>%
  select(order_id, product_category_name_english) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_category_name_english, value, fill = 0)

# convert to transaction object
cat_transaction  <- as(as.matrix(order_ID_productCat_wide[, -1]), "transactions")
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
# 1 order 1 basket - items -> seller_id (6 rules)

# dataframe with order ID and seller ID
order_ID_sellerID_long <- data[c('order_id','seller_id')]
order_ID_sellerID_long$seller_id <- as.factor(order_ID_sellerID_long$seller_id)

# convert to wide format
order_ID_sellerID_wide <- order_ID_sellerID_long %>%
  select(order_id, seller_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(seller_id, value, fill = 0)

# convert to transaction object
seller_transaction  <- as(as.matrix(order_ID_sellerID_wide[, -1]), "transactions")
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
# 1 order 1 basket - items (21 rules)

# dataframe with order ID and product ID
order_ID_productID_long <- data[c('order_id','product_id')]
order_ID_productID_long$product_id <- as.factor(order_ID_productID_long$product_id)

# convert to transaction object
product_transaction <- transactions(order_ID_productID_long, format = "long")
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


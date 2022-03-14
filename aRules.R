setwd(paste(getwd(),'/Data',sep=""))
library(arules)
library(tidyr)
library(dplyr)

data <- read.csv('Orders_merged.csv')

orderID_productCat_long <- data[c('order_id','product_category_name_english')]
orderID_productCat_long$order_id <- as.factor(orderID_productCat_long$order_id)
orderID_productCat_long$product_category_name_english <- as.factor(orderID_productCat_long$product_category_name_english)

orderID_productCat_long <- orderID_productCat_long %>%
  select(order_id, product_category_name_english) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(product_category_name_english, value, fill = 0)

transaction  <- as(as.matrix(orderID_productCat_long[, -1]), "transactions")
transaction_df <- as(transaction, "data.frame")

frequent.items <- eclat(transaction, parameter = list(supp = 0.05, maxlen = 10))
inspect(frequent.items)
itemFrequencyPlot(transaction, topN=10, type="relative", main="Top 10 Item Frequency")

rules <- apriori (transaction, parameter = list(supp = 0.0001, conf = 0.0001, minlen = 2))
inspect(rules)
rules.df <- as(rules, "data.frame")
subRules<-rules[quality(rules)$lift > 1]
subRules.df <- as(subRules, "data.frame")




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

rules <- apriori (transaction, parameter = list(supp = 0.0001, conf = 0.0001, minlen = 2))
inspect(rules)
rules.df <- as(rules, "data.frame")
subRules<-rules[quality(rules)$lift > 1]
subRules.df <- as(subRules, "data.frame")

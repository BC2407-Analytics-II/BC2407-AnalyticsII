#################################  B C 2 4 0 7   S E M I N A R   1  #################################
###########################################  T E A M   7  ###########################################
################################### Instructor: Prof Neumann Chew ###################################

# <-------- This R script is formatted to fit on a window of width specified by this line --------> #

tryCatch(setwd(paste(getwd(),'/Data',sep="")), error = function(e) {    # set working directory to 
  paste('Directory is:', getwd())                                     # the 'Data' folder in the
})                                                                      # group project.

# import required libraries
library(arules)
library(tidyr)
library(dplyr)
library(mondate)

#####################################################################################################
#######                                   DATA PREPROCESSING                                  #######

data <- read.csv('Orders_merged.csv')

# create a new variable 'year/quarter' - "Q?/YY"
data$order_purchase_timestamp <-
  as.POSIXct(data$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
data$'year/quarter' <- paste(format(data$order_purchase_timestamp, "%y/"), 0, 
      sub( "Q", "", quarters(data$order_purchase_timestamp) ), sep = "")

# create a new variable customerUID_year/quarter' - "customer UID-Q?/YY"
data$'customerUID_year/quarter' <- paste(data$customer_unique_id, as.character(data$'year/quarter'), 
                                         sep = "-")
data$'customerUID_year/quarter' <- as.factor(data$'customerUID_year/quarter')

# create a new variable 'product' - "product category/seller id (product id)"
data$product <- paste(data$product_category_name_english, "/",
                      data$seller_id, " (", 
                      data$product_id, ")", 
                      sep ='')

#####################################################################################################
#######                          ASSOCIATION RULE MINING (CATEGORIES)                         #######
# # 1 customer (within same quarter) 1 basket - items -> categories (6 rules)
# 
# # dataframe with customer UID and product category
# customerUID_productCat_long <- data[c('customerUID_year/quarter','product_category_name_english')]
# customerUID_productCat_long$product_category_name_english <- as.factor(
#  customerUID_productCat_long$product_category_name_english)
# 
# # convert to wide format
# customerUID_productCat_wide <- customerUID_productCat_long %>%
#   select(`customerUID_year/quarter`, product_category_name_english) %>% 
#   distinct() %>%
#   mutate(value = 1) %>%
#   spread(product_category_name_english, value, fill = 0)
# 
# # convert to transaction object
# cat_transaction  <- as(as.matrix(customerUID_productCat_wide[, -1]), "transactions")
# cat_transaction_df <- as(cat_transaction, "data.frame") # for viewing
# 
# # get frequent product categories
# frequent.cats <- eclat(cat_transaction, parameter = list(supp = 0.01, maxlen = 10))
# inspect(frequent.cats)
# itemFrequencyPlot(cat_transaction, topN=10, type="relative", main="Top 10 Categories Frequency")
# 
# # get rules (min. count = 2)
# cat_rules <- apriori (cat_transaction, parameter = list(supp = 0.00002, conf = 0.1, minlen = 2))
# inspect(cat_rules)
# cat_rules.df <- as(cat_rules, "data.frame")
# cat_subRules<-cat_rules[quality(cat_rules)$lift > 1] # get those with lift > 1 
# cat_subRules.df <- as(cat_subRules, "data.frame")

#####################################################################################################
#######                           ASSOCIATION RULE MINING (SELLERS)                           #######

# 1 customer 1 basket - items -> seller_id (15 rules)

# dataframe with customer UID and seller ID
customerQ_sellerID_long <- data[c('customerUID_year/quarter','seller_id')]
customerQ_sellerID_long$seller_id <- as.factor(customerQ_sellerID_long$seller_id)

# convert to wide format
customerQ_sellerID_wide <- customerQ_sellerID_long %>%
  select('customerUID_year/quarter', seller_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(seller_id, value, fill = 0)

# convert to transaction object
seller_transaction  <- as(as.matrix(customerQ_sellerID_wide[, -1]), "transactions")
seller_transaction_df <- as(seller_transaction, "data.frame")

# get frequent sellers
frequent.sellers <- eclat(seller_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.sellers)
itemFrequencyPlot(seller_transaction, topN=10, type="relative", main="Top 10 Sellers Frequency")

# get rules (min. count = 5) sufficient??
seller_rules <- apriori (seller_transaction, parameter = list(supp = 0.00005, conf = 0.05, 
                                                              minlen = 2))
inspect(seller_rules)
seller_rules.df <- as(seller_rules, "data.frame")
seller_subRules<-seller_rules[quality(seller_rules)$lift > 1] # get those with lift > 1 
seller_subRules.df <- as(seller_subRules, "data.frame")

# save subrules as csv file
write.csv(seller_subRules.df, "Olist subRules (sellers).csv")
#####################################################################################################
#######                          ASSOCIATION RULE MINING (PRODUCTS)                           #######

# 1 customer (within same quarter) 1 basket - items (28 rules)

# dataframe with customer UID and product ID
customerQ_product_long <- data[c('customerUID_year/quarter','product')]
customerQ_product_long$product <- as.factor(customerQ_product_long$product)

# convert to transaction object
product_transaction <- transactions(customerQ_product_long, format = "long")
product_transaction_df <- as(product_transaction,"data.frame")

# get frequent products
frequent.products <- eclat(product_transaction, parameter = list(supp = 0.001, maxlen = 10))
inspect(frequent.products)
itemFrequencyPlot(product_transaction, topN=10, type="relative", main="Top 10 Products Frequency")

# get rules (min. count = 5) sufficient??
product_rules <- apriori (product_transaction, parameter = list(supp = 0.00005, conf = 0.1, 
                                                                minlen = 2))
inspect(product_rules)
product_rules.df <- as(product_rules, "data.frame")
product_subRules<-product_rules[quality(product_rules)$lift > 1] # get those with lift > 1 
product_subRules.df <- as(product_subRules, "data.frame")

# save subrules as csv file
write.csv(product_subRules.df, "Olist subRules (products).csv")
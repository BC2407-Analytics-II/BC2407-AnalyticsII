#################################  B C 2 4 0 7   S E M I N A R   1  #################################
###########################################  T E A M   7  ###########################################
################################### Instructor: Prof Neumann Chew ###################################

# <-------- This R script is formatted to fit on a window of width specified by this line --------> #

# set working directory to the relative path of the '2. Datasets' folder of the grp project
tryCatch(setwd(paste(getwd(),'./../2. Datasets',sep="")), error = function(e) {   
    paste('Directory is:', getwd())                                               
})                                                                                

# import required libraries
library(arules)
library(tidyr)
library(dplyr)
library(mondate)

#####################################################################################################
#######                                   DATA PREPROCESSING                                  #######

## UCI DATASET ##
uci_data <- read.csv('uci_online_retail_cleaned.csv')

uci_data$InvoiceNo <- as.factor(uci_data$InvoiceNo)

# create a new variable 'product' - "product description (StockCode)"
uci_data$product <- paste(uci_data$Description, " (", uci_data$StockCode, sep = '')
uci_data$product <- as.factor(uci_data$product)

## OLIST DATASET ##

olist_data <- read.csv('Orders_merged.csv')

# create a new variable 'year/quarter' - "Q?/YY"
olist_data$order_purchase_timestamp <-
  as.POSIXct(olist_data$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")
olist_data$'year/quarter' <- paste(format(olist_data$order_purchase_timestamp, "%y/"), 0, 
                                   sub( "Q", "", quarters(olist_data$order_purchase_timestamp) ), sep = "")

# create a new variable customerUID_year/quarter' - "customer UID-Q?/YY"
olist_data$'customerUID_year/quarter' <- paste(olist_data$customer_unique_id, 
                                               as.character(olist_data$'year/quarter'), 
                                               sep = "-")
olist_data$'customerUID_year/quarter' <- as.factor(olist_data$'customerUID_year/quarter')

# create a new variable 'product' - "product category/seller id (product id)"
olist_data$product <- paste(olist_data$product_category_name_english, "/",
                            olist_data$seller_id, " (", 
                            olist_data$product_id, ")", 
                            sep ='')

#####################################################################################################
#######                         ASSOCIATION RULE MINING (UCI PRODUCTS)                        #######

# convert to wide format
invoiceNo_product_wide <- uci_data %>%
  select(InvoiceNo, product) %>% 
  distinct() %>%
  mutate(value = 1) %>%
  spread(product, value, fill = 0)

# convert to transaction object
uci_transaction  <- as(as.matrix(invoiceNo_product_wide[, -1]), "transactions")
uci_transaction_df <- as(uci_transaction, "data.frame") # for viewing

# get frequent product categories
frequent.cats <- eclat(uci_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.cats)
itemFrequencyPlot(uci_transaction, topN=10, type="relative", main="Top 10 items (stock code)")

# get rules (min. count = 2)
uci_rules <- apriori (uci_transaction, parameter =  list(minlen = 2, supp=0.01, conf = 0.01, 
                                                         target = "rules"))
inspect(uci_rules)
uci_rules.df <- as(uci_rules, "data.frame")
uci_subRules<-uci_rules[quality(uci_rules)$lift > 1] # get those with lift > 1 
uci_subRules.df <- as(uci_subRules, "data.frame") 

# save subrules as csv file
write.csv(uci_subRules.df, "UCI subRules.csv")

#####################################################################################################
#######                        ASSOCIATION RULE MINING (OLIST SELLERS)                        #######

# 1 customer 1 basket - items -> seller_id (15 rules)

# dataframe with customer UID and seller ID
customerQ_sellerID_long <- olist_data[c('customerUID_year/quarter','seller_id')]
customerQ_sellerID_long$seller_id <- as.factor(customerQ_sellerID_long$seller_id)

# convert to wide format
customerQ_sellerID_wide <- customerQ_sellerID_long %>%
  select('customerUID_year/quarter', seller_id) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(seller_id, value, fill = 0)

# convert to transaction object
olist_seller_transaction  <- as(as.matrix(customerQ_sellerID_wide[, -1]), "transactions")
olist_seller_transaction_df <- as(olist_seller_transaction, "data.frame")

# get frequent sellers
frequent.sellers <- eclat(olist_seller_transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.sellers)
itemFrequencyPlot(olist_seller_transaction, topN=10, type="relative", main="Top 10 Sellers Frequency")

# get rules (min. count = 5) sufficient??
olist_seller_rules <- apriori (olist_seller_transaction, parameter = list(supp = 0.00005, conf = 0.05, 
                                                                          minlen = 2))
inspect(olist_seller_rules)
olist_seller_rules.df <- as(olist_seller_rules, "data.frame")
olist_seller_subRules<-olist_seller_rules[quality(olist_seller_rules)$lift > 1] # get those with lift > 1 
olist_seller_subRules.df <- as(olist_seller_subRules, "data.frame")

# save subrules as csv file
write.csv(olist_seller_subRules.df, "Olist subRules (sellers).csv")

#####################################################################################################
#######                        ASSOCIATION RULE MINING (OLIST PRODUCTS)                       #######

# 1 customer (within same quarter) 1 basket - items (28 rules)

# dataframe with customer UID and product ID
customerQ_product_long <- olist_data[c('customerUID_year/quarter','product')]
customerQ_product_long$product <- as.factor(customerQ_product_long$product)

# convert to transaction object
olist_product_transaction <- transactions(customerQ_product_long, format = "long")
olist_product_transaction_df <- as(product_transaction,"data.frame")

# get frequent products
frequent.products <- eclat(olist_product_transaction, parameter = list(supp = 0.001, maxlen = 10))
inspect(frequent.products)
itemFrequencyPlot(olist_product_transaction, topN=10, type="relative", main="Top 10 Products Frequency")

# get rules (min. count = 5) sufficient??
olist_product_rules <- apriori (olist_product_transaction, parameter = list(supp = 0.00005, conf = 0.1, 
                                                                            minlen = 2))
inspect(olist_product_rules)
olist_product_rules.df <- as(olist_product_rules, "data.frame")
olist_product_subRules<-olist_product_rules[quality(olist_product_rules)$lift > 1] # get those with lift > 1 
olist_product_subRules.df <- as(olist_product_subRules, "data.frame")

# save subrules as csv file
write.csv(olist_product_subRules.df, "Olist subRules (products).csv")

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

#####################################################################################################
#######                                   DATA PREPROCESSING                                  #######

data <- read.csv('uci_online_retail_cleaned.csv')

data$InvoiceNo <- as.factor(data$InvoiceNo)

# create a new variable 'product' - "product description (StockCode)"
data$product <- paste(data$Description, " (", data$StockCode, sep = '')
data$product <- as.factor(data$product)

# convert to wide format
invoiceNo_product_wide <- data %>%
  select(InvoiceNo, product) %>% 
  distinct() %>%
  mutate(value = 1) %>%
  spread(product, value, fill = 0)

# convert to transaction object
transaction  <- as(as.matrix(invoiceNo_product_wide[, -1]), "transactions")
transaction_df <- as(transaction, "data.frame") # for viewing

#####################################################################################################
#######                                    DATA EXPLORATION                                   #######

# get frequent product categories
frequent.cats <- eclat(transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.cats)
itemFrequencyPlot(transaction, topN=10, type="relative", main="Top 10 items (stock code)")

#####################################################################################################
#######                                 ASSOCIATION RULE MINING                               #######

# get rules (min. count = 2)
rules <- apriori (transaction, parameter =  list(minlen = 2, supp=0.01, conf = 0.01, 
                                                     target = "rules"))
inspect(rules)
rules.df <- as(rules, "data.frame")
subRules<-rules[quality(ules)$lift > 1] # get those with lift > 1 
subRules.df <- as(subRules, "data.frame") 

# save subrules as csv file
write.csv(subRules.df, "UCI subRules.csv")
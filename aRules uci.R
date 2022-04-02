setwd(paste(getwd(),'/Data',sep=""))
library(arules)
library(tidyr)
library(dplyr)

data <- read.csv('uci_online_retail_cleaned.csv')

data$InvoiceNo <- as.factor(data$InvoiceNo)

data$StockCode <- as.factor(data$StockCode)

# convert to wide format
invoiceNo_stockCode_wide <- data %>%
  select(InvoiceNo, StockCode) %>% 
  distinct() %>%
  mutate(value = 1) %>%
  spread(StockCode, value, fill = 0)

# convert to transaction object
transaction  <- as(as.matrix(invoiceNo_stockCode_wide[, -1]), "transactions")
transaction_df <- as(transaction, "data.frame") # for viewing

# get frequent product categories
frequent.cats <- eclat(transaction, parameter = list(supp = 0.01, maxlen = 10))
inspect(frequent.cats)
itemFrequencyPlot(transaction, topN=10, type="relative", main="Top 10 items (stock code)")

# get rules (min. count = 2)
cat_rules <- apriori (transaction, parameter =  list(minlen = 2, supp=0.01, conf = 0.01, target = "rules"))
inspect(cat_rules)
cat_rules.df <- as(cat_rules, "data.frame")
cat_subRules<-cat_rules[quality(cat_rules)$lift > 1] # get those with lift > 1 
cat_subRules.df <- as(cat_subRules, "data.frame")

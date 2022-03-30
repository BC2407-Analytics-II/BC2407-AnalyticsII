library(randomForest)

setwd(paste(getwd(),'/Data',sep=""))

source("../helperFns.R")

df <- read.csv("uci_online_retail_cleaned_CLV.csv")


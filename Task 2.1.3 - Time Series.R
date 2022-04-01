#################################  B C 2 4 0 7   S E M I N A R   1  #################################
###########################################  T E A M   7  ###########################################
################################### Instructor: Prof Neumann Chew ###################################

# <-------- This R script is formatted to fit on a window of width specified by this line --------> #

tryCatch(setwd(paste(getwd(),'/Data',sep="")), error = function(e) {    # set working directory to 
    paste('Directory is:', getwd())                                     # the 'Data' folder in the
})                                                                      # group project.

source("../helperFns.R")    # import list of helper functions we've written separately

library("dplyr")            # for group_by
library("forecast")         # for generating h-period ahead forecasts

#####################################################################################################
#######                                   DATA PREPROCESSING                                  #######

#### read in our two different datasets ####
df <- read.csv('uci_online_retail_cleaned_CLV.csv')
df2 <- read.csv('Orders_merged_CLV.csv')

# we want to calculate CLV over time. therefore, we need to group sales by TIME, not by customer.
#### remove the RFM columns ####
rmCols <- c('RECENCY', 'FREQUENCY', 'MONEY', 
           'RECENCY_normalised', 'FREQUENCY_normalised', 'MONEY_normalised')
df <- df[,!(names(df) %in% rmCols)]
df2 <- df2[,!(names(df2) %in% rmCols)]

#### calculate rev ####
df['CLV'] <- df['Quantity'] * df['UnitPrice']
df2['CLV'] <- 1 * df2['price']

#### create a month-year column ####
df$InvoiceDate <- as.POSIXct(df$InvoiceDate,format="%Y-%m-%d %H:%M:%S",tz="Europe/London")
df2$order_purchase_timestamp <-
    as.POSIXct(df2$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")

df['MnthYr'] <- format(as.Date(df$InvoiceDate), "%Y-%m")
df2['MnthYr'] <- format(as.Date(df2$order_purchase_timestamp), "%Y-%m")

#### group by MnthYr and sum CLV ####
dfTS <- df %>% group_by(MnthYr) %>%
    summarise(
        total_CLV = sum(CLV),
        .groups = 'drop'
        )
dfTS <- dfTS[order(dfTS$MnthYr),]

df2TS <- df2 %>% group_by(MnthYr) %>%
    summarise(
        total_CLV = sum(CLV),
        .groups = 'drop'
    )
df2TS <- df2TS[order(df2TS$MnthYr),]

#### last month of df is incomplete - remove ####
dfTS <- dfTS[!(dfTS$MnthYr=='2011-12'),]

#### 2016 data for df2 is incomplete - remove ####
df2TS <- df2TS[!(df2TS$MnthYr=='2016-09'),]
df2TS <- df2TS[!(df2TS$MnthYr=='2016-10'),]
df2TS <- df2TS[!(df2TS$MnthYr=='2016-12'),]

#### create time series object ####
dfTS.ts <- ts(dfTS$total_CLV, frequency = 12, start = c(2010,12))
dfTS.ts

plot.ts(dfTS.ts, ylab = "CLV", xlab = "Year-Quarter",
        main = "Customer Lifetime Value over time",
        sub = "Source: UCI")

df2TS.ts <- ts(df2TS$total_CLV, frequency = 12, start = c(2017,1))
df2TS.ts

plot.ts(df2TS.ts, ylab = "CLV", xlab = "Year-Quarter",
        main = "Customer Lifetime Value over time",
        sub = "Source: Olist")

#######                                    TRAIN-TEST SPLIT                                   #######
trainset1 <- ts(dfTS.ts[-c(11:12)], frequency = 12, start = c(2010,12))
trainset1
testset1 <- ts(dfTS.ts[c(11:12)], frequency = 12, start = c(2011,10))
testset1

trainset2 <- ts(df2TS.ts[-c(18:20)], frequency = 12, start = c(2017,1))
trainset2
testset2 <- ts(df2TS.ts[c(18:20)], frequency = 12, start = c(2018,6))
testset2

#################################    SIMPLE EXPONENTIAL SMOOTHING   #################################

# Simple Exponential Smoothing - df1 ---------------------------------------------
m.ses <- HoltWinters(trainset1, seasonal = "multiplicative", beta=F, gamma=F)
m.ses
## Optimal value of alpha = 0.5241683 Minimise SSE of one period ahead forecast.
## Coefficient a = last value of Lt.
plot(m.ses, main = "Simple Exp Smoothing on Short Data")
#black = observed, red = one period ahead forecast
m.ses$fitted
m.ses.forecasts <- forecast(m.ses, h = 2)
plot(m.ses.forecasts, main = "2 Period Ahead Forecasts based on SES")
accuracy(m.ses.forecasts, testset1)

# Simple Exponential Smoothing - df2 ---------------------------------------------
m.ses <- HoltWinters(trainset2, seasonal = "multiplicative", beta=F, gamma=F)
m.ses
## Optimal value of alpha = 0.7446712 Minimise SSE of one period ahead forecast.
## Coefficient a = last value of Lt.
plot(m.ses, main = "Simple Exp Smoothing on Medium Data")
#black = observed, red = one period ahead forecast
m.ses$fitted
m.ses.forecasts <- forecast(m.ses, h = 3)
plot(m.ses.forecasts, main = "3 Period Ahead Forecasts based on SES")
accuracy(m.ses.forecasts, testset2)


#################################           HOLT'S METHOD           #################################

# Holt's Method - df1 ---------------------------------------------
m.holt <- HoltWinters(trainset1, seasonal = "multiplicative", gamma=F)
m.holt
plot(m.holt, main = "Holt's Method on Short Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = 2)
plot(m.holt.forecasts, main = "2 Period Ahead Forecasts based on Holt's")
accuracy(m.holt.forecasts, testset1)

# Holt's Method - df2 ---------------------------------------------
m.holt <- HoltWinters(trainset2, seasonal = "multiplicative", gamma=F)
m.holt
plot(m.holt, main = "Holt's Method on Medium Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = 2)
plot(m.holt.forecasts, main = "3 Period Ahead Forecasts based on Holt's")
accuracy(m.holt.forecasts, testset2)

#################################        HOLT-WINTER'S METHOD       #################################
m.winters <- HoltWinters(trainset1, seasonal = "multiplicative")

m.winters <- HoltWinters(trainset2, seasonal = "multiplicative")

#### bringing in a new dataset for Holt-Winters ####
df3 = read.csv('Raw Data/timeseries.csv')
df3['CLV'] = df3['Sales']
df3$Order.Date <- as.POSIXct(df3$Order.Date,format="%d/%m/%Y")
df3['MnthYr'] <- format(as.Date(df3$Order.Date), "%Y-%m")
df3TS = df3 %>% group_by(MnthYr) %>%
    summarise(
        total_CLV = sum(CLV),
        .groups = 'drop'
    )
df3TS = df3TS[order(df3TS$MnthYr),]
#data is complete - don't need remove
df3TS.ts <- ts(df3TS$total_CLV, frequency = 12, start = c(2015,1))
df3TS.ts

plot.ts(df3TS.ts, ylab = "CLV", xlab = "Year-Quarter",
        main = "Customer Lifetime Value over time",
        sub = "Source: Kaggle")

trainset3 <- ts(df3TS.ts[-c(45:48)], frequency = 12, start = c(2015,1))
trainset3
testset3 <- ts(df3TS.ts[c(45:48)], frequency = 12, start = c(2018,9))
testset3

m.holt <- HoltWinters(trainset3, seasonal = "multiplicative")
m.holt
plot(m.holt, main = "Holt's Method on Long Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = 2)
plot(m.holt.forecasts, main = "4 Period Ahead Forecasts based on Holt's")
accuracy(m.holt.forecasts, testset3)

#######                                          END                                          #######
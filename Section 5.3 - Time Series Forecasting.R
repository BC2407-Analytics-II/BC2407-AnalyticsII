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
library("lubridate")        # for date_decimal
library("ggplot2")

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
df['revenue'] <- df['Quantity'] * df['UnitPrice']
df2['revenue'] <- 1 * df2['price']

#### create a month-year column ####
df$InvoiceDate <- as.POSIXct(df$InvoiceDate,format="%Y-%m-%d %H:%M:%S",tz="Europe/London")
df2$order_purchase_timestamp <-
    as.POSIXct(df2$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S",tz="America/Sao_Paulo")

df['MnthYr'] <- format(as.Date(df$InvoiceDate), "%Y-%m")
df2['MnthYr'] <- format(as.Date(df2$order_purchase_timestamp), "%Y-%m")

#### total revenue ####
df.totalRevenue <- df %>% group_by(MnthYr) %>%
    summarise(
        total_Rev = sum(revenue),
        .groups = 'drop'
    )
df.totalRevenue <- df.totalRevenue[order(df.totalRevenue$MnthYr),]

df2.totalRevenue <- df2 %>% group_by(MnthYr) %>%
    summarise(
        total_Rev = sum(revenue),
        .groups = 'drop'
    )
df2.totalRevenue <- df2.totalRevenue[order(df2.totalRevenue$MnthYr),]

#### total purchases ####
df.totalPurchases <- df %>% group_by(MnthYr) %>%
    summarise(
        total_Pur = sum(Quantity),
        .groups = 'drop'
    )
df.totalPurchases <- df.totalPurchases[order(df.totalPurchases$MnthYr),]

df2.totalPurchases <- df2 %>% group_by(MnthYr) %>%
    summarise(
        total_Pur = n(),
        .groups = 'drop'
    )
df2.totalPurchases <- df2.totalPurchases[order(df2.totalPurchases$MnthYr),]

#### total unique customers ####
df.totalCustomers <- df %>% group_by(MnthYr) %>%
    summarise(
        total_Cust = n_distinct(CustomerID),
        .groups = 'drop'
    )
df.totalCustomers <- df.totalCustomers[order(df.totalCustomers$MnthYr),]

df2.totalCustomers <- df2 %>% group_by(MnthYr) %>%
    summarise(
        total_Cust = n_distinct(customer_id),
        .groups = 'drop'
    )
df2.totalCustomers <- df2.totalCustomers[order(df2.totalCustomers$MnthYr),]

#### join all into one dataframe ####
df.MERGE = merge(x = df.totalRevenue, y = df.totalPurchases, by = 'MnthYr', all.x = TRUE)
df.MERGE = merge(x = df.MERGE, y = df.totalCustomers, by = 'MnthYr', all.x = TRUE)

df2.MERGE = merge(x = df2.totalRevenue, y = df2.totalPurchases, by = 'MnthYr', all.x = TRUE)
df2.MERGE = merge(x = df2.MERGE, y = df2.totalCustomers, by = 'MnthYr', all.x = TRUE)

#### calculate average purchase value ####
df.MERGE['AVG_PUR_VAL'] = df.MERGE$total_Rev/df.MERGE$total_Pur
df.MERGE['AVG_PUR_FREQ'] = df.MERGE$total_Pur/df.MERGE$total_Cust

df2.MERGE['AVG_PUR_VAL'] = df2.MERGE$total_Rev/df2.MERGE$total_Pur
df2.MERGE['AVG_PUR_FREQ'] = df2.MERGE$total_Pur/df2.MERGE$total_Cust

#### calculate CLV ####
df.MERGE['CLV'] = df.MERGE['AVG_PUR_VAL']*df.MERGE['AVG_PUR_FREQ']
df2.MERGE['CLV'] = df2.MERGE['AVG_PUR_VAL']*df2.MERGE['AVG_PUR_FREQ']

#### group by MnthYr and sum CLV ####
dfTS <- df.MERGE %>% group_by(MnthYr) %>%
    summarise(
        total_CLV = sum(CLV),
        .groups = 'drop'
    )
dfTS <- dfTS[order(dfTS$MnthYr),]

df2TS <- df2.MERGE %>% group_by(MnthYr) %>%
    summarise(
        total_CLV = sum(CLV),
        .groups = 'drop'
    )
df2TS <- df2TS[order(df2TS$MnthYr),]

#### 2016 data for df2 is incomplete - remove ####
df2TS <- df2TS[!(df2TS$MnthYr=='2016-09'),]
df2TS <- df2TS[!(df2TS$MnthYr=='2016-10'),]
df2TS <- df2TS[!(df2TS$MnthYr=='2016-12'),]

#### create time series object ####
dfTS.ts <- ts(dfTS$total_CLV, frequency = 12, start = c(2010,12))
dfTS.ts

#################################################################
######################### FUNCTION: viz #########################
#################################################################
### This function takes in a dataframe (without quotes), a hexadecimal colour code (with "quotes")
### for the colour of the line, and the dataset's source (with "quotes")
### and prints the time-series in ggplot.
viz <- function(df, color_string, source_string) {
    df_viz=df
    df_viz$MnthYr <- paste(df_viz$MnthYr,'-01',sep='')              # change to YYYY-MM-DD (DD=01)
    df_viz$MnthYr <- as.POSIXct(df_viz$MnthYr,format="%Y-%m-%d")    # change back to datetime
    df_viz$MnthYr <- as.Date(df_viz$MnthYr)
    
    options(scipen=10000)                                           # don't use scientific notation
    print(
        ggplot(df_viz, aes(x=MnthYr, y=total_CLV, group=1)) + 
            geom_line(color=color_string, size=1.25) + 
            labs(y='Customer Lifetime Value / $', x='Month-Year',
                 title='Customer Lifetime Value over time',
                 caption=paste('Source:',source_string)) +                # change font (in helperFns)
            theme(plot.title = element_text(hjust = 0.5)) +             # centre-align the title
            scale_x_date(date_labels =  "%b %Y")                        # show as 'Mth 20XX'
    )
}
#################### END FUNCTION DEFINITION ####################
#################################################################

viz(dfTS, "skyblue", "UCI")

df2TS.ts <- ts(df2TS$total_CLV, frequency = 12, start = c(2017,1))
df2TS.ts

viz(df2TS, "springgreen2", "Olist")

#####################################################################################################
#######                                    TRAIN-TEST SPLIT                                   #######
FORECAST_PERIODS = 3

end = nrow(dfTS)
start = end-FORECAST_PERIODS+1
trainset1 <- ts(dfTS.ts[-c(start:end)], frequency = 12, start = c(2010,12))
trainset1
testset1 <- ts(dfTS.ts[c(start:end)], frequency = 12, start = c(2011,10))
testset1

end = nrow(df2TS)
start = end-FORECAST_PERIODS+1
trainset2 <- ts(df2TS.ts[-c(start:end)], frequency = 12, start = c(2017,1))
trainset2
testset2 <- ts(df2TS.ts[c(start:end)], frequency = 12, start = c(2018,6))
testset2

#####################################################################################################
#################################    SIMPLE EXPONENTIAL SMOOTHING   #################################

# Simple Exponential Smoothing - df1 ---------------------------------------------
m.ses <- HoltWinters(trainset1, seasonal = "multiplicative", beta=F, gamma=F)
m.ses
## Coefficient a = last value of Lt.
plot(m.ses, main = "Simple Exp Smoothing on Short Data")
#black = observed, red = one period ahead forecast
m.ses$fitted
m.ses.forecasts <- forecast(m.ses, h = FORECAST_PERIODS)

#################################################################
##################### FUNCTION: plotForecast ####################
#################################################################
### This function takes in a forecast object (without quotes), and number of predictions
### and prints the time-series in ggplot.
plotForecast <- function(model, forecast, trainset, testset, method) {
    options(scipen=10000)                      
    
    forecast_plot = autoplot(model$fitted[,1], series = 'Forecasted') + 
        autolayer(forecast, series = 'Forecasted') 
    
    print(
        forecast_plot +
            autolayer(trainset, series = 'Actual') +
            autolayer(testset, series = 'Actual') +
            scale_x_continuous(labels = function(x) {format(date_decimal(x), "%b %Y")}) +
            labs(y='Customer Lifetime Value', x='Month-Year',
                 title=paste(FORECAST_PERIODS,'Period Ahead Forecasts based on',method),
            ) +
            theme(plot.title = element_text(hjust = 0.5))
    )
}
#################### END FUNCTION DEFINITION ####################
#################################################################

plotForecast(m.ses, m.ses.forecasts, trainset1, testset1, "SES on UCI")

#https://stackoverflow.com/questions/38649068/r-why-does-it-mean-when-the-mase-of-a-forecast-model-is-nan
ses.1 = accuracy(m.ses.forecasts, testset1, d=1, D=0)
ses.1

# Simple Exponential Smoothing - df2 ---------------------------------------------
m.ses <- HoltWinters(trainset2, seasonal = "multiplicative", beta=F, gamma=F)
m.ses
## Optimal value of alpha = 0.7446712 Minimise SSE of one period ahead forecast.
## Coefficient a = last value of Lt.
plot(m.ses, main = "Simple Exp Smoothing on Medium Data")
#black = observed, red = one period ahead forecast
m.ses$fitted
m.ses.forecasts <- forecast(m.ses, h = FORECAST_PERIODS)
plotForecast(m.ses, m.ses.forecasts, trainset2, testset2, "SES on Olist")
ses.2 = accuracy(m.ses.forecasts, testset2)
ses.2

#####################################################################################################
#################################           HOLT'S METHOD           #################################

# Holt's Method - df1 ---------------------------------------------
m.holt <- HoltWinters(trainset1, seasonal = "multiplicative", gamma=F) #optimisation difficulties
m.holt
plot(m.holt, main = "Holt's Method on Short Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = FORECAST_PERIODS)
plotForecast(m.holt, m.holt.forecasts, trainset1, testset1, "Holt's on UCI")
holt.1 = accuracy(m.holt.forecasts, testset1, d=1, D=0)
holt.1

# Holt's Method - df2 ---------------------------------------------
m.holt <- HoltWinters(trainset2, seasonal = "multiplicative", gamma=F)
m.holt
plot(m.holt, main = "Holt's Method on Medium Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = FORECAST_PERIODS)
plotForecast(m.holt, m.holt.forecasts, trainset2, testset2, "Holt's on Olist")
holt.2 = accuracy(m.holt.forecasts, testset2)
holt.2

#####################################################################################################
#################################        HOLT-WINTER'S METHOD       #################################
m.winters <- HoltWinters(trainset1, seasonal = "multiplicative")
# time series has less than 2 periods

m.winters <- HoltWinters(trainset2, seasonal = "multiplicative") 
# time series has less than 2 periods

#### bringing in a new dataset for Holt-Winters ####
df3 = read.csv('Raw Data/timeseries.csv')
#### calculate rev ####
df3['revenue'] <- df3['Sales']
#### create a month-year column ####
df3$Order.Date <- as.POSIXct(df3$Order.Date,format="%d/%m/%Y")
df3['MnthYr'] <- format(as.Date(df3$Order.Date), "%Y-%m")
#### total revenue ####
df3.totalRevenue <- df3 %>% group_by(MnthYr) %>%
    summarise(
        total_Rev = sum(revenue),
        .groups = 'drop'
    )
df3.totalRevenue <- df3.totalRevenue[order(df3.totalRevenue$MnthYr),]
#### total purchases ####
df3.totalPurchases <- df3 %>% group_by(MnthYr) %>%
    summarise(
        total_Pur = n(),
        .groups = 'drop'
    )
df3.totalPurchases <- df3.totalPurchases[order(df3.totalPurchases$MnthYr),]
#### total unique customers ####
df3.totalCustomers <- df3 %>% group_by(MnthYr) %>%
    summarise(
        total_Cust = n_distinct(Customer.ID),
        .groups = 'drop'
    )
df3.totalCustomers <- df3.totalCustomers[order(df3.totalCustomers$MnthYr),]
#### join all into one dataframe ####
df3.MERGE = merge(x = df3.totalRevenue, y = df3.totalPurchases, by = 'MnthYr', all.x = TRUE)
df3.MERGE = merge(x = df3.MERGE, y = df3.totalCustomers, by = 'MnthYr', all.x = TRUE)
#### calculate average purchase value ####
df3.MERGE['AVG_PUR_VAL'] = df3.MERGE$total_Rev/df3.MERGE$total_Pur
df3.MERGE['AVG_PUR_FREQ'] = df3.MERGE$total_Pur/df3.MERGE$total_Cust
#### calculate CLV ####
df3.MERGE['CLV'] = df3.MERGE['AVG_PUR_VAL']*df3.MERGE['AVG_PUR_FREQ']
#### group by MnthYr and sum CLV ####
df3TS <- df3.MERGE %>% group_by(MnthYr) %>%
    summarise(
        total_CLV = sum(CLV),
        .groups = 'drop'
    )
df3TS = df3TS[order(df3TS$MnthYr),]
#data is complete - don't need remove
df3TS.ts <- ts(df3TS$total_CLV, frequency = 12, start = c(2015,1))
df3TS.ts

viz(df3TS, "violet", "Kaggle")

end = nrow(df3TS)
start = end-FORECAST_PERIODS+1
trainset3 <- ts(df3TS.ts[-c(start:end)], frequency = 12, start = c(2015,1))
trainset3
testset3 <- ts(df3TS.ts[c(start:end)], frequency = 12, start = c(2018,10))
testset3

m.holt <- HoltWinters(trainset3, seasonal = "multiplicative", gamma=F)
m.holt
plot(m.holt, main = "Holt's Method on Long Data")
m.holt.forecasts <- forecast(m.holt, h = FORECAST_PERIODS)
plotForecast(m.holt, m.holt.forecasts, trainset3, testset3, "Holt's on Kaggle")
holt.3 = accuracy(m.holt.forecasts, testset3)
holt.3

m.winters <- HoltWinters(trainset3, seasonal = "multiplicative")
m.winters
plot(m.winters, main = "Holt-Winter's Method on Long Data")
m.winters.forecasts <- forecast(m.winters, h = FORECAST_PERIODS)
plot(m.winters.forecasts)
plotForecast(m.winters, m.winters.forecasts, trainset3, testset3, "Winter's on Kaggle")
winter.3 = accuracy(m.winters.forecasts, testset3)
winter.3

#####################################################################################################
#########################################        ARIMA       ########################################

#################################################################
################## FUNCTION: plotForecastARIMA ##################
#################################################################
### This function is like plotforecast, but specifically for ARIMA
plotForecastARIMA <- function(model, forecast, trainset, testset, str) {
    options(scipen=10000)                      
    forecast_plot = autoplot(fitted(model), series = 'Forecasted') + 
        autolayer(forecast, series = 'Forecasted') 
    
    print(
        forecast_plot +
            autolayer(trainset, series = 'Actual') +
            autolayer(testset, series = 'Actual') +
            scale_x_continuous(labels = function(x) {format(date_decimal(x), "%b %Y")}) +
            labs(y='Customer Lifetime Value', x='Month-Year',
                 title=paste(FORECAST_PERIODS,'Period Ahead Forecasts based on',str),
            ) +
            theme(plot.title = element_text(hjust = 0.5))
    )
}
#################### END FUNCTION DEFINITION ####################
#################################################################

#### AUTOREGRESSIVE // Partial Autocorrelation Plots for p ####
pacf(dfTS.ts, main='Partial Autocorrelation Plots for UCI')       
# number of values exceeding blue dotted threshold line = 0
pacf(df2TS.ts, main='Partial Autocorrelation Plots for Olist')      
# number of values exceeding blue dotted threshold line = 0
pacf(df3TS.ts, main='Partial Autocorrelation Plots for Kaggle')      
# number of values exceeding blue dotted threshold line = 1

#### INTEGRATED // Differencing Plots for d ####
plot.ts(dfTS.ts, main='Time Series Plot for UCI')
plot.ts(diff(dfTS.ts, differences=1))
#already have stationary mean and variance = 0

plot.ts(df2TS.ts, main='Time Series Plot for Olist')
plot.ts(diff(df2TS.ts, differences=1))
#already have stationary mean and variance = 0

plot.ts(df3TS.ts, main='Time Series Plot for Kaggle')
plot.ts(diff(df3TS.ts, differences=1), main='Differencing Plot for Kaggle (d=1)')
plot.ts(diff(df3TS.ts, differences=2), main='Differencing Plot for Kaggle (d=2)')
plot.ts(diff(df3TS.ts, differences=3), main='Differencing Plot for Kaggle (d=3)')
plot.ts(diff(df3TS.ts, differences=3), main='Differencing Plot for Kaggle (d=4)')
plot.ts(diff(df3TS.ts, differences=3), main='Differencing Plot for Kaggle (d=5)')
#larger variance at the start cannot seem to be resolved with differencing. =0

#### MOVING AVERAGE // Autocorrelation Plots for q ####
acf(dfTS.ts, main='Autocorrelation Plots for UCI')       
# number of values exceeding blue dotted threshold line = 1
acf(df2TS.ts, main='Autocorrelation Plots for Olist')
# number of values exceeding blue dotted threshold line = 1
acf(df3TS.ts, main='Autocorrelation Plots for Kaggle')
# number of values exceeding blue dotted threshold line = 1


# ARIMA - df1 ---------------------------------------------
m.arima <- arima(trainset1, order=c(0,0,1))
arimaorder(m.arima)
m.arima.forecasts <- forecast(m.arima, h = 3)
m.arima.forecasts
m.arima.forecasts$mean
plot(m.arima.forecasts)
plotForecastARIMA(m.arima, m.arima.forecasts, trainset1, testset1, 'ARIMA on UCI')
arima.1 = accuracy(m.arima.forecasts, testset1, d=1, D=0)
arima.1

m.arima <- auto.arima(trainset1)
arimaorder(m.arima)
m.arima.forecasts <- forecast(m.arima, h = 3)
m.arima.forecasts
m.arima.forecasts$mean
plot(m.arima.forecasts)
plotForecastARIMA(m.arima, m.arima.forecasts, trainset1, testset1, 'Auto ARIMA on UCI')
arima.1.auto = accuracy(m.arima.forecasts, testset1, d=1, D=0)
arima.1.auto

# ARIMA - df2 ---------------------------------------------
m.arima <- arima(trainset2, order=c(0,0,1))
arimaorder(m.arima)
m.arima.forecasts <- forecast(m.arima, h = 3)
m.arima.forecasts
m.arima.forecasts$mean
plot(m.arima.forecasts)
plotForecastARIMA(m.arima, m.arima.forecasts, trainset2, testset2, 'Auto/ARIMA on Olist')
arima.2 = accuracy(m.arima.forecasts, testset2)
arima.2

m.arima <- auto.arima(trainset2)
arimaorder(m.arima)
m.arima.forecasts <- forecast(m.arima, h = 3)
m.arima.forecasts
m.arima.forecasts$mean
plot(m.arima.forecasts)
plotForecastARIMA(m.arima, m.arima.forecasts, trainset2, testset2,'Auto/ARIMA on Olist')
arima.2.auto = accuracy(m.arima.forecasts, testset2)
arima.2.auto

# ARIMA - df3 ---------------------------------------------
m.arima <- arima(trainset3, order=c(1,0,1))
arimaorder(m.arima)
m.arima.forecasts <- forecast(m.arima, h = 3)
m.arima.forecasts
m.arima.forecasts$mean
plot(m.arima.forecasts)
plotForecastARIMA(m.arima, m.arima.forecasts, trainset3, testset3, 'ARIMA on Kaggle')
arima.3 = accuracy(m.arima.forecasts, testset3)
arima.3

m.arima <- auto.arima(trainset3)
arimaorder(m.arima)
m.arima.forecasts <- forecast(m.arima, h = 3)
m.arima.forecasts
m.arima.forecasts$mean
plot(m.arima.forecasts)
plotForecastARIMA(m.arima, m.arima.forecasts, trainset3, testset3, 'Auto ARIMA on Kaggle')
arima.3.auto = accuracy(m.arima.forecasts, testset3)
arima.3.auto
#######                                          END                                          #######
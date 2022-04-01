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
        labs(y='Customer Lifetime Value', x='Month-Year',
             title='Customer Lifetime Value over time',
             caption=paste('Source:',source_string)) +
        change_font +                                               # change font (in helperFns)
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

#######                                    TRAIN-TEST SPLIT                                   #######
FORECAST_PERIODS = 3

end = nrow(dfTS)
start = end-FORECAST_PERIODS+1
trainset1 <- ts(dfTS.ts[-c(start:end)], frequency = 12, start = c(2010,12))
trainset1
testset1 <- ts(dfTS.ts[c(start:end)], frequency = 12, start = c(2011,9))
testset1

end = nrow(df2TS)
start = end-FORECAST_PERIODS+1
trainset2 <- ts(df2TS.ts[-c(start:end)], frequency = 12, start = c(2017,1))
trainset2
testset2 <- ts(df2TS.ts[c(start:end)], frequency = 12, start = c(2018,6))
testset2

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
        change_font + 
        theme(plot.title = element_text(hjust = 0.5))
    )
}
#################### END FUNCTION DEFINITION ####################
#################################################################

plotForecast(m.ses, m.ses.forecasts, trainset1, testset1, "SES")

ses.1 = accuracy(m.ses.forecasts, testset1)
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
plotForecast(m.ses, m.ses.forecasts, trainset2, testset2, "SES")
ses.2 = accuracy(m.ses.forecasts, testset2)
ses.2

#################################           HOLT'S METHOD           #################################

# Holt's Method - df1 ---------------------------------------------
m.holt <- HoltWinters(trainset1, seasonal = "multiplicative", gamma=F) #optimisation difficulties
m.holt
plot(m.holt, main = "Holt's Method on Short Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = FORECAST_PERIODS)
plotForecast(m.holt, m.holt.forecasts, trainset1, testset1, "Holt's")
holt.1 = accuracy(m.holt.forecasts, testset1)
holt.1

# Holt's Method - df2 ---------------------------------------------
m.holt <- HoltWinters(trainset2, seasonal = "multiplicative", gamma=F)
m.holt
plot(m.holt, main = "Holt's Method on Medium Data")
m.holt$fitted
m.holt.forecasts <- forecast(m.holt, h = FORECAST_PERIODS)
plotForecast(m.holt, m.holt.forecasts, trainset2, testset2, "Holt's")
holt.2 = accuracy(m.holt.forecasts, testset2)
holt.2

#################################        HOLT-WINTER'S METHOD       #################################
m.winters <- HoltWinters(trainset1, seasonal = "multiplicative")
# time series has less than 2 periods

m.winters <- HoltWinters(trainset2, seasonal = "multiplicative") 
# time series has less than 2 periods

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
df3TS <- df3TS[!(df3TS$MnthYr=='2018-12'),]

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
plotForecast(m.holt, m.holt.forecasts, trainset3, testset3, "Holt's")
holt.3 = accuracy(m.holt.forecasts, testset3)
holt.3

m.winters <- HoltWinters(trainset3, seasonal = "multiplicative")
m.winters
plot(m.winters, main = "Holt-Winter's Method on Long Data")
m.winters.forecasts <- forecast(m.winters, h = FORECAST_PERIODS)
plot(m.winters.forecasts)
plotForecast(m.winters, m.winters.forecasts, trainset3, testset3, "Holt's")
winter.3 = accuracy(m.winters.forecasts, testset3)
winter.3

#######                                          END                                          #######



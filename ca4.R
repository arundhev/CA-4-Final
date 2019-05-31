getwd()

rent_data <- read.csv("C:/Users/Arun/Documents/CA-4/HRA.csv", stringsAsFactors = FALSE)

View(rent_data)

# select subset
df1 <- subset(rent_data, select=c(Year.,Dublin ))
df1


library('ggplot2')
library('forecast')
library('tseries')

# using btime series we have to find whether the data is stationary or not
ts_dublin <- ts(df1$Dublin, start = c(1978,1),  frequency = 11)
ts_dublin

# The graph shows the data is not stationary
plot(ts_dublin)

start(ts_dublin)
end(ts_dublin)



View(ts_dublin)
# Using Lag data
View(ts_dublin)
#View(dublin)
library(dplyr)
lagged_ts <- lag(ts_dublin,50)
#plot(lagged_data)
#plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)

library(forecast)
library(tseries)
acf_val <- Acf(df1$Dublin) # autocorrelation
acf_val


#
#Partial auto correlation function
pacf_result <- Pacf(df1$Dublin)
pacf_result
# Test if a time series is stationary
library(tseries)
# p-value < 0.05 indicates the TS is stationary
# In this eample, Nile data is not stationary
adf.test(df1$Dublin)

# hence the TS is a stationary
View(df1$Dublin)
arima_model <- Arima(df1$Dublin, order = c(0,1,2))
arima_model

#compare with manual selected model
accuracy(arima_model)

arema_model <- arima(df1$Dublin)
arema_model

accuracy(arema_model)

# AIC value is best for auto arima model

qqnorm(arema_model$residuals)
qqline(arema_model$residuals)

View(arema_model)

Box.test(arema_model$residuals, type = "Ljung-Box")
forecast(arema_model, 15)
plot(forecast(arima_model, 15), xlab = "Year", ylab = "House Rent In Dublin")

#Limerick---------------


getwd()

rent_data <- read.csv("C:/Users/Arun/Documents/CA-4/HRA.csv", stringsAsFactors = FALSE)

View(rent_data)

# select subset
df2 <- subset(rent_data, select=c(Year.,Limerick ))
df2


library('ggplot2')
library('forecast')
library('tseries')

# using btime series we have to find whether the data is stationary or not
ts_limerick <- ts(df2$Limerick, start = c(2000,1),  frequency = 11)
ts_limerick

# The graph shows the data is not stationary
plot(ts_limerick)

#start value and end value
start(ts_limerick)
end(ts_limerick)

library(forecast)
library(tseries)
#auto correlation function
acf_val <- Acf(df2$Limerick)
acf_val


#
#Partial auto correlation function
pacf_result <- Pacf(df2$Limerick)
pacf_result

# Test if a time series is stationary
library(tseries)

# p-value < 0.05 indicates the TS is stationary
adf.test(df2$Limerick)

# hence the TS is a stationary
View(df2$Limerick)
arima_model <- Arima(df1$Limerick, order = c(0,1,2))
arima_model

#compare with manual selected model
accuracy(arima_model)

arema_model <- arima(df1$Limerick)
arema_model

accuracy(arema_model)

# AIC value is best for auto arima model

qqnorm(arema_model$residuals)
qqline(arema_model$residuals)

View(arema_model)

Box.test(arema_model$residuals, type = "Ljung-Box")
forecast(arema_model,)
plot(forecast(arima_model, 15), xlab = "Year", ylab = "House Rent In Limerick")



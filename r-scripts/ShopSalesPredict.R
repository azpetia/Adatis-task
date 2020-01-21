rm(list=ls())
# Set working directory
setwd("D:\\Business-Analysis-Sofia-University\\BI\\project")
# Import the csv data 
dataset=read.csv("Adatis.Shop.Sales.csv",sep=",",dec=".",na.strings = c("NA",""," ","  "),stringsAsFactors = F)

#Cleaning Data
colnames(dataset)[1]="OrderDate"

library("lubridate")
dataset$time=ymd(dataset$OrderDate)

colnames(dataset)[3]="orderdate"
colnames(dataset)[2]="price"
dataset = dataset[,!names(dataset) %in% c("OrderDate")]
class(dataset$orderdate)




#Plot and convert to ln format
library("MASS")
library("forecast")
library("tseries")
lnstock = log(dataset$price[1:720])

#ACF, PACF and Dickey-Fuller Test
acf(lnstock, lag.max = 20)  
pacf(lnstock, lag.max = 20) 
difflnstock = diff(lnstock, 1)
difflnstock  
adf.test(lnstock)
adf.test(difflnstock)

#Time series and auto.rima
pricearima = ts(lnstock, start = c(2014,197), frequency = 365)


fitlnstock = auto.arima(pricearima)
fitlnstock
plot(pricearima, type = "l")
title("Predicted Quarter Sales")
exp(lnstock)

#Forecasted Values from ARIMA
forecastedvalues_ln = forecast(fitlnstock, h = 92)
forecastedvalues_ln
plot(forecastedvalues_ln)
forecastedvalues_ln
pricearima

forecastedvaluesextracted = as.numeric(forecastedvalues_ln$mean)
finalforecastvalues = exp(forecastedvaluesextracted) #exponent function to conver our logarithms back into the proper prices
finalforecastvalues

#Percantage Error
forecastedShopSales = data.frame(dataset$price[720:811], finalforecastvalues)
col_headings=c("ActualPrice","ForecastedPrice")
names(forecastedShopSales)=col_headings
attach(forecastedShopSales)
percentage_error=((forecastedShopSales$ActualPrice-forecastedShopSales$ForecastedPrice)/forecastedShopSales$ActualPrice)
percentage_error
mean(percentage_error)

#Ljung -Box
#Box.test(fitlnstock$residuals, lag=5, type="Ljung Box")
#Box.test(fitlnstock$residuals, lag=10, type="Ljung Box")
#Box.test(fitlnstock$residuals, lag=15, type="Ljung Box")

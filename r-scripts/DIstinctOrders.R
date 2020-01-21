rm(list=ls())
# Set working directory
setwd("D:\\Business-Analysis-Sofia-University\\BI\\project")
# Import the csv data 
dataset=read.csv("Adatis.OrderIdNew.csv",sep=",",dec=".",na.strings = c("NA",""," ","  "),stringsAsFactors = F)

#Cleaning Data
colnames(dataset)[1]="OrderId"
library("lubridate")
dataset$time=ymd_hms(dataset$OrderDate)
colnames(dataset)[3]="orderdate"
dataset = dataset[,!names(dataset) %in% c("OrderDate")]

dataset = dataset[,!names(dataset) %in% c("Year","MonthOfYear","DayInWeek")]
dataset$OrderId=ifelse(dataset$OrderId > 4000, 1,"datasetOrderId")

library(dplyr)
dataset1=dataset %>%
  group_by(orderdate) %>%
  summarise(DistinctOrderId=sum(OrderId))  

#Plot and convert to ln format
library("MASS")
library("forecast")
library("tseries")
lnstock = log(dataset1$DistinctOrderId[1:275])

#ACF, PACF and Dickey-Fuller Test
acf(lnstock, lag.max = 20)
pacf(lnstock, lag.max = 20)
difflnstock = diff(lnstock, 1)
difflnstock

adf.test(lnstock)
adf.test(difflnstock)

#Time series and auto.rima
OrdersCount = ts(lnstock, start = c(2015,243), frequency = 365)


fitlnstock = auto.arima(OrdersCount)
fitlnstock
plot(pricearima, type = "l")
title("Predicted Quarter ID")
exp(lnstock)

#Forecasted Values from ARIMA
forecastedvalues_ln = forecast(fitlnstock, h = 92)
forecastedvalues_ln
plot(forecastedvalues_ln)
forecastedvalues_ln
OrdersCount

forecastedvaluesextracted = as.numeric(forecastedvalues_ln$mean)
finalforecastvalues = exp(forecastedvaluesextracted)
finalforecastvalues

#Percantage Error
forecastedOrders = data.frame(dataset1$DistinctOrderId[275:362], finalforecastvalues)
col_headings=c("ActualOrders","ForecastedOrders")
names(forecastedOrders)=col_headings
attach(forecastedOrders)
percentage_error=((forecastedOrders$ActualOrders-forecastedOrders$ForecastedOrders)/forecastedOrders$ActualOrders)
percentage_error
mean(percentage_error)

write.csv(dataset1, file = "DistinctOrders.csv")

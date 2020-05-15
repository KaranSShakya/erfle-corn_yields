###   Library---------------
#General
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(reshape2)

#Panel Regression
library(sandwich)
library(lmtest)
library(AER) #ARIMA MODELS
library(forecast)
library(plm)
library(stargazer)
library(knitr)
library(xtable)
library(margins)

###   Base data file---------------
data0 <- read_excel("reg-stata.xlsx")
data1 <- data0 %>% 
  select(-1)

### Base forecast file------------
data2 <- data1 %>% 
  select(-1) %>% 
  group_by(Year) %>% 
  summarise(avg_temp=mean(Temperature), avg_prec=mean(Precipitation)) %>% 
  ungroup()

### ARIMA Forecast----------------
#Year-Temperature
temp <- data2 %>% 
  select(2)
temp.ts <- ts(temp, start=1998, end=2017, frequency = 1)
plot(temp.ts)

arima.temp <- arima(temp.ts, order=c(3,1,1))
summary(arima.temp)
plot(forecast(arima.temp, 5))
forecast(arima.temp, 5)

#Year-Precipitation
prec <- data2 %>% 
  select(3)
prec.ts <- ts(prec, start=1998, end=2017, frequency = 1)
plot(prec.ts)

arima.prec <- arima(prec.ts, order=c(3,1,1))
summary(arima.prec)
plot(forecast(arima.prec, 5))
forecast(arima.prec, 5)









## if your timeseries goes up suddenly, find what time it bent?
# histogram, see for fat tail, no need for normality, transform your data,auto corellation, initial trial model, 
#other associated data, 1-2 pages
library(purrr) #checking dataframe
library(tidyr)
library(TSA)
library(lmtest)
library(knitr)
library(FSAdata)
library(fUnitRoots)
library(bestglm)
library(FitAR)
library(forecast)
library(rmarkdown)
library(readr)
library(tseries)
library(fBasics)
library(zoo)
library(ggplot2)
library(ggfortify)
require(dplyr)
library(car)

setwd("D:/Fall 2021_7th/Timeseries/project")
traffic_data<-read.csv(file = "Metro_Interstate_Traffic_Volume.csv", header=TRUE)
head(traffic_data)
#traffic_data=data[1:40000,] # select first 10000 rows for analysis

#----data preprocessing------------------------------------------------

sum(is.na(traffic_data))

hist(traffic_data$traffic_volume,breaks = 20)
hist(log(traffic_data$traffic_volume),breaks = 20) # w/0 log may be good
qqPlot(traffic_data$traffic_volume)

hist(traffic_data$temp,breaks = 20)
qqPlot(traffic_data$temp)

monthly_traffic<- tidyr::separate(traffic_data, date_time, c("date", "time"), sep = " ")

monthly_traffic <- tidyr::separate(monthly_traffic, date, c("year", "month", "day"), sep = "-")

monthly_traffic_data <- monthly_traffic %>% group_by(year,month) %>% summarise(vol_by_month = sum(traffic_volume))

df = ts(as.vector(monthly_traffic_data$vol_by_month), start=2012, end=2018, frequency=12)
plot(df, ylab="Traffic Volume", type="o", main="Monthly Traffic Volume")


plot(y=df,x=zlag(df), ylab="Traffic Volume", xlab="Previous month traffic volume", main="Previous Month Traffic Volume")

# --------linear trend model---------
linear = lm(df~time(df))
summary(linear)

plot(df, ylab="Time", type="o", main=" Fitted Linear Model to Traffic Volume Data")
abline(linear)

hist(df, breaks=12, col="red", main ="Histogram of Traffic Volume Data")

#--------------NA-------------

#traffic_data$date_time <-  as.POSIXct(traffic_data$date_time, format='%m/%d/%Y %H:%M')


#splitted date_time column to date and time
#traffic_data$Date <- format(traffic_data$date_time, format='%Y-%m-%d')
#head(traffic_data)

#traffic_data$Time <- format(traffic_data$date_time,"%H:%M:%S")
#head(traffic_data)

#head(dmy)


# find acf and pacf
tp <- ts(data = traffic_data[,c('traffic_volume')], start = c(2012,10,02), frequency = 12)
lag.plot(tp,lags=1,do.lines=FALSE)

acf(tp, lag.max = 15)#We see significant serial auto corelation at lag 1.
Acf(tp, lag.max = 15) #weak 7 day 
pacf(tp, lag.max = 15) 
eacf(tp)
 
Box.test(tp, lag=2, type = "Ljung-Box")

#----ar model with original series---------------------------------------------------------------------------

m1 = Arima(tp, order=c(2, 0, 0),seasonal=c(1,0,1))
m1
coeftest(m1)
# ---------------Auto arima with original series--------

fit0 <- auto.arima(tp) # (2,0,5)
summary(fit0)
coeftest(fit0)

Acf(fit0$residuals, lag.max = 20)
Pacf(fit0$residuals, lag.max = 20)
Box.test(fit0$residuals, lag=20, type="Ljung")

adf.test(fit0$residuals)
kpss.test(fit0$residuals, null= c('Level', 'Trend')) # stationary,No nned to do difference



#---------------ARIMA with difference-------------------------------------------------------------------------------
# residuals, backtesting, garch,seasonaity

#----------auto.arima--------------------Do auto arima with original series -----------------------------
# use ARMA with (2,1,0)

#-----Residual Analysis---------------------------------------------------

Acf(m1$residuals, lag.max = 20)
Pacf(m1$residuals, lag.max = 20)
Box.test(m1$residuals, lag=20, type="Ljung")

adf.test(m1$residuals)
kpss.test(m1$residuals, null= c('Level', 'Trend'))
qqnorm(m1$residuals)
qqline(m1$residuals)

# Backtesting-------------------------------
source("D:/Fall 2021_7th/Timeseries/Examples & notes/week5/TimeSeriesRegression/eacf.R")
source("D:/Fall 2021_7th/Timeseries/Examples & notes/week5/TimeSeriesRegression/backtest.R")
n = length(tp)
b1 = backtest(m1, tp, h=1, orig=.9*n)
b2 = backtest(fit0, tp, h=1, orig=.9*n)

f <- forecast(m1, h=10)
f
plot(forecast(m1, h=10))
f$mean
# Garch------------------------------------------
install.packages("fGarch")
library(fGarch)

install.packages("rugarch")
library(rugarch)

# Forecasting------------------------------------------
forecast(fit0, h=20)

plot(forecast(fit0, h=20))
plot(forecast(fit0, h=20),include=50, ylim = c(10.45,10.55))






setwd("C:/Users/rduvv/Desktop/data/Cotton_Seed_Price")
#Remove. Rownames. remove the 2002 partial list. Convert MOnth Char to Date format. sort it by Year and month.
library(forecast)
library(tseries)
#library(TTR)
minData <- read.csv("mod-min-cotton-price.csv", header = T)
# create time series data
values <- minData$Min_x0020_Price
tsmin <- ts(values, start = c(2003,1), end = c(2015, 12), frequency = 12)

# plot and explore
plot(tsmin)
seasonplot(x = tsmin, year.labels = T, year.labels.left = T, col=1:20) # comes from forecast package
monthplot(x = tsmin) # comes from forecast package

# now perfomr unit root tests or tests to check stationarity of data
adf.test(tsmin, alternative = "stationary") # test of stationary from tseries package
ndiffs(tsmin, alpha = 0.05, test = c("kpss","adf", "pp"), max.d = 2)
nsdiffs(tsmin, m=frequency(12), test = c("ocsb", "ch"), max.D = 1)

decompose(tsmin)
plot(decompose(tsmin))
plot(log(tsmin))
boxplot(tsmin~cycle(tsmin),col = "green", xlab = "Calender month", ylab = "average seed Price (Rs/quintal)", main = "Monthly cotton seed price fluctuations between 2003-2015")

#Holt-Winters exponential smoothing to make short-term forecast.
seedPrice <-  HoltWinters(log(tsmin))
seedPrice
plot(seedPrice)

seedPriceForecast <- forecast.HoltWinters(seedPrice, h = 24)
seedPriceForecast
plot(seedPriceForecast)

# testing the model 
acf(seedPriceForecast$residuals, lag.max = 20)
Box.test(seedPriceForecast$residuals, lag = 20, type = "Ljung-Box")

plot(seedPriceForecast$residuals)
hist(seedPriceForecast$residuals, col = "red")
# kernal density plot of errors
errorDensity <- density(seedPriceForecast$residuals)
plot(errorDensity, col = "green", main = "Distribution of residual errors")
polygon(errorDensity, col = "red", border = "blue")

# try ARIMA model
plot(acf(tsmin))
plot(pacf(tsmin))
arimaFit <- auto.arima(log10(tsmin), approximation = F, trace = F)
summary(arimaFit)

plot(arimaFit$residuals)
hist(arimaFit$residuals)

pred <- predict(arimaFit, n.ahead = 12)
10^(pred$pred)
plot(10^(pred$pred))

# try ets
etsFit <- ets(tsmin, model = "ZZZ")
summary(etsFit)
plot(etsFit)
plot(forecast(etsFit, h = 24))


accuracy(arimaFit)
accuracy(seedPriceForecast)
accuracy(etsFit)

#exploring data wiht pots.
#Check for trend up or down
# check for seasonlaity
seasonplot(x = tsmin, year.labels = T, year.labels.left = T, col=1:20)
monthplot(x = tsmin)



# Simple forecasts. 
meanf(tsmin) # the future values are equal to the mean of the time series
naive(tsmin) # the future values are equal to the last value in the time series
snaive(tsmin) # the future values are equal to the the last observed value from the same season of the year
#Drift method
#A variation on the naïve method is to allow the forecasts to increase or decrease over time, 
#where the amount of change over time (called the drift) is set to be the average change seen in the historical data.
rwf(tsmin, 24, drift=TRUE)
res<-residuals(rwf(tsmin,24,drift = T))
res

layout(1:4)
plot(HoltWinters(tsmin, alpha=0.25, beta=FALSE, gamma=FALSE), main="Alpha=0.25")
plot(HoltWinters(tsmin, alpha=0.5, beta=FALSE, gamma=FALSE), main="Alpha=0.5")
plot(HoltWinters(tsmin, alpha=0.75, beta=FALSE, gamma=FALSE), main="Alpha=0.75")
plot(HoltWinters(tsmin, alpha=1, beta=FALSE, gamma=FALSE), main="Alpha=1")

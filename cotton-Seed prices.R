setwd("C:/Users/rduvv/Desktop/data")
library(XML)
#Take 1 works perfectly
file <- "Cotton_Seed_2015.xml"
doc15 <- xmlInternalTreeParse(file, useInternalNodes = T) 
rootnode15 <- xmlRoot(doc15)
tables15 <- rootnode15[["Body"]][["showResponse"]][["showResult"]][["diffgram"]][["NewDataSet"]]
data15 <- xmlSApply(tables15, function(x) xmlSApply(x, xmlValue)) 
final.15.df <- data.frame(t(data15),row.names=NULL)
write.csv(final.15.df, file = "FY15-cotton-seed.csv")

# read FY15 data 
fy15Data <- read.csv("FY15-cotton-seed.csv", header = T, row.names = 1)
fy15Data$Arrival_Date <- as.character(fy15Data$Arrival_Date)
fy15Data$Date <- as.Date(fy15Data$Arrival_Date, format = "%d/%m/%Y")
fy15Data$Month <- format(fy15Data$Date, format = "%b")
fy15Data$Year <- format(fy15Data$Date, format = "%Y")
min.monthAgg.15 <- aggregate(Min_x0020_Price ~ Year + Month, data = fy15Data, FUN = mean)
max.monthAgg.15 <- aggregate(Max_x0020_Price ~ Year + Month, data = fy15Data, FUN = mean)
model.monthAgg.15 <- aggregate(Modal_x0020_Price ~ Year + Month, data = fy15Data, FUN = mean)

# scrape data for FY13
doc13 <- xmlTreeParse("Cotton_Seed_2013.xml")
rootnode13 <- xmlRoot(doc13)
table13 <- rootnode13[["Body"]][["showResponse"]][["showResult"]][["diffgram"]][["NewDataSet"]]
data13 <- xmlSApply(table13, function(x) xmlSApply(x, xmlValue)) 
final.13.df <- data.frame(t(data13),row.names=NULL)
write.csv(final.13.df, file = "FY13-cotton-seed.csv")
# read Fy13 Data
fy13Data <- read.csv("FY13-cotton-seed.csv", header = T, row.names = 1)
fy13Data$Arrival_Date <- as.character(fy13Data$Arrival_Date)
fy13Data$Date <- as.Date(fy13Data$Arrival_Date, format = "%d/%m/%Y")
fy13Data$Month <- format(fy13Data$Date, format = "%b")
fy13Data$Year <- format(fy13Data$Date, format = "%Y")
class(fy13Data$Year)
min.monthAgg.13 <- aggregate(Min_x0020_Price ~ Year + Month, data = fy13Data, FUN = mean)
max.monthAgg.13 <- aggregate(Max_x0020_Price ~ Year + Month, data = fy13Data, FUN = mean)
model.monthAgg.13 <- aggregate(Modal_x0020_Price ~ Year + Month, data = fy13Data, FUN = mean)

# scrape data for FY14
doc14 <- xmlTreeParse("Cotton_Seed_2014.xml")
rootnode14 <- xmlRoot(doc14)
table14 <- rootnode14[["Body"]][["showResponse"]][["showResult"]][["diffgram"]][["NewDataSet"]]
data14 <- xmlSApply(table14, function(x) xmlSApply(x, xmlValue))
final.14.df <- data.frame(t(data14),row.names=NULL)
write.csv(final.14.df, file = "FY14-cotton-seed.csv")

# read FY14 data
fy14Data <- read.csv("FY14-cotton-seed.csv", header = T, row.names = 1)
fy14Data$Arrival_Date <- as.character(fy14Data$Arrival_Date)
fy14Data$Date <- as.Date(fy14Data$Arrival_Date, format = "%d/%m/%Y")
fy14Data$Month <- format(fy14Data$Date, format = "%b")
fy14Data$Year <- format(fy14Data$Date, format = "%Y")
min.monthAgg.14 <- aggregate(Min_x0020_Price ~ Year + Month, data = fy14Data, FUN = mean)
max.monthAgg.14 <- aggregate(Max_x0020_Price ~ Year + Month, data = fy14Data, FUN = mean)
model.monthAgg.14 <- aggregate(Modal_x0020_Price ~ Year + Month, data = fy14Data, FUN = mean)



# prepare the next one # not working
newDoc <- xmlTreeParse("Cotton_Seed_2001-2012.xml", useInternalNodes = T)
rootNodes <- xmlRoot(newDoc)
newTables <- rootNodes[["Body"]][["showResponse"]][["showResult"]][["diffgram"]][["NewDataSet"]]
newDataFrame <- xmlToDataFrame(newTables)
#newData <- xmlSApply(newTables, function(x) xmlSApply(x, xmlValue))
#newDataFrame <- data.frame(t(newData), row.names = NULL)
write.csv(newDataFrame, file = "FY01-12-Cotton-seed.csv")

# read data
tenyeardata <- read.csv("FY01-12-Cotton-seed.csv", header = T, row.names = 1)
tenyeardata$Arrival_Date <- as.character(tenyeardata$Arrival_Date)
tenyeardata$Date <- as.Date(tenyeardata$Arrival_Date, format = "%d/%m/%Y")
tenyeardata$Month <- format(tenyeardata$Date, format = "%b")
tenyeardata$Year <- format(tenyeardata$Date, format = "%Y")
min.monthAgg.10 <- aggregate(Min_x0020_Price ~ Year + Month, data = tenyeardata, FUN = mean)
max.monthAgg.10 <- aggregate(Max_x0020_Price ~ Year + Month, data = tenyeardata, FUN = mean)
model.monthAgg.10 <- aggregate(Modal_x0020_Price ~ Year + Month, data = tenyeardata, FUN = mean)

min.data <- rbind(min.monthAgg.10, min.monthAgg.13,min.monthAgg.14,min.monthAgg.15)
write.csv(min.data, file = "min-cotton-price.csv")
#Remove. Rownames. remove the 2002 partial list. Convert MOnth Char to Date format. sort it by Year and month.
library(forecast)
library(TTR)
minData <- read.csv("min-cotton-price.csv", header = T)
values <- minData$Min_x0020_Price
tsmin <- ts(values, start = c(2003,1), end = c(2015, 12), frequency = 12)
plot(tsmin)
ts.plot(tsmin)
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

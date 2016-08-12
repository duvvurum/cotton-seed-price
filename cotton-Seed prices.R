setwd("C:/Users/rduvv/Desktop/data/Cotton_Seed_Price")
library(XML)

# ten year data
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


#Fy15 data 
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
# prepare state wise aggregates alogn with month/Year
min.state.15 <- aggregate(Min_x0020_Price ~ Year + Month + State, data = fy15Data, FUN = mean)
max.state.15 <- aggregate(Max_x0020_Price ~ Year + Month + State, data = fy15Data, FUN = mean)
model.state.15 <- aggregate(Modal_x0020_Price ~ Year + Month + State, data = fy15Data, FUN = mean)
temp.state.15 <- merge(min.state.15, max.state.15)
state.15 <- merge(temp.state.15,model.state.15)


min.data <- rbind(min.monthAgg.10, min.monthAgg.13,min.monthAgg.14,min.monthAgg.15)
write.csv(min.data, file = "min-cotton-price.csv")

# before proceeding to the next step do the following. Remove. Rownames.
#remove the 2002 partial list. Convert MOnth Char to Date format. sort it by Year and month.

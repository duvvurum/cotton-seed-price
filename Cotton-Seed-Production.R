setwd("C:/Users/rduvv/Desktop/data/Cotton_Seed_Price/cotton_prod")
# data is downloaded from the http://apps.fas.usda.gov/psdonline/psdDownload.aspx and filtered for required values
library(reshape2)
library(ggfortify)
library(tfplot)
# read raw data
rawYield <- read.csv("threeCountryYield.csv", header = T)
rawArea <- read.csv("threeCountryHarvested.csv", header = T)

# make data ts object compatible
yield <- dcast(rawYield, Market_Year ~ Country_Name)
area <- dcast(rawArea, Market_Year ~ Country_Name)

#prepare time series objects
tsareaData <- ts(area, start = 1960, end = 2016, frequency = 1)
tsyieldData <- ts(yield, start = 1960, end = 2016, frequency = 1)


# plot the base
par(mfrow=c(2,1))
plot(tsareaData[,2:4], plot.type = "single", col = 1:ncol(tsareaData[,2:4]), lty = c(1:3),lwd = 3, xlab = "Year", ylab = "Harvested area(Hectare X 1000)", main = "Harvested area between 1960-2016")
grid(lty=2, col=gray(.8))
legend("topleft", colnames(tsareaData[,2:4]), col = 1:ncol(tsareaData[,2:4]),lty=c(1:3), lwd =3,  cex=.65)

plot(tsyieldData[,2:4], plot.type = "single", col = 1:ncol(tsyieldData[,2:4]), lty = c(1:3),lwd = 3, xlab = "Year", ylab = "Yield (Kg/Ha)", main = "Yield between 1960-2016")
grid(lty=2, col=gray(.8))
legend("topleft", colnames(tsyieldData[,2:4]), col = 1:ncol(tsyieldData[,2:4]),lty=c(1:3), lwd =3,  cex=.65)

# percent change
autoplot(percentChange(tsyieldData[,2:4], lag = 30), ylab = "Percetn change in Yield", main = "Percentage change in yield over 30 year period")
autoplot(percentChange(tsareaData[,2:4], lag = 30), ylab = "Percent change in Harvested Area", main = "Percentage change in harvested area over 30 year period")




# plot of percent change India
autoplot(percentChange(tsyieldData[,3], lag = 30), facets = F, ylab = "Percent change in yield", main =" Yield changes in India")
autoplot(percentChange(tsareaData[,3], lag = 30), facets = F, ylab = "percent change in area",main = "Harvested area changes in India")

# plot of percent change in China
autoplot(percentChange(tsyieldData[,2], lag = 30), facets = F, ylab = "Percent change in yield", main = "Yield changes in China")
autoplot(percentChange(tsareaData[,2], lag = 30), facets = F, ylab = "percent change in area", main = "Harvested area changes in china")

# plot of percent change in USA
autoplot(percentChange(tsyieldData[,4], lag = 30), facets = F, ylab = "Percent change in yield", main =" Yield changes in USA")
autoplot(percentChange(tsareaData[,4], lag = 30), facets = F, ylab = "percent change in area",main = "Harvested area changes in USA")

# change point estimates
library(changepoint)
mvalueBin <- cpt.mean(tsyieldData[,3], method = "BinSeg", Q = 3)
mvalueBinYield <- cpt.mean(tsyieldData[,3], method = "BinSeg", Q = 3)
autoplot(mvalueBinYield, main = "Changepoints in Yield in India")
mvalueBinArea <- cpt.mean(tsareaData[,3], method = "BinSeg", Q = 3)
autoplot(mvalueBinArea, main = "Changepoints in cultivated area in India")
# segNeigh





meltedYield <- melt(rawYield, id.vars = c("Market_Year", "Country_Name"))
meltedArea <- melt(rawArea, id.vars = c("Market_Year", "Country_Name"))


#install.packages("ggfortify")
library(ggfortify)
autoplot(tsareaData[,2:4],ts.geom='ribbon', stacked = T, facets = F, xlab = "year", ylab = "Harvested Area (X1000 Hectares)")
autoplot(tsyieldData[,2:4],ts.geom = 'ribbon', facets = F)



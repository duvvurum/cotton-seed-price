library(plotly)
library(reshape2)
library(knitr)

# read in data
tenyeardata <- read.csv("FY01-12-Cotton-seed.csv", header = T, row.names = 1)
fy13Data <- read.csv("FY13-cotton-seed.csv", header = T, row.names = 1)
fy14Data <- read.csv("FY14-cotton-seed.csv", header = T, row.names = 1)
fy15Data <- read.csv("FY15-cotton-seed.csv", header = T, row.names = 1)

# combine all data and format date column
fullData <- rbind(tenyeardata, fy13Data, fy14Data, fy15Data)
fullData$Arrival_Date <- as.character(fullData$Arrival_Date)
fullData$formattedArrival <- as.Date(fullData$Arrival_Date, format = "%d/%m/%Y")
fullData$year <- format(fullData$formattedArrival, format = "%Y")
fullData$charMon <- format(fullData$formattedArrival, format = "%b")
fullData$numMon <- format(fullData$formattedArrival, format = "%m")
fullData$monthly <- as.Date(cut(fullData$formattedArrival, breaks = "months")) 
fullData$Mon <- paste0(fullData$numMon, "_", fullData$charMon)
fullData$Year <- as.Date(cut(fullData$formattedArrival, breaks = "year"))
fullData$Month <- as.Date(cut(fullData$formattedArrival, breaks = "month"))

# prepare data for first plot and Plot with Plotly
fullData$meanPrice <- rowMeans(subset(fullData, select = c("Min_x0020_Price", "Max_x0020_Price")))
monthlyAgg <- aggregate(meanPrice ~ monthly, data = fullData, FUN = mean)
tsPlot <- plot_ly(monthlyAgg, x = monthly, y = meanPrice, name = "Average price", mode = "lines + markers", session="knitr")%>% add_trace( y = fitted(loess(meanPrice~as.numeric(monthly))), x = monthly, name = "loess smooth", session = "knitr")%>%
  layout(title = "Monthly average seed prices (2002-2015)")
tsPlot


# second plot
finalData <- subset(fullData, fullData$State == c("Andhra Pradesh","Gujarat", "Haryana", "Karnataka", "Madhya Pradesh", "NCT of Delhi", "Orissa", "Punjab", "Uttar Pradesh"))
meltedfinal <- melt(finalData, id.vars = c("year", "State"), measure.vars = "meanPrice", value.name = "meanPrice")
byyearStateavg <- aggregate(meanPrice ~ State+year, data = meltedfinal, FUN = mean)
x <-  plot_ly(byyearStateavg, x = year, y = meanPrice, group = State, name = "average Price") %>% layout(title = "Average seed prices  across years by State (2002-2015)")
x


# Third plot
byStateMeanPrice <- aggregate(meanPrice ~ State + Mon, data = finalData, FUN = mean)
a <- plot_ly(byStateMeanPrice, x = State, y = meanPrice, type = "box", color = State, boxpoints = "all") %>% layout(title = "State wise average price variations (2002-2015)", showlegend = FALSE,margins = list(b = 100, l = 50, t= 50, r= 50))
a


# fourth plot
sorted <- finalData[order(finalData$Mon),]
b <- plot_ly(sorted, x = Mon, y = meanPrice, type = "box", color = Mon, boxpoints = "all", jitter = 0.3) %>%
  layout(title = "Monthly average price variations (2002-2015)", showlegend = FALSE,margins = list(b = 100, l = 50, t= 50, r= 50)) %>%
  add_trace( y = mean(finalData$meanPrice), type = "box", name = "Mean of Mean")
b


# fifth Plot

# data for by Year
byYear <- aggregate(cbind(Min_x0020_Price, Max_x0020_Price, Modal_x0020_Price) ~ year, data = finalData, FUN = mean)
byYear$minMaxDiff <- byYear$Max_x0020_Price - byYear$Min_x0020_Price

# data for by Month
byMonth <- aggregate(cbind(Min_x0020_Price, Max_x0020_Price, Modal_x0020_Price) ~ Mon, data = finalData, FUN = mean)
byMonth$minMaxDiff <- byMonth$Max_x0020_Price - byMonth$Min_x0020_Price

# data for by State
byState <- aggregate(cbind(Min_x0020_Price, Max_x0020_Price, Modal_x0020_Price) ~ State, data = finalData, FUN = mean)
byState$minMaxDiff <- byState$Max_x0020_Price - byState$Min_x0020_Price
# plot
p <- subplot(
  plot_ly(byYear, x = year, y = minMaxDiff, name = "Min. Max Diff"),
  plot_ly(byMonth, x = Mon, y = minMaxDiff, name = "Min. Max. Diff"),
  plot_ly(byState, x = State, y = minMaxDiff, name = "Min. Max. Diff"),
  margin = 0.05,
  nrows = 3
) %>% layout(#showlegend = FALSE, 
  margins = list(b = 150, l = 55, t= 50, r= 50),
  title = "Price violatility across years and months and states (2002-2015)")
p


# Sixth Plot
# to visualizae the deviation between min/max and Modal prize
meltedYear <- melt(byYear)
meltedState <- melt(byState)
p2 <- subplot(
  plot_ly(meltedYear, x = year, y = value, symbol = variable),
  plot_ly(meltedState, x = State, y = value, group = variable),
  margin = 0.05,
  nrows = 2
) %>% layout(
  showlegend = T,
  margins = list(b = 100, l = 50, t= 50, r= 50),
  title = "Deviations from modal price (2002-2015)")
p2
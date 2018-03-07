library(ggplot2)
library(dplyr)

country <- "Canada"
pop.type <- "Refugees (incl. refugee-like situations)"
time.series <- read.csv('data/time_series.csv', stringsAsFactors = FALSE, fileEncoding
                        = "UTF-8-BOM")
time.series <- filter(time.series, time.series$Country...territory.of.asylum.residence == country,
                      time.series$Population.type == pop.type)

time.series.grouped <- aggregate(x = time.series$Value, by = list(time.series$Year), FUN = sum)%>%
  na.omit(time.series.grouped)
colnames(time.series.grouped)[1] <- "Year"
colnames(time.series.grouped)[2] <- "Value"

time.series[,1] <- sapply(time.series[,1], as.numeric)
time.series[,5] <- sapply(time.series[,5], as.numeric)

ggplot(data = time.series.grouped) +
  geom_point(mapping = aes(x = Year, y = Value), color = "blue") +
  geom_line(mapping = aes(x = Year, y = Value), color = "blue")

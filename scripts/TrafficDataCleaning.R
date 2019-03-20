library(tidyr)

source("scripts/TrafficDataFunctions.R")

# Get the names of all the traffic webpage files in the "data" subfolder
trafficFileNames <- list.files("data/traffic", pattern = "*.traffic.htm", full.names = TRUE)
widthFileNames <- list.files("data/traffic", pattern = "*.widths.processed.txt", full.names = TRUE)

# Get the actual names of all the roads that have been imported
roadnames <- gsub("data/traffic/", "", gsub(".traffic.htm", "", trafficFileNames))

# Scrape all of the road files that are available into dfs
dfTraffic <- lapply(trafficFileNames, ScrapeTraffic) # This is still a little buggy with small roads
dfWidth <- lapply(widthFileNames, importWidth)

# Merge this list of dfs into one large df
dfTraffic <- do.call(rbind,dfTraffic)
dfWidth <- do.call(rbind,dfWidth)

dfTraffic <- dfTraffic %>% group_by(Road)
dfWidth <- dfWidth %>% group_by(Road)

## Write to file
write.csv(dfTraffic, file = "data/Traffic.csv")
write.csv(dfWidth, file = "data/Width.csv")
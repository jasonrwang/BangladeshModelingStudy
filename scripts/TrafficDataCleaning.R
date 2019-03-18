library(tidyr)

source("scripts/TrafficDataFunctions.R")

# Get the names of all the traffic webpage files in the "data" subfolder
filenames <- list.files("data/traffic", pattern = "*.traffic.htm", full.names = TRUE)
# Get the actual names of all the roads that have been imported
roadnames <- gsub("data/traffic/", "", gsub(".traffic.htm", "", filenames))

# Scrape all of the road files that are available into dfs
ldf <- lapply(filenames, ScrapeTraffic) # This is still a little buggy with small roads
# Name each df in ldf by its actual name
names(ldf) <- roadnames

# Summary statistics for all the roads
res <- lapply(ldf, summary)
names(res) <- substr(filenames, 6, 30)

## Write to file
write.csv(df, file = "data/TrafficN1.csv")

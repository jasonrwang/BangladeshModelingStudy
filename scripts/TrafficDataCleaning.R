library(tidyr)

source("scripts/TrafficDataFunctions.R")

# Get the names of all the traffic webpage files in the "data" subfolder
filenames <- list.files("data", pattern = "*.traffic.htm", full.names = TRUE)
# Get the actual names of all the roads that have been imported
roadnames <- gsub("data/", "", gsub(".traffic.htm", "", filenames))

# Scrape all of the road files that are available into dfs
ldf <- lapply(filenames, ScrapeTraffic)
# Name each df in ldf by its actual name
names(ldf) <- roadnames

# Summary statistics for all the roads
res <- lapply(ldf, summary)
names(res) <- substr(filenames, 6, 30)
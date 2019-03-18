library(tidyr)

source("scripts/TrafficDataFunctions.R")

# Get the names of all the traffic webpage files in the "data" subfolder
filenames <- list.files("data/traffic", pattern = "*.traffic.htm", full.names = TRUE)

# Get the actual names of all the roads that have been imported
roadnames <- gsub("data/traffic/", "", gsub(".traffic.htm", "", filenames))


## Scrape all of the road files that are available into dfs

# Create an empty dataset
df <- c(0)

# Scrape each road file into df
for (file in filenames) {
  df <- rbind(df, ScrapeTraffic(file))
}

# Remove the empty dummy row
df <- df[-1, ]


## Write to file
write.csv(df, file = "data/Traffic.csv")

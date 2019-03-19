library(tidyr)

source("scripts/TrafficDataFunctions.R")

# Get the names of all the traffic webpage files in the "data" subfolder
filenames <- list.files("data/traffic", pattern = "*.traffic.htm", full.names = TRUE)

# Get the actual names of all the roads that have been imported
roadnames <- gsub("data/traffic/", "", gsub(".traffic.htm", "", filenames))

# Scrape all of the road files that are available into dfs
df <- lapply(filenames, ScrapeTraffic) # This is still a little buggy with small roads
# Merge this list of dfs into one large df
df <- do.call(rbind,df)
df <- df %>% group_by(Road)

## Write to file
write.csv(df, file = "data/Traffic.csv")

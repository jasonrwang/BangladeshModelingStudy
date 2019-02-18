### Overall Function to Clean Roads

# Load libraries and other scripts
library(dplyr)

source('scripts/RoadDataFunctions.R')

# Load data
df <- read.csv('data/Roads_InfoAboutEachLRP.csv') %>% group_by(road)

VisualizeRoads(df, c('N2'))

# Run different outlier detection techniques

df <- RoadCooksDis_LatLon(df)  
df <- RoadDeltaLon(df)
df <- RoadDeltaLat(df)

# Find the 95th percentile limit for each grouped set of lon and lat coordinates, and join to original df
dfSum <- summarize(df, quantLimitLat = quantile(deltaLat, c(.97)), quantLimitLon = quantile(deltaLon, c(.97))) %>% group_by(road)
df <- inner_join(df,dfSum, by = "road")

# Call out the outliers
df <- mutate(df, outlier = cookd > 0.05 | deltaLat > quantLimitLat | deltaLon > quantLimitLon)

VisualizeRoads(df, c('N2'), outlier = TRUE)
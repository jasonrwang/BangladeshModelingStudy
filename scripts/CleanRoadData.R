### Overall Function to Clean Roads

# Load libraries and other scripts
library(dplyr)

source('scripts/RoadDataFunctions.R')

# Load data
df <- read.csv('data/Roads_InfoAboutEachLRP.csv', stringsAsFactors = FALSE ) %>% group_by(road)

VisualizeRoads(df, c('N2'))

# Run different outlier detection techniques
q <- 0.99 # The quantile value we want to use
df <- RoadCooksDis_LatLon(df)  
df <- RoadDeltaLon(df, q)
df <- RoadDeltaLat(df, q)

# Call out the outliers
# df <- mutate(df, outlier = cookd > 0.05 | deltaLat > quantLimitLat | deltaLon > quantLimitLon)

VisualizeRoads(df, c('N2'), outlier = TRUE)
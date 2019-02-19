##########################################
#
#   Overall Function to Clean Road Data
#
#   This code:
#     1. Loads the raw data
#     2. Group by road
#     3. Runs initial chainage check
#     3. Delta Distance Method
#       a. Checks for the change in distance between all points by coordinates and by chainage
#       b. Checks the difference between these two methods and identifies outliers
#       c. Fix outliers using interpolation method
#     4. General Cleaning
#       a. Assumes differences larger than .5º is a  ±1º typo, and corrects this to match other data
#       b. If differences are less than .5º, then interpolate
#     5. Check start and end points
#       a. If outlier, remove
#       b. Grouped
#     6. Cook's Method
#     Remove large outliers
#
#   To-do
#     a. Automatically fix sequential starts (needs to be at the beginning)
#     b. Clean points by delta distance method
#
########################################

# ===================================
#   Load libraries and other scripts
# ===================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(geosphere)
# library(xlsx)

source('scripts/RoadDataFunctions.R')

## Load data
df <- read.csv('data/Roads_InfoAboutEachLRP.csv',
  stringsAsFactors = FALSE ) %>% group_by(road)

# min(df[which(df$road == "Z4606"),]$lon)

# Visually inspect roads
VisualizeRoads(df, c('N2'), 'Initial Inspection')

# ==================================
#   Run different outlier detection
#   and cleaning techniques
# ==================================

# Create a new empty log for cleaning
cat("CLEANING RECORD STARTS\n\n\n", file = "CleaningRecord.txt", append = FALSE)

# 1. Fix chainage reversal/duplicates problem 
df <- ByChainage(df)
VisualizeRoads(df, c('N6'),'Before Cleaning')

# Changed outlier flagging logic to be by-type and not a general outlier category
# VisualizeRoads(df, c('N2'), 'Outliers', outlier = TRUE)

# =========================
#  Data cleaning for Roads 
# =========================

# Split the data frame by road
df1 <- split(df, df$road)

# Clean the dataset
df1 <- lapply(df1, RoadDiffDistClean)

# Cook's Distance Method
df1 <- df1 %>% RoadCooksDis_LatLon() %>% CooksDistanceClean() # Now cleans by Cook's Distance without splitting by road

q <- 0.99 # The quantile value we want to use
df1 <- RoadDeltaLon(df1, q)
df1 <- RoadDeltaLat(df1, q)

# Clean the dataset
df4 <- lapply(df3, fun_endpt)

# Reshape the dataset
df5 <- bind_rows(df4)

# Plot a road to check the results
VisualizeRoads(df5, c('N6'),'After Cleaning')

# =====================
#  Write data to file 
# =====================

# Write Roads_InfoAboutEachLRP file
write.csv(df5, file = "Roads_InfoAboutEachLRP.csv", col.names = TRUE, row.names = FALSE)

df6 <- as.character(unique(df5$road))
df6 <- as.data.frame.vector(df6)
colnames(df6) <- "road"

title <- c("road", "lrp1", "lat1", "lon1", "lrp2", "lat2", "lon2")
write.table(t(title), file = "_roads.tcv", sep = "\t", col.names = FALSE, row.names = FALSE)

for (ii in 1:nrow(df6)) {
  vec_road <- c(df6[ii, 1])
  df_road <- filter(df5, road == vec_road)
  for (n in 1:nrow(as.data.frame(df3[ii]))) {
    #print(as.character(df_road$lrp[n]))
    if(!is.na(df_road$lat[n])){
      vec_road <- cbind(vec_road, as.character(df_road$lrp[n]), df_road$lat[n], df_road$lon[n])
    }
  }
  write.table(as.array(vec_road), file = "_roads.tcv", sep = "\t", append = TRUE, col.names = FALSE, row.names = FALSE)
}

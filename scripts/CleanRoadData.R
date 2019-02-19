######################################## 
#
#   Overall Function to Clean Road Data
#
#   This code:
#     1. Loads the raw data
#     2. Group by road
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

# Visually inspect roads
VisualizeRoads(df, c('N2'), 'Initial Inspection')

# ==================================
#   Run different outlier detection
#   and cleaning techniques
# ==================================

#  1. Fix chainage reversal/duplicates problem 
df1 <- ByChainage(df)
VisualizeRoads(df1, c('N602'),'Before Cleaning')

# Detection methods
q <- 0.99 # The quantile value we want to use
# Cleans by Cook's Distance without splitting by road
df <- df %>% RoadCooksDis_LatLon() %>% CooksDistanceClean()
df <- RoadDeltaLon(df, q)
df <- RoadDeltaLat(df, q)

VisualizeRoads(df, c('N2'), 'Outliers', outlier = TRUE)

# Create a new empty log for cleaning
cat("CLEANING RECORD STARTS\n\n\n", file = "CleaningRecord.txt", append = FALSE)


# =========================
#  Data cleaning for Roads 
# =========================

# Split the data frame by road
df2 <- split(df1, df1$road)

# Clean the dataset
df3 <- lapply(df2, fun_road)

# Clean the dataset
df4 <- lapply(df3, fun_endpt)

# Clean the dataset
df4 <- lapply(df4, fun_cooksD)

# Plot a road to check the results
VisualizeRoads(df3, c('N102'),'After Cleaning')

# Reshape the dataset
df5 <- bind_rows(df4)

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

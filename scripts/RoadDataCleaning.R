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
library(xlsx)

source('scripts/RoadDataFunctions.R')

strroad <- c('N507')

## Load data
df <- read.csv('data/Roads_InfoAboutEachLRP.csv',
  stringsAsFactors = FALSE ) %>% group_by(road)

# Visually inspect roads
VisualizeRoads(df, strroad, 'Initial Inspection')


# ==================================
#   Run different outlier detection
#   and cleaning techniques
# ==================================

# Create a new empty log for cleaning
cat("CLEANING RECORD STARTS\n\n\n", file = "CleaningRecord.txt", append = FALSE)


# 1. Fix chainage reversal/duplicates problem 
df <- ByChainage(df)
VisualizeRoads(df, strroad,'After ByChainage')

# Changed outlier flagging logic to be by-type and not a general outlier category
# VisualizeRoads(df, strroad, 'Outliers', outlier = TRUE)

# =========================
#  Data cleaning for Roads 
# =========================
# =========================
#  Fix start point outlier 
# =========================

# lat = lat - 1 needed : Z3711
df$lat[which(df$road == "Z3711")[1]] <- df$lat[which(df$road == "Z3711")[1]] - 1

# lon = lon - 1 needed : Z4606 & Z1129
df$lon[which(df$road == "Z4606")[1]] <- df$lon[which(df$road == "Z4606")[1]] - 1
df$lon[which(df$road == "Z1129")[1]] <- df$lon[which(df$road == "Z1129")[1]] - 1

# Specific fix - duplicate lat for Z7606
df$lat[which(df$road == "Z7606")[1]] <- df$lat[which(df$road == "Z7606")[2]]

# Remove roads that do not exist : Z1030 & Z1081 & Z1401
df <- df[-which(df$road == "Z1030"), ]
df <- df[-which(df$road == "Z1081"), ]
df <- df[-which(df$road == "Z1401"), ]

# Write to cleaning record
cat(c("Fix start point outlier to: \n", 
      "Z3711 lat = lat -1 = ", df$lat[which(df$road == "Z3711")[1]], "\n",
      "Z1129 lon = lon -1 = ", df$lon[which(df$road == "Z1129")[1]], "\n",
      "Z1129 lon = lon -1 = ", df$lon[which(df$road == "Z1129")[1]], "\n",
      "Remove roads that do not exist: Z1030 & Z1081 & Z1401 \n\n"), 
    file = "CleaningRecord.txt", append = TRUE)

# Split the data frame by road
df1 <- split(df, df$road)

# Clean the dataset
df1 <- lapply(df1, RoadDiffDistClean)

# Clean the dataset
df1 <- lapply(df1, fun_endpt)

# Reshape the dataset
df1<- bind_rows(df1)

# Clean using RoadLargeDeltaClean
q <- 0.995 # The quantile value we want to use
df2 <- RoadDeltaLon(df1, q)
df3 <- RoadDeltaLat(df2, q)
df4 <- RoadLargeDeltaClean(df3)

# Cook's Distance Method - Remove
df1 <- df1 %>% RoadCooksDis_LatLon() %>% CooksDistanceClean() # Now cleans by Cook's Distance without splitting by road

# Plot a road to check the results
VisualizeRoads(df4, strroad,'After Cleaning', TRUE)

df5 <- df4 %>% RoadDeltaLon(q) %>% RoadDeltaLat(q) %>% RoadLargeDeltaClean()
VisualizeRoads(df5, strroad,'After Cleaning', TRUE)

# =====================
#  Write data to file 
# =====================
# =====================

# Reshape the dataset
df5 <- bind_rows(df4)

# Write Roads_InfoAboutEachLRP file
write.csv(df5, file = "Roads_InfoAboutEachLRP.csv", col.names = TRUE, row.names = FALSE)

df6 <- as.character(unique(df5$road))
df6 <- as.data.frame.vector(df6)
colnames(df6) <- "road"

title <- c("road", "lrp1", "lat1", "lon1", "lrp2", "lat2", "lon2")
write.table(t(title), file = "_roads.tcv", sep = "\t", col.names = FALSE, row.names = FALSE)

for (ii in 1:nrow(df6)) {
  # Per road, filter the road dataset
  vec_road <- c(df6[ii, 1])
  df_road <- filter(df5, road == vec_road)
  
  # Per row within the specific road
  for (n in 1:nrow(df_road)) {
    #print(as.character(df_road$lrp[n]))
    if(!is.na(df_road$lat[n])){
      vec_road <- cbind(vec_road, as.character(df_road$lrp[n]), df_road$lat[n], df_road$lon[n])
    }
  }
  write.table(as.array(vec_road), file = "_roads.tcv", sep = "\t", append = TRUE, col.names = FALSE, row.names = FALSE)
}

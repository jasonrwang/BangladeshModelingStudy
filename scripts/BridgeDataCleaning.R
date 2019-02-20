# Load library
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(xlsx)

# Load datasets
df_road <- read.csv("Roads_InfoAboutEachLRP.csv")
df1 <- read.csv("BMMS_overview.csv", header = TRUE)
colnames(df1)[1] <- "road"  # Fix the column name

# Inspect the dataframe df1
#any(is.na(df1$lat))        # lat: NA value presents
#any(is.na(df1$lon))        # lon: NA value presents

# ==================================
#  Fix the lat/lon reversal problem 
# ==================================

# Create a function that fix the lat/lon reversal problem
fun_reversal <- function(xb) {
  # xb is a bridge data point (a row in the df)
  if(any(is.na(xb[18]), is.na(xb[19]))){
    # NA value presents in lat / lon
    # Skip this bridge
  } else {
    lat <- as.double(xb[18])
    lon <- as.double(xb[19])
    if(lon > 20.757 & lon < 26.635 & lat > 88.000 & lat < 92.615){
      # If 20.757< lon < 26.635 AND 88.000 < lat < 92.615
      # Swap the lon and lat column
      xb[18] <- lon
      xb[19] <- lat
    } else {
      # Skip this bridge
    }
  }
  return(xb)
}

# Loop the function per row and reshape the dataframe
df2 <- apply(df1, 1, fun_reversal)
df2 <- as.data.frame(t(df2))

# Drop the data points whose km/chainage value is NA
df2 <- drop_na(df2, c(km, chainage))

head(df2)

# =====================
#  Remove some bridges
# =====================

# Create a list with the length (max chainage value) for each road
df_RL <- select(df_road, road, chainage)
df_RL <- df_RL %>% group_by(road) %>% summarise(length = max(chainage))

# Create a cleaning function - remove some data points
fun_cln <- function(xb) {
  # xb is a bridge data point (a row in the df)
  # xb[2] is the km / chainage value (position) of the bridge
  
  # Get the corresponding limit chainage value (= road length)
  RL <- df_RL$length[which(df_RL$road == as.character(xb[1]))]
  #print(c(as.character(xb[1]), RL))
  
  if(!any(df_RL$road == as.character(xb[1]))) {
    # The road where the bridge is located does not exist in road dataset
    # Remove this bridge
    cat(c("Remove bridge @ road", as.character(xb[1]), "\t chainage", xb[2], "\n Road does not exist \n\n"), 
        file = "CleaningRecordBridge.txt", append = TRUE)
    xb <- rep(NA, 20)
  } else if (as.double(as.character(xb[2])) > RL) {
    # The bridge is beyond the road length
    # Remove this bridge
    cat(c("Remove bridge @ road", as.character(xb[1]), "\t chainage", xb[2], "\n Bridge is beyond road length \n\n"), 
        file = "CleaningRecordBridge.txt", append = TRUE)
    xb <- rep(NA, 20)
  } else {
    # The bridge is within the road length
    # Keep the bridge data
  }
  return(xb)
}

# Loop the function per row and reshape the dataframe
df3 <- apply(df2, 1, fun_cln)
df3 <- as.data.frame(t(df3))
df3 <- df3[apply(df3, 1, function(x) any(!is.na(x))), ]  #remove whole-NA rows
colnames(df3) <- colnames(df1)  # fix the column names

head(df3)

# ========================
#  Fix per chainage value 
# ========================

cat("START INTERPOLATING THE BRIDGE DATA\n===========================================\n\n\n", 
    file = "CleaningRecordBridge.txt", append = TRUE)

# Create a interpolating function
fun_ipola <- function(xb) {
  # xb is a bridge data point (a row in the df)
  # xb[2] is the km / chainage value (position) of the bridge
  # xb[18] is the lat value
  # xb[19] is the lon value
  
  # Get the sub-dataset of road where the bridge is located
  dfRR <- df_road %>% filter(road == as.character(xb[1])) %>%
    select(road, chainage, lat, lon)
  
  if(nrow(dfRR) < 2) {
    # If the road contains only one data point
    # Skip this bridge
  } else {
    # Start interpolating: 
    # Within the road dataset, 
    # find the closest road chainage value that is smaller than the bridge chainage value
    dd <- dfRR$chainage - as.double(as.character(xb[2]))
    ii <- max(which.max(dd > 0) - 1, 1) # get the index for such road point and 
    # make sure it is >= 1 (because of the start-point outlier removal of road thing)
    
    # Compute the interpolated lat/lon values from the two neighbouring road data points
    ylat <- c(dfRR$lat[ii], dfRR$lat[ii+1])
    ylon <- c(dfRR$lon[ii], dfRR$lon[ii+1])
    x <- c(dfRR$chainage[ii], dfRR$chainage[ii+1])
    cat(c("Interpolate from road coordinates: \n",
          as.character(xb[1]), "\tindex = ", ii, "\n", 
          "lat: \t\t", ylat, "\n", 
          "lon: \t\t", ylon, "\n", 
          "chainage: \t", x, "\n",
          "bridge chainage: ", as.double(as.character(xb[2])), "\n"),
        file = "CleaningRecordBridge.txt", append = TRUE)
    
    # Update the bridge coordinate
    xb[18] <- approx(x, ylat, xout = as.double(as.character(xb[2])))[2]
    xb[19] <- approx(x, ylon, xout = as.double(as.character(xb[2])))[2]
    cat(c("New bridge coord: \t", as.double(xb[18]), as.double(xb[19]), "\n\n"),
        file = "CleaningRecordBridge.txt", append = TRUE)
  }
  return(xb)
}

# Loop the function per row and reshape the dataframe
df4 <- apply(df3, 1, fun_ipola)
df5 <- sapply(df4, function(x) x)
df5 <- as.data.frame(t(df5))

head(df5)

# ========================
#  Write the df into file 
# ========================

write.xlsx(df5, file = "BMMS_overview.xlsx", sheetName = "BMMS_overview", col.names = TRUE, row.names = FALSE, showNA = TRUE)

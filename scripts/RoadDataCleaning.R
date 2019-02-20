# Data Cleaning for Road Data

# Load library
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(geosphere)
library(xlsx)


# Load data
df <- read.csv("./data/Roads_InfoAboutEachLRP.csv")

# ==========================================
#  Fix chainage reversal/duplicates problem 
# ==========================================

df1 <- df %>% group_by(road) %>% mutate(chainage2 = lead(chainage))

id_chainage <- c(0)

for (nrr in 1:nrow(df1)) {
  if(is.na(df1$chainage2[nrr] == TRUE)){
    # Here it is the last data point of the road
  } else if(df1$chainage[nrr] < df1$chainage2[nrr]){
    # Normal data point: The next chainage value is larger than this one
  } else {
    # Record the index of the point
    id_chainage <- c(id_chainage, nrr)
  }
}

# Remove the "bad" chainage datapoints
df1 <- df1[-id_chainage, ]

# Remove the "chainage2" column
df1 <- df1[1:7]

# Plot the raw road data
plt1 <- ggplot(filter(df1, road == "Z7404"), aes(lon, lat)) +
  geom_point() +
  geom_path() +
  ggtitle("Before cleaning")

ggplotly(plt1)

# =========================
#  Data cleaning for Roads 
# =========================

cat("CLEANING RECORD STARTS\n\n\n", file = "CleaningRecord.txt", append = FALSE)

# =========================
#  Fix start point outlier 
# =========================

# lat = lat - 1 needed : Z3711
df1$lat[which(df1$road == "Z3711")[1]] <- df1$lat[which(df1$road == "Z3711")[1]] - 1

# lon = lon - 1 needed : Z4606 & Z1129
df1$lon[which(df1$road == "Z4606")[1]] <- df1$lon[which(df1$road == "Z4606")[1]] - 1
df1$lon[which(df1$road == "Z1129")[1]] <- df1$lon[which(df1$road == "Z1129")[1]] - 1

# Remove roads that do not exist : Z1030 & Z1081 & Z1401
df1 <- df1[-which(df1$road == "Z1030"), ]
df1 <- df1[-which(df1$road == "Z1081"), ]
df1 <- df1[-which(df1$road == "Z1401"), ]

# Write to cleaning record
cat(c("Fix start point outlier to: \n", 
      "Z3711 lat = lat -1 = ", df1$lat[which(df1$road == "Z3711")[1]], "\n",
      "Z1129 lon = lon -1 = ", df1$lon[which(df1$road == "Z1129")[1]], "\n",
      "Z1129 lon = lon -1 = ", df1$lon[which(df1$road == "Z1129")[1]], "\n",
      "Remove roads that do not exist: Z1030 & Z1081 & Z1401 \n\n"), 
    file = "CleaningRecord.txt", append = TRUE)


# ================================
#  Modify by +1/-1 OR Interpolate 
# ================================

# Split the data frame by road
df2 <- split(df1, df1$road)

# Create a function to modify outlier
fun_pt <- function(p, q, r){
  # p is a vector of a specific data point
  # q is a vector of the previous data point of p
  # [road  chainage  lrp lat lon type  name]
  if(abs(as.double(p[4]) - as.double(q[4])) > 0.5){
    p[4] <- p[4] - as.integer(p[4]) + as.integer(q[4])
    cat(c("Fixed to:\n p1: ", as.double(p[5]), "\t", as.double(p[4]), "\n\n"), file = "CleaningRecord.txt", append = TRUE)
  } else if (abs(as.double(p[5]) - as.double(q[5])) > 0.5) {
    p[5] <- p[5] - as.integer(p[5]) + as.integer(q[5])
    cat(c("Fixed to:\n p1: ", as.double(p[5]), "\t", as.double(p[4]), "\n\n"), file = "CleaningRecord.txt", append = TRUE)
  } else {
    
    # Check lat moving trimmean
    if(abs(mean(c(p$lat, q$lat, r$lat)) - p$lat) > 0.01){
      # if the mean value with and without the point differs much
      # the point is an outlier (peak)
      # replace the value with the trimmean value
      p$lat <- mean(c(q$lat, r$lat))
      cat(c("Fixed to (TRIMMEAN):\n p1: \n", p$lat, "\t", p$lon, "\n\n"), file = "CleaningRecord.txt", append = TRUE)
    }
    
    # Check lon moving trimmean
    if(abs(mean(c(p$lon, q$lon, r$lon)) - p$lon) > 0.045){
      # if the mean value with and without the point differs much
      # the point is an outlier (peak)
      # replace the value with the trimmean value
      p$lon <- mean(c(q$lon, r$lon))
      cat(c("Fixed to (TRIMMEAN):\n p1: \n", p$lat, "\t", p$lon, "\n\n"), file = "CleaningRecord.txt", append = TRUE)
    }
  }
  return(p)
}

# Create a function to run through each road
fun_road <- function(df){
  # df is the dataset of (each) road
  #print(df)  # @Unhash for debugging@
  
  for (nn in 2:(nrow(df)-1)) {
    # for each row / data point in the specific road
    
    # If the road contains only one data point, then break for-loop
    if( nrow(df) <= 2 ){
      break
    }
    
    # compute the distance d1 & d2 w.r.t the next data point
    #     d1 : distance based on chainage
    #     d2 : distance based on coordinates
    
    d1 <- df$chainage[nn] - df$chainage[nn-1]
    d2 <- distm(c(df$lon[nn-1], df$lat[nn-1]),
                c(df$lon[nn], df$lat[nn]),
                fun = distHaversine) / 1000
    #cat(c(d1, d2, as.character(df$road[nn]), nn, df$chainage[nn]))  # @Unhash for debugging@
    
    # Compare the two distances
    if(abs(d1 - d2) > 3){
      # The difference is beyond limit, i.e., the data point is an outlier
      cat(c(as.character(df$road[nn]), "\tn = ", nn, "\t p1 chainage @ ", df$chainage[nn], "\n",
            "p1: ", df$lon[nn], "\t", df$lat[nn], "\n", 
            "p2: ", df$lon[nn-1], "\t", df$lat[nn-1], "\n",
            "d1 = ", d1, ", d2 = ", d2, "\n"), file = "CleaningRecord.txt", append = TRUE)
      
      # Fix the data point
      df[nn, ] <- fun_pt(df[nn, ], df[nn-1, ], df[nn+1, ])
    }
  }
  # return the cleaned road dataset
  return(df)
}

# Clean the dataset
df3 <- lapply(df2, fun_road)

# =============================
#  Drop the start/end outliers 
# =============================

fun_endpt <- function(df) {
  # df is the dataset of (each) road
  
  # If the road contains only one data point, then skip
  if( nrow(df) <= 2 ){
    # Skip the road dataset
  } else {
    # Start point outliers
    # compute the distance d1 & d2 w.r.t the next data point
    #     d1 : distance based on chainage
    #     d2 : distance based on coordinates
    
    d1 <- df$chainage[2] - df$chainage[1]
    d2 <- distm(c(df$lon[1], df$lat[1]),
                c(df$lon[2], df$lat[2]),
                fun = distHaversine) / 1000
    
    # Compare the two distances
    if(abs(d1 - d2) > 3){
      # The difference is beyond limit, i.e., the data point is an outlier
      cat(c(as.character(df$road[2]), "\t Start point removed: \n",
            "p: ", df$lon[2], "\t", df$lat[2], "\n",
            "d1 = ", d1, ", d2 = ", d2, "\n\n"), file = "CleaningRecord.txt", append = TRUE)
      
      # Remove the point
      df <- df[-c(1), ]
    }
    
    # End point outliers
    # compute the distance d1 & d2 w.r.t the next data point
    #     d1 : distance based on chainage
    #     d2 : distance based on coordinates
    
    d1 <- df$chainage[nrow(df)] - df$chainage[nrow(df)-1]
    d2 <- distm(c(df$lon[nrow(df)-1], df$lat[nrow(df)-1]),
                c(df$lon[nrow(df)], df$lat[nrow(df)]),
                fun = distHaversine) / 1000
    
    # Compare the two distances
    if(abs(d1 - d2) > 3){
      # The difference is beyond limit, i.e., the data point is an outlier
      cat(c(as.character(df$road[nrow(df)]), "\t End point removed: \n",
            "p: ", df$lon[nrow(df)], "\t", df$lat[nrow(df)], "\n",
            "d1 = ", d1, ", d2 = ", d2, "\n\n"), file = "CleaningRecord.txt", append = TRUE)
      
      # Remove the point
      df <- df[-c(nrow(df)), ]
    }
  }
  return(df)
}

# Clean the dataset
df4 <- lapply(df3, fun_endpt)


# ===============
#  Visualisation  
# ===============

# Plot dataset for verification of the results
plt2 <- ggplot(df4$Z7404, aes(lon, lat)) +
  geom_point() +
  geom_path() +
  ggtitle("After cleaning")

ggplotly(plt2)


# =====================
#  Write data to file 
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

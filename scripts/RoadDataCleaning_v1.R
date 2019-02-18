# Data Cleaning for Road Data

# Load library
library(dplyr)
library(ggplot2)
library(plotly)
library(geosphere)
library(xlsx)


# Load data
df <- read.csv("./data/Roads_InfoAboutEachLRP.csv")

# ===============================
#  Fix chainage reversal/duplicates problem 
# ===============================

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


# =========================
#  Data cleaning for Roads 
# =========================


# Split the data frame by road
df2 <- split(df1, df1$road)

# Create a function to modify outlier
fun_pt <- function(p, q){
  # p is a vector of a specific data point
  # q is a vector of the previous data point of p
  # [road  chainage  lrp lat lon type  name]
  if(as.integer(p[4]) != as.integer(q[4])){
    p[4] <- p[4] - as.integer(p[4]) + as.integer(q[4])
    #print(c(p[4], q[4]))
  } else if (as.integer(p[5]) != as.integer(q[5])) {
    p[5] <- p[5] - as.integer(p[5]) + as.integer(q[5])
    #print(c(p[5], q[5]))
  }
  return(p)
}

# Create a function to run through each road
fun_road <- function(df){
  # df is the dataset of (each) road
  #print(df)  # @Unhash for debugging@
  
  for (nn in 2:(nrow(df))) {
    # for each row / data point in the specific road
    
    # If the road contains only one data point, then break for-loop
    if( nrow(df) == 1 ){
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
    if(abs(d1 - d2) > 1){
      # The difference is beyond limit, i.e., the data point is an outlier
      cat(c(as.character(df$road[nn]), nn, df$chainage[nn], "\n",
            df$lon[nn], df$lat[nn], df$lon[nn-1], df$lat[nn-1], "\n",
            d1, d2, "\n\n"))
      
      # Fix the data point
      df[nn, ] <- fun_pt(df[nn, ], df[nn-1, ])
    }
  }
  # return the cleaned road dataset
  return(df)
}

# Clean the dataset
df3 <- lapply(df2, fun_road)

# Plot a road to check the results
ggplot(df3$N102, aes(lon, lat)) +
  geom_point() +
  geom_path()

# Reshape the dataset
df4 <- bind_rows(df3)



# =====================
#  Write data to file 
# =====================

write.csv(df4, file = "./data/Roads_InfoAboutEachLRP_v1.csv")

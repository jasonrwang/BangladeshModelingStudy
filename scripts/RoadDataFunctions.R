### Functions to support CleanRoadData.R

# ========================================
#   Different Outlier Detection Methods
# ========================================

# Cook's Distance

RoadCooksDis_LatLon <- function(df) {
    df <- df %>% mutate(
        cookd = cooks.distance(lm(lat ~ lon)) > 0.05
    )
    return(df)
}

# Group the dataset by each road (e.g. N6) and find the change in lon and lat (assumes data is ordered)

RoadDeltaLon <- function(df, q = 0.95) {
    df <- df %>% mutate(
        deltaLon = ifelse(row_number() == 0,0,abs(lead(lon) - lon)), # Find abs change in lon (ignoring first entry)
        deltaLon = ifelse(is.na(deltaLon), 0, deltaLon),
        quantLimitLon = quantile(deltaLon, q),
        #outlier = deltaLon > quantLimitLon
    )
    quantLimitLon_All <- quantile(df$deltaLon,q)
    df <- df %>% mutate(
        outlier = deltaLon > quantLimitLon_All
    )
    return(df)
}

RoadDeltaLat <- function(df, q = 0.95) {
    df <- df %>% mutate(
        deltaLat = ifelse(row_number() == 0,0,abs(lead(lat) - lat)), # Find abs change in lon
        deltaLat = ifelse(is.na(deltaLat), 0, deltaLat),
        quantLimitLat = quantile(deltaLat, q),
        # outlier = deltaLat > quantLimitLat
    )
    quantLimitLat_All <- quantile(df$deltaLat,q)
    df <- df %>% mutate(
        outlier = deltaLat > quantLimitLat_All
    )
    return(df)
}

ByChainage <- function(df) {
    df <- df %>% mutate(chainage2 = lead(chainage))

    id_chainage <- c(0)

    for (nrr in 1:nrow(df)) {
        if(is.na(df$chainage2[nrr] == TRUE)){
            # Here it is the last data point of the road
        } else if(df$chainage[nrr] < df$chainage2[nrr]){
            # Normal data point: The next chainage value is larger than this one
        } else {
            # Record the index of the point
            id_chainage <- c(id_chainage, nrr)
        }
    }

    # Remove the "bad" chainage datapoints
    df <- df[-id_chainage, ]

    # Remove the "chainage2" column
    df <- df[1:7]
    return(df)
}

# ==============================
#   Different Cleaning Methods
# ==============================

# Modify by +1/-1 (data misentry) OR interpolate (noise)
fun_pt <- function(p, q, r){
    ## p,q,r are vectors of all data points we want to fix
    # p is a specific data point
    # q precedes p and r follows p
    # [road  chainage  lrp lat lon type  name]

    ## First check for outliers based on data misentry (e.g. 23 instead of 22)
    # Column 4 and 5 are lon/lat
    if(abs(as.double(p[4]) - as.double(q[4])) > 0.5){
        p[4] <- p[4] - as.integer(p[4]) + as.integer(q[4])
        cat(c("Fixed to:\n p1: ", as.double(p[5]), "\t", as.double(p[4]), "\n\n"), file = "CleaningRecord.txt", append = TRUE)
    } else if (abs(as.double(p[5]) - as.double(q[5])) > 0.5) {
        p[5] <- p[5] - as.integer(p[5]) + as.integer(q[5])
        cat(c("Fixed to:\n p1: ", as.double(p[5]), "\t", as.double(p[4]), "\n\n"), file = "CleaningRecord.txt", append = TRUE)
    } else {
        ## Then deal with the rest of the outliers as noise

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

    # If the road contains only one data point, then break for-loop
    if( nrow(df) <= 2 ){
    } else {
        for (nn in 2:(nrow(df)-1)) {
            # for each row / data point in the specific road

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
    }
    # return the cleaned road dataset
    return(df)
}

# Drop start/end outlier
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

CooksDistanceClean <- function(df) {
    CooksRows <- which(df$cookd) # Find outlier row numbers
    for (i in CooksRows){
        cat(c(as.character(df$road[i]), "\t Remove CooksD:\n p: \t", df$lon[i], "\t", df$lat[i], "\n\n"), file = "CleaningRecord.txt", append = TRUE)
    }
    df <- df[-CooksRows,]
}

# =================
#   Visualization
# =================

VisualizeRoads <- function( df, roads = c('N6'),
        title = 'INSERT TITLE',outlier = FALSE ) {
    # Plot the road with paths linked between points
    roads  # Plot selected roads
    p <- ggplot(filter(df, road %in% roads), aes(lon, lat)) +
    geom_point(shape = 1) + 
    geom_path() + 
    ggtitle(title)

    if (outlier) {
       list_Outliers <- filter(df, outlier == TRUE, road %in% roads)
       p <- p + geom_point(data = list_Outliers, aes(lon, lat), colour = "green", shape = 2, size = 5)
    }

    ggplotly(p)
}

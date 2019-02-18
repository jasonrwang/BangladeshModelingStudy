### Functions to support CleanRoadData.R

## Different Outlier Detection Methods

# Cook's Distance

RoadCooksDis_LatLon <- function(df) {
    df <- df %>% mutate(
        cookd = cooks.distance(lm(lat ~ lon)), # Also find each datapoint's Cook's Distance within each group
    )
    return(df)
}

# Group the dataset by each road (e.g. N6) and find the change in lon and lat (assumes data is ordered)

RoadDeltaLon <- function(df) {
    df <- df %>% mutate(
        deltaLon = ifelse(row_number() == 0,0,abs(lead(lon) - lon)), # Find abs change in lon (ignoring first entry)
        deltaLon = ifelse(is.na(deltaLon), 0, deltaLon),
    )
    return(df)
}

RoadDeltaLat <- function(df) {
    df <- df %>% mutate(
        deltaLat = ifelse(row_number() == 0,0,abs(lead(lat) - lat)), # Find abs change in lon
        deltaLat = ifelse(is.na(deltaLat), 0, deltaLat) 
    )
    return(df)
}

## Visualizing Road Data to Look for Outliers

# Load necessary libraries
library(ggplot2)
library(plotly)

VisualizeRoads <- function( df, roads = c('N6'), outlier = FALSE ) {
    # Plot the road with paths linked between points
    roads  # Plot selected roads
    p <- ggplot(filter(df, road %in% roads), aes(lon, lat)) +
    geom_point(shape = 1) + 
    geom_path() + 
    ggtitle("Road")

    if (outlier) {
       list_Outliers <- filter(df, outlier == TRUE, road %in% roads)

    p <- p + geom_point(data = list_Outliers, aes(lon, lat), colour = "green", shape = 2, size = 5)
    }

    ggplotly(p)
}

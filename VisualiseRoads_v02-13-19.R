### Data Cleaning for Road Data

# Load library
library(dplyr)
library(ggplot2)
library(plotly)

# Load data
df <- read.csv("data/Roads_InfoAboutEachLRP.csv")

## Create a full list of roads
#ListOfRoad <- unique(df$road)
#ListOfRoad <- data.frame(road = ListOfRoad, RoadType = substring(ListOfRoad, 1, 1))

# Split raw dataframe into lists by road
df_split <- split(df, df$road)

# Modify here the road name! 
list_SplitedRoad <- select(df_split$N6, lon, lat)

# Plot the road with paths linked between points
p <- ggplot(list_SplitedRoad, aes(lon, lat)) +
  geom_point(shape = 1) + 
  geom_path() + 
  ggtitle("Road")

ggplotly(p)

## Below are different methods to finding outliers

# Calculate the Cooks distance
cookd <- cooks.distance(lm(list_SplitedRoad$lat ~ list_SplitedRoad$lon))
plot(cookd)

cookd <- cookd > 0.05

# Summary statistics of deltas

# First, find the change between datapoints
list_SplitedRoad <- list_SplitedRoad %>%
  mutate(deltaLon = ifelse(row_number() == dim(list_SplitedRoad)[1],0,abs(lead(lon) - lon))) %>% # Find abs change in lon
  mutate(deltaLon = ifelse(is.na(deltaLon), 0, deltaLon)) %>%
  mutate(deltaLat = ifelse(row_number() == dim(list_SplitedRoad)[1],0,abs(lead(lat) - lat))) %>% # Find abs change in lon
  mutate(deltaLat = ifelse(is.na(deltaLat), 0, deltaLat))

deltaLat <- quantile(list_SplitedRoad$deltaLat, c(.95))
deltaLon <- quantile(list_SplitedRoad$deltaLon, c(.95))

# We only care about points whose absolute delta is larger than normal
deltOut <- list_SplitedRoad$deltaLon > deltaLon | list_SplitedRoad$deltaLat > deltaLat

# save the result to the dataframe
list_SplitedRoad$outlier <- cookd | deltOut

# Plot the outliers on the graph
list_Outliers <- filter(list_SplitedRoad, outlier == 1)

p <- p + geom_point(data = list_Outliers, aes(lon, lat), colour = "red", shape = 2, size = 2.5)

ggplotly(p)

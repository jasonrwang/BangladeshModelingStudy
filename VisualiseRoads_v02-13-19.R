# Data Cleaning for Road Data

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

# Calculate the Cooks distance
cookd <- cooks.distance(aov(list_SplitedRoad$lat ~ list_SplitedRoad$lon))
cookd <- cookd > 0.05
plot(cookd)

# save the result to the dataframe
list_SplitedRoad$outlier <- cookd

# Plot the outliers on the graph
list_Outliers <- filter(list_SplitedRoad, outlier == 1)

p <- p + geom_point(data = list_Outliers, aes(lon, lat), colour = "red", shape = 2)

ggplotly(p)

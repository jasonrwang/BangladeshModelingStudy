# Data Cleaning for Road Data

# Set working directory
setwd("D:/EPA/EPA Semester-1.3/EPA1351 Advanced Discrete Simulation/Assignment 1/_Practice")

# Load library
library(dplyr)
library(ggplot2)
library(plotly)

# Load data
df <- read.csv("Roads_InfoAboutEachLRP.csv")

# Create a full list of roads
ListOfRoad <- unique(df$road)
ListOfRoad <- data.frame(road = ListOfRoad, RoadType = substring(ListOfRoad, 1, 1))

# Split raw dataframe into lists by road
df_split <- split(df, df$road)

# Modify here the road name! 
list_SplitedRoad <- select(df_split$N102, lon, lat)

# Plot the road with paths linked between points
p <- ggplot(list_SplitedRoad, aes(lon, lat)) +
  geom_point(shape = 1) + 
  geom_path() + 
  ggtitle("list_SplitedRoad")

p <- ggplotly(p)

p
       
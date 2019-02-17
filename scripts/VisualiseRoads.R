### Data Cleaning for Road Data

# Load library
library(dplyr)
library(ggplot2)
library(plotly)

# Load data
df <- read.csv("./data/Roads_InfoAboutEachLRP.csv")

# Plot the road with paths linked between points
roads = c("N6") # Plot selected roads
p <- ggplot(filter(df, road %in% roads), aes(lon, lat)) +
  geom_point(shape = 1) + 
  geom_path() + 
  ggtitle("Road")

ggplotly(p)

# Group the dataset by each road (e.g. N6) and find the change in lon and lat (assumes data is ordered)
df <- df %>% group_by(road) %>%
  mutate(
    cookd = cooks.distance(lm(lat ~ lon)), # Also find each datapoint's Cook's Distance within each group
    deltaLon = ifelse(row_number() == dim(list_SplitedRoad)[1],0,abs(lead(lon) - lon)), # Find abs change in lon
    deltaLon = ifelse(is.na(deltaLon), 0, deltaLon),
    deltaLat = ifelse(row_number() == dim(list_SplitedRoad)[1],0,abs(lead(lat) - lat)), # Find abs change in lon
    deltaLat = ifelse(is.na(deltaLat), 0, deltaLat)    
  )

# Find the 95th percentile limit for each grouped set of lon and lat coordinates, and join to original df
dfSum <- summarize(df, quantLimitLat = quantile(deltaLat, c(.97)), quantLimitLon = quantile(deltaLon, c(.97))) %>% group_by(road)
df <- inner_join(df,dfSum, by = "road")

# Call out the outliers
df <- mutate(df, outlier = cookd > 0.05 | deltaLat > quantLimitLat | deltaLon > quantLimitLon)

# Plot the outliers on the graph
list_Outliers <- filter(df, outlier == TRUE, road %in% roads)

p <- p + geom_point(data = list_Outliers, aes(lon, lat), colour = "green", shape = 2, size = 5)

ggplotly(p)

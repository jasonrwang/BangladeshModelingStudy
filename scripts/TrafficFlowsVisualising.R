## Visualising Traffic per transport mode

# Load library
library(dplyr)
library(ggplot2)
library(ggmap)

# Load data
df <- read.csv('data/_roads3.csv')
df_traffic <- read.csv('data/Traffic.csv') %>% select(-X)
df_nrLanes <- read.table('data/traffic/N1.widths.processed.txt', header = TRUE)

## ===============================================================
##  Visualising N1 (or candidate road) Traffic per transport mode 
## ===============================================================

# Assign candidate road
cRoad = c("N1")

# Subset the data for candidate road
df_rr <- df %>% filter(road == cRoad)
df_rr_traffic <- df_traffic %>% filter(Road == cRoad)
df_rr_nrLanes <- df_nrLanes %>% filter(roadNo == cRoad)

# Add a column of colour of road chainange points
df_rr <- df_rr %>% mutate(colourTraffic = 'red')
df_rr$colourTraffic[150:300] <- 'blue'  # mark a road segment to blue for testing


## Plot the results on map of Bangladesh

get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = c("color", "bw")) %>% ggmap() +
  geom_point(aes(x = lon, y = lat), data = df_rr, colour = df_rr$colourTraffic)


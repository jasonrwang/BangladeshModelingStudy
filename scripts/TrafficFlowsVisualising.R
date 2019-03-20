## Visualising Traffic per transport mode

# Load library
library(dplyr)
library(ggplot2)
library(ggmap)

source("scripts/TrafficDataFunctions.R")

# Load data
df <- read.csv('data/_roads3.csv')
df_nrLanes <- read.csv('data/Width.csv') %>% select(-X)
df_traffic <- read.csv('data/Traffic.csv') %>% select(-X)
df_bridge <- readxl::read_excel('data/BMMS_overview.xlsx')

# Assign candidate road
cRoad = c("N1")

# Assign transport mode
tMode <- c('Heavy.Truck')

## =============================
##  Data prep for visualisation 
## =============================

## Subset the data for candidate road
df_rr <- df %>% filter(road == cRoad)
df_rr_nrLanes <- df_nrLanes %>% filter(Road == cRoad)
df_rr_traffic <- df_traffic %>% filter(Road == cRoad)
df_rr_bridge <- df_bridge %>% filter(road == cRoad)

## =======================
##  Merge nrLanes dataset 
## =======================

## Calculate the weighted nrLanes
df_rr$nrLanesW <- df_rr %>% apply(1, nrLanesW)

## ============================
##  Merge traffic flow dataset 
## ============================

## Modify dtype for traffic flow dataset (from factor to numeric)

# Get the columns whose dtype is factor
indx <- sapply(df_rr_traffic, is.factor)

# Remove the ones that should have dtype as factor
indx[c('Road', 'Description', 'LRP.start', 'LRP.end', 'Road.Code', 'Side')] <- FALSE

# Fix the dtype to numeric
df_rr_traffic[indx] <- lapply(df_rr_traffic[indx], function(x) as.numeric(as.character(x)))

## Sum the AADT of L/R components for link
df_rr_trafficSum <- df_rr_traffic %>% select(-c(1:9, 26, 28)) %>% 
  group_by(Segment) %>% summarise_all(sum)

df_rr_trafficSum <- df_rr_traffic %>% select(c(1:9, 26:27)) %>% 
  group_by(Segment) %>% top_n(1, Road.Code) %>%
  right_join(df_rr_trafficSum, by = 'Segment') %>%
  select(-Road)

## Merge with road dataset

df_rr <- df_rr %>% full_join(df_rr_trafficSum, 
                              by = c("chainage" = "Chainage.start"))

# Fill in NAs with latest non-NA row (startChainage of road link)
df_rr <- df_rr %>% fill(colnames(df_rr)[9:length(df_rr)]) %>% drop_na()

## ===========================
##  Calculate traffic density  
## ===========================

## Traffic density = AADT / nrLanes (weighted)
df_rr[19:35] <- df_rr[19:35] / df_rr$nrLanesW


## ===============================================================
##  Visualising N1 (or candidate road) Traffic per transport mode 
## ===============================================================

# Subset the dataset for assigned transport mode
df_rrV <- df_rr %>% select(c('chainage', 'lat', 'lon', tMode))

# Rename column name
colnames(df_rrV)[4] <- 'Traffic.Density'

## Plot the roads on map of Bangladesh
get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = c("color", "bw")) %>% ggmap() +
  geom_point(data = df_rrV, aes(x = lon, y = lat, colour = Traffic.Density), stroke = 1) + 
  scale_colour_gradient(low = 'grey', high = 'red') +
  ggtitle(paste(tMode, '(AADT / nrLanes)'))


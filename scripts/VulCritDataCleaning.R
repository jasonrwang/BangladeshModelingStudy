source("scripts/VulCritDataFunctions.R")

# Get the names of all the traffic webpage files in the "data" subfolder
trafficFileNames <- list.files("data/traffic", pattern = "*.traffic.htm", full.names = TRUE)
widthFileNames <- list.files("data/traffic", pattern = "*.widths.processed.txt", full.names = TRUE)

# Get the actual names of all the roads that have been imported
roadnames <- gsub("data/traffic/", "", gsub(".traffic.htm", "", trafficFileNames))

# Scrape all of the road files that are available into dfs
df_traffic <- lapply(trafficFileNames, ScrapeTraffic) # This is still a little buggy with small roads
df_nrLanes <- lapply(widthFileNames, importWidth)

# Merge this list of dfs into one large df
df_traffic <- do.call(rbind,df_traffic)
df_nrLanes <- do.call(rbind,df_nrLanes)

df_traffic <- df_traffic %>% group_by(Road)
df_nrLanes <- df_nrLanes %>% group_by(Road)

## Determine vulnerability by the number of bridges with concerns in it.
# dfVulBridge_roadSection <- # For all road sections in the country
# THe above might need to be ungrouped and then sorted
# dfVulBridge_road <- # For each road

# ## Write to file
# write.csv(df_traffic, file = "data/Traffic.csv")
# write.csv(df_nrLanes, file = "data/Width.csv")


## ======================================
## Visualising Traffic per transport mode
## ======================================

# Load library
library(ggplot2)
library(ggmap)
library(plotly)

# Load other useful data
df <- read.csv("data/_roads3.csv", stringsAsFactors = FALSE)
df_bridge <- readxl::read_excel("data/BMMS_overview.xlsx")

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
df_rr$nrLanesW <- df_rr %>% apply(1, nrLanesW) # Does this line work?

## ============================
##  Merge traffic flow dataset 
## ============================

## Modify dtype for traffic flow dataset (from factor to numeric)

# Get the columns whose dtype is factor
indx <- sapply(df_rr_traffic, is.factor)

# Remove the ones that should have dtype as factor
indx[
    c('Road', 'Description', 'LRP.start', 'LRP.end', 'Road Code', 'Side')
] <- FALSE

# Fix the dtype to numeric
df_rr_traffic[indx] <- lapply(
    df_rr_traffic[indx], function(x) as.numeric(as.character(x))
)

## Sum the AADT of L/R components for link
df_rr_trafficSum <- df_rr_traffic %>% select(-c(1:9, "Road.Code", "Side")) %>% 
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

## =======================
##  Merge bridges dataset   
## =======================

df_rr <- BridgeVul(df_rr, df_rr_bridge)
unique(a$Vulnerability)

## ===============================================================
##  Visualising N1 (or candidate road) Traffic per transport mode 
## ===============================================================

# Subset the dataset for assigned transport mode
df_rrV <- df_rr %>% select(c('chainage', 'lat', 'lon', tMode)) %>% 
  data.table::setnames(old = tMode, new = 'TrafficDensity')

# 
df_rr_bridge$condition <- factor(df_rr_bridge$condition)

## Plot the roads and bridges on map of Bangladesh
get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
  geom_point(data = df_rrV, aes(x = lon, y = lat, colour = TrafficDensity)) +
  scale_colour_gradient(name = tMode, guide = 'colorbar', low = 'pink', high = 'red') +
  geom_point(data = df_rr_bridge, aes(x = lon, y = lat, shape = condition), size = 1.5) +
  scale_shape_manual(name = 'Bridge category', guide = 'legend', values = c(0, 1, 2, 5)) +
  ggtitle(paste('Traffic density (AADT / nrLanes)'))

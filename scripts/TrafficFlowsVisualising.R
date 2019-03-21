## Visualising Traffic per transport mode

# Load library
library(dplyr)
library(ggplot2)
library(ggmap)
library(plotly)
library(leaflet)
library(shiny)

source("scripts/TrafficFlowsVisualisingFunction.R")

## Load data
df <- read.csv('data/_roads3.csv')
df_nrLanes <- read.csv('data/Width.csv') %>% select(-X)
df_traffic <- read.csv('data/Traffic.csv', stringsAsFactors = FALSE) %>% select(-X)
df_bridge <- readxl::read_excel('data/BMMS_overview.xlsx')

## Assign the bridge condition scores
lbcond <- as.vector(c("A" = 1, "B" = 2, "C" = 3, "D" = 4))


## =============================
##  Data prep for visualisation 
## =============================

## Merge nrLanes dataset ##

df_all <- df
df_all$nrLanesW <- df_all %>% apply(1, CalnrLanesW) # Get the weighted nrLanesW
df_all <- df_all %>% fill(nrLanesW) # fill the NA-nrLanesW of last dtRow for every road

# Create a RoadChainage column for road-node specification
df_all <- df_all %>% mutate(RoadChainage = paste(as.character(road), 
                                                 as.character(chainage), 
                                                 sep = "-"))

## Merge traffic flow dataset ##

# Make a list of traffic flow vol colnames
colnameTraffic <- c("Heavy.Truck", "Medium.Truck", "Small.Truck",
                    "Large.Bus", "Medium.Bus","Micro.Bus", 
                    "Utility", "Car", "Auto.Rickshaw", 
                    "Motor.Cycle", "Bi.Cycle", "Cycle.Rickshaw", "Cart",
                    "Motorized", "Non.Motorized", "Total.AADT", "PCE") %>% as.array()

# Fix dtype for traffic flow data
df_traffic[colnameTraffic] <- sapply(df_traffic[colnameTraffic], function(x) as.numeric(x))

# Drop the rows with traffic flow data of NAs
df_traffic <- df_traffic %>% drop_na(PCE)

# Prepare the RHS-dataset (sum of traffic flow vol of L/R components for link)
df_trafficSumRHS <- df_traffic %>% 
  group_by(Road, Segment) %>%
  select(c('Road', colnameTraffic, 'Segment')) %>%
  summarise_all(sum) %>%
  ungroup()
df_trafficSumRHS <- df_trafficSumRHS %>% 
  mutate(RoadSegment = paste(as.character(Road), as.character(Segment), sep = "-")) %>%
  select(-Road, -Segment)

# Prepare the LHS-dataset (first chainage dtRow for each link) 
df_trafficSumLHS <- df_traffic %>% 
  group_by(Road, Segment) %>%
  select(Road, LRP.start, Chainage.start, Road.Code, Segment) %>% 
  top_n(1, Road.Code)
df_trafficSumLHS <- df_trafficSumLHS %>% 
  mutate(RoadSegment = paste(as.character(Road), as.character(Segment), sep = "-"))

# Merge the whole traffic datasets
df_trafficSum <- full_join(df_trafficSumLHS, df_trafficSumRHS, by = 'RoadSegment') %>%
  # Create a RoadChainage column for road-node specification
  mutate(RoadChainage = paste(as.character(Road), 
                              as.character(Chainage.start), 
                              sep = "-"))  %>%
  ungroup() %>%
  select(-Road, -LRP.start, -Chainage.start, -Segment, -Road.Code)

# Merge traffic with full road dataset
df_all_t <- left_join(df_all, df_trafficSum, by = "RoadChainage")

# Fill in NAs with latest non-NA row (startChainage of road link)
df_all_t <- df_all_t %>% fill(colnames(df_trafficSum))

# Drop the roadRow where nrLanes = 0
df_all_t <- df_all_t[-which(df_all_t$nrLanesW == 0), ]


##  Calculate traffic density ## 

## Traffic density = trafic flow volumne / nrLanes (weighted)
df_all_t[colnameTraffic] <- df_all_t[colnameTraffic] / df_all_t$nrLanesW


##  Calculate vulnerability (per road link) ##
# This is really bad code, but 
roadnames <- unique(df_all$road)
df_vul <- AllRoadBridgeVul(roadnames, df_all)
df_all_v <- full_join(df_all_t, df_vul,
  by = c("road", "chainage", "lrp", "lat", "lon", "gap", "type", "name", "nrLanesW"))
names(df_all_v)
## ===============================================================
##  Visualising N1 (or candidate road) Traffic per transport mode 
## ===============================================================

## Assign candidate road
cRoad = c("N1")

## Assign transport mode
#
# "Heavy.Truck"  "Medium.Truck"  "Small.Truck"
# "Large.Bus"    "Medium.Bus"    "Micro.Bus"
# "Utility"      "Car"           "Auto.Rickshaw"
# "Motor.Cycle"  "Bi.Cycle"      "Cycle.Rickshaw"  "Cart"
# "Motorized"    "Non.Motorized" "Total.AADT"      "PCE"
tMode <- c('Cycle.Rickshaw')


## Subset the data for candidate road ##
df_rr <- df_all_t %>% filter(road == cRoad) 
df_rr_bridge <- df_bridge %>% filter(road == cRoad)


# Create a palette that maps bridge condition factor levels to colors
palBridge <- colorFactor(c("grey", "yellow", "orange", "red"), 
                         domain = c("A", "B", "C", "D"))
# Create a palette that maps road traffic to continuour colors
palRoad <- colorNumeric("GnBu", df_rr[tMode], na.color = NA)

m <- leaflet() %>% addTiles(group = "Openstreet map") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB (Default)") %>%
  # Overlay groups - road data
  addCircleMarkers(data = df_rr, ~lon, ~lat,
                   label = ~as.character(lrp),
                   radius = 5,
                   weight = 1,
                   color = 'grey80',
                   fillColor = ~palRoad(df_rr[[tMode]]),
                   fillOpacity = 1,
                   group = "Road"
                   ) %>%
  addLegend("bottomright", 
            pal = palRoad, 
            values = df_rr[[tMode]],
            group = "Road",
            title = "Traffic density") %>%
  
  # Overlay groups - by bridge condition A/B/C/D
  addCircleMarkers(data = df_rr_bridge, ~lon, ~lat, 
                   label = ~as.character(condition), 
                   popup = ~as.character(LRPName),
                   radius = 4,
                   weight = 1,
                   color = 'black',
                   fillColor = ~palBridge(condition), 
                   fillOpacity = ~ifelse(condition == "A", 0.5, 0.9),
                   group = ~as.character(condition)
                   ) %>% 
  addLegend("bottomright", 
            pal = palBridge, 
            values = df_rr_bridge$condition,
            group = c("A", "B", "C", "D"),
            title = "Bridge condition") %>%
  addLayersControl(
    baseGroups = c("CartoDB (Default)", "Openstreet map"),
    overlayGroups = c("A", "B", "C", "D", "Road"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  ) %>%
  setView(lng = 90.3, lat = 24, zoom = 7)

m


## ===========================================
##  Visualising criticality and vulnerability  
## ===========================================

## Criticality ##

df_Criticality <- df_all_t %>% 
  select(RoadSegment, PCE) %>% 
  arrange(desc(PCE)) %>%
  distinct()

df_all_t_top10 <- df_all_t %>% semi_join(df_Criticality[1:10, ], by = "RoadSegment")

## Plot all criticality (filtered)
get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
  geom_point(data = filter(df_all_t, PCE > 10000), aes(x = lon, y = lat, colour = PCE)) +
  scale_colour_gradient(name = "Weighed traffic density (ppl/yr)", guide = 'colorbar', low = 'pink', high = 'red') +
  ggtitle(paste('Criticality per road segment (> 10,000 ppl/yr)'))

## Plot top-10 criticality
get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
  geom_point(data = df_all_t_top10, aes(x = lon, y = lat, colour = PCE)) +
  scale_colour_gradient(name = "Weighed traffic density (ppl/yr)", guide = 'colorbar', low = 'pink', high = 'red') +
  ggtitle(paste('Top-10 most critical road segments'))

## Zoom-in plot
get_stamenmap(bbox = c(left  = 89.4, bottom = 23.0, 
                       right = 91.6,    top = 24.5), 
              zoom = 8, maptype = "terrain", color = 'bw') %>% ggmap() +
  geom_point(data = df_all_t_top10, aes(x = lon, y = lat, colour = PCE)) +
  scale_colour_gradient(name = "Weighed traffic density (ppl/yr)", guide = 'colorbar', low = 'pink', high = 'red') +
  ggtitle(paste('Top-10 most critical road segments (zoom-in)'))

## Vulnerability ##

df_vulnerability <- df_all_v %>% 
  select(road, RoadSegment, lat, lon, Vulnerability) %>% 
  arrange(desc(Vulnerability)) %>%
  distinct()

# df_vulnerability[1:10,] %>% select(-c(lat,lon))

## Plot all criticality (filtered)
get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5545, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
  geom_point(data = df_all_v, aes(x = lon, y = lat, colour = Vulnerability)) +
  scale_colour_gradient(name = "Vulnerability", guide = 'colorbar', low = 'pink', high = 'red') +
  ggtitle(paste('Vulnerability per road segment'))

## Plot top-10 criticality
get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.45, 
                       right = 92.8135,    top = 26.7182), 
              zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
  geom_point(data = df_vulnerability[1:10,], aes(x = lon, y = lat, colour = Vulnerability)) +
  scale_colour_gradient(name = "Vulnerability", guide = 'colorbar', low = 'pink', high = 'red') +
  ggtitle(paste('Top-10 most vulnerable road segments'))

# Filter first by criticality
df_vulnerability_busy <- df_all_v %>% 
  filter(PCE > 2000) %>%
  select(road, RoadSegment, lat, lon, Vulnerability) %>% 
  arrange(desc(Vulnerability)) %>%
  distinct()

df_vulnerability_busy[1:10,]
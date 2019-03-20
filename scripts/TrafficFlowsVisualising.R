## Visualising Traffic per transport mode

# Load library
library(dplyr)
library(ggplot2)
library(ggmap)
library(plotly)
library(leaflet)
library(shiny)

source("scripts/TrafficFlowsVisualisingFunction.R")

# Load data
df <- read.csv('data/_roads3.csv')
df_nrLanes <- read.csv('data/Width.csv') %>% select(-X)
df_traffic <- read.csv('data/Traffic.csv', stringsAsFactors = FALSE) %>% select(-X)
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

# Make a list of traffic flow vol colnames
colnameTraffic <- c("Heavy.Truck", "Medium.Truck", "Small.Truck",
                    "Large.Bus", "Medium.Bus","Micro.Bus", 
                    "Utility", "Car", "Auto.Rickshaw", 
                    "Motor.Cycle", "Bi.Cycle", "Cycle.Rickshaw", "Cart",
                    "Motorized", "Non.Motorized", "Total.AADT", "PCE") %>% as.array()

# Prepare the RHS-dataset (sum of traffic flow vol of L/R components for link)
df_rr_trafficSum <- df_rr_traffic %>% select(c(colnameTraffic, 'Segment')) %>%
  sapply(function(x) as.numeric(x)) %>% as.data.frame() %>% 
  group_by(Segment) %>% summarise_all(sum)

# Merge the LHS-dataset (first chainage dtRow for each link) with prepared RHS-dataset 
df_rr_trafficSum <- df_rr_traffic %>% select(LRP.start, Chainage.start, Road.Code, Segment) %>% 
  group_by(Segment) %>% top_n(1, Road.Code) %>%
  right_join(df_rr_trafficSum, by = 'Segment')

## Merge with road dataset
df_rr <- df_rr %>% full_join(df_rr_trafficSum, 
                             by = c("chainage" = "Chainage.start"))

# Fill in NAs with latest non-NA row (startChainage of road link)
df_rr <- df_rr %>% fill(colnames(df_rr)[which(colnames(df_rr) == "LRP.start") : length(df_rr)])

# Drop the roadRow where nrLanes = 0
df_rr <- df_rr[-which(df_rr$nrLanesW == 0), ]


## ===========================
##  Calculate traffic density  
## ===========================

## Traffic density = AADT / nrLanes (weighted)
df_rr[colnameTraffic] <- df_rr[colnameTraffic] / df_rr$nrLanesW

## =========================================
##  Calculate vulnerability (per road link) 
## =========================================

#BridgeVul(df_rr, df_rr_bridge)
#unique(a$Vulnerability)


## ===============================================================
##  Visualising N1 (or candidate road) Traffic per transport mode 
## ===============================================================



# Create a palette that maps bridge condition factor levels to colors
palBridge <- colorFactor(c("grey", "yellow", "orange", "red"), 
                         domain = c("A", "B", "C", "D"))
# Create a palette that maps road traffic to continuour colors
palRoad <- colorNumeric("GnBu", df_rr[tMode], na.color = NA)

m <- leaflet() %>% addTiles(group = "General map") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB (Default)") %>%
  # Overlay groups - road data
  addCircleMarkers(data = df_rr, ~lon, ~lat,
                   label = ~as.character(lrp),
                   radius = 5,
                   weight = 1,
                   color = 'grey80',
                   fillColor = ~palRoad(Heavy.Truck), # NEED fix - to tMode
                   fillOpacity = 1,
                   group = "Road"
                   ) %>%
  addLegend("bottomright", 
            pal = palRoad, 
            values = df_rr$Heavy.Truck,
            group = "Road",
            title = "Road") %>%
  
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
    baseGroups = c("CartoDB (Default)", "General map"),
    overlayGroups = c("A", "B", "C", "D", "Road"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  ) %>%
  setView(lng = 90.3, lat = 24, zoom = 7)

m

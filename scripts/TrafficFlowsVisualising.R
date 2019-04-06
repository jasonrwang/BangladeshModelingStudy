## Visualising Traffic per transport mode for Assignment 3

# Load library
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
                                                 sep = "-")
                            )

# Drop the roadRow where nrLanes = 0
df_all <- df_all[-which(df_all$nrLanesW == 0), ]


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



##  Calculate traffic density ## 

## Traffic density = trafic flow volumne / nrLanes (weighted)
df_all_t[colnameTraffic] <- df_all_t[colnameTraffic] / df_all_t$nrLanesW


##  Calculate vulnerability (per road link) ##

# Prepare a list of RoadSegment
df_all_v <- df_all_t %>% select(road, chainage, RoadSegment) %>%
  group_by(RoadSegment) %>% top_n(1, chainage) %>% ungroup()

# Get the vulnerability score
# Something in this function is broken :|
df_all_v$Vul <- df_all_v %>% apply(1, CalVulnerbility)  

# Merge dataset back to full road dataset
df_all_vc <- left_join(df_all_t, select(df_all_v, Vul, RoadSegment), by = "RoadSegment")

#unique(df_all_vc$Vul)  # for debugging


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

TrafficVis(cRoad, tMode, df_all_t, df_bridge)

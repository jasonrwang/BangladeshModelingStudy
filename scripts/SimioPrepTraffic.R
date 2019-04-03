## Visualising Traffic per transport mode

# Set working directory
# setwd("D:/EPA/EPA Semester-1.3/EPA1351 Advanced Discrete Simulation/BangladeshModelingStudy")

# Load library
library(dplyr)
library(tidyr)

# Load functions for calculating nrLanes
source("scripts/TrafficFlowsVisualisingFunction.R")

cRoad = "N1"

## Load data and filter by road N1
df_road <- read.csv('data/_roads3.csv', stringsAsFactors = FALSE) %>% filter(road == cRoad)
df_nrLanes <- read.csv('data/Width.csv', stringsAsFactors = FALSE) %>% select(-X) %>% filter(Road == cRoad)
df_traffic <- read.csv('data/Traffic.csv', stringsAsFactors = FALSE) %>% select(-X) %>% filter(Road == cRoad)
df_bridge <- readxl::read_excel('data/BMMS_overview.xlsx') %>% filter(road == cRoad) %>%
                  select(road, chainage, LRPName, lat, lon, name, condition)

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

## Combine transport modes by main categories
# Truck     <= "Heavy.Truck"    "Medium.Truck"   "Small.Truck"
# Bus       <= "Large.Bus"      "Medium.Bus"     "Micro.Bus"
# Car       <= "Utility"        "Car"
# Motorbike <= "Auto.Rickshaw"  "Motor.Cycle"
# Bicycle   <= "Bi.Cycle"       "Cycle.Rickshaw"

df_trafficSumRHS <- df_trafficSumRHS %>% 
  transmute(Truck = Heavy.Truck + Medium.Truck + Small.Truck,
            Bus   = Large.Bus + Medium.Bus + Micro.Bus,
            Car   = Utility + Car,
            Motorbike = Auto.Rickshaw + Motor.Cycle,
            Bicycle = Bi.Cycle + Cycle.Rickshaw,
            RoadSegment = RoadSegment)

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
                              sep = "-"))  %>% ungroup()
# Rename column name
colnames(df_trafficSum)[3] <- "chainage"


## Merge road and traffic data
df <- full_join(df_road, df_trafficSum, by = "chainage") %>% arrange(chainage)

# Fill in the NAs with previous node
# This step is explicitly for a few outlier nodes that 
#   do not match between road and traffic data
df[which(is.na(df$lat)), 
   c("road", "lrp", "lat", "lon", "gap", "type", "name")
   ] <- df[which(is.na(df$lat))-1, 
           c("road", "lrp", "lat", "lon", "gap", "type", "name")]

# Drop duplicated/unnecessary columns
df <- df %>% select(-Road, -LRP.start, -Road.Code, -RoadSegment, -RoadChainage)

# Fill in the Nr Segment
df <- df %>% fill(Segment)

# Fill in zeros for the Traffic NAs
colnameTrafficComb <- c("Truck", "Bus", "Car", "Motorbike", "Bicycle")
df[which(is.na(df$Bus)), colnameTrafficComb] <- 0

## Get weighted nrLanes
df$nrLanesW <- df %>% apply(1, CalnrLanesW) 
df <- df %>% fill(nrLanesW)

# Get traffic density
df[colnameTrafficComb] <- df[colnameTrafficComb] / df$nrLanesW

## Add Bridges

names(df_bridge) <- c("road", "chainage", "lrp", "lat", "lon", "name", "condition")
a <- bind_rows(df, df_bridge) %>% arrange(lrp)

a %>% group_by(lrp) %>% filter
a[which(duplicated(a$chainage)),c("chainage","lrp","name","condition")]
b <- df_bridge[which(duplicated(df_bridge$chainage)),c("chainage","lrp","name","condition")]
head(b %>% group_by(chainage) %>% mutate(match = condition != lead(condition)),20)


## Add a column for destination
df <- df %>% mutate(destination = lag(lrp))


## Write data to csv file
write.csv(df, file = "data/N1-FullRoadTraffic.csv")

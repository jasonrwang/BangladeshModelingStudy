## Visualising Traffic per transport mode

# Set working directory
setwd("D:/EPA/EPA Semester-1.3/EPA1351 Advanced Discrete Simulation/BangladeshModelingStudy")

# Load library
library(dplyr)
library(tidyr)

# Load functions for calculating nrLanes
source("scripts/TrafficFlowsVisualisingFunction.R")

# Define road name
cRoad = "N1"

## Load data and filter by road N1
df_road <- read.csv('data/_roads3.csv', stringsAsFactors = FALSE) %>% filter(road == cRoad)
df_nrLanes <- read.csv('data/Width.csv', stringsAsFactors = FALSE) %>% select(-X) %>% filter(Road == cRoad)
df_traffic <- read.csv('data/Traffic.csv', stringsAsFactors = FALSE) %>% select(-X) %>% filter(Road == cRoad)
df_bridge <- readxl::read_excel('data/BMMS_overview.xlsx') %>% filter(road == cRoad) %>%
  select(road, chainage, LRPName, lat, lon, name, condition) 


## ==================== ##
## Merge bridge dataset ##
## ==================== ##

# Align dataframe structure
df_road <- df_road %>% select(-gap) %>% mutate(condition = "road")

colnames(df_bridge)[which(colnames(df_bridge) == "LRPName")] <- "lrp"
df_bridge <- df_bridge %>% mutate(type = "Bridge") %>% 
  # Sort by condition (worse condition on top) and chainage
  arrange(desc(condition)) %>% arrange(chainage)

# Remove duplicated bridges (keep the bridge with worse condition)
df_bridge <- df_bridge[-which(duplicated(df_bridge$lrp)), ]
df_bridge <- df_bridge[-which(duplicated(df_bridge$chainage)), ]

# To avoid duplicated lrp name after merger of road and bridge dataset
# Add a suffix to bridge lrp name
df_bridge$lrp <- paste(df_bridge$lrp, "_B", sep = "")

## Bind dataframes and sort by chainage
df <- union(df_road, df_bridge) %>% arrange(chainage)

# Manual inspection : check for duplicates
which(duplicated(df$lrp))
which(duplicated(df$chainage))


## ========================== ##
## Merge traffic flow dataset ##
## ========================== ##

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

colnameTrafficComb <- c("Truck", "Bus", "Car", "Motorbike", "Bicycle")

df_trafficSumRHS <- df_trafficSumRHS %>% 
  transmute(Truck = Heavy.Truck + Medium.Truck + Small.Truck,
            Bus   = Large.Bus + Medium.Bus + Micro.Bus,
            Car   = Utility + Car,
            Motorbike = Auto.Rickshaw + Motor.Cycle,
            Bicycle = Bi.Cycle + Cycle.Rickshaw,
            RoadSegment = RoadSegment)

## Calculate the delta traffic volume between regments
df_trafficSumRHS_Delta <- df_trafficSumRHS %>% transmute(Truck     = Truck - lag(Truck),
                                                         Bus       = Bus - lag(Bus),
                                                         Car       = Car - lag(Car),
                                                         Motorbike = Motorbike - lag(Motorbike),
                                                         Bicycle   = Bicycle - lag(Bicycle),
                                                         RoadSegment = RoadSegment)
# Pass the initial traffic volume
df_trafficSumRHS_Delta[1, ] <- df_trafficSumRHS[1, ]

# Assign a small value (0.001) to the rows with zero
df_trafficSumRHS_Delta[which(df_trafficSumRHS_Delta[colnameTrafficComb] == 0), colnameTrafficComb] <- 0.001


# Prepare the LHS-dataset (first chainage dtRow for each link) 
df_trafficSumLHS <- df_traffic %>% 
  group_by(Road, Segment) %>%
  select(Road, LRP.start, Chainage.start, Road.Code, Segment) %>% 
  top_n(1, Road.Code)
df_trafficSumLHS <- df_trafficSumLHS %>% 
  mutate(RoadSegment = paste(as.character(Road), as.character(Segment), sep = "-"))

# Merge the whole traffic datasets
df_trafficSum <- full_join(df_trafficSumLHS, df_trafficSumRHS_Delta, by = 'RoadSegment') %>%
  # Create a RoadChainage column for road-node specification
  mutate(RoadChainage = paste(as.character(Road), 
                              as.character(Chainage.start), 
                              sep = "-"))  %>% ungroup()
# Rename column name
colnames(df_trafficSum)[3] <- "chainage"


## Merge road and traffic data
df <- full_join(df, df_trafficSum, by = "chainage") %>% arrange(chainage)


# Manual inspection : explicitly for two outlier nodes found that 
#   do not match between road and traffic data
which(is.na(df$lat))

# Fill in the NAs with next node and remove duplicates
df[which(is.na(df$lat)), c("road", "lrp", "lat", "lon", "type", "name", "condition")] <- 
  df[which(is.na(df$lat))+1, c("road", "lrp", "lat", "lon", "type", "name", "condition")]
df <- df[-which(duplicated(df$lrp)), ]


# Drop duplicated/unnecessary columns
df <- df %>% select(-Road, -LRP.start, -Road.Code, -RoadSegment, -RoadChainage)

# Fill in the Nr Segment
df <- df %>% fill(Segment)

# Fill in zeros for the Traffic NAs
df[which(is.na(df$Bus)), colnameTrafficComb] <- 0


## ===================== ##
## Calculate nr of Lanes ##
## ===================== ##

## Get the nrLanes
df$nrLanes <- apply(df, 1, function (rrRow) {
  # Return the first TRUE where chainge < EndChainage of nrLanes dataset
  return(df_nrLanes$nrLanes[match(TRUE, df_nrLanes$Chainage.end > as.numeric(rrRow[2]))])
})
# Fill in for the last chainage node
df <- df %>% fill(nrLanes)

# Get traffic density (obsoleted, using traffic volume in Simio instead)
#df[colnameTrafficComb] <- df[colnameTrafficComb] / df$nrLanes


## Add a column for destination
df <- df %>% mutate(destination = lead(lrp))


## Write data to csv file
write.csv(df, file = "data/N1-FullRoadTraffic.csv")

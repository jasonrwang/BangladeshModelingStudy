## The purpose of the R script is to 
##   create a dataset of combined bridges and roads for N1
##   to model trucks travelling from Chittagong to Dhaka in Simio
## 
## The finalised data structure will be
##   road / chainage / lrp / lat / lon / name //
##   condition / type / length / IsBridge / destination

## Load library
library(dplyr)
library(tidyr)

##-----------
# Load Data
##-----------

## Load road data
dfRoad <- read.csv("data/_roads3.csv", stringsAsFactors = FALSE ) %>% 
  # Select subeset road rows on N1
  filter(road == "N1") %>%
  # Remove unnecessary columns
  select(-gap) %>%
  # Assign road to column [condition]
  mutate(condition = "Road") %>%
  # Assign zero to column [length]
  mutate(length = 0) %>%
  # Assign FALSE to column [IsBridge]
  mutate(IsBridge = FALSE)

## Load bridge data
dfBridge <- read.csv("data/BMMS_overview.csv", stringsAsFactors = FALSE ) %>% 
  # Select subset bridges rows on N1
  filter(road == "N1") %>%
  # Drop the bridges that contain NAs
  drop_na() %>%
  # Sort the bridge dataset by LRPName and then chainage
  arrange(LRPName) %>% arrange(chainage) %>%
  # Assign TRUE to column [IsBridge]
  mutate(IsBridge = TRUE) %>%
  # Select columns that are necessary for Simio model
  select(c("road", "chainage", "LRPName", "lat","lon", "type", "name", "condition", "length", "IsBridge"))

colnames(dfBridge) <- colnames(dfRoad) # align the column names

## Merge roads and bridges
dfMerger <- bind_rows(dfRoad, dfBridge) %>%
  arrange(chainage)

## Drop the duplicate rows by their lrp name & chainage
dfMergerX <- dfMerger %>% group_by(lrp) %>% group_by(chainage) %>% 
  # Keep the bridge which is in the best condition, or
  # Keep the bridge and discard the road
  top_n(-1, condition) %>% 
  # If the duplicated bridges have identical category, check length and 
  #    keep the shorter bridge
  top_n(-1, length) %>%
  # Ungroup the dataset
  ungroup()

# Remove all the remaining duplicates having identical chainage & lrp
dfMergerX <- dfMergerX %>% distinct(chainage, lrp, .keep_all = TRUE)

# Double-check on which rows were dropped-off
dfMergerDiff <- setdiff(dfMerger, dfMergerX)

# Rename the deplicated lrp names (resulted from merger of bridge and road)
dfMergerX$lrp <- ifelse(duplicated(dfMergerX$lrp), paste(dfMergerX$lrp, "1", sep = "_"), dfMergerX$lrp)

# Get the subset before chainage = 241.063 km (where Chittagong city area ends)
dfMergerX <- dfMergerX %>% filter(chainage <= 241.063)

# Assign the previous row as the [destination] (since trucks travel from Chittagong)
dfMergerX <- dfMergerX %>% mutate(destination = lag(lrp))

## Write to file
write.csv(dfMergerX, file = "data/MergedN1.csv")

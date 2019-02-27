library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(geosphere)

source('scripts/RoadDataFunctions.R')

## Load road data
dfRoad <- read.csv('data/_roads3.csv',
  stringsAsFactors = FALSE ) %>% filter(road == 'N1') %>%
  select(-name,-gap)

# Save filtered data into its own file for speed in Simio
# write.csv(df1, file = 'data/N1roads.csv')

## Load bridge data

dfBridge <- read.csv('data/BMMS_overview.csv',
  stringsAsFactors = FALSE ) %>% filter(road == 'N1') %>% select(-name)

# Remove unnecessary columns
dfBridge <- dfBridge %>% select(c(
    'LRPName','chainage','condition','lat','lon'
))
names(dfBridge) <- c('lrp','chainage','condition','lat','lon')

# There are some duplicate entries for bridges by condition.
# Take a conservative approach and assume the worse condition.
dfBridge <- dfBridge %>% group_by(lrp)
####### Logic to remove the 'less strict' condition

# Remove duplicates and make a column for the destination node (lag by one)
df <- bind_rows(dfRoad,dfBridge) # This may be a flawed join approach
df <- df %>% #arrange(chainage) %>%
    inner_join(dfRoad,dfBridge,by='lrp',suffix = c('.road','.bridge')) %>%
    mutate(destination = lead(lrp))

df %>% filter(is.na(road.road))

write.csv(df, file = 'data/combinedRoads.csv')


#### Other

# # Is this part necessary?
# df %>% mutate(
#     latDiff = abs(lat.road - lat.bridge)/lat.road,
#     lonDiff = abs(lon.road - lon.bridge)/lon.road,
#     lat = ifelse(latDiff > 0.05, lat.road, lat.bridge) # Default to road data
# )


# # Find all the data with any Bridge-type data
# a <- filter(df1, stringr::str_detect(type,'Bridge'))

# df1 <- df1 %>% mutate(
#     ObjectType = case_when(
#         stringr::str_detect(type,'Bridge') ~ 'TransferNode',
#         TRUE ~ 'BasicNode' # For all other types
#     )
# )

#

VisualizeRoads(df, 'N1')

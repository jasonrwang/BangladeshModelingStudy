library(dplyr)
library(tidyr)

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
    'LRPName','chainage','condition','lat','lon','length'
))
names(dfBridge) <- c('lrp','chainage','condition','lat','lon','length')

# There are some duplicate entries for bridges by condition.
# Take a conservative approach and assume the worse condition.

dfBridge <- dfBridge %>% group_by(lrp) %>% top_n(1,condition)
# There are still some duplicates left here.. and they're not all the same.
# Let's naively sample one (FOR NOW)
dfBridge <- dfBridge %>% sample_n(1) %>% ungroup()# Should end up with 639
# dfBridge %>% top_n(1,condition) %>% filter(n() > 1)

# Remove duplicates and make a column for the destination node (lag by one)
df <- full_join(dfRoad,dfBridge,by='lrp',suffix = c('.road','.bridge')) %>%
    mutate(
        chainage = ifelse(is.na(chainage.road),chainage.bridge,chainage.road),
        lat = ifelse(is.na(lat.road),lat.bridge,lat.road),
        lon = ifelse(is.na(lon.road),lon.bridge,lon.road),
        length = ifelse(is.na(length),0,length)
        ) %>%
    #filter(-chainage)
    arrange(chainage) %>%
    mutate(destination = lead(lrp))


# Note there are coordinate differences
filter(dfRoad, lrp == 'LRP008b')$lon
filter(dfBridge, lrp == 'LRP008b')$lon

write.csv(df, file = 'data/combinedRoadsBridges.csv')


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


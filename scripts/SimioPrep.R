library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(geosphere)

## Load data
df <- read.csv('data/_roads3.csv',
  stringsAsFactors = FALSE ) %>% group_by(road)

df1 <- filter(df, road == 'N1')

# # Find all the data with any Bridge-type data
# a <- filter(df1, stringr::str_detect(type,'Bridge'))
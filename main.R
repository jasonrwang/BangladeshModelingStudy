### Introduction
# The one main file to rule them all

## Load top-level necessary libraries
library(dplyr)
library(tidyr)
library(DBI)
library(RMySQL)
library(leaflet)
library(shiny)

## Connect to MySQL Server
# Parameters
db_name<- 'epa1351'
db_host<-'localhost'
db_port<- 3306
db_user<- 'g3' 
db_password<-'epaRocks4!'
db_table <- 'lab4test'

# Connect to the SQL database
conn <- dbConnect(RMySQL::MySQL(),
                 dbname=db_name,
                 host=db_host,
                 port=db_port,
                 user=db_user,
                 password=db_password)

# # Source (load) all the files in the scripts folder
# file.sources = list.files("scripts/*.R")
# sapply(file.sources, source)

source("scripts/ShinyFunctions.R")

## For assignment 4
# Prompt the user for which scenario they want to run??? (or in Simio)

# # Allow the user to pick a road they want to analyze (N1)
# road <- "N1"
# # Load the road files
# nrRows <- 69 # number of segments in road

# Let the user see all the bridges in the selected road

PCE_Car <- 1
PCE_Bus <- mean(c(3, 3.29, 3.26, 3.17, 3.06))
PCE_Truck <- mean(c(3, 3.27, 3.21, 3.33, 3.41))
PCE_Motorcycle <- mean(c(0.75, 0.59, 0.55, 0.61, 0.54))

dfAll <- read.csv("data/N1-FullRoadTraffic.csv", stringsAsFactors = FALSE) %>% filter(!is.na(lrp)) %>%
            select(lrp, lat, lon, condition, Segment) %>% group_by(Segment)
names(dfAll) <- c("LRP", "lat", "lon", "condition", "segment")

## Bonus 1:
# Prompt the user to 'break down' any of the roads (does this need to be in real-time or can it be at the beginning?)
# brokenBridges <- c("A", "B")

# Write road, bridge, and traffic data into main MySQL table

# Pass the broken bridges to MySQL

# Run Shiny visualization
clearOutputFile()

shinyApp(ui, server)

## Bonus 2: Allow a replay where user can change the speed

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

## Bonus 1:
# Prompt the user to 'break down' any of the roads (does this need to be in real-time or can it be at the beginning?)

brokenBridgeTypes <- c("C","D") # The types of bridges broken down

# Write road, bridge, and traffic data into main MySQL table
# source("scripts/SimioPrepTraffic.R")

# Pass the broken bridges to MySQL

## Run Shiny visualization
# Load known bridges
brokenBridges <- read.csv("data/N1-FullRoadTraffic.csv", stringsAsFactors = FALSE)
brokenBridges <- brokenBridges %>% filter((condition != "road") & (condition %in% brokenBridgeTypes)) %>%
            select(c(lrp,lat,lon, condition))

shinyApp(ui_live, server_live) # Doesn't work for the last hour

## Bonus 2: Allow a replay where user can change the speed
shinyApp(ui_replay, server_replay)

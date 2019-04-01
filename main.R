## Pseudo-code

# Load top-level necessary libraries
library(dplyr)
library(tidyr)

# Source (load) all the files in the scripts folder
file.sources = list.files("scripts/*.R")
sapply(file.sources, source)

## For assignment 4
# Prompt the user for which scenario they want to run??? (or in Simio)

# Allow the user to pick a road they want to analyze (N1)
road = "N1"

# Let the user see all the bridges in the selected road

## Bonus 1:
# Prompt the user to 'break down' any of the roads (does this need to be in real-time or can it be at thte beginning?)

# Write road, bridge, and traffic data into main MySQL table

# Pass the broken bridges to MySQL

## Continuously
for (...) {
# When it knows that something has completed from Simio, refresh the visualization
# e.g. read from a separate table where a value needs to go up

# Read Simio output (from MySQL)

# Visualize that output
}

## Bonus 2: Allow a replay where user can change the speed
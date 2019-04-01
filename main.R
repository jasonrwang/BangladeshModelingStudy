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
road <- "N1"
# nrNodes <- number of nodes in road

# Let the user see all the bridges in the selected road

## Bonus 1:
# Prompt the user to 'break down' any of the roads (does this need to be in real-time or can it be at thte beginning?)

# Write road, bridge, and traffic data into main MySQL table

# Pass the broken bridges to MySQL

## Continuously
SQL_length <- currentTime <- 0
for (... in ...) {
    # Only read/refresh when a new hour's information has been written
    SQL_length_new = # query # Query MySQL for the dimensions of the table

    if (SQL_length_new > SQL_length) {
        # Ensure all information has been written
        if (! SQL_length %% nrNodes ) {
            # Read Simio output (from MySQL)
            # Append only the latest data though!
            # query for rows from currentTime
            currentTime <- currentTime + 1 # Look at the next hour
        }
        SQL_length = SQL_length_new
    }

# Visualize that output
}

## Bonus 2: Allow a replay where user can change the speed
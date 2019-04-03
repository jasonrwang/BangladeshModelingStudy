## Pseudo-code

# Load top-level necessary libraries
library(dplyr)
library(tidyr)
library(RMySQL)
library(DBI)

# Source (load) all the files in the scripts folder
file.sources = list.files("scripts/*.R")
sapply(file.sources, source)

## For assignment 4
# Prompt the user for which scenario they want to run??? (or in Simio)

# Allow the user to pick a road they want to analyze (N1)
road <- "N1"
# Load the road files
nrNodes <- # number of nodes in road

# Let the user see all the bridges in the selected road

## Connect to MySQL Server
# Parameteres
db_name<- 'epagroup3'
db_host<-'xxx.xxx.xxx.xxx'
db_port<- 3306
db_user<- 'epaselma' 
db_password<-'xxx'
db_table <- 'testreadwrite'

# Connect to the SQL database
conn <- dbConnect(RMySQL::MySQL(),
                 dbname=db_name,
                 host=db_host,
                 port=db_port,
                 user=db_user,
                 password=db_password)

## Bonus 1:
# Prompt the user to 'break down' any of the roads (does this need to be in real-time or can it be at thte beginning?)

# Write road, bridge, and traffic data into main MySQL table

# Pass the broken bridges to MySQL

## Continuously
SQL_length <- currentTime <- 0
finish_length <- nrNodes * timePeriods # The MySQL table should end up this long after the simulation

while (SQL_length < finish_length) {
    # Only read/refresh when a new hour's information has been written
    SQL_length_new = dbGetQuery(conn, paste("SELECT COUNT(*) FROM", db_table )) # Query MySQL for the dimensions of the table

    if (SQL_length_new > SQL_length) {
        # Ensure all information has been written
        if (! SQL_length_new %% nrNodes ) {
            # Read Simio output (from MySQL)
            # Append only the latest data though!
            # query for rows from currentTime
            currentTime <- currentTime + 1 # Look at the next hour

            query <- paste("SELECT * FROM", db_table, "WHERE Integer1 =", integer)
            df_new <- dbGetQuery(conn, query)
        }
        SQL_length = SQL_length_new
    }

# Visualize that output (update visualization)
}

## Bonus 2: Allow a replay where user can change the speed

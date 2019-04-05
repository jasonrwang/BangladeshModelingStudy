## Pseudo-code

# Load top-level necessary libraries
library(dplyr)
library(tidyr)
library(RMySQL)
library(DBI)
library(leaflet)
library(shiny)

# Source (load) all the files in the scripts folder
file.sources = list.files("scripts/*.R")
sapply(file.sources, source)

source("ShinyFunctions.R")

## For assignment 4
# Prompt the user for which scenario they want to run??? (or in Simio)

# Allow the user to pick a road they want to analyze (N1)
road <- "N1"
# Load the road files
nrRows <- 69 # number of segments in road

# Let the user see all the bridges in the selected road

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

# # Use sample results
# SimioOutput <- read.csv('data/SimioOutput.csv', stringsAsFactors = FALSE)
# name <- 'lab4test'
# dbWriteTable(conn, name, SimioOutput,overwrite = TRUE)

PCE_Car <- 1
PCE_Bus <- mean(c(3, 3.29, 3.26, 3.17, 3.06))
PCE_Truck <- mean(c(3, 3.27, 3.21, 3.33, 3.41))
PCE_Motorcycle <- mean(c(0.75, 0.59, 0.55, 0.61, 0.54))

dfAll <- read.csv("data/N1-FullRoadTraffic.csv", stringsAsFactors = FALSE) %>% filter(!is.na(lrp)) %>%
            select(lrp, lat, lon, condition, Segment) %>% group_by(Segment)
names(dfAll) <- c("LRP", "lat", "lon", "condition", "segment")

## Bonus 1:
# Prompt the user to 'break down' any of the roads (does this need to be in real-time or can it be at the beginning?)
brokenBridges <- c("A", "B")

# Write road, bridge, and traffic data into main MySQL table

# Pass the broken bridges to MySQL

# Predeclare Shiny stuff
ui <- fluidPage(
    mainPanel(
        leafletOutput(outputId = "N1map")
    ),
    absolutePanel(
        top = 400, left = 20,
        radioButtons("trafficType", h3("Traffic Type"),
                choices = list("PCE" = 1, "Truck" = 2,
                                "Bus" = 3, "Car" = 4,
                                "Motorbike" = 5,
                                "Bicycle" = 6),
                                selected = 1)
    )
)

read_ID <- 0
server <- function(input, output, session) {
    # Create a reactivePoll that updates when the data changes
    # dfDisplay <- reactiveFileReader(1000, session, 'data/RealTimeVis.csv', GetLastHour)
    dfDisplay <- GetLastHour('data/RealTimeVis.csv')

    ## Wait until the file is ready:
    # Wait if empty DB
    while( dbGetQuery(conn, paste("SELECT COUNT(*) FROM", db_table )) == 0 ){
        print("Empty DB")
        next
    }
     # Wait if still first hour
    while (updateSQLtraffic(read_ID) == 0) {
        print("First Hour")
        next
    }
    
    #function to read the DB and write to file if it's ready!

    update_data <- function(read_ID) {
      print("server read_ID")
      print(read_ID)
      read_ID <- updateSQLtraffic(read_ID)
    }
    # update_data(read_ID)

    # Create a palette that maps bridge traffic levels to colors
    palNorm <- colorNumeric(
            c("grey", "yellow", "orange", "red"), domain = c(0,1))

    # Display the data
    output$N1map <- renderLeaflet({
        leaflet() %>%
            setView(lng = 90.4, lat = 24, zoom = 7) %>%
            addProviderTiles(
                providers$CartoDB.Positron,
                options = providerTileOptions(noWrap = TRUE)
            ) %>%
        addCircleMarkers(
            data = dfDisplay, ~lon, ~lat, # Add roads
            label = ~as.character(LRP),
            radius = 5,
            weight = 1,
            color = 'grey80',
            fillColor = ~palNorm(PCE), # Make dynamic
            fillOpacity = 1,
            group = "Road"
        )
        invalidateLater(1000,session)
        print(c("invalidated and update_data():"))
        update_data(read_ID)
    })
}
shinyApp(ui, server)


# observe({
        #     proxy <- leafletProxy("mymap", data = data)
        #     proxy %>% clearMarkers()
        #     if (input$markers) {
        #         proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,
        #                                 label = ~as.character(paste0("Magnitude: ", sep = " ", mag))) %>%
        #         addLegend("bottomright", pal = pal2, values = data$depth_type,
        #                     title = "Depth Type",
        #                     opacity = 1)}
        #     else {
        #         proxy %>% clearMarkers() %>% clearControls()
        #     }
        # })

    # Overlay groups - road data
        # addLegend("bottomright", 
        #             pal = palRoad, 
        #             values = df_rr[[tMode]],
        #             group = "Road",
        #             title = "Traffic density") #%>%

        # Overlay groups - by bridge condition A/B/C/D
        # addCircleMarkers(data = df_rr_bridge, ~lon, ~lat, 
        #                 label = ~as.character(condition), 
        #                 popup = ~as.character(LRPName),
        #                 radius = 4,
        #                 weight = 1,
        #                 color = 'black',
        #                 fillColor = ~palBridge(condition), 
        #                 fillOpacity = ~ifelse(condition == "A", 0.5, 0.9),
        #                 group = ~as.character(condition)
        #                 ) %>% 
        # addLegend("bottomright", 
        #             pal = palBridge, 
        #             values = df_rr_bridge$condition,
        #             group = c("A", "B", "C", "D"),
        #             title = "Bridge condition") %>%
        # addLayersControl(
        #     baseGroups = c("CartoDB (Default)", "Openstreet map"),
        #     overlayGroups = c("A", "B", "C", "D", "Road"),
        #     options = layersControlOptions(collapsed = FALSE),
        #     position = "topleft"
        # )

## Bonus 2: Allow a replay where user can change the speed

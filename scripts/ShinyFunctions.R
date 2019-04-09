# Load data we need for visualization
PCE_Car <- 1
PCE_Bus <- mean(c(3, 3.29, 3.26, 3.17, 3.06))
PCE_Truck <- mean(c(3, 3.27, 3.21, 3.33, 3.41))
PCE_Motorcycle <- mean(c(0.75, 0.59, 0.55, 0.61, 0.54))

dfAll <- read.csv('data/N1-FullRoadTraffic.csv', stringsAsFactors = FALSE)
dfAll <- dfAll %>% select(lrp, lat, lon, condition, Segment)
names(dfAll) <- c("LRP", "lat", "lon", "condition", "segment")

newSQLdata <- function() {
    while (dbGetQuery(conn, paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
        print("newSQLdata: Empty DB")
        return(0)
    }
    # Used in a reactivePoll –> needs to return a new value to signal it to change!
    # Find the latest hour written/this is a fast function
    latest_ID <- dbGetQuery(conn,
            paste("SElECT ID FROM ", db_table," ORDER BY ID DESC LIMIT 1") )
    return(latest_ID)
}

GetLatestHour <- function(filename) {
    while (dbGetQuery(conn, paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
        print("newSQLdata: Empty DB")
    }
    # When the latest_ID changes read in the latest data
    # Find the latest hour written/this is a fast function
    latest_ID <- dbGetQuery(conn,
        paste("SElECT ID FROM ", db_table, " ORDER BY ID DESC LIMIT 1")) 
    read_ID <- latest_ID - 1

    # Fetch the completed data from SQL
    query <- paste("SELECT * FROM", db_table, "WHERE ID =", read_ID)
    SimioOutput <- dbGetQuery(conn, query)
    SimioOutput <- SimioOutput %>% group_by(Time) %>%
            mutate(
                PCE = TrafficTruck * PCE_Truck +
                TrafficBus * PCE_Bus +
                TrafficCar * PCE_Car +
                (TrafficMotorbike + TrafficBicycle) * PCE_Motorcycle
            ) %>%
            mutate(
                # Normalize to that time period:
                PCE = PCE/max(PCE),
                TrafficTruck = TrafficTruck/max(TrafficTruck),
                TrafficBus = TrafficBus/max(TrafficBus),
                TrafficCar = TrafficCar/max(TrafficCar),
                TrafficMotorbike = TrafficMotorbike/max(TrafficMotorbike),
                TrafficBicycle = TrafficBicycle/max(TrafficBicycle)
            )
    dfDisplay <- left_join(dfAll, SimioOutput, by = "LRP") %>%
                    group_by(segment) %>%
                    fill(c("ID", "Time", "TrafficTruck", "TrafficBus",
                            "TrafficCar", "TrafficMotorbike", "TrafficBicycle", "PCE"),
                    .direction = "down")
    
    return(dfDisplay)
}

ui_live <- fluidPage(
    mainPanel(
        leafletOutput(outputId = "N1map")
    ),
    sidebarPanel(
        textOutput("TimeNow"),
        radioButtons("selectedTraffic", h3("Traffic Type"),
                choices = list("PCE" = "PCE", "Truck" = "TrafficTruck",
                                "Bus" = "TrafficBus", "Car" = "TrafficCar",
                                "Motorbike" = "TrafficMotorbike",
                                "Bicycle" = "TrafficBicycle"),
                                selected = "PCE")
    )
)

server_live <- function(input, output, session) {
    # Wait until the file is ready
    while (dbGetQuery(conn,
        paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
        print("newSQLdata: Empty DB")
    }

    # Create a reactivePoll that updates when the data changes
    # newSQLdata returns the latest hour of data
    # GetLatestHour extracts the latest *complete* hour of data once newSQL updates
    dfDisplay <- reactivePoll(1000, session, newSQLdata, GetLatestHour)

    # Create a palette that maps bridge traffic levels to colors
    palNorm <- colorNumeric(
            c("grey", "yellow", "orange", "red"), domain = c(0,1))
    
    # Display the data
    output$N1map <- renderLeaflet({
        leaflet() %>%
        setView(lng = 91.5, lat = 22.1, zoom = 7) %>%
        addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addLegend("bottomright", 
            pal = palNorm, 
            values = seq(0,1,0.1),
            group = "Road",
            title = "Normalized Traffic Volume") %>%
        addLegend("bottomright", 
            pal = colorFactor(c("blue"), domain = "Broken"), 
            values = "Broken",
            group = "Bridge",
            title = "Broken Bridge") %>%
        addLayersControl(
            baseGroups = c("CartoDB (Default)"),
            overlayGroups = c("Bridge", "Road"),
            options = layersControlOptions(collapsed = FALSE),
            position = "bottomleft"
        )
    })
    
    observe({
        while (dbGetQuery(conn,
            paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
            print("observe: Empty DB")
        }
        output$TimeNow <- renderText({
            paste("Time (Hour) Since Beginning of Run:", max(dfDisplay()$Time))
        })
        leafletProxy("N1map") %>%
            clearShapes() %>%
            # Show the road nodes
            addCircleMarkers(
                data = dfDisplay(), ~lon, ~lat,
                label = ~as.character(LRP),
                radius = 5,
                weight = 1,
                color = 'grey0',
                fillColor = ~palNorm(eval(parse(text = input$selectedTraffic))),
                fillOpacity = 1,
                group = "Road"
            ) %>%
            # Let the user see all the bridges in the selected road
            addCircleMarkers(
                data = brokenBridges, ~lon, ~lat,
                label = ~as.character(condition),
                radius = 6,
                weight = 1,
                color = 'blue',
                fillColor = 'blue',
                fillOpacity = 0.1,
                group = "Bridge"
            )            
    })
}

GetAllHours <- function() {
    while (dbGetQuery(conn,
        paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
        print("GetAllHours: Empty DB")
    }

    # Fetch the completed data from SQL
    SimioOutput <- dbReadTable(conn, db_table)
    SimioOutput <- SimioOutput %>% group_by(Time) %>%
            mutate(
                PCE = TrafficTruck * PCE_Truck +
                TrafficBus * PCE_Bus +
                TrafficCar * PCE_Car +
                (TrafficMotorbike + TrafficBicycle) * PCE_Motorcycle
            ) %>%
            mutate(
                # Normalize to that time period:
                PCE = PCE/max(PCE),
                TrafficTruck = TrafficTruck/max(TrafficTruck),
                TrafficBus = TrafficBus/max(TrafficBus),
                TrafficCar = TrafficCar/max(TrafficCar),
                TrafficMotorbike = TrafficMotorbike/max(TrafficMotorbike),
                TrafficBicycle = TrafficBicycle/max(TrafficBicycle)
            )
    # This doesn't accurately do the expansion for all nodes and all times.
    # It instead just keeps the values for the segment head nodes.
    # This could be improved perhaps using an expand.grid() function...
    dfOut <- left_join(dfAll, SimioOutput, by = "LRP") %>%
                    group_by(segment) %>%
                    fill(c("ID", "Time", "TrafficTruck", "TrafficBus",
                            "TrafficCar", "TrafficMotorbike", "TrafficBicycle", "PCE"),
                    .direction = "down")
    
    return(dfOut)
}

ui_replay <- fluidPage(
    mainPanel(
        leafletOutput(outputId = "N1map")
    ),
    sidebarPanel(
        textOutput("TimeNow"),
        radioButtons("selectedTraffic", h3("Traffic Type"),
                choices = list("PCE" = "PCE", "Truck" = "TrafficTruck",
                                "Bus" = "TrafficBus", "Car" = "TrafficCar",
                                "Motorbike" = "TrafficMotorbike",
                                "Bicycle" = "TrafficBicycle"),
                                selected = "PCE"),
        sliderInput("replayspeed", h4("Replay Speed (Seconds per Hour)"),
            min = 0, max = 5, value = 3, step = 0.25)
    )
)


server_replay <- function(input, output, session) {
    # Wait until the file is ready
    while (dbGetQuery(conn,
        paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
        print("SQL Data: Empty DB")
    }

    # GetLatestHour extracts all the data from MySQL (improperly...)
    dfOut <- GetAllHours()
    endTime <- max(dfOut$Time)

    TimeNow <- 0
    update_data <- function() {
        df <- dfOut %>% filter(Time == TimeNow)        
        output$TimeNow <- renderText({
            paste("Time (Hour) Since Beginning of Run:", TimeNow)
        })
        return(df)
    }
    
    # Create a palette that maps bridge traffic levels to colors
    palNorm <- colorNumeric(
            c("grey", "yellow", "orange", "red"), domain = c(0,1))

    # Display the data
    output$N1map <- renderLeaflet({
        leaflet() %>%
        setView(lng = 91.5, lat = 22.1, zoom = 7) %>%
        addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addLegend("bottomright", 
            pal = palNorm, 
            values = seq(0,1,0.1),
            group = "Road",
            title = "Normalized Traffic Volume") %>%
        addLegend("bottomright", 
            pal = colorFactor(c("blue"), domain = "Broken"), 
            values = "Broken",
            group = "Bridge",
            title = "Broken Bridge") %>%
        addLayersControl(
            baseGroups = c("CartoDB (Default)"),
            overlayGroups = c("Bridge", "Road"),
            options = layersControlOptions(collapsed = FALSE),
            position = "bottomleft"
        )
    })
    
    observe({
        while (dbGetQuery(conn,
            paste("SElECT COUNT(*) FROM ", db_table)) == 0) {
            print("observe: Empty DB")
        }
        
        if (TimeNow < endTime){
            TimeNow <<- (TimeNow + 1)
            invalidateLater(input$replayspeed*1000, session)
        } else {
            return()
        }
        leafletProxy("N1map") %>%
            clearShapes() %>%
            # Show the road nodes
            addCircleMarkers(
                data = update_data(), ~lon, ~lat,
                label = ~as.character(LRP),
                radius = 5,
                weight = 1,
                color = 'grey0',
                fillColor = ~palNorm(eval(parse(text = input$selectedTraffic))),
                fillOpacity = 1,
                group = "Road"
            ) %>%
            # Let the user see all the bridges in the selected road
            addCircleMarkers(
                data = brokenBridges, ~lon, ~lat,
                label = ~as.character(condition),
                radius = 6,
                weight = 1,
                color = 'blue',
                fillColor = 'blue',
                fillOpacity = 0.1,
                group = "Bridge"
            )            
    })
}
# Create temporary file to write outputs to
clearOutputFile <- function() {
    outputfile = "LRP,lat,lon,condition,segment,row_names,ID,Time,TrafficTruck,TrafficBus,TrafficCar,TrafficMotorbike,TrafficBicycle,PCE\n"
    cat(outputfile, file = 'data/RealTimeVis.csv', sep = ",")
}

newSQLdata <- function() {
    while (dbGetQuery(conn, "SElECT COUNT(*) FROM lab4test") == 0) {
        print("newSQLdata: Empty DB")
        return(0)
    }
    # Used in a reactivePoll –> needs to return a new value to signal it to change!
    latest_ID <- dbGetQuery(conn, "SElECT ID FROM lab4test ORDER BY ID DESC LIMIT 1") # Find the latest hour written/this is a fast function
    return(latest_ID)
}

GetLatestHour <- function(filename) {
    while (dbGetQuery(conn, "SElECT COUNT(*) FROM lab4test") == 0) {
        print("newSQLdata: Empty DB")
        # return(0)
    }
    # When the latest_ID changes read in the latest data and write it to a CSV
    latest_ID <- dbGetQuery(conn, "SElECT ID FROM lab4test ORDER BY ID DESC LIMIT 1") # Find the latest hour written/this is a fast function
    read_ID <- latest_ID - 1
    print(c("GetLatestHour read_ID:", read_ID))

    # Fetch the completed data from SQL
    query <- paste("SELECT * FROM", db_table,
            "WHERE ID =", read_ID)
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
                        fill(c("row_names","ID", "Time", "TrafficTruck", "TrafficBus",
                                "TrafficCar", "TrafficMotorbike", "TrafficBicycle", "PCE"),
                        .direction = "down")
        # Save outputs into a csv to trigger reactive file reader and to save for replay later
        write.table(dfDisplay, file = 'data/RealTimeVis.csv',  # append doesn't work with write.csv
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
        
        return(dfDisplay)
}

ui <- fluidPage(
    mainPanel(
        leafletOutput(outputId = "N1map")
    ),
    sidebarPanel(
        radioButtons("selectedTraffic", h3("Traffic Type"),
                choices = list("PCE" = "PCE", "Truck" = "TrafficTruck",
                                "Bus" = "TrafficBus", "Car" = "TrafficCar",
                                "Motorbike" = "TrafficMotorbike",
                                "Bicycle" = "TrafficBicycle"),
                                selected = "PCE")
    ),
    fluidRow(
        top = 500, left = 40,
        textOutput("trafficType")
    )
)

server <- function(input, output, session) {
    # Wait until the file is ready
    while (dbGetQuery(conn, "SElECT COUNT(*) FROM lab4test") == 0) {
        print("newSQLdata: Empty DB")
        # return(0)
    }

    # Create a reactivePoll that updates when the data changes
    # newSQLdata returns the latest hour of data
    # GetLatestHour extracts the latest *complete* hour of data once newSQL updates
    dfDisplay <- reactivePoll(1000, session, newSQLdata, GetLatestHour)

    # Create a palette that maps bridge traffic levels to colors
    palNorm <- colorNumeric(
            c("grey", "yellow", "orange", "red"), domain = c(0,1))

    output$trafficType <- renderText({
        paste(input$selectedTraffic)
    })

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
            title = "Normalized Traffic Volume")
    })
    
    observe({
        while (dbGetQuery(conn, "SElECT COUNT(*) FROM lab4test") == 0) {
            print("observe: Empty DB")
        }
        leafletProxy("N1map", data = dfDisplay()) %>%
            clearShapes() %>%
            addCircleMarkers(
                label = ~as.character(LRP),
                radius = 5,
                weight = 1,
                color = 'grey80',
                fillColor = ~palNorm(PCE),
                fillOpacity = 1,
                group = "Road"
            )
    })
}
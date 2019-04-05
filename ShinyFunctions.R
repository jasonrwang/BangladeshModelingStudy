# Create temporary file to write outputs to
outputfile = "LRP,lat,lon,condition,segment,row_names,ID,Time,TrafficTruck,TrafficBus,TrafficCar,TrafficMotorbike,TrafficBicycle,PCE\n"
cat(outputfile, file = 'data/RealTimeVis.csv', sep = ",")

GetLastHour <- function(filename) {
    dfDisplay <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
    latest_hour <- max(dfDisplay$Time)
    dfDisplay <- dfDisplay  %>% filter(Time == latest_hour)
    return(dfDisplay)
}

a <- GetLastHour('data/RealTimeVis.csv')

updateSQLtraffic <- function(read_ID) {
    # Read Simio output (from MySQL)
    # Append only the latest data though!

    # Don't read the same hour multiple times
    latest_ID <- dbGetQuery(conn, "SElECT ID FROM lab4test ORDER BY ID DESC LIMIT 1") # Find the latest hour written/this is a fast function
    print(c("Comparison read_ID", read_ID))
    if (latest_ID - read_ID < 2) {
        return(read_ID)
        # Act only once the last hour is completed writing to MySQL!
        # SQL_length <- dbGetQuery(conn, paste("SELECT COUNT(*) FROM", db_table )) # Query MySQL for the dimensions of the table
    } else {
        print("TIME TO WRITE")
        print(c("latest_ID", latest_ID))
        print(c("read_ID", read_ID))
        read_ID <- latest_ID - 1
        print(c("new read_ID", read_ID))
    
        query <- paste("SELECT * FROM", db_table,
            "WHERE ID =", read_ID)
        SimioOutput <- dbGetQuery(conn, query)
        # Flush results
        if (length(dbListResults(conn)) > 0) {
            dbClearResult(dbListResults(conn)[[1]])
        }
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
        print(c("returned read_ID", read_ID))
        return(read_ID)
    }
}


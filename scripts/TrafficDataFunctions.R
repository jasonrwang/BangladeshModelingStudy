library(rvest)
library(dplyr)
library(tidyr)

ScrapeTraffic <- function(file) {
    # Load the HTML page
    trafficPage <- read_html(file, trim = TRUE)

    # Select the table that we actually want (the fifth <table> element)
    df <- html_table(html_nodes(trafficPage, "table")[[5]],
        header = FALSE, fill = TRUE)
    
    # Initial cleaning
    df <- df %>% slice(-c(0,1,2,3)) %>% # Remove the unnecessary rows
        # Take the road name and separate it into a more readable version
        separate(X1, c("Road", "Road2"), sep = "-") %>%
        mutate(
            Segment = unlist(strsplit(Road2, "[[:alpha:]]")),
            Side = ifelse(grepl("[[:alpha:]]", Road2),
                gsub("[[:digit:]]", "", Road2), "NA")
        ) %>%
        select(-c(Road2, X26)) # Road2 is temporary, X26 is a duplicate (AADT)

    names(df) <- c("Road", "Description",
        "LRP.start", "Offset.start", "Chainage.start",
        "LRP.end", "Offset.end", "Chainage.end",
        "Length (km)", "Heavy Truck", "Medium Truck", "Small Truck",
        "Large Bus", "Medium Bus", "Micro Bus", "Utility", "Car",
        "Auto Rickshaw", "Motor Cycle", "Bi-Cycle", "Cycle Rickshaw",
        "Cart", "Motorized", "Non Motorized", "Total AADT", "Segment", "Side")

    # PCE Values, with the first one given by the MoC
    # Others from Gaji, M. (2018) https://www.iccesd.com/proc_2018/Papers/r_p4854.pdf
    PCE_Car <- 1
    PCE_Pickup <- PCE_Car
    PCE_Bus <- mean(c(3, 3.29, 3.26, 3.17, 3.06))
    PCE_BusMini <- mean(c(3, 2.39, 2.28, 2.47, 2.32))
    PCE_Truck <- mean(c(3, 3.27, 3.21, 3.33, 3.41))
    PCE_TruckMed <- mean(c(3, 2.27, 2.11, 2.31, 2.19))
    PCE_CNG <- mean(c(3, 2.27, 2.11, 2.31, 2.19))
    PCE_Motorcycle <- mean(c(0.75, 0.59, 0.55, 0.61, 0.54))

    # Now calculate PCEs (note columns originally in string data type)
    df <- df %>% mutate(
        PCE = (
            as.numeric(`Heavy Truck`) * PCE_Truck +
            as.numeric(`Medium Truck`) * PCE_TruckMed +
            as.numeric(`Small Truck`) * PCE_Pickup +
            as.numeric(`Large Bus`) * PCE_Bus +
            as.numeric(`Medium Bus`) * PCE_BusMini +
            as.numeric(`Micro Bus`) * PCE_BusMini +
            as.numeric(Utility) * PCE_Pickup +
            as.numeric(Car) * 1 +
            as.numeric(`Auto Rickshaw`) * PCE_Motorcycle +
            as.numeric(`Motor Cycle`) * PCE_Motorcycle +
            as.numeric(`Bi-Cycle`) * PCE_Motorcycle +
            as.numeric(`Cycle Rickshaw`) * PCE_Motorcycle +
            as.numeric(`Cart`) * PCE_Motorcycle
        )
    )

    return(df)
}

ScrapeNrLanes <- function(file) {
  # 
}
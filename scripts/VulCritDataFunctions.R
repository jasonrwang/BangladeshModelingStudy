# We need rvest to scrape HTML files
library(rvest) 

## Read in files with data about road width and the number of lanes
importWidth <- function(file) {
    df <- read.table(file,header = TRUE, sep = "\t")
    names(df) <- c("Road", "roadID", "Chainage.start", "Chainage.end", "width", "nrLanes")
    return(df)
}

## Scrape Ministry HTML files with traffic counts for Bangladesh's roads
ScrapeTraffic <- function(file) {
    # Load the HTML page
    trafficPage <- read_html(file, trim = TRUE, options = c("NOERROR", "NOBLANKS"))
    # trafficPage <- read_html("data/traffic/N203.traffic.htm",trim = TRUE, options = c("NOERROR", "NOBLANKS"))
    # Select the table that we actually want (the fifth <table> element)
    df <- html_table(html_nodes(trafficPage, "table")[[5]],
        header = FALSE, fill = TRUE)
    
    # Initial cleaning
    df <- df %>% slice(-c(0,1,2,3)) %>% # Remove the unnecessary rows
        # Take the road name and separate it into a more readable version (but keep it)
        mutate(
            RoadCode = X1
        ) %>%
        separate(X1, c("Road", "Road2"), sep = "-") %>%
        mutate(
            Segment = unlist(strsplit(Road2, "[[:alpha:]]")),
            Side = ifelse(grepl("[[:alpha:]]", Road2),
                gsub("[[:digit:]]", "", Road2), "NA")
        ) %>%
        select(-c(Road2, X26)) # Road2 is temporary, X26 is a duplicate (AADT)
    
    # Some files are completely empty. Skip these!
    if (!nrow(df)) return(NULL)

    names(df) <- c("Road", "Description",
        "LRP.start", "Offset.start", "Chainage.start",
        "LRP.end", "Offset.end", "Chainage.end",
        "Length (km)", "Heavy Truck", "Medium Truck", "Small Truck",
        "Large Bus", "Medium Bus", "Micro Bus", "Utility", "Car",
        "Auto Rickshaw", "Motor Cycle", "Bi-Cycle", "Cycle Rickshaw",
        "Cart", "Motorized", "Non Motorized", "Total AADT",
        "Road Code", "Segment", "Side")

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

## Calculate weighted nrLanes for roadRow data
nrLanesW <- function(rrRow) {
  # Create an empty vector for storing target indice of nrLanes dataset
  id <- c()
  
  # Get row index for input road row data
  jj <- which(!is.na(match(df_rr$chainage, as.numeric(rrRow[2]))))
  
  # Calculate the chainage values of current and next node
  rr1 <- as.numeric(df_rr[jj  , 2])
  rr2 <- as.numeric(df_rr[jj+1, 2])
  #print(c(rr1, rr2))   # for debugging
  
  # Compare the chainage values with that of nrLanes dataset
  for (ii in 1:nrow(df_rr_nrLanes)) {
    # If looped to the last row
    if (is.na(as.numeric(df_rr_nrLanes$Chainage.start[ii+1]))) {
      id <- c(id, ii)
      break
    }
    # Otherwise, start finding targeted index ii
    if (rr1 < as.numeric(df_rr_nrLanes$Chainage.start[ii+1])) {
      # Link startChainage >= startChainage of nrLanesRow at index = ii
      # Hence the index = ii is within scope
      id <- c(id, ii)  # add the index to target vector
      
      # Check the chainage of next node
      if (rr2 < as.numeric(df_rr_nrLanes$Chainage.end[ii])) {
        # Link endChaninage <= endchainage of nrLanesRow at index = ii
        # Type-2 Link
        #print(c(rr1, rr2))          # for debugging
        #print(df_rr_nrLanes[id, ])  # for debugging
        break
      } else if (rr2 < as.numeric(df_rr_nrLanes$Chainage.end[ii+1])) {
        # Link endChaninage <= endchainage of nrLanesRow at index = ii+1
        # Type-1a or -1b Link
        id <- c(id, ii+1)  # add the index to target vector
        #print(c(rr1, rr2))          # for debugging
        #print(df_rr_nrLanes[id, ])  # for debugging
        break
      }
    }
    ii =+ 1
  }
  # Found the target nrLanes sub-dataset for input road link
  rr_nrLanes <- df_rr_nrLanes[id, ]
  
  # Replace the very start and end chainage values
  rr_nrLanes[1, 3] <- rr1
  rr_nrLanes[nrow(rr_nrLanes), 4] <- rr2
  #print(c(rr1, rr2)) # for debugging
  #print(rr_nrLanes)  # for debugging
  
  # Calculate weighted nrLanes as sumproduct
  rr_nrLanes <- rr_nrLanes %>% 
    transmute(nrLanesW = (Chainage.end - Chainage.start) * nrLanes) %>%
    as.vector() %>% sum() %>% as.numeric() / (rr2 - rr1)
  
  # Return weighted nrLanes
  return(rr_nrLanes)
}

## Calculate Vulnerability of road links based on the bridges in the link

BridgeVul <- function(df, df_bridge) {
  # Takes in a road df and a bridge df, and finds the vul of all bridges in each road link
  # Best used with df_rr and df_rr_bridge
  df <- df %>% mutate(Vulnerability = NA)
  # Assumes input of df that is already grouped by road or is just for one road
  for (link in 1:(nrow(df)-1)) {
    df_bridge_subset = filter(df_bridge,
      (chainage >= df[link,]$chainage) & (chainage < df[link+1,]$chainage) )$condition
    
    # Sometimes, there are no bridges
    if (!(length(df_bridge_subset))){
      df[link,]$Vulnerability = 0
    } else {
      df[link,]$Vulnerability = sum(sapply(df_bridge_subset, BridgeRating))
    }
  }
  return(df)
}

BridgeRating <- function(Code) {
  # Takes in a bridge condition code and returns a number
  # This is so-far arbitrary, but assigns a vulnerability score!
  if (Code == "A"){
    return(0)
  } else if (Code == "B") {
    return(1)
  } else if (Code == "C") {
    return(2)
  } else if (Code == "D") {
    return(3)
  } else {
    return("NA")
  }
}

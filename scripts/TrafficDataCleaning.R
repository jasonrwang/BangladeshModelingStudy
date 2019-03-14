library(rvest)

# Load the HTML page
trafficPage <- read_html("data/N1.traffic.htm", trim = TRUE)

# Select the table that we actually want (the fifth <table> element)
df <- html_table(html_nodes(trafficPage, "table")[[5]],
    header = FALSE, fill = TRUE)

# Initial cleaning
df <- df %>% slice(-c(0,1,2,3)) %>% # Remove the unnecessary rows
    # Take the road name and separate it into a more readable version
    separate(X1, c("Road", "Road2"), sep = "-") %>%
    mutate(
        Segment = strsplit(Road2, "[[:alpha:]]"),
        Side = ifelse(grepl("[[:alpha:]]", Road2),
            gsub("[[:digit:]]", "", Road2), "NA"),
    ) %>%
    select(-c(Road2, X26)) # Road2 is temporary, X26 is a duplicate (AADT)

names(df) <- c("Road", "Description",
    "LRP.start", "Offset.start", "Chainage.start",
    "LRP.end", "Offset.end", "Chainage.end",
    "Length (km)", "Heavy Truck", "Medium Truck", "Small Truck",
    "Large Bus", "Medium Bus", "Micro Bus", "Utility", "Car",
    "Auto Rickshaw", "Motor Cycle", "Bi-Cycle", "Cycle Rickshaw",
    "Cart", "Motorized", "Non Motorized", "Total AADT", "Segment", "Side")

library(rvest)

# Load the HTML page
trafficPage <- read_html("data/N1.traffic.htm", trim = TRUE)

# Select the table that we actually want (the fifth <table> element)
df <- html_table(html_nodes(trafficPage, "table")[[5]],
    header = FALSE, fill = TRUE)

# Remove the unnecessary rows at the beginning
df <- df %>% slice(-c(0,1,2))

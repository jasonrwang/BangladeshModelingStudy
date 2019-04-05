library(RMySQL)
library(DBI)

## Connect to MySQL Server
# Parameters
db_name<- 'epa1351'
db_host<-'localhost'
db_port<- 3306
db_user<- 'g3' 
db_password<-'epaRocks4!'
db_table <- 'lab4'

# Connect to the SQL database
conn <- dbConnect(RMySQL::MySQL(),
                 dbname=db_name,
                 host=db_host,
                 port=db_port,
                 user=db_user,
                 password=db_password)

# Write sample results
SimioHsinOutput <- read.csv('data/SimioOutput.csv', stringsAsFactors = FALSE)

name <- 'lab4test'

# Clear Table
dbWriteTable(conn, name, data.frame(NULL), overwrite = TRUE)

for (row in seq(1, dim(SimioHsinOutput)[1])) {
    if (row == 1){
        dbWriteTable(conn, name, SimioHsinOutput[row,], overwrite = TRUE)
    } else{
        dbWriteTable(conn, name, SimioHsinOutput[row,], append = TRUE)
    }
    Sys.sleep(0.1)
}
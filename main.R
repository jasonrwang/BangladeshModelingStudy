### General dependencies – load before running any other scripts
library(dplyr)
library(tidyr)

# Source (load) all the files in the scripts folder
file.sources = list.files("scripts/*.R")
sapply(file.sources, source)
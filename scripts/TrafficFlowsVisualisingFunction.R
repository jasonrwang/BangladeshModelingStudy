# Load library
library(dplyr)
library(tidyr)

# ==================================
#  FUNCTIONS ON NODE <-> LINK LEVEL 
# ==================================

## Calcualte weighted nrLanes for roadRow data

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
      if (rr2 <= as.numeric(df_rr_nrLanes$Chainage.end[ii])) {
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

# =====================================
#  FUNCTIONS ON LINK <-> NETWORK LEVEL 
# =====================================



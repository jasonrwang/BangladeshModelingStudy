# ===========
#  FUNCTIONS 
# ===========

## Calculate weighted nrLanes for roadRow data

CalnrLanesW <- function(rrRow) {
  #print(rrRow[c("road", "chainage")])  # for debugging
  # Create an empty vector for storing target indice of nrLanes dataset
  id <- c()
  
  # Get the data subset for the corresponding road
  cRoad <- as.character(rrRow[1])
  
  df_rr <- df %>% filter(road == cRoad)
  df_rr_nrLanes <- df_nrLanes %>% filter(Road == cRoad)
  
  # Get row index for input road row data
  jj <- which(!is.na(match(df_rr$chainage, as.numeric(rrRow[2]))))
  
  # Calculate the chainage values of current and next node
  rr1 <- as.numeric(df_rr[jj  , 2])
  rr2 <- as.numeric(df_rr[jj+1, 2])
  #print(c(rr1, rr2))   # for debugging
  
  # Compare the chainage values with that of nrLanes dataset
  for (ii in 1:nrow(df_rr_nrLanes)) {
    # If looped to the last row
    if (is.na(as.numeric(df_rr_nrLanes$Chainage.start[ii+1])) || is.na(rr2)) {
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
  
  # When the link has no corresponding nrLanes due to gap between roads
  # the weighted nrLanes goes negative
  rr_nrLanes <- max(rr_nrLanes, 0)
  
  # Return weighted nrLanes
  #print(rr_nrLanes)  # for debugging
  return(rr_nrLanes)
}


## Calculate Vulnerability of road links based on the bridges in the link

CalVulnerbility <- function(rrRow) {
  #print(rrRow[c("road", "chainage")])  # for debugging
  # Create empty vulnerability value sumVul = 0
  sumVul <- 0
  
  # Get the data subset for the corresponding road / link
  cRoad <- as.character(rrRow[1])
  cRoadLink <- as.character(rrRow[3])
  
  df_rrLink <- df_all_t %>% 
    filter(RoadSegment == cRoadLink) %>%
    select(road, chainage, RoadSegment) %>%
    arrange(chainage)
  df_rr_bridge <- df_bridge %>% filter(road == cRoad)
  
  # Calculate the chainage values of first and last node
  rr1 <- as.numeric(df_rrLink[1, 2])
  rr2 <- as.numeric(df_rrLink[nrow(df_rrLink), 2])
  #print(c(cRoadLink, "chainage", rr1, rr2))   # for debugging
  
  if (is.na(rr2)) {
    # This is the endChainage of the road
    next
  } 
  else if (nrow(df_rr_bridge) == 0) {
    # There is no bridge in the road
    next
  } else {
    # Compare the chainage values with that of bridge dataset
    rr1ii <- which.max(df_rr_bridge$chainage >= rr1)
    rr2ii <- which.min(df_rr_bridge$chainage <= rr2)
    #print(c("ii = ", rr1ii, rr2ii, 
    #        df_rr_bridge$chainage[rr1ii],
    #        df_rr_bridge$chainage[rr2ii]))   # for debugging
    
    if (is.null(rr1ii) || is.null(rr2ii)) {
      # No bridges found within the road segment
      #print("GOES LOOP-1")  # for debugging
    } else if (rr1ii == rr2ii) {
      # No bridges found within the road segment
      #print("GOES LOOP-2")  # for debugging
    } else {
      # Get the bridge condition values
      #print("GOES LOOP-3")  # for debugging
      inbridge <- df_rr_bridge$condition[rr1ii:(rr2ii - 1)]
      
      # Replace the bridge condition with assigned scores
      inbridge[which(inbridge == "A")] <- 1
      inbridge[which(inbridge == "B")] <- 2
      inbridge[which(inbridge == "C")] <- 3
      inbridge[which(inbridge == "D")] <- 4
      
      sumVul <- sum(as.numeric(inbridge))
      
      #print(c("inbridge: ", inbridge)) # for debugging
    }
  }
  
  # Return bridge score
  #print(c(cRoadLink, sumVul))  # for debugging
  return(sumVul)
}

TrafficVis <- function(cRoad, tMode, df_all_t, df_bridge) {
  ## Subset the data for candidate road ##
  df_rr <- df_all_t %>% filter(road == cRoad) 
  df_rr_bridge <- df_bridge %>% filter(road == cRoad)

  # Create a palette that maps bridge condition factor levels to colors
  palBridge <- colorFactor(c("grey", "yellow", "orange", "red"), 
                          domain = c("A", "B", "C", "D"))
  # Create a palette that maps road traffic to continuour colors
  palRoad <- colorNumeric("GnBu", df_rr[tMode], na.color = NA)

  m <- leaflet() %>% addTiles(group = "Openstreet map") %>% 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB (Default)") %>%
    # Overlay groups - road data
    addCircleMarkers(data = df_rr, ~lon, ~lat,
                    label = ~as.character(lrp),
                    radius = 5,
                    weight = 1,
                    color = 'grey80',
                    fillColor = ~palRoad(df_rr[[tMode]]),
                    fillOpacity = 1,
                    group = "Road"
                    ) %>%
    addLegend("bottomright", 
              pal = palRoad, 
              values = df_rr[[tMode]],
              group = "Road",
              title = "Traffic density") %>%
    
    # Overlay groups - by bridge condition A/B/C/D
    addCircleMarkers(data = df_rr_bridge, ~lon, ~lat, 
                    label = ~as.character(condition), 
                    popup = ~as.character(LRPName),
                    radius = 4,
                    weight = 1,
                    color = 'black',
                    fillColor = ~palBridge(condition), 
                    fillOpacity = ~ifelse(condition == "A", 0.5, 0.9),
                    group = ~as.character(condition)
                    ) %>% 
    addLegend("bottomright", 
              pal = palBridge, 
              values = df_rr_bridge$condition,
              group = c("A", "B", "C", "D"),
              title = "Bridge condition") %>%
    addLayersControl(
      baseGroups = c("CartoDB (Default)", "Openstreet map"),
      overlayGroups = c("A", "B", "C", "D", "Road"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topleft"
    ) %>%
    setView(lng = 90.3, lat = 24, zoom = 7)

  m

  ## ===========================================
  ##  Visualising criticality and vulnerability  
  ## ===========================================

  ## Criticality ##

  df_Criticality <- df_all_vc %>% 
    select(RoadSegment, PCE) %>% 
    arrange(desc(PCE)) %>%
    distinct()

  df_all_t_top10 <- df_all_t %>% semi_join(df_Criticality[1:10, ], by = "RoadSegment")

  ## Plot all criticality (filtered)
  get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                        right = 92.8135,    top = 26.7182), 
                zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
    geom_point(data = filter(df_all_t, PCE > 10000), aes(x = lon, y = lat, colour = PCE)) +
    scale_colour_gradient(name = "Weighed traffic density (ppl/yr)", guide = 'colorbar', low = 'pink', high = 'red') +
    ggtitle(paste('Criticality per road segment (> 10,000 ppl/yr)'))

  ## Plot top-10 criticality
  get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5845, 
                        right = 92.8135,    top = 26.7182), 
                zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
    geom_point(data = df_all_t_top10, aes(x = lon, y = lat, colour = PCE)) +
    scale_colour_gradient(name = "Weighed traffic density (ppl/yr)", guide = 'colorbar', low = 'pink', high = 'red') +
    ggtitle(paste('Top-10 most critical road segments'))

  ## Zoom-in plot
  get_stamenmap(bbox = c(left  = 89.4, bottom = 23.0, 
                        right = 91.6,    top = 24.5), 
                zoom = 8, maptype = "terrain", color = 'bw') %>% ggmap() +
    geom_point(data = df_all_t_top10, aes(x = lon, y = lat, colour = PCE)) +
    scale_colour_gradient(name = "Weighed traffic density (ppl/yr)", guide = 'colorbar', low = 'pink', high = 'red') +
    ggtitle(paste('Top-10 most critical road segments (zoom-in)'))

  # Top-10 most critical : bar plot
  ggplot(data = top_n(df_Criticality, 10, PCE), aes(x = reorder(RoadSegment, -PCE), y = PCE)) +
    geom_bar(stat="identity", width = 0.5) +
    xlab("Road segment name") +
    ylab("PCE-weighted traffic volume (ppl/day)") +
    ggtitle("Criticality per road segment") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  ## Vulnerability ##

  df_vulnerability <- df_all_vc %>%
    select(RoadSegment, Vul) %>% 
    arrange(desc(Vul)) %>%
    distinct()

  df_all_t_top10v <- df_all_vc %>% semi_join(df_vulnerability[1:10, ], by = "RoadSegment")

  ## Plot all vulnerability (filtered)
  get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5545, 
                        right = 92.8135,    top = 26.7182), 
                zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
    geom_point(data = filter(df_all_vc, Vul > 20), aes(x = lon, y = lat, colour = Vul)) +
    scale_colour_gradient(name = "Vulnerability", guide = 'colorbar', low = 'pink', high = 'red') +
    ggtitle(paste('Vulnerability per road segment (> 20 vul score)'))

  ## Plot top-10 vulnerability
  get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.45, 
                        right = 92.8135,    top = 26.7182), 
                zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
    geom_point(data = df_all_t_top10v, aes(x = lon, y = lat, colour = Vul)) +
    scale_colour_gradient(name = "Vulnerability", guide = 'colorbar', low = 'pink', high = 'red') +
    ggtitle(paste('Top-10 most vulnerable road segments'))

  # Filter first by criticality then plot vulnerability
  get_stamenmap(bbox = c(left  = 87.8075, bottom = 20.5545, 
                        right = 92.8135,    top = 26.7182), 
                zoom = 7, maptype = "terrain", color = 'bw') %>% ggmap() +
    geom_point(data = filter(df_all_vc, PCE > 3000, Vul > 20), aes(x = lon, y = lat, colour = Vul)) +
    scale_colour_gradient(name = "Vulnerability", guide = 'colorbar', low = 'pink', high = 'red') +
    ggtitle(paste('Vulnerability per road segment \n(filtered traffic > 3000 ppl/day & vul score > 20)'))

  # Top-10 most critical : bar plot
  ggplot(data = top_n(df_vulnerability, 10, Vul), aes(x = reorder(RoadSegment, -Vul), y = Vul)) +
    geom_bar(stat="identity", width = 0.5) +
    xlab("Road segment name") +
    ylab("Condition-weighted bridge counts (# bridge * score)") +
    ggtitle("Vulnerability per road segment") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}
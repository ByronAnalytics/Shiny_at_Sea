setDepthReq <- function(stationID, stationTable = NULL){
  #############################################################################
  # DESCRIPTION:
  #   Each station has a minimum depth required for ctd sampling to be valide. 
  #   this uses a lookup table to match the station id and return the minimum 
  #   depth needed for a valid cast.
  # INPUTS:
  #   stationID - station id to be matched to station table
  #   stationTable - dataframe of station ids, if null, default table in data folder
  #                  is used.
  # OUTPUTS:
  #  minDepth - value to be plotted on output graphs showing the min depths.
  ##########################################################################
  
  # Verify stationTable object, setup stations lookup table
  if(is.null(stationTable)){
    stations <- read.csv("Data/Station_LocationDepth.csv")
  } else if(is.list(stationTable)){
    stations <- stationTable
  } else if(is.vector(stationTable)){
    stations <- as.factor("Station" = names(stationTable), "Depth" = stationTable)
  } else stop(paste("stationTable object is", 
                    typeof(stationTable), 
                    ". setDepthReq requires list or named vector", sep = ""))
  
  stationData <- stations[match(stationID, stations$Station),]
  if(is.na(stationData$Depth)){
    warning("Station not found - Depth Set to 1")
    stationData$Depth <- 1
  }
  
  return(c("minDepth" = stationData$Depth))
}
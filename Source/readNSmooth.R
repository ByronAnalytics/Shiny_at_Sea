
readNSmooth <- function(FILE){
  #############################################################################
  # DESCRIPTION:
  #   This function takes in data exported by seabird and traditionally read 
  #   into IGODS using the jibbily_goo function.Then smooths and pick out  
  #   outliers for each relevent CDT parameter
  # EXPECTED FORMAT:
  #   Anticipating a data file with metadata header. "# name =" id's data col
  #   metatdata header ends with "*END*"  
  # INPUTS:
  #   file name and path.
  # RETURNS: 
  #   a list of lists
  # SUMMARY:
  #   1) takes in data exported by seabird using jibbily_goo
  #   2) standardizes columns and column names
  #   3) melts dataframe
  #   4) splits data frame by parameter type to a list of dataframes
  #   5) runs list via lappy through weighted.smoothing and markOutliers functions
  #   6) reteurns a list of lists - 3 data frames for each parameter:
  #      Keep (valid data points), Discard (outliers), model (all original data) 
  #      also returns station, a character value identifying the station the data is from
  # DEPENDS ON:
  # oce, reshape2, strinr
  #   * NOTE: For perfomance, use library::function()
  #############################################################################
  DATA <- oce::read.oce(FILE)
  
  #getting station number from cast metadata
  filePathSplit <- stringr::str_split(DATA@metadata$hexfilename, "\\\\")
  filePathSplitStation <- filePathSplit[[1]][length(filePathSplit[[1]])]
  station <- toupper(substr(filePathSplitStation, 1, nchar(filePathSplitStation)-4)) #also returning this value later

  #getting in date from cast metadata
  DATE <- substr(as.character(DATA@metadata$systemUploadTime), 1,10)
  
  
  #making dataframe out of data list from ctd object
  DATA <- as.data.frame(DATA@data)
    # DATA$station <- station
    # DATA$date <- DATE
  # makind dummy CDOM column for old kelp CTD without CDOM sensor
  if (!(any(colnames(DATA) %in% c("cdom.", "fluorescence.2")))){
    DATA$CDOM <- -1
  }
  
  #getting rid of not valid data (bad decent reate or sample flag)
  DATA <- DATA[(DATA$descent >= 0.249) & !is.na(DATA$flag) ,]  
  
  # invert Depth  
  DATA$depth <- -DATA$depth
  
  # Rename Select sensors  
  # TODO: OCE Column names formating changed, re-do later.
  if(any(names(DATA) == "cdom.")) {
    names(DATA)[names(DATA) == "cdom."] <- "CDOM"
  } else if(any(names(DATA) == "fluorescence.2")){
    names(DATA)[names(DATA) == "fluorescence.2"] <- "CDOM"
  }
  names(DATA)[names(DATA) == "beamTransmission"] <- "xms"
  names(DATA)[names(DATA) == "fluorescence"] <- "fluorometry"
  names(DATA)[names(DATA) == "oxygen3"] <- "dox"
  
  # Convert list of vectors to list of df with depth, 'y' = sensor value
  DATA <- lapply(DATA, function(value, depth){data.frame(depth, value)}, DATA$depth)
  print(names(DATA))
  # Select Main WQ Parameters
   ctdSplit <- DATA[c("temperature", "salinity", "pH", "xms", "fluorometry",
                         "dox", "CDOM")]

  # ctdSplit <- DATA  
  #running smoothing function
  smoothList <- lapply(ctdSplit, WeightedSmoothing, 10)
  
  #getting outliers, creates a list of lists, one for each parameter
  smoothOutlierList <- lapply(smoothList, markOutliers, Threshold = 1.75)
  
  out<- list(station,smoothOutlierList, DATE)
  names(out)<- c("station","data", "date")
  return(out)
}

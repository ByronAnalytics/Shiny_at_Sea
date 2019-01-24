#############################################################################
# DESCRIPTION:
#   Generate overview plots with original kept/excluded values, bined values
#   and icon for empty bins
# INPUTS:
#  keepObs, excludeObs - dataFrames with depth as 'x' and observation as 'y'
#  plotLabel - Character string passed to x-lab and title
#  maxDepth - Depth requirement for a 'good station'
#  station - Character string of with station ID
# OUTPUTS:
#  Returns a ggplot
# REQUIRES:
#  Libraries: cairo, png, grid - for custom point object
#  Functions: CreateBins
#############################################################################

OverviewPlot <- function(keepObs, excludeObs, binnedObs, metaData, checkBoxState, sensor){
  if(checkBoxState == TRUE){
  #getting parameters mins and maxes out of metaData
    paramMin <- metaData$defaultParamRange$minimum[metaData$defaultParamRange$oceName == sensor]
    paramMax <- metaData$defaultParamRange$maximum[metaData$defaultParamRange$oceName == sensor]
    if(length(paramMin) == 0){
      paramMin <- NULL
      paramMax <- NULL
      }
  } else {
    paramMin <- NULL
    paramMax <- NULL
  }
    
  # When enough points are removed to generate an 'empty'depth bin, 
  # the top plotting function is used. Otherwise a basic plot is made.
  if(any(is.na(binnedObs$y))){
    # Create DataFrame for plotting empty Bins
    missingBinned <- binnedObs[is.na(binnedObs$y),]
    missingBinned$y[is.na(missingBinned$y)] <- mean(binnedObs$y, na.rm = TRUE)
    
    # Use excluded values to position Exluded point over data
    excludeBinned <- CreateBins(excludeObs$x, excludeObs$y, metaData$maxDepth)
    missingBinned$y <- excludeBinned$y[match(missingBinned$x, excludeBinned$x)]
    
    PLOT <- ggplot(keepObs, aes(y, x)) + geom_point(color = "#56B4E9", alpha = 0) +
      geom_hline(yintercept =  metaData$maxDepth * -1, color = "#990000", linetype = "dashed") +
      geom_point(data = binnedObs, aes(y,x), shape = 21, fill = "#FFFF00", size = 5) +
      geom_point(data = keepObs, aes(y, x), color = "#56B4E9", alpha = .8) +
      geom_point(data = excludeObs, color = "#FF6666") + 
      geom_point(data = missingBinned, aes(y,x), shape = 21, fill = "black", size = 5) +
      geom_point(data = missingBinned, aes(y,x), shape = 120, color = "red1", size = 8) +
      labs(x = sensor, y = "Depth (m)", title = paste(metaData$station, sensor, sep=" - "))+
      coord_cartesian(xlim = c(paramMin, paramMax))
    
  } else {
    PLOT<- ggplot() + 
      geom_point(data = keepObs, aes(y, x), color = "#56B4E9", alpha = 0) +
      geom_hline(yintercept =  metaData$maxDepth * -1, color = "#990000", linetype = "dashed") +
      geom_point(data = binnedObs, aes(y,x), shape = 21, fill = "#FFFF00", size = 5) +
      geom_point(data = keepObs, aes(y, x), color = "#56B4E9", alpha = .8) +
      geom_point(data = excludeObs, aes(y, x), color = "#FF6666") + 
      labs(x = sensor, y = "Depth (m)", title = paste(metaData$station, sensor, sep=" - "))+
      coord_cartesian(xlim = c(paramMin, paramMax))
  }

  return(PLOT)
}

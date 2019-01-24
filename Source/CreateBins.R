#############################################################################
# DESCRIPTION:
#   Given a dataframe of data and depth, calculated mean(data) on 1meter depth
#   intervals from x.5 to x+1

# INPUTS:
#   depths - vector of depths associated with observation vector
#   observations - vector of CTD data sorted by depth

# OUTPUTS:
#  Returns Dataframe with 2 variables:
#    meanObs - average of observations
#    depth - vector of integers from 1 to max depth
#TEST DATA:
# obs <- rnorm(90, 10, 2)
# dep <- seq(0.1, 9, by = .1)
# mxDp <- 15
# CreateBins(dep, obs, mxDp)
#############################################################################
CreateBins <- function(depths, observation, maxDepth) {
  # when a previously missing bin is filled in, this prevents -inf value for depth
  if(length(depths) == 0) return(data.frame(x = 0, y = 0))
  
  DATA <- data.frame(depths * -1, observation)
  DATA <- DATA[!is.na(DATA$observation),]
  
  binnedData <- sapply(1:round(max(DATA$depths), digits= 0), 
                      function(x){
                        startDepth <- x - 0.5
                        stopDepth  <- x + 0.5
                        
                        obsBin <- DATA$observation[DATA$depths >= startDepth & DATA$depths < stopDepth]
                        if(length(obsBin) < 1) obsBin <- NA
                        
                        meanObs <- mean(obsBin)
                        return(c("x" = x, "y" = meanObs))
                      })
  
  binnedData <- as.data.frame(t(binnedData))
  binnedData$x <- binnedData$x * -1
  
  return(binnedData)
}



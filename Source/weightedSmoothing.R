WeightedSmoothing <- function(df, window){
  #############################################################################
  # DESCRIPTION:
  #   This function applies a weighted smoothing average to observations. Mean 
  #   for scanning window is calculated as a weighted average where observations
  #   with a greater deviation from the window mean are downweighted.
  # INPUTS:
  #   x - vector of depths associated with observation vector
  #   observations - vector of CTD data sorted by depth
  #   window - size of smoothing window, reduces model sample size by window size
  # OUTPUTS:
  #  Returns Dataframe with 4 variables:
  #    predictor - in most cases depth
  #    observations - original data values
  #    modeled.obs - estimated value for givin depth
  #    modeled.sd - standard deviation around given depth for scanning window.
  #############################################################################
  
  #QC Variables, handle Errors
  if(!is.numeric(df$value)) {
    stop("Response object is not Numeric.")
  }
  if(window > length(df$value)) stop("Window length excedes number of observations.")
  
  # TODO: Check for missing values
  # if(any(is.na(DATA$CDOM))){
  #   warning(paste("Missing values in", names(df), "rows with NA removed")
  # }
  
  #setup dataframe to be returned
  model.data <- data.frame(x = df$depth,
                           y = df$value,  
                           modeled.y = vector(mode = "numeric", length = length(df$value)),  
                           y.sd = vector(mode = "numeric", length = length(df$value)))  
  
  #Scanning window to predict value
  for(i in 1:length(model.data$y)){
    
    # Setup Scanning window
    first <- i-floor(window/2)
    
    if(window %% 2 == 0) {             #Window isn't centered if an even scanning window is selected.
      last <- i + (floor(window/2)-1)  #EVEN DIAGRAM [n,n,X,n] where x is predicted value, n's are averaged in scan window
    } else {
      last <- i + (floor(window/2))    #ODD DIAGRAM  [n,n,X,n,n] where x is predicted value, n's are averaged in scan window
    }
    
    # FLOW CONTROL - Addresses issue of scanning window including values outside of observation set
    # For the first&last (window/2) values in the vector, model data is 1:window 
    # EXAMPLE: [X,n,n,n,n, ...] [n,X,n,n,n, ...] [n,n,X,n,n, ...] [..., n,n,X,n,n, ...] 
    #          [..., n,n,X,n,n, ...][... ,n,n,X,n,n], [... ,n,n,n,X,n] [... ,n,n,n,n,X]
    
    if(first < 1) {  # Scanning window at begining of vector
      obs.window <- df$value[1:window]  
      
    } else if(last > length(model.data$y)) {  # Scanning window at end of vector 
      obs.window <- df$value[(length(model.data$y)-(window-1)):length(model.data$y)]
    
    } else {  # Scanning window is contained within the vector of observations
      obs.window <- df$value[first:last]  
    }
    
    #Calculate mean for scanning window
    w.mean <- mean(obs.window)              
    
    #Generate vecor of absolute deviations from mean for each observation
    AD.vector <- abs(obs.window - w.mean)   # D   - R vectorization calcs AD for each value of obs.window
    
    if(sum(AD.vector == 0)){
      # If there is no variation in the window
      model.data$modeled.y[i] <- model.data$y[i]
      model.data$y.sd[i] <- 0
      
    } else {
      AD.conv.vector <- (1 - AD.vector/sum(AD.vector))
      weight.vector <- AD.conv.vector/sum(AD.conv.vector)
      
      #append Dataframe
      model.data$modeled.y[i] <- sum(weight.vector * obs.window)
      model.data$y.sd[i] <- sd(obs.window)
    }
  }
  return(model.data)
}



















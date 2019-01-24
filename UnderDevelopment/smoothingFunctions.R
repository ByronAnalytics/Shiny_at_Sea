#Collection of smoothing functions for CTD data

loess.filter <- function(x ,y, span = 0.4, should.plot = TRUE, ci.num.sd=3, ...){
  #   Applies loess smoothing function to variabls, in anticipation of CTD Data, it plots with 
  #   depth on the Y-axis and the variable of interest on the X-axis. confidence intervals are 
  #   calculated with SD = ci.num.sd
  #   
  
  loess.function <- loess(y~x, span = span)
  loess.predicted <- predict(loess.function, se=TRUE)
  loess.upper <- loess.predicted$fit + ci.num.sd*(loess.predicted$se.fit*sqrt(length(loess.predicted)))
  loess.lower <- loess.predicted$fit - ci.num.sd*(loess.predicted$se.fit*sqrt(length(loess.predicted)))
  
  if(should.plot == TRUE){
    #generate Labels
    main.lab <- strsplit(deparse(substitute(y)), "\\$")[[1]][1]
    x.lab <- strsplit(deparse(substitute(y)), "\\$")[[1]][2]
    y.lab <- strsplit(deparse(substitute(x)), "\\$")[[1]][2]
    
    
    plot(-x~y, pch=16, main = "main.lab", xlab= "x.lab", ylab = "y.lab")
    lines(-x~loess.predicted$fit, col="red", lwd = 2)
    lines(-x~loess.upper, lty=2, col = "red")
    lines(-x~loess.lower, lty=2, col = "red")
  }
  return(loess.predicted)
}



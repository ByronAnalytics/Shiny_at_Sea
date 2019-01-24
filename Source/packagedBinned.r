formatForOutput <- function(dataList, dataFrame){ 
  if(is.null(dataList[[1]][[dataFrame]])) {
    return(data.frame())
    } else {
      dataList <- as.list(dataList)
      columnNames <- sapply(dataList, function(x) x$sensor)
      
      # iterate through list to extract probe values
      tempData <- lapply(dataList, function(li){
                          tempDf <- data.frame(li[[dataFrame]]$y, li[[dataFrame]]$x)
                          names(tempDf) <- c(li$sensor, "depth")
                          return(tempDf)
                        })
      
      allData <- plyr::join_all(tempData, by = "depth", type = "full")
      
      # is consistent across all list elements, use first 
      
        allData$depth <- -1 * allData$depth
        allData <- allData[order(allData$depth),]
        allData <- allData[, c("depth", columnNames)]
      
      return(allData)
      }
}

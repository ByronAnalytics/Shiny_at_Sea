### modifications to return one dataframe with a true/false column for keeping or discarding data
# for testing, run lines withing readNSmooth function first --- CTDModel <- smoothList$CDOM

markOutliers <- function(CTDModel, Threshold = 2){

  # Keep if: lower limit >= values <= Upper Limit OR NA OR 0
  keep.vector <- CTDModel$y >= CTDModel$modeled.y - Threshold*CTDModel$y.sd &
                 CTDModel$y <= CTDModel$modeled.y + Threshold*CTDModel$y.sd | 
                 is.na(CTDModel$modeled.y) | CTDModel$y.sd == 0
  
  CTDModel$toKeep <- keep.vector

  return(CTDModel)

}

jibbily_goo <- function(FILE){
  #############################################################################
  # DESCRIPTION:
  #   This function takes in data exported by seabird and traditionally read 
  #   into IGODS. Header is scanned for column-names and order, date, and 
  #   station is pulled from file name.
  # EXPECTED FORMAT:
  #   Anticipating a data file with metadata header. "# name =" id's data col
  #   metatdata header ends with "*END*"  
  # INPUTS:
  #   file name and path.
  # RETURNS: 
  #   dataframe
  # SUMMARY:
  #   1) break file at "*END*" for seperate header and data processing
  #   2) Extract variable names and paste to data objects
  #   3) Insert Date and Station columns
  #TESTING: FILE <- "Data/a6.txt"
  #############################################################################
  kDataBreak <- "*END*"  # Marks end of header
  kFileName <- "* FileName"
  kFileDate <- "* System UpLoad Time"  
  kColumnID <- "# name"
  
  #Generates list with each element a line in the csv file.
  Lines <- readLines(FILE)
  
  # Capture Site: 
  #  Locate filepath, extract file name, split on '.' and drop file extension
  filename <- Lines[grep(kFileName, Lines)]
  site <- toupper(unlist(strsplit(basename(filename), split = "\\."))[1])
  
  # Capture Date: 
  samp.date <- Lines[grep(kFileDate, Lines)]
  samp.date <- unlist(strsplit(samp.date, split = "= "))[2]
  
  date <- as.Date(samp.date, format = "%B %d %Y")
  
  #Generate Data Frame
  start.point <- grep(kDataBreak, Lines) + 1
  lines.length <- length(Lines)
  
  DATA <- read.table(text=Lines[c(start.point:lines.length)])
  
  
  # Generate Vector of column headers, add to data frame
  names.list <- strsplit(Lines[grep(kColumnID, Lines)], split = ":")
  column.names <- vector(mode = "character")
  
  for(i in 1:length(names.list)){
    temp <- unlist(names.list[[i]])[1]
    column.names[i] <- strsplit(temp, "= ")[[1]][2]  # split on =, keep element after
  }
  colnames(DATA) <- column.names
  
  #append columns for date and station
  station <- rep(site, times = length(DATA[,1]))
  date <- rep(date, times = length(DATA[,1]))
  DATA <- cbind(station, date, DATA)
  
  return(DATA)
}


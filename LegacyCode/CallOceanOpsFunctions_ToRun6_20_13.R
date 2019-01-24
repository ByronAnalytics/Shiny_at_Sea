# Get the user to set the quantiles they want for calculating "outliers" (done - below in quants.to.use)
# Get user to set date range for historical data calculation (done - below in hist.dates)
# Made plot filename save with the date sampled... (done)
# Add CDOM to the plots with Chl-a and transmissivity (on the Chl-a scale) (done)
# Increase font size (and change plot label location - Maiko's request) (done)

# would they ever want to plot these for more than one date? if so take max date or date range (ask Greg)
# DO/Ph scatter maybe just the last plot with all stations colored by station?... (ask Greg)
# [GUI work] For GUI - "Popup with a "Save to Default" OR "Click here to browse" would be excellent." Greg 

#setwd("Y:\\EMTS\\41.Sections\\MBOO\\60.Work_Groups\\IT_GIS\\JensWork\\Jodi\\OceanOperations\\")  # this would be based on user input
setwd("E:\\JodiStuff\\OceanOperatiosn\\")
#setwd("F:\\JodiStuff\\OceanOperatiosn\\")

rm(list = ls())
options(stringsAsFactors = F)

quants.to.use <- c(.01,.99) #Option to be made available in the GUI
hist.dates <- c("2005-1-1", "2012-12-31") # Option to be made available in the GUI

# Read in Files
ctd <- read.csv("13May2013_PLOO_WQ.CSV", skip = 36) # This would be one day's worth of data that was created during field sampling
#ctd <- read.csv("25April2013_Kelp_WQ.CSV", skip = 36) # This would be one day's worth of data that was created during field sampling
ctd.hist <- read.csv("Historical_CTD_Data.csv") # These are the data that will get passed to the quantiles function to generate the denotations on the plots

# Run raw.plots on the one day's worth of data
source("OceanConditionsQAQCFunctions.R")

# Run CTD QA/QC Plots
run.raw.plots(ctd, ctd.hist, hist.dates, quants.to.use)  # This would result in plots that would appear on the screen (and user could chose a directory they could be saved to).

# Run dissolved oxygen and pH scatter plots
scat.do.ph(ctd)
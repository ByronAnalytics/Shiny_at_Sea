generate.quantiles <- function(ctd.historical, quants.to.use) {
  # Function takes historical data and calculates quantiles set by the user
  #  of all historical data
  # A data frame is returned with the variable, 10% value, and 90% value
  
  library(reshape2)
  hist.melt <- melt(ctd.historical, id = c("station", "sample_date", "depth_meter"))
  hist.split <- split(hist.melt, hist.melt$variable)
  
  par.quants <- lapply(hist.split, function(x) {
                       values <- quantile(x$value, probs = quants.to.use, na.rm = TRUE)
                       out <- data.frame(variable = unique(x$variable), low = values[1], high = values[2])
                       }
                 )
  values.to.test <- do.call(rbind, par.quants)
  rownames(values.to.test) <- NULL 
  return(values.to.test)

}

flag.data <- function(ctd, values2flag) {
  # Function takes ctd data and quantiles given by the user and flags values that exceed these thresholds.
  #  The data are coded with a color designation for plotting "purple" for values below the lower quantile value 
  #  and "red" for values above the upper quantile value
  # A dataframe with the raw data flagged is returned

  ctd.merge <- merge(ctd, values2flag)
  ctd.merge$flag <- NA
  ctd.merge$flag[ctd.merge$value <= ctd.merge$low] <- "low"
  ctd.merge$flag[ctd.merge$value >= ctd.merge$high] <- "high"

  # Assign colors for plotting
  ctd.merge$flag.color <- NA
  ctd.merge$flag.color[ctd.merge$flag == "low"] <- "purple"
  ctd.merge$flag.color[ctd.merge$flag == "high"] <- "red"

  # Remove rows for which the value is -1
  ctd.merge <- ctd.merge[which(ctd.merge$value != -1), ]
   
  return(ctd.merge)   
   
}

plot.format <- function(ctd) {
  # Function adds a column for plot colors to the data frame so each 
  #  color can be passed to the plot function
  # Also the plot group is set below (distinguishes the groups of variables to be plotted
  #  together and will plot those)
  ctd$plot.color <- NA
  ctd$plot.color[ctd$variable == "cdom"] <- "brown"
  ctd$plot.color[ctd$variable == "dox"] <- "green"
  ctd$plot.color[ctd$variable == "fluorometry"] <- "purple"
  ctd$plot.color[ctd$variable == "pct_light"] <- "darkgreen"
  ctd$plot.color[ctd$variable == "ph"] <- "orange"
  ctd$plot.color[ctd$variable == "salinity"] <- "blue"
  ctd$plot.color[ctd$variable == "temp"] <- "red"
  
  ctd$plot.group <- NA
  ctd$plot.group[ctd$variable %in% c("temp", "salinity")] <- 1
  ctd$plot.group[ctd$variable %in% c("ph", "dox")] <- 2
  ctd$plot.group[ctd$variable %in% c("cdom", "fluorometry", "pct_light")] <- 3

  return(ctd)
}  

make.plot <- function(ctd) {
  # Makes a plot of one parameter with no axes
  plot(ctd$value, -ctd$depth_meter,
       type="b", lty=1, col=unique(ctd$plot.color), 
       xlab="", ylab="", mgp=c(.1,.1,0), 
       cex.axis=1.2, tcl=-0.1, cex=1.5, las=1, axes = FALSE)
}

mark.high.low <- function(ctd) {
   # Puts circles around points that exceed high and low quantiles
   points(ctd$value[!is.na(ctd$flag)], 
             -ctd$depth_meter[!is.na(ctd$flag)], 
             col = ctd$flag.color[!is.na(ctd$flag)], 
             cex = 2, pch = 21, lwd = 3)
}

overlay.plots <- function(ctd) {
  # Plots each variable in a given plot group - so makes 1 plot with all variables
  #  to be plotted together overlayed on top of each other

  # Plot each variable
  for(var in unique(ctd$variable)) {
    to.plot <- ctd[ctd$variable == var, ]
    make.plot(to.plot)
    mark.high.low(to.plot)
    par(new=T)
  }
  
  # Create Axes
  axis(side=3,cex.axis=1.2, tcl=-0.1, mgp=c(2,0.3,0))
  axis(side=2,cex.axis=1.2, tcl=-0.1, mgp=c(2,0.3,0), las = 1)
  axis(side=1,cex.axis=1.2, tcl=-0.1, mgp=c(2,0.3,0))
  mtext(unique(ctd$variable)[2], side = 3, line = 1.2, col = 1 , cex = 1)
  if(any(ctd$variable == "cdom")) mtext("fluor/CDOM", side=1, col=1,line=1.2, cex=1)
  if(!(any(ctd$variable == "cdom"))) mtext(unique(ctd$variable)[1], side=1, col=1,line=1.2, cex=1)
  mtext("Depth (m)",side=2, col=1,line=1.4,cex=1)
 
  # Create legend
  legend("bottomright", as.character(unique(ctd$variable)), bty="n",col = unique(ctd$plot.color), 
          lty= c(1,1), merge = TRUE, cex=1.4)
  legend("topleft",c(paste("Station ",unique(ctd$station),sep=""),as.character(unique(ctd$sample_date))), bty="n",cex=1.4)
  par(new=F)
  box()
}

plot.data <- function(ctd) {
  # Function takes ctd data for one station and date combination, orders it
  #  and then splits by plot group
  # Makes a total of three plots per station date combination
  #   Pairs temp + salinity
  #   transmissivity + chl a + CDOM (if present)
  #   DO + pH

  # Order the raw data by varable and then by depth
  ctd <- ctd[order(ctd$variable, -ctd$depth_meter),]	

  # Make three plots for each station / sample date combination 
  #  with two or three parms per plot
  ctd.list <- split(ctd, ctd$plot.group)           
  plot.out <- lapply(ctd.list, overlay.plots)
 
}    

run.raw.plots <- function(ctd, ctd.historical, hist.dates, quants.to.use) {
  # Create profile plots by station by date of all parameters, flagging values above the upper quantile
  #   set by the user and below the lower quantile set by the user (quants.to.use) of historical data
  #   between a set of dates given by the user (hist.dates) 
  #   and below the 10% values of all historical raw data
  # Takes a dataframe with ctd data which must include transmissivity, 
  #   chl a, temperature, salinity, DO and pH
  #    Pairs temp + salinity
  #    transmissivity + chl a + CDOM (if present)
  #    DO + pH
  # Also takes a historical dataset to calculate quantiles with
  # JPS 5/29/13
  
  # Subset historical data to date range desired
  ctd.historical <- ctd.historical[as.Date(ctd.historical$sample_date) > as.Date(hist.dates[1]) & 
						as.Date(ctd.historical$sample_date) < as.Date(hist.dates[2]), ]

  # Generate quantiles for all historical data
  flags <- generate.quantiles(ctd.historical, quants.to.use)
  print(paste("Historical dates used to calculate quantiles were between", as.Date(hist.dates[1]), "and", as.Date(hist.dates[2])))
  print(paste("Quantiles for each variable are (", quants.to.use[1]*100,"%"," and ", quants.to.use[2]*100, "%)", ":", sep = "")) 
  print(flags)
    
  # Format ctd data from raw IGODS format
  ctd <- format.ctd(ctd)

  # Format ctd data to the proper plot formatting
  ctd <- plot.format(ctd)
  
  # Flag the data
  flag.ctd <- flag.data(ctd, flags)

  # Write a .csv file with flagged data
  to.write <- flag.ctd[!is.na(flag.ctd$flag), c("station","sample_date", "variable", "depth_meter", "value", "low", "high", "flag", "flag.color")]
  fn.date <- format(as.Date(unique(to.write$sample_date), "%m/%d/%Y"), "%Y%m%d")
  write.csv(to.write, paste("CTD_QAQC_FlaggedVals_", fn.date, ".csv", sep=""), row.names = F)

  # Splitting the data by station by date
  ctd.list <- split(flag.ctd, paste(flag.ctd$station, flag.ctd$sample_date))

  pdf(paste("CTD_QAQC_Plots_", fn.date, ".pdf",sep=""),width=8.5, height=11)
  par(mfrow=c(1,3),mar=c(1,3.5,4,1), omi=c(1,1,1,1), oma=c(6,4,4,4)) #Bottom,Left, Top,Right.  #,pty="s"  oma=c(0,0,0,0)
  junk <- lapply(ctd.list,plot.data)
  dev.off()

}

scatter.plot <- function(to.plot, xlim, ylim) {
  # Function creates by station and sample date scatter plots of dissolved oxygen and pH
  # Takes a dataframe of data only by station and sample date combination

  to.plot$sample_date <- format(as.Date(to.plot$sample_date, "%m/%d/%Y"), "%b %d, %Y")
  plot(to.plot$do, to.plot$ph, xlab = "Dissolved Oxygen", ylab = "pH",
       main = paste("Station", unique(to.plot$station), "sampled", unique(to.plot$sample_date)),
       pch = 19, xlim = xlim, ylim = ylim)
}

scat.do.ph <- function(ctd) {
  # Function creates by station and sample date combination scatter plots of DO and pH (by calling scatter.plot function)
  #   And generates one scatter plot of all dissolved oxygen and pH for all data in ctd
  # Takes a dataframe of CTD data to plot called ctd
  # Plots are put into a .pdf file and saved to the working directory
  
  # Format the IGODS ctd data
  ctd <- format.ctd(ctd)
  ctd <- dcast(ctd, station + sample_date + depth_meter ~ variable, value.var = "value") 
  ctd <- ctd[which(ctd$dox > -1 & ctd$ph > -1), ]
  
  # Setting the ranges for the values on the axes
  xlim <- c(min(ctd$dox), max(ctd$dox))
  ylim <- c(min(ctd$ph), max(ctd$ph))

  # Split the dataframe ctd by station and sample_date to make individual cast plots
  ctd.list <- split(ctd, paste(ctd$station, ctd$sample_date))
  
  # Do by station by date scatter plots of DO/pH (per cast pots)
  fn.date <- format(as.Date(unique(ctd$sample_date), "%m/%d/%Y"), "%Y%m%d")
  pdf(paste("DO_PH_ScatterPlots_", fn.date,".pdf", sep = ""))
  junk <- lapply(ctd.list, scatter.plot, xlim = xlim, ylim = ylim)

  # Do one overall scatter plot of DO and pH at the end for the entire dataset
  plot(ctd$do, ctd$ph, xlab = "Dissolved Oxygen", ylab = "pH",
       main = paste("DO and pH for all stations sampled on", unique(ctd$sample_date)),
       pch = 19, xlim = xlim, ylim = ylim)
  
  # Close the .pdf file that was generated
  dev.off()
 
}

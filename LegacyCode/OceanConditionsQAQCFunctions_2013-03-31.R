generate.quantiles <- function(ctd.historical) {
  # Function takes historical data and calculates 10% and 90% values for the distribution
  #  of all historical data
  # A data frame is returned with the variable, 10% value, and 90% value
  
  library(reshape2)
  hist.melt <- melt(ctd.historical, id = c("station", "sample_date", "depth_meter"))
  hist.split <- split(hist.melt, hist.melt$variable)
  par.quants <- lapply(hist.split, function(x) {
                       values <- quantile(x$value, probs = c(0.1, 0.9), na.rm = TRUE)
                       out <- data.frame(variable = unique(x$variable), low.10 = values[1], high.90 = values[2])
                       }
                 )
  values.to.test <- do.call(rbind, par.quants)
  rownames(values.to.test) <- NULL 

  return(values.to.test)

}

flag.data <- function(ctd, values2flag) {
  # Function takes ctd data and the 90% and 10% quantiles and flags values that exceed these thresholds.
  #  The data are coded with a color designation for plotting "purple" for values below the 10% value 
  #  and "red" for values above the 90% value
  # A dataframe with the raw data flagged is returned

  library(reshape2)
  ctd.melt <- melt(ctd, id = c("station", "sample_date", "depth_meter"))
  ctd.merge <- merge(ctd.melt, values2flag)
  ctd.merge$flag <- NA
  ctd.merge$flag[ctd.merge$value <= ctd.merge$low.10] <- "low"
  ctd.merge$flag[ctd.merge$value >= ctd.merge$high.90] <- "high"

  # Assign colors for plotting
  ctd.merge$color <- NA
  ctd.merge$color[ctd.merge$flag == "low"] <- "purple"
  ctd.merge$color[ctd.merge$flag == "high"] <- "red"
   
  return(ctd.merge)   
   
}

individual.plots <- function(ctd, to.plot, cols) {
  # Function plots two variables in one plot space
  #  data are in the dataframe ctd
  #  variables to plot are designated by to.plot argument and the colors to plot them in are designated by cols
  # Flagged values outside the quantile ranges are plotted with colors designated by the column "color" in the ctd dataframe 
  #  and plots those with purple (low) or red (high) circles around the data point
  # Legends are generated

  par(new=F)
  plot(ctd$value[ctd$variable == to.plot[1]],
       -ctd$depth_meter[ctd$variable == to.plot[1]],
       type="b",lty=1,col=cols[1],xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1)
 
  par(new=T)
	   
  # Label any highs and lows in the data as compared to the quantiles
  if(any(!is.na(ctd$flag[ctd$variable == to.plot[1]]))) {
       points(ctd$value[ctd$variable == to.plot[1] & !is.na(ctd$flag)], 
             -ctd$depth_meter[ctd$variable == to.plot[1] & !is.na(ctd$flag)], 
             col = ctd$color, cex = 2.5, pch = 21, lwd = 3)
  }
  par(new=T)
  # Creating the second plot     
  plot(ctd$value[ctd$variable == to.plot[2]],
        -ctd$depth_meter[ctd$variable == to.plot[2]],
        type="b",lty=1,col=cols[2],xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1,axes=F)
       
  # Label any highs and lows in the data as compared to the quantiles
  if(any(!is.na(ctd$flag[ctd$variable == to.plot[2]]))) {
      points(ctd$value[ctd$variable == to.plot[2] & !is.na(ctd$flag)], 
            -ctd$depth_meter[ctd$variable == to.plot[2] & !is.na(ctd$flag)], 
            col = ctd$color, cex = 2.5, pch = 21, lwd = 3)
    }
                  
  # Creating axes
  axis(side=3,cex.axis=0.8, tcl=-0.1, mgp=c(2,0.3,0))
  mtext(to.plot[2],side=3,line=1.2,col=1,cex=0.65)
  mtext(to.plot[1],side=1, col=1,line=1.2,cex=0.65)
  mtext("Depth (m)",side=2, col=1,line=1.4,cex=0.65)
  legend("bottomright", c(to.plot[1], to.plot[2]), bty="n",col = cols,lty= c(1,1),merge = TRUE, cex=0.55)
  legend("bottomleft",c(paste("Station ",unique(ctd$station),sep=""),as.character(unique(ctd$sample_date))), bty="n",cex=0.8)
  par(new=F)
}


plot.data <- function(ctd) {
  # Function takes ctd data for one station and date combination
  #   calls the individual.plots combination to make each plot (with two variables per plot)
  # Makes a total of three plots per station date combination
  #   Pairs temp + salinity
  #   transmissivity + chl a
  #   DO + pH

  # Order the raw data by varable and then by depth
  ctd <- ctd[order(ctd$variable, -ctd$depth_meter),]	

  # Make three plots for each station / sample date combination 
  #  with two parms per plot           
  individual.plots(ctd, to.plot = c("temp", "salinity"), cols = c("red", "blue"))
  individual.plots(ctd, to.plot = c("dox", "ph"), cols = c("green", "orange"))
  individual.plots(ctd, to.plot = c("fluorometry", "pct_light"), cols = c("purple", "darkgreen"))
}    


run.raw.plots <- function(ctd, ctd.historical) {
  # Create profile plots by station by date of all parameters, flagging values above the 90% 
  #   and below the 10% values of all historical raw data
  # Takes a dataframe with ctd data which must include transmissivity, 
  #   chl a, temperature, salinity, DO and pH
  #    Pairs temp + salinity
  #    transmissivity + chl a
  #    DO + pH
  # Also takes a historical dataset to calculate quantiles with
  # JPS 3/31/13
  # Depends on the functions generate.quantiles and flag.data   
 
  # Generate quantiles for all historical data
  flags <- generate.quantiles(ctd.historical)
   
  # Flag the data
  flag.ctd <- flag.data(ctd, flags)

  # Splitting the data by station by date
  ctd.list <- split(flag.ctd, paste(flag.ctd$station, flag.ctd$sample_date))

  pdf(paste("CTD_QAQC_Plots_", Sys.Date(), ".pdf",sep=""),width=8.5, height=11)
  par(mfrow=c(1,3),mar=c(1,3.5,4,1), omi=c(1,1,1,1), oma=c(6,4,4,4)) #Bottom,Left, Top,Right.  #,pty="s"  oma=c(0,0,0,0)
  junk <- lapply(ctd.list,plot.data)
  dev.off()

}

scatter.plot <- function(to.plot) {
  # Function creates by station and sample date scatter plots of dissolved oxygen and pH
  # Takes a dataframe of data only by station and sample date combination

  to.plot$sample_date <- format(as.Date(to.plot$sample_date, "%m/%d/%Y"), "%b %d, %Y")
  plot(to.plot$do, to.plot$ph, xlab = "Dissolved Oxygen", ylab = "pH",
       main = paste("Station", unique(to.plot$station), "sampled", unique(to.plot$sample_date)),
       pch = 19)
}

scat.do.ph <- function(ctd) {
  # Function creates by station and sample date combination scatter plots of DO and pH (by calling scatter.plot function)
  #   And generates one scatter plot of all dissolved oxygen and pH for all data in ctd
  # Takes a dataframe of CTD data to plot called ctd
  # Plots are put into a .pdf file and saved to the working directory

  # Split the dataframe ctd by station and sample_date to make individual cast plots
  ctd.list <- split(ctd, paste(ctd$station, ctd$sample_date))
  
  # Do by station by date scatter plots of DO/pH (per cast pots)
  pdf(paste("DO_PH_ScatterPlots_", Sys.Date(),".pdf", sep = ""))
  junk <- lapply(ctd.list, scatter.plot)

  # Do one overall scatter plot of DO and pH at the end for the entire dataset
  plot(ctd$do, ctd$ph, xlab = "Dissolved Oxygen", ylab = "pH",
       main = paste("DO and pH for all stations sampled on", unique(ctd$sample_date)),
       pch = 19)
  
  # Close the .pdf file that was generated
  dev.off()
 
}

############### OLD VERSION WITHOUT FLAGGING #####################
raw.plots <- function(ctd) {
   # Create profile plots by station by date of all parameters
   #   Pairs temp + salinity
   #   transmissivity + chl a
   #   DO + pH
   # Takes a dataframe with ctd data which must include transmissivity, 
   #   chl a, temperature, salinity, DO and pH
   # JPS March 4th, 2013
   
   # THIS IS AN OLD VERSION WITHOUT FLAGGING... JUST IN CASE
   # FIRST WILL WANT TO USE GENERATE QUANTILES TO GET HISTORICAL QUANTILES FOR DATA
   # THEN WILL WANT TO FLAG THE DATA

	ctd.list <- split(ctd,paste(ctd$station,ctd$sample_date))

		plot.data <- function(ctd) {
		   par(new=F)
		   ctd <- ctd[order(-ctd$depth_meter),]	

               plot(ctd$temp,-ctd$depth_meter,type="b",lty=1,col="red",xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1)
		   par(new=T)
		   plot(ctd$salinity,-ctd$depth_meter,type="b",lty=1,col="blue",xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1,axes=F)
               axis(side=3,cex.axis=0.8, tcl=-0.1, mgp=c(2,0.3,0))
 		   mtext("Salinity",side=3,line=1.2,col=1,cex=0.65)
		   mtext("Temperature",side=1, col=1,line=1.2,cex=0.65)
		   mtext("Depth (m)",side=2, col=1,line=1.4,cex=0.65)
		   legend("bottomright", c("Temperature", "Salinity"), bty="n",col = c("red","blue"),lty= c(1,1),merge = TRUE, cex=0.55)
               legend("bottomleft",c(paste("Station ",unique(ctd$station),sep=""),as.character(unique(ctd$sample_date))), bty="n",cex=0.8)

		   par(new=F)
 		   plot(ctd$dox,-ctd$depth_meter,type="b",lty=1,col="green",xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1)
		   par(new=T)
		   plot(ctd$ph,-ctd$depth_meter,type="b",lty=1,col="orange",xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1,axes=F)
               axis(side=3,cex.axis=0.8, tcl=-0.1, mgp=c(2,0.3,0))
 		   mtext("pH",side=3,line=1.2,col=1,cex=0.65)
		   mtext("DO",side=1, col=1,line=1.2,cex=0.65)
		   mtext("Depth (m)",side=2, col=1,line=1.4,cex=0.65)
		   legend("bottomright", c("DO", "pH"), bty="n",col = c("green","orange"),lty= c(1,1),merge = TRUE, cex=0.55)
               legend("bottomleft",c(paste("Station ",unique(ctd$station),sep=""),as.character(unique(ctd$sample_date))), bty="n",cex=0.8)

		   par(new=F)
 		   plot(ctd$fluorometry,-ctd$depth_meter,type="b",lty=1,col="purple",xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1)
		   par(new=T)
		   plot(ctd$pct_light,-ctd$depth_meter,type="b",lty=1,col="darkgreen",xlab="",ylab="",mgp=c(2,0.3,0), cex.axis=0.8, tcl=-0.1, cex=.8, las=1,axes=F)
               axis(side=3,cex.axis=0.8, tcl=-0.1, mgp=c(2,0.3,0))
 		   mtext("Transmissivity",side=3,line=1.2,col=1,cex=0.65)
		   mtext("Chl a",side=1, col=1,line=1.2,cex=0.65)
		   mtext("Depth (m)",side=2, col=1,line=1.4,cex=0.65)
		   legend("bottomright", c("Chl a", "Transmissivity"), bty="n",col = c("purple","darkgreen"),lty= c(1,1),merge = TRUE, cex=0.55)
               legend("bottomleft",c(paste("Station ",unique(ctd$station),sep=""),as.character(unique(ctd$sample_date))), bty="n",cex=0.8)

		   par(new=F)
		}

	pdf(paste("RawDataPlots_",max(as.numeric(substr(ctd$sample_date,1,4))),".pdf",sep=""),width=8.5, height=11)
	par(mfrow=c(1,3),mar=c(1,3.5,4,1), omi=c(1,1,1,1), oma=c(6,4,4,4)) #Bottom,Left, Top,Right.  #,pty="s"  oma=c(0,0,0,0)
	junk <- lapply(ctd.list,plot.data)
	dev.off()

}




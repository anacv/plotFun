#' Plot time series.
#' 
#' Plot a time series plot, with the possibility of applying a temporal aggregation (see \code{data_agg}). 
#' 
#' @param data Matrix with the data to be plotted (time steps in rows, stations in columns). The column names are used for labels in the plot, otherwise a,b,c,... are used.
#' @param dates Vector of dates (POSIX class) of the original data.
#' @param agg Level of temporal aggregation: daily ("D", only for hourly input data), monthly ("M"), seasonal ("S"), annual ("Y"). Default: NULL (all data is plotted using the original temporal resolution). This argument is only valid for \code{type="ts"}.
#' @param aggFun Function for temporal aggregation, if desired. Possible values: mean, median, min, max. Default: mean.
#' @param season Vector with the desired month (season) to be plotted, e.g. 8 is August, c(6,7,8) is JJA, with the operation of 'aggFun'. Default: NULL (no seasonal/monthly slice considered).
#' @param hour logical. Is the input data hourly? Hourly data can only use aggregation 'D'. Default: FALSE.
#' @param output.data Logical, return plotted data or not. Default: FALSE.
#' @param stn.together Logical, set to TRUE to plot all stations with colour lines in one plot. Default: FALSE. Only available for time series plots.
#' @param col.lines Vector with colours for lines. Only available for time series plots.
#' @param title character string with the main title. A 1-element vector would set the same title to all subplots, a n-element vector would have the titles for n subplots.
#' @param input.mch logical. If the input data is from DWH, the hourly needs to be aggregated differently. See \code{data_agg}. Default: FALSE
#' @return A time series plot. Plotted data if 'output.data'=TRUE. The result is a list with the aggregated data, aggregated dates, function and type of aggregation.
#' @details Plot time series of one vector. It is possible to plot the "raw" series or aggregate it temporarily or display a specific season/month. If the input data is a matrix, by default a separate panel is obtained for each station. 'stn.together' allows to plot all stations together (without subplot).
#' @import grDevices graphics 


plotFun.ts.orig <-
  function (data, dates, hour=FALSE, agg=NULL, aggFun="mean", season=NULL, 
            output.data=FALSE, stn.together=FALSE, col.lines=rainbow(10), title=NULL, input.mch=FALSE) {


  ## Prepare data to plot (temporal aggregation or not)
    if(!is.null(agg) & !is.null(season)) print("Arguments 'agg' and 'season' provided. Only 'season' will be used")

    if(is.null(agg) & is.null(season)){ # no temporal aggregation
      
      data.plot <- as.matrix(data) # to avoid problems with one station.
      dates.plot <- dates
      

    } else{ # temporal aggregation
        
        # Set 'agg' for the desired 'season', 'M' for individual months and 'S' for blocks of months.
        if(!is.null(season)){
          if(length(season)==1){agg <- "M"} else{agg <- "S"}
        }

        # Aggregate data temporarily 
        aux.agg <- data_agg(data = data, dates=dates, agg=agg, aggFun= aggFun, input.mch)
        data.plot <- as.matrix(aux.agg$data.agg) # To avoid problems with one station
        dates.plot <- aux.agg$dates.agg

        # Check length of plot data and dates
        assertthat::assert_that(nrow(data.plot)==length(dates.plot))
        
        # Filter by argument 'season'
        if(!is.null(season)){
          if(length(season)==1){
            aux <- lapply(season, function(x) which(as.numeric(format(as.Date(dates.plot), '%m')) %in% x))
            ind.time <- sort(unlist(aux))
            data.plot <- data.plot[ind.time,]
            dates.plot <- dates.plot[ind.time]
            if(length(dates.plot)==1) data.plot <- as.matrix(data.plot) else data.plot <- as.matrix(data.plot)
            
          } else{
            if(identical(season,c(12,1,2))) pos.seas <-1
            if(identical(season,c(3,4,5))) pos.seas <-2
            if(identical(season,c(6,7,8))) pos.seas <-3
            if(identical(season,c(9,10,11))) pos.seas <-4
            data.plot <- data.plot[seq(pos.seas,length(dates.plot),4),]
            dates.plot <- dates.plot[seq(pos.seas,length(dates.plot),4)]
            # Build subtitle with aggFun and season
            if(identical(season, c(12,1,2))) seasonName <- 'winter'
            if(identical(season, c(3,4,5))) seasonName <- 'spring'
            if(identical(season, c(6,7,8))) seasonName <- 'summer'
            if(identical(season, c(9,10,11))) seasonName <- 'autumn'
            }
          if(length(dates.plot)==1) data.plot <- t(as.matrix(data.plot)) else data.plot <- as.matrix(data.plot)
         }
    }

   ## For legend
   if(is.null(colnames(data))) colnames(data.plot) <- letters[1:ncol(data)]
     
   ## Plot stations (rows) 
   if(stn.together==TRUE){ # all in same plot
      ylims <- rep(NA,2)
      ylims[1] <- floor(min(data.plot, na.rm=T))
      ylims[2] <- ceiling(max(data.plot, na.rm=T))
      for(count.stn in 1:ncol(data)){
        plot(data.plot[,count.stn], type="o", pch=19, xlab = "",  ylab="",
             xaxt="n",  col=col.lines[count.stn], ylim=ylims, axes=FALSE, lwd=1.5)
        par(new=T)
      }
      legend("bottomright", legend=colnames(data.plot), col=col.lines[1:ncol(data.plot)], bty = "n", cex=1.2, lty=1)
      title(title)
       
      # customize Y-axis
      axis(2,line=.5, las=1, cex.axis=1.5)
      axis(2,line=.5,col="white",tcl=0,lwd=4, las=1, cex.axis=1.5)
      grid(nx=NA,ny=NULL,col="grey55",lty=1,lwd=.5)
      
      # customize X-axis
      if(length(dates.plot)<=5){x.ticks <- unique(as.integer(pretty(seq(1, length(dates.plot)))))
      }else{
        x.ticks <- pretty(seq(1, length(dates.plot)))
        x.ticks <- x.ticks[-c(1, length(x.ticks))] # zero and maximum out of limits
      }
      
      if(!is.null(season)){
        axis(1, at=x.ticks, labels=format(as.Date(dates.plot[x.ticks]),'%Y'), cex.axis=1.5)
        axis(1, at=x.ticks, labels=format(as.Date(dates.plot[x.ticks]),'%Y'), col="white",tcl=0, lwd=2, cex.axis=1.5)
      } else{
        axis(1, at=x.ticks, labels=dates.plot[x.ticks], cex.axis=1.5)
        axis(1, at=x.ticks, labels=dates.plot[x.ticks], col="white",tcl=0, lwd=2, cex.axis=1.5)
      }
      

    }else{ # subplots
      par(mfrow=c(ncol(data),1))
      for(count.stn in 1:ncol(data.plot)){
        ylims <- rep(NA,2)
        ylims[1] <- floor(min(data.plot[,count.stn], na.rm=T))
        ylims[2] <- ceiling(max(data.plot[,count.stn], na.rm=T))
        
        if(is.null(season) & is.null(agg)){
          plot(data.plot[,count.stn], type="o", pch=19, xlab = "", ylab="", ylim=ylims,
             xaxt="n", axes=FALSE, lwd=1.5) # to colour the points, need to plot them separately
        }else{
          plot(data.plot[,count.stn], type="n", xlab = "", ylab="", ylim=ylims,
               xaxt="n", axes=FALSE) # lines needs to call a plot first
          lines(data.plot[,count.stn], xlab = "", ylab="", ylim=ylims,
                xaxt="n", lwd=2, type="l")
          points(data.plot[,count.stn], pch=19, xlab = "", ylab="", ylim=ylims,
               xaxt="n", col="grey40")
        }
        
 
        # plot station name
        mtext(colnames(data.plot)[count.stn], font=2,side=3,line=0,adj=.01,cex=1.2)
        
        # plot title and subtitle
        if(length(title)==1) title(title) else title(title[count.stn])

        # customize Y-axis
        axis(2,line=.5, las=1, cex.axis=1.5)
        axis(2,line=.5,col="white",tcl=0,lwd=4, las=1, cex.axis=1.5)
        grid(nx=NA,ny=NULL,col="grey55",lty=1,lwd=.5)
        
        # customize X-axis
        if(length(dates.plot)<=5){x.ticks <- unique(as.integer(pretty(seq(1, length(dates.plot)))))
        }else{
          x.ticks <- pretty(seq(1, length(dates.plot)))
          x.ticks <- x.ticks[-c(1, length(x.ticks))] # zero and maximum out of limits
        }
        
        if(!is.null(season)){
          axis(1, at=x.ticks, labels=format(as.Date(dates.plot[x.ticks]),'%Y'), cex.axis=1.5)
          axis(1, at=x.ticks, labels=format(as.Date(dates.plot[x.ticks]),'%Y'), col="white",tcl=0, lwd=2, cex.axis=1.5)
        } else{
          axis(1, at=x.ticks, labels=dates.plot[x.ticks], cex.axis=1.5)
          axis(1, at=x.ticks, labels=dates.plot[x.ticks], col="white",tcl=0, lwd=2, cex.axis=1.5)
        }
        
      }
    }

    
    ## Export plotted data
    df <- data.frame(data=data.plot)
    if(!is.null(colnames(data))) colnames(df) <- colnames(data)
    res <- list(data=df, dates=dates.plot, agg=agg, season=season, aggFun=aggFun)

    
} # end plotFun.ts.orig

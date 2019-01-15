#' Plot time series of heat stress indices
#' 
#' Plot a heat stress object coming from \code{calc_heatstress.R}. 
#' 
#' @param data List with the data and meta data as a result of \code{calc_heatstress.R}.
#' @param agg Level of temporal aggregation: daily ("D", only for hourly input data), monthly ("M"), seasonal ("S"), annual ("Y"). Default: NULL (all data is plotted using the original temporal resolution). This argument is only valid for \code{type="ts"}.
#' @param aggFun Function for temporal aggregation, if desired. Possible values: mean, median, min, max. Default: mean.
#' @param season Vector with the desired month (season) to be plotted, e.g. 8 is August, c(6,7,8) is JJA, with the operation of 'aggFun'. Default: NULL (no seasonal/monthly slice considered).
#' @param heat.index Desired heat stress index to plot (only one at a time). Possible options: 'wbt', 'wbgt.shade', 'wbgt.sun', 'hi_max', 'swbgt', 'apparentTemp', 'effectiveTemp', 'humidex', 'discomInd'.
#' @param index.name Name of the desired heat stress index to plot (used for plots labels). No default.
#' @param unit Character string with the text to be added as unit of the index.
#' @param width Length of time steps to be considered for the running mean. Default: NULL (no running mean). Only valid for \code{type="ts"}.
#' @param lang Language to be used in the plot. Possible values: "G", "F", "I", "E". Default: "E".
#' @param output.data Logical, return plotted data or not. Default: FALSE.
#' @param omit.plot Logical, set to TRUE to avoid the display of the plot. Default: FALSE.
#' @param stn.together Logical, set to TRUE to plot all stations with colour lines in one plot. Default: FALSE. Only available for time series plots.
#' @param col.lines Vector with colours for lines. Only available for time series plots.
#' @return A time series plot. Plotted data if 'output.data'=TRUE.
#' @details Plot time series for the desired heat stress index, level of temporal aggregation or season/month. A running mean can be added using the argument 'width'.
#' @export
#' @import grDevices graphics 
#' @examples \dontrun{
#' # plot daily values of WBT in Geneva and Basel with a moving window
#'  plot_ts_heatstress(result, sta = c("GVE", "BAS"), heat.index = c("wbt"), width=30) 
#'  # plot monthly maximum values of WBT in Geneva and Basel
#'  plot_ts_heatstress(result, sta = c("GVE", "BAS"), agg="M", aggFun="max", heat.index = c("wbt")) 
#'  # plot July's maximum values of WBT in Geneva and Basel
#'  plot_ts_heatstress(result, sta = c("GVE", "BAS"), season=7, aggFun="max", heat.index = c("wbt")) 
#'  # plot summer maximum values of WBT in Geneva and Basel
#'  plot_ts_heatstress(result, sta = c("GVE", "BAS"), season=c(6,7,8), aggFun="max", 
#'  heat.index = c("wbt")) 
#' }
#' 


plot_ts_heatstress <-
  function (data, heat.index=NULL, index.name=NULL,  agg=NULL, aggFun="mean", season=NULL, 
            unit=unit, width=NULL, lang="E", output.data=FALSE, omit.plot=FALSE, stn.together=FALSE, col.lines=rainbow(10)) {


  
  ## Temporal resolution of original data
    if(format(data$dates[2], '%H')=="00") granul <- "D" else granul <- "H"
    if(granul=="H" & !is.null(agg)) assertthat::assert_that((agg!="M" & agg!="S" & agg!="Y"), msg= "Invalid aggregation for hourly data, only daily aggregation is available.")
    if(granul=="D" & !is.null(agg)) assertthat::assert_that((agg!="D"), msg= "Invalid aggregation for daily data")
    
  ## Prepare data to plot (temporal aggregation or not)
    if(!is.null(agg) & !is.null(season)) print("Arguments 'agg' and 'season' provided. Only 'season' will be used")

    if(is.null(agg) & is.null(season)){ # no temporal aggregation
      
      data.plot <- as.matrix(data$data)
      dates.plot <- data$dates
      if(granul=="D"){
        if(lang=="E" | lang=="G") subtitle <- paste(cat.func::upper.first(cat.lang::get.text("daily",lang)), cat.lang::get.text("series",lang), sep=" ")
        if(lang=="F" | lang=="I") subtitle <- paste(cat.func::upper.first(cat.lang::get.text("series",lang)), cat.lang::get.text("daily",lang), sep=" ")
        
      }
      if(granul=="H"){
        if(lang=="E" | lang=="G") subtitle <- paste(cat.func::upper.first(cat.lang::get.text("hourly",lang)), cat.lang::get.text("series",lang), sep=" ")
        if(lang=="F" | lang=="I") subtitle <- paste(cat.func::upper.first(cat.lang::get.text("series",lang)), cat.lang::get.text("hourly",lang), sep=" ")
      } 

    } else{ # temporal aggregation
        
        # Set 'agg' for the desired 'season'    
        if(!is.null(season)){
          if(length(season)==1){agg <- "M"} else{agg <- "S"}
        }

        # Aggregate data temporarily 
        aux.agg <- data.agg(data = data$data, dates=data$dates, agg=agg, aggFun= aggFun)
        data.plot <- aux.agg$data.agg
        dates.plot <- aux.agg$dates.agg

        # To avoid problems with one station
        data.plot <- as.matrix(data.plot)

        # Check length of plot data and dates
        assertthat::assert_that(nrow(data.plot)==length(dates.plot))
        
        # Set title for plot
        if(agg=="D"){
         agg.title <- cat.lang::get.text("daily",lang)
        } 
        if(agg=="M"){
          agg.title <- cat.lang::get.text("monthly",lang)
        }
        if(agg=="S"){
           agg.title <- cat.lang::get.text("seasonal",lang)
        }
        if(agg=="Y"){
          agg.title <- cat.lang::get.text("annual",lang)
        }

        # Filter by argument 'season'
        if(!is.null(season)){
          if(length(season)==1){
           # aux <- lapply(season, function(x) which(as.numeric(format(dates.plot,'%m')) %in% x))
            aux <- lapply(season, function(x) which(datefuns::month(dates.plot,format='%Y-%m-%d') %in% x))
            ind.time <- sort(unlist(aux))
            data.plot <- data.plot[ind.time,]
            dates.plot <- dates.plot[ind.time]
            if(length(dates.plot)==1) data.plot <- as.matrix(data.plot) else data.plot <- as.matrix(data.plot)
            
            # Build subtitle with aggFun and season
            if(lang=="E") subtitle <- paste(cat.func::upper.first(cat.lang::get.text(paste0("month.",season), lang)), cat.lang::get.text(aggFun, lang), sep=" ")
            if(lang=="G") subtitle <- paste(paste0("Monats", cat.lang::get.text(aggFun, lang)),cat.func::upper.first(cat.lang::get.text(paste0("month.",season), lang)), sep=" ")
            if(lang=="F" | lang=="I") subtitle <- paste(cat.func::upper.first(cat.lang::get.text(aggFun, lang)), cat.lang::get.text(paste0("month.",season), lang), sep=" ")
            
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
            if(lang=="E") subtitle <- paste(cat.func::upper.first(cat.lang::get.text(seasonName, lang)), cat.lang::get.text(aggFun, lang), sep=" ")
            if(lang=="G"){
              if(identical(season, c(3,4,5))) subtitle <- paste0(cat.func::upper.first(cat.lang::get.text(seasonName, lang)),'s', cat.lang::get.text(aggFun, lang)) else subtitle <- paste0(cat.func::upper.first(cat.lang::get.text(seasonName, lang)), cat.lang::get.text(aggFun, lang))
            } 
            if(lang=="F") subtitle <- paste(cat.func::upper.first(cat.lang::get.text(aggFun, lang)), cat.lang::get.text(seasonName, lang), sep=" ")
            if(lang=="I"){
              if(identical(season, c(6,7,8)) & any(match(aggFun, c("min", "max"), nomatch = FALSE))) subtitle <- paste(cat.func::upper.first(cat.lang::get.text(aggFun, lang)), "estivo", sep=" ") else subtitle <- paste(cat.func::upper.first(cat.lang::get.text(aggFun, lang)), cat.lang::get.text(seasonName, lang), sep=" ")
            } 
          }
          if(length(dates.plot)==1) data.plot <- t(as.matrix(data.plot)) else data.plot <- as.matrix(data.plot)
        } else{
          # Build subtitle with aggFun and agg
          if(lang=="E") subtitle <- paste(cat.func::upper.first(agg.title), cat.lang::get.text(aggFun, lang), sep=" ")
          if(lang=="G") {
            if(agg=="D") subtitle <- paste0("Tages", cat.lang::get.text(aggFun, lang))
            if(agg=="M") subtitle <- paste0("Monats", cat.lang::get.text(aggFun, lang))
            if(agg=="S" & any(match(aggFun, c("mean", "max", "min"), nomatch = FALSE))) subtitle <- paste("Saisonales", cat.func::upper.first(cat.lang::get.text(aggFun, lang)))
            if(agg=="S" & aggFun=="median") subtitle <- paste("Seasonaler", cat.func::upper.first(cat.lang::get.text(aggFun, lang)))
            if(agg=="Y") subtitle <- paste0("Jahres", cat.lang::get.text(aggFun, lang))
          }
          if(lang=="F" | lang=="I") subtitle <- paste(cat.func::upper.first(cat.lang::get.text(aggFun, lang)), agg.title, sep=" ")
        }
    }

   ## Omit plot or not
   if(!omit.plot){
     ## Plot stations (rows) 
      if(stn.together==TRUE){ # all in same plot
        ylims <- rep(NA,2)
        ylims[1] <- floor(min(data.plot, na.rm=T))
        ylims[2] <- ceiling(max(data.plot, na.rm=T))
        for(count.stn in 1:length(data$sta.subset)){
          plot(data.plot[,count.stn], type="o", pch=19, xlab = "",  ylab="",
               xaxt="n",  col=col.lines[count.stn], ylim=ylims, axes=FALSE, lwd=1.5)
          par(new=T)
        }
        legend("bottomright", legend=data$stn.info$station_name[match(data$sta.subset,data$stn.info$nat_abbr)], col=col.lines[1:length(data$sta.subset)], bty = "n", cex=1.2, lty=1)
        
        # plot title and subtitle
        title(main=paste0(index.name, ' ',unit,' \n'), cex.main=1.8)
        if(!is.null(season)){
          mtext(paste(subtitle," (", format(as.Date(dates.plot[1]),'%Y'),' - ', format(as.Date(dates.plot[length(dates.plot)]),'%Y'),')',sep="" ), font=3, line=0.8,side=3, cex=1.4)
        }else{
          mtext(paste(subtitle," (", dates.plot[1],' - ', dates.plot[length(dates.plot)],')',sep="" ), font=3, line=0.8,side=3, cex=1.4)
        }
        
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
        par(mfrow=c(length(data$sta.subset),1))
        for(count.stn in 1:length(data$sta.subset)){
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
          
          if(!is.null(width)){ # add moving mean
            ylims <- par("usr")[3:4]
            xlims <- par("usr")[1:2]
            par(new=T); plot(caTools::runmean(data.plot[, count.stn], width), type="l", col="red", lwd=4,
                             ylim=ylims, xlim=xlims, xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab = NULL, ylab=NULL, ann=F, axes=FALSE)
          }
          
          # plot station name
          #mtext(names(data[[count.index]][ind.stn[count.stn]]), at=c(par("usr")[1]+1.5e6,par("usr")[3]))
          mtext(data$stn.info$station_name[match(data$sta.subset[count.stn],data$stn.info$nat_abbr)], font=2,side=3,line=0,adj=.01,cex=1.2)
          
          # plot title and subtitle
          title(main=paste0(index.name, ' ',unit,' \n'), cex.main=1.8)
          if(!is.null(season)){
            mtext(paste(subtitle," (", format(as.Date(dates.plot[1]),'%Y'),' - ', format(as.Date(dates.plot[length(dates.plot)]),'%Y'),')',sep="" ), font=3, line=0.8,side=3, cex=1.4)
          }else{
            mtext(paste(subtitle," (", dates.plot[1],' - ', dates.plot[length(dates.plot)],')',sep="" ), font=3, line=0.8,side=3, cex=1.4)
          }
          
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

    # copyright and tool name
    mch.sign <- cat.lang::get.text("copyright",lang)
    mtext(mch.sign,side=3,line=0,adj=1,cex= 1.2)
    #cat.func::add.copyright(lang, inset.pos = "topright", cex=0.8)
    cat.func::add.tool.time("climdex.heat","climdex.heat", cex=1)
    
    }
    
    ## Export plotted data
    if(output.data){
      df <- data.frame(data=unname(data.plot))
      colnames(df)[1:length(data$sta.subset)] <- data$sta.subset
      res <- list(df, dates=as.POSIXct(dates.plot), stn.info=data$stn.info, agg=agg, season=season, aggFun=aggFun)
      names(res)[[1]] <- heat.index

      # Add class to create a climdex.heat object
      class(res) <- "climdex.heat"
      
      return(res)
    } 
} # end plot_ts_heatstress

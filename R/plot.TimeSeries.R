#' Wrapper function for plot.ts.orig.
#' 
#' Wrapper function for plot.ts.orig, i.e. allow saving and assertion statements.
#' 
#' @param export Logical, plot is exported to file (TRUE) or not (FALSE). Default: FALSE.
#' @param outDir Directory to export the plot. Default: working directory.
#' @param outNam Name of the exported file. Default: climdex.heat
#' @param outDev Extension of the exported file. Possible values: pdf, png. Default: pdf.
#' @param window.width Width of the plotting window in inches. Default: 7 for map, 14 for time series.
#' @param window.height Height of the plotting window in inches. Default: 5 for map, 8.5 for time series.
#' @inheritParams plot.ts.orig
#' @return A time series plot. Plotted data if 'output.data'=TRUE.
#' @details Plot time series of one vector. It is possible to plot the "raw" series or aggregate it temporarily or display a specific season/month. If the input data is a matrix, by default a separate panel is obtained for each station. 'stn.together' allows to plot all stations together (without subplot).
#' @export plot.TimeSeries
#' @import grDevices graphics 
#' @examples \dontrun{
#'# Generate some data
#'dates.day <- seq(as.POSIXct(as.Date("20000101", format='%Y%m%d'), tz="UTC"), 
#'as.POSIXct(as.Date("20171231", format='%Y%m%d'), tz="UTC"), by="day") 
#'dates.hour <-seq(as.POSIXlt("201601010000", format='%Y%m%d%H%M'), 
#'as.POSIXlt("201712312350", format='%Y%m%d%H%M'), by="hour") 
#'data.day <- array(runif(length(dates.day)*2, -5, 30), dim=c(length(dates.day),2)) 
#'data.hour <- array(runif(length(dates.hour)*2, -5, 30), dim=c(length(dates.hour),2)) 
#'# Plot original data
#'plot.TimeSeries(data.day, dates.day, title = c("Original daily data"))
#'plot.TimeSeries(data.day, dates.day, title = c("Original hourly data"), hour=T)
#'# Plot temporarily aggregated data
#'plot.TimeSeries(data.day, dates.day, title = "Yearly data", agg="Y")
#'plot.TimeSeries(data.day, dates.day, title = "Monthly data", agg="M")
#'plot.TimeSeries(data.day, dates.day, title = "Seasonal data", agg="S")
#'plot.TimeSeries(data.hour, dates.hour, hour=T, title = "Daily data", agg="D")
#'# Plot a specific season or month 
#'plot.TimeSeries(data.day, dates.day, title = "Seasonal data in summer", season = c(6,7,8))
#'plot.TimeSeries(data.day, dates.day, title = c("Monthly data in June A", "Monthly data in June B"), 
#'season = 6) # and different subtitles
#'# Plot all stations together instead of in subplot 
#'plot.TimeSeries(data.day, dates.day, title = "Monthly data in June A", season = 6, stn.together=T)
#'# Plot and save plotted data
#'res <- plot.TimeSeries(data.day, dates.day, title = "Monthly data in June", season = 6, output.data = T)
#'# Export plot to file
#'plot.TimeSeries(data.day, dates.day, title = "Monthly data in June", season = 6, export = T)
#'# Change window size
#'plot.TimeSeries(data.day, dates.day, title = "Monthly data in June", season = 6, 
#'window.width = 8, window.height = 4)
#'}


plot.TimeSeries <-
  
  function(data, dates, hour=FALSE, agg=NULL, aggFun="mean", season=NULL, 
            export=FALSE, outDir = "current", outNam = "plot1",outDev = "pdf", title=NULL, input.mch=FALSE,
            window.width=NULL, window.height=NULL, output.data=FALSE, stn.together=FALSE, col.lines=rainbow(10)){
 
    # Assertion statements
    if(!is.null(agg)) assertthat::assert_that(any(grepl(agg,c("D","M", "S","Y"))), msg = "Invalid 'agg' argument.")
    if(!is.null(season)) assertthat::assert_that(any(identical(season, c(12,1,2)) | identical(season, c(3,4,5)) |
                                                       identical(season, c(6,7,8)) | identical(season, c(9,10,11))
                                                     | sum(season %in% c(1:12))==1),  msg = "Invalid 'season' argument.")
    if(!is.null(aggFun)) assertthat::assert_that(any(grepl(aggFun,c("mean","median", "max", "min"))), msg = "Invalid 'aggFun' argument.")
    
    if(hour) granul <- "H" else granul <- "D"
    if(granul=="H" & !is.null(agg)) assertthat::assert_that(agg!="M" & agg!="S" & agg!="Y", msg= "Invalid aggregation for hourly data, only daily aggregation is available.")
    if(granul=="H") assertthat::assert_that(is.null(season), msg= "Invalid aggregation for hourly data, only daily aggregation is available.")
    if(granul=="D" & !is.null(agg)) assertthat::assert_that((agg!="D"), msg= "Invalid aggregation for daily data")
    
   
    ## Output directory and file
    if (outDir == "current") {outDir <- getwd()}
    outFil <- paste(outDir,"/",outNam,".",outDev,sep="")
    assertthat::assert_that(file.exists(outDir))
    assertthat::assert_that(any(grepl(outDev,c("pdf","png"))), msg = "Invalid 'outDev' argument.")
    

    if(is.null(window.width)) window.width=10; if(is.null(window.height)) window.height=5
 
    ## Export plot or not
    if (export){
      if(outDev=="png") png(outFil,width=window.width, height=window.height, units="in", res=300)  
      if (outDev=="pdf") pdf(outFil,width=window.width, height=window.height) 
    } else {if(options("device")=="RStudioGD") x11(width=window.width, height=window.height) else  dev.new(width=window.width, height=window.height)}
  

    ## Plot time series plot
    res <- plot.ts.orig(data, dates, hour=hour, agg=agg, aggFun=aggFun, season=season, 
                output.data=output.data, stn.together=stn.together, col.lines=col.lines, 
                title=title, input.mch=input.mch)
        
 
    if (export) dev.off()
    
    ## Export plot data
    if(output.data)  return(res)
    
  }
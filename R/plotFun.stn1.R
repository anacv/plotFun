#' Plot stations (single plot).
#' 
#' Plot a figure based on point stations, for a single plot.
#' 
#' @param x vector to be plotted. 
#' @param lon vector of longitudes. It must have the same length as x.
#' @param lat vector of latitudes. It must have the same length as x.
#' @param xlims 2-element vector defining the minimum and maximum longitud for the plotting area.
#' @param ylims 2-element vector defining the minimum and maximum latitude for the plotting area.
#' @param breaks vector of values defining the intervals to be used in the colorbar. 
#' @param palette character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.
#' @param title.main character string with the title.
#' @param cex.marker numeric value giving the marker expansion factor. Default: 1.
#' @param cex.main numeric value giving the title expansion factor. Default: 1.
#' @param mark.border logical. If black contour for the cicles is plotted. Default=TRUE.
#' @param axis.lab display or not lon/lat ticks and labels. Default=FALSE
#'
#' @author Ana Casanueva (16.02.2017)
#' 

plotFun.stn1 <- function(x, lon, lat, xlims, ylims, breaks= breaks, palette=palette, cex.marker, cex.main, title.main, axis.lab, mark.border=mark.border) {
  
  if(length(lon)== length(x) & length(lat)== length(x) & length(lon)== length(lat)){
    
    # Number of intervals to plot
    lev <- length(breaks)-1; 
    
    # Interpolate palette to the number of levels
    cols <- colorRampPalette(palette)(lev)
    
    ### The follwoing gives problems, sometimes fixed changes to unique alone! Solution below.
    ## Set the values in x to the intervals and assing colours 	
    #class <- classInt::classIntervals(x, lev,  style = "fixed", fixedBreaks=breaks)
    ##		class <- classInt::classIntervals(x, style = "fixed", fixedBreaks=breaks)
    #print(paste("Plot using style", attr(class,"style"),sep=" "))
    #colcode <- classInt::findColours(class,cols)
    #pos.colour <- findInterval(x, breaks, rightmost.closed = TRUE)
    pos.colour <- findInterval(x, breaks, all.inside = TRUE)
    colcode <- cols[pos.colour]
    
    # Plot
    maps::map(xlim=xlims, ylim=ylims, mar=c(0,0.3,2,0)) # set mar to reduce space between plots
    if(axis.lab==FALSE) maps::map.axes(labels = FALSE, tick = FALSE) # no lon,lat labels
    if(axis.lab==TRUE) maps::map.axes()
    if(mark.border==TRUE){
      points(lon,lat, cex=cex.marker, pch=21, bg=colcode, col="black") 
    } else{
      points(lon,lat, cex=cex.marker, pch=16, col=colcode)
    }
    #
    
    title(main= title.main, cex.main=cex.main, line=0)
  } else stop("Number of stations does not match the data size")
  
}


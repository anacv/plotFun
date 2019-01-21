#' Plot grid (single plot).
#' 
#' Plot a figure based on a regular grid, for a single plot.
#' 
#' @param x matrix to be plotted. 
#' @param lon array of longitudes
#' @param lat array of latitudes
#' @param lonLims 2-elemtent vector with the range of longitudes to be plotted. By default all the data is plotted.
#' @param latLims 2-elemtent vector with the range of latitudes to be plotted. By default all the data is plotted.
#' @param breaks vector of values defining the intervals to be used in the colorbar. 
#' @param palette character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.
#' @param title.main character string with the title.
#' @param cex.main numeric value giving the title expansion factor. Default: 1.
#' @param lon.points vector of longitudes for grid points to be marked in the map.
#' @param lat.points vector of latitudes for grid points to be marked in the map.
#'
#' @author Ana Casanueva (16.02.2017)
#' 

plotFun.grid1 <- function(x, lon, lat, lonLims,latLims,  breaks= breaks, palette=palette, cex.main, title.main, lon.points=lon.points, lat.points=lat.points) {
  
  # Plot subregion or not
  ind.x <- which(lon >= lonLims[1] & lon <= lonLims[2])
  ind.y <- which(lat >= latLims[1] & lat <= latLims[2])
  lon <- lon[ind.x]
  lat <- lat[ind.y]
  x <- x[ind.x, ind.y]
  
  # Number of intervals to plot
  lev <- length(breaks)-1; 
  
  # Interpolate palette to the number of levels
  cols <- colorRampPalette(palette)(lev)
  
  # set values above and below the break to the maximum and minimum for plotting.
  if(length(which(x < min(breaks)))!=0 | length(which(x > max(breaks)))!=0)	print("Warning: values out of the range have been set to maximum or minimum of the colorbar")
  x[which(x<min(breaks))] <- min(breaks)
  x[which(x>max(breaks))] <- max(breaks)
  
  # plot map
  par(mar=c(0.3,0.3,2,0)) # reduce space around plot
  image(lon, lat, x, breaks= breaks, col=cols, main=title.main, cex.main=cex.main, xaxt="n", yaxt="n", xlab=NA, ylab=NA)
  points(lon.points, lat.points, lwd=2, cex=1.5)
  map(add=T)
}


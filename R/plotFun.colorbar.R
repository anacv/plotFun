#' Plot the colorbar.
#' 
#' Plot the colorbar.
#' 
#' @param breaks vector of values defining the intervals to be used in the colorbar. 
#' @param palette character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.
#' @param unit.text character string to be placed in the colorbar with the units.
#' @param cex.unit numeric value giving the expansion factor of the units text. Default:1.
#' @param cex.textcbar numeric value giving the expansion factor of the colorbar text. Default:1.
#'
#' @author Ana Casanueva (16.02.2017)
#' 

plotFun.colorbar <- function(breaks, palette=palette, unit.text, cex.unit, cex.textcbar) {
  
  
  # noo only write some labels, but all colours!!
  
  # Number of intervals to plot
  lev <- length(breaks)-1; 
  
  # Interpolate palette to the number of levels
  cols <- colorRampPalette(palette)(lev)
  
  # Define colorbar
  col.bar <- matrix(seq(1.5,length(breaks)-0.5),nrow=1,ncol=lev) 
  
  # Plot colorbar
  image(x=1,y=seq(1.5,length(breaks)-0.5),z=col.bar,axes=F,col=cols,xlab="",ylab="")
  par(las=1) # axis labeling always horizontal
  
  # Write only 10 breaks when there are too many
  if(length(breaks)>100){
    axis(4,at=seq(1,length(breaks),100),labels=breaks[seq(1,length(breaks),100)] , cex.axis=cex.textcbar)
  }else{
    axis(4,at=(1:length(breaks)),labels=breaks, cex.axis=cex.textcbar)
  }
  mtext(text=unit.text, side=3, line=0.5, cex=cex.unit)
  box()  
}


#' Barplot with whiskers.
#' 
#' Barplot with whiskers, for instance to show mean and range of climate projections.
#' 
#' @param obs observed value.
#' @param ctl multi-model median/mean value in the historical period.
#' @param ctl.rg 2-element vector with the multi.model range (e.g. 5-95th percentiles) in the historical period.
#' @param p1 multi-model median/mean value in the short-term future period.
#' @param p1.rg 2-element vector with the multi.model range (e.g. 5-95th percentiles) in the short-term future period.
#' @param p1 multi-model median/mean value in the mid-term future period.
#' @param p1.rg 2-element vector with the multi.model range (e.g. 5-95th percentiles) in the mid-term future period.
#' @param p1 multi-model median/mean value in the long-term future period.
#' @param p1.rg 2-element vector with the multi.model range (e.g. 5-95th percentiles) in the long-term future period.
#' @param col 4-element vector with the colours for the four bars.
#' @param ylab character string for the Y-axis.
#' @param xlab 4-element vector to label the bars. 
#' @param stn.name character with the name of the station.
#' @param breaks vector of values with the Y-axis ticks.
#' @param add.lines should horizontal lines be added for reference? Default: FALSE.
#' @param lines values in the Y-axis to plot a horizontal line.
#' @param pch.obs type of marker for the observations. Default:8.
#' @param cex.obs relative size for the marker representing the observations. Default: 1.5.
#' 
#' @return A bar plot consisting on 4 bars (e.g. historical and 3 future periods) and highlight the observations with a markers. Uncertainty is shown though small segments.
#'
#' @author Ana Casanueva (22.03.2019)
#' 

plotFun.barplotScen <- function(obs, ctl, ctl.rg, p1, p1.rg, p2, p2.rg, p3, p3.rg, col, ylab=NULL, xlab=NULL, stn.name=NULL, breaks=seq(0, ceiling(p3.rg[2])), add.lines=F,lines=NULL, pch.obs=8, cex.obs=1.5){
  
  ylim <- range(breaks)
  centers <- barplot(c(ctl, p1, p2, p3), col=col, ylim=ylim, names.arg=xlab, yaxt="n", ylab=ylab, cex.lab=1.3, border="white", cex.names=1.2)
  if(add.lines){
    abline(h=lines,lty=5,lwd=0.6)
  }
  barCenters <- centers[1:length(centers)] 
  segments(barCenters, c(ctl.rg[1], p1.rg[1], p2.rg[1], p3.rg[1]), barCenters, c(ctl.rg[2], p1.rg[2], p2.rg[2], p3.rg[2]))
  arrows(barCenters, c(ctl.rg[1], p1.rg[1], p2.rg[1], p3.rg[1]), barCenters, c(ctl.rg[2], p1.rg[2], p2.rg[2], p3.rg[2]), angle = 90, code = 3, length = 0.05)
  # add marker for obs
  points(barCenters[1], obs, pch=pch.obs, cex=cex.obs, col="red")
  axis(2, at=breaks, labels=breaks, cex.axis=1.2, las=1)
  usr <- par('usr')
  text(usr[1]+((usr[2]-usr[1])*0.015), usr[4]-((usr[4]-usr[3])*0.05),stn.name, font=2, col='black', adj=0, cex=1.3)
}

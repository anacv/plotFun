#' Plot 2-dimensional kernel density plots and the histograms of the two input variables.
#'
#' @param hu: vector with dew point temperature data.
#' @param ta: vector with air temperature data.
#' @param xlim: 2-element vector with X-axis limits for the density plot. Default: range of hu.
#' @param ylim: 2-element vector with Y-axis limits for the density plot. Default: range of ta.
#' @param breaks.den: vector of breaks for the density plots. Example: breaks.den=seq(min, max, length=1000). By default, breaks.den is adjusted to the minimum and maximum kernel densities.
#' @param n.bins: number of bins for kernel density calculation. Default= 25.
#' @param add.contours: logical. Add (default) or not contours for the kernel densities.
#' @param xlab: X-axis label.
#' @param ylab: Y-axis label.
#' @param title: title above the plot.	
#' @return Plot with the 2-dimensional kernel densities and historgrams for the input variables.
#' @details The two-dimensional kernel density estimation is done with the function kde2d in package MASS.
#' @import MASS RColorBrewer
#' @author: Ana Casanueva, 13.12.2018


plot.kde2dhist <- function(hu, ta, xlim=range(hu, na.rm=T), ylim=range(ta, na.rm=T), breaks.den=NULL, title=NULL, xlab="Relative Humidity", ylab="Maximum temperature", n.bins=25, add.contours=TRUE){


	# *** Build data frame with input variables and histograms ***
	aux <- hu + ta
	hu <- hu[which(!is.na(aux))]
	ta <- ta[which(!is.na(aux))]

	x <- seq(floor(xlim[1]), ceiling(xlim[2]), length=n.bins) # refers to hu
	y <- seq(floor(ylim[1]), ceiling(ylim[2]), length=n.bins) # refers to ta

	df <- data.frame(x=hu, y =ta)
	h1 <- hist(df$x, breaks=x, plot=F)
	h2 <- hist(df$y, breaks=y, plot=F)
	top <- max(h1$counts, h2$counts)


	# *** Calculate 2D Kernel density ***
	k <- kde2d(df$x, df$y, n=n.bins, lims= c(xlim[1],xlim[2], ylim[1], ylim[2]))

	# *** Settings for density plot ***
	if(is.null(breaks.den)) breaks.den <- seq(0, max(k$z), length=1000) 
	k$z[k$z > max(breaks.den)] <- max(breaks.den) # otherwise values >maximum are plotted in white.
	my.palette <- rev(brewer.pal(n = 11, name = "Spectral"))
	lev <- length(breaks.den)-1;
	cols <- colorRampPalette(my.palette)(lev)	


	# *** Begin plot ***
	dev.new(width=7, height=6)
	layout(rbind(
	c(0,7,0,0),
	c(0,2,0,0),
	c(4,1,3,6),
	c(0,5,0,0)),
	heights=c(0.05,0.25,0.55,0.1),
	widths=c(0.08,0.53,0.25,0.15))

	# plot the 2-D density
	par(mar=c(3,3,1,1))
	image(k, breaks=breaks.den, col=cols, las=1, cex.axis=1.5) #plot the image

	# add contours
	if(add.contours) contour(k, levels=round(breaks.den[seq(1,length(breaks.den),100)],4), add=TRUE) # same values as in the colorbar for the densities

	# add histograms on top and right
	par(mar=c(0,2,1,0))
	plot(h1, xlim=xlim, col="blue", xaxt = "n", yaxt = "n", main=NA)
	par(mar=c(2,0,0.5,1))
	plot(0,0, type = "n", xlim = c(0, top), ylim = ylim, bty = "n", xaxt = "n", yaxt = "n")
	rect(0,h2$breaks[1:(length(h2$breaks) - 1)], h2$counts, h2$breaks[2:length(h2$breaks)], col="red")	

	# add labels for the axis
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.6,0.5,ylab, srt=90,cex=1.5)
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.55,0.5,xlab, cex=1.5)

	# add color bar
	par(mar=c(3,0.6,3,4.5))
	plot.colorbar(round(breaks.den,4), palette=cols, " ", 1, 1.3)

	# plot title
	par(mar=c(0.1,2,0.2,0.5))
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.5,0.5,title,font=2,cex=1.5)

}



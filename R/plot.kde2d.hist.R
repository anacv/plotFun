#' Plot 2-dimensional kernel density plots and the histograms of the two input variables.
#'
#' Plot 2-dimensional kernel density plots and the histograms of the two input variables.
#'
#' @param var1 vector with data for one variable.
#' @param var2 vector with data for another variable.
#' @param xlim 2-element vector with X-axis limits for the density plot. Default: range of var1.
#' @param ylim 2-element vector with Y-axis limits for the density plot. Default: range of var2.
#' @param breaks.den vector of breaks for the density plots. Example: breaks.den=seq(min, max, length=1000). By default, breaks.den is adjusted to the minimum and maximum kernel densities.
#' @param n.bins number of bins for kernel density calculation. Default= 25.
#' @param add.contours logical. Add (default) or not contours for the kernel densities.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param title title above the plot.	
#' @param cex.main Relative size of the plot title. Default:1.5.
#' @return Plot with the 2-dimensional kernel densities and historgrams for the input variables.
#' @details The two-dimensional kernel density estimation is done with the function kde2d in package MASS.
#' @import MASS RColorBrewer
#' @export plot.kde2d.hist
#' @author Ana Casanueva, 13.12.2018
#' @examples \dontrun{
#' # Generate data
#' tas <- rnorm(1000, mean=15, sd=2)
#' td <- rnorm(1000, mean=8, sd=1)
#' # Plot "-Dim Kernel density with histograms
#' plot.kde2d.hist(var1=td, var2=tas, xlab="Dew point temp.", ylab="Air temperature", 
#' n.bins=25, add.contours=FALSE, title="2D density plot", cex.main=2)
#' # add contours for the density values
#' plot.kde2d.hist(var1=td, var2=tas, xlab="Dew point temp.", ylab="Air temperature", 
#' n.bins=25, add.contours=TRUE, title="2D density plot with contours", cex.main=2)
#' }

plot.kde2d.hist <- function(var1, var2, xlim=range(var1, na.rm=T), ylim=range(var2, na.rm=T), breaks.den=NULL, title=NULL, cex.main=1.5, xlab=NULL, ylab=NULL, n.bins=25, add.contours=TRUE){


	# *** Build data frame with input variables and histograms ***
	aux <- var1 + var2
	var1 <- var1[which(!is.na(aux))]
	var2 <- var2[which(!is.na(aux))]

	x <- seq(floor(xlim[1]), ceiling(xlim[2]), length=n.bins) # refers to var1
	y <- seq(floor(ylim[1]), ceiling(ylim[2]), length=n.bins) # refers to var2

	df <- data.frame(x=var1, y =var2)
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
	text(0.5,0.5,title,font=2,cex=cex.main)

}



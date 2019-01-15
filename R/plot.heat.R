#' Plot heat index as a function of the input variables.
#'
#' @param hu: vector with relative humidity data.
#' @param ta: vector with air temperature data.
#' @heat.index: heat index to plot as a function of dew point and air temperature. Available: wbt, swbgt, hi.
#' @param xlim: 2-element vector with X-axis limits for the density plot. Default: range of hu.
#' @param ylim: 2-element vector with Y-axis limits for the density plot. Default: range of ta.
#' @param breaks.index: vector of breaks for the heat index values. Example: breaks.den=seq(min, max, length=1000). By default, breaks is adjusted to the minimum and maximum values of the index.
#' @param n.bins: number of bins for kernel density calculation. Default= 500
#' @param add.contours: logical. Add (default) or not contours for heat index values.
#' @param add.points: logical. Add (default) or not points with the eactual time series.
#' @param xlab: X-axis label.
#' @param ylab: Y-axis label.
#' @param title: title above the plot.	
#' @return Plot with the heat index values as a function of the input variables.
#' @details Needed packages: HeatStress, RColorBrewer.
#' @author: Ana Casanueva, 13.12.2018



plot.heat <- function(hu, ta, xlim=range(hu, na.rm=T), ylim=range(ta, na.rm=T), heat.index=NULL,  breaks.index=NULL, title=NULL, xlab="Relative Humidity", ylab="Maximum temperature", n.bins=500, add.points=TRUE, add.contours=TRUE){



	library("HeatStress")
	library("RColorBrewer")

	if(n.bins>100) message("Patience: number of bins is ", n.bins)

	# *** Prepare data to plot ***
	x <- seq(floor(xlim[1]), ceiling(xlim[2]), length=n.bins) # refers to hu
	y <- seq(floor(ylim[1]), ceiling(ylim[2]), length=n.bins) # refers to ta

	mat <- matrix(NA, ncol=length(x), nrow=length(y))
	for(i in 1:length(y)){
		for(j in 1:length(x)){
			if(heat.index=="wbt") mat[i,j]<- wbt.Stull(y[i], x[j])
			if(heat.index=="swbgt") mat[i,j]<- swbgt(y[i], x[j])
			if(heat.index=="hi") mat[i,j]<- hi(y[i], x[j])

		}
	}
	df <- list(x=x, y =y, z=mat)

	# *** Plot settings ***
	if(is.null(breaks.index)) breaks.index <- seq(floor(min(mat, na.rm=T)), ceiling(max(mat, na.rm=T)))
	my.palette <- rev(brewer.pal(n = 11, name = "Spectral"))
	lev <- length(breaks.index)-1;
	cols <- colorRampPalette(my.palette)(lev)

	# *** Begin plot ***
	dev.new(width=6, height=6)
	layout(rbind(
	c(2,1,4),
	c(0,3,0)),
	heights=c(0.9,0.05),
	widths=c(0.08,0.78,0.15))

	df$z[df$z>max(breaks.index)] <- max(breaks.index) # otherwise values below or above are not plotted
	df$z[df$z<min(breaks.index)] <- min(breaks.index)
	image(df, breaks=breaks.index, col=cols, las=1, cex.axis=1.5, main=title, cex.main=1.5)

	if(add.contours) contour(df, levels=breaks.index, add=T, col="slategrey", xlim= xlim, ylim=ylim, xaxs="i", yaxs="i" )
	if(add.points)	points(hu, ta, pch=21, xaxs="i", yaxs="i", xlim= xlim, ylim=ylim, las=1, cex=1)

	# add labels for the axis
	par(mar=c(0.1,2,0.2,0.5))
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.6,0.5,ylab, srt=90,cex=1.5)
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.55,0.5,xlab, cex=1.5)

	# add color bar
	par(mar=c(3,0.6,3,4.5))
	plot.colorbar(round(breaks.index,4), palette=cols, "\260C", 1, 1.3)

}


#' Plot the colorbar.
#' 
#' Plot the colorbar.
#' 
#' @param breaks: vector of values defining the intervals to be used in the colorbar. 
#' @param palette: character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.
#' @param unit.text: character string to be placed in the colorbar with the units.
#' @param cex.unit: numeric value giving the expansion factor of the units text. Default:1.
#' @param cex.textcbar: numeric value giving the expansion factor of the colorbar text. Default:1.
#'
#' @author Ana Casanueva (16.02.2017)
#' 

plot.colorbar <- function(breaks, palette=palette, unit.text, cex.unit, cex.textcbar) {

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
		axis(4,at=seq(1,length(breaks), 100),lab=round(breaks[seq(1,length(breaks),100)],4) , cex.axis=cex.textcbar)
	}else{
		axis(4,at=(1:length(breaks)),lab=breaks, cex.axis=cex.textcbar)
	}

	mtext(text=unit.text, side=3, line=0.5, cex=cex.unit)
	box()  
}



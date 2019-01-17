#' Plot heat index as a function of the input variables.
#' 
#' The index is calculated with the R package \code{HeatStress}.
#'
#' @param hu vector with relative humidity data. \code{hu} or \code{td} are mandatory.
#' @param td vector with dew point temperature data.\code{hu} or \code{td} are mandatory.
#' @param ta vector with air temperature data.
#' @param heat.index heat index to plot as a function of dew point/relative humidity and air temperature. Available (based on 2 variables): swbgt, hi, wbt.Stull, wbgt.Bernard, apparentTemp, effectiveTemp, humidex, discomInd.
#' @param xlim 2-element vector with X-axis limits for the density plot. Default: range of hu/td.
#' @param ylim 2-element vector with Y-axis limits for the density plot. Default: range of ta.
#' @param breaks.index vector of breaks for the heat index values. By default, breaks is adjusted to the minimum and maximum values of the index.
#' @param n.bins number of bins for kernel density calculation. Default= 500
#' @param add.contours logical. Add (default) or not contours for heat index values.
#' @param add.points logical. Add (default) or not points with the actual time series.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param title title above the plot.
#' @param unit.text	character string to be placed in the colorbar with the units.
#' @param cex.main Relative size of the plot title. Default:1.5.
#' @param cex.unit numeric value giving the expansion factor of the units text. Default:1.
#' @param cex.textcbar numeric value giving the expansion factor of the colorbar text. Default:1.3.
#' @return Plot with the heat index values as a function of the input variables.
#' @details The two input variables of the desired index need to be provided, tas and either td or hu. Needed packages: HeatStress, RColorBrewer.
#' @import HeatStress RColorBrewer
#' @author Ana Casanueva, 13.12.2018
#' @export
#' @examples \dontrun{
#' # Generate data
#' tas <- rnorm(150, mean=15, sd=2)
#' dew <- rnorm(150, mean=8, sd=1)
#' hurs <- sample(1:100,150, replace=T)
#' # Plot the heat plot
#' plot.heat(hu=hurs, ta=tas, heat.index="wbt", title="Heat stress plot", cex.main=1.5, 
#' xlab="Relative Humidity", ylab="Air temp.", n.bins=500)
#' # Add points, change index
#' plot.heat(hu=hurs, ta=tas, heat.index="swbgt", title="Heat stress plot", cex.main=1.5, 
#' xlab="Relative Humidity", ylab="Air temp.", n.bins=500, add.points=F)
#' # Add contours, change index
#' plot.heat(td=dew, ta=tas, heat.index="wbgt.shade", title="Heat stress plot", cex.main=1.5, 
#' xlab="Dew point temp.", ylab="Air temp.", n.bins=500, add.contours=F)
#' }




plot.heat <- function(hu=NULL, td=NULL, ta, xlim=NULL, ylim=range(ta, na.rm=T), heat.index=NULL,  breaks.index=NULL, title=NULL, cex.main=1.5, xlab=NULL, ylab=NULL, n.bins=500, add.points=TRUE, add.contours=TRUE, unit.text=NULL, cex.unit=1, cex.textcbar=1.3){

 
	if(n.bins>100) message("Patience: number of bins is ", n.bins)

  assertthat::assert_that(any(match(heat.index, c("wbt", "wbgt.shade", "hi", "swbgt", "humidex", "discomInd","apparentTemp","effectiveTemp"), nomatch=FALSE)), msg = "Check heat index name.")
  
  assertthat::assert_that((!is.null(hu) | !is.null(td)), msg="td or hu is mandatory")
  if(any(match(heat.index, c('swbgt', 'hi', 'wbt', 'apparentTemp', 'effectiveTemp', 'humidex', 'discomInd')), na.rm=T)) assertthat::assert_that(!is.null(hu), msg="hu is missing")
  if(match(heat.index, 'wbgt.shade', nomatch = F)) assertthat::assert_that(!is.null(td), msg="td is missing")
  
  if(is.null(unit.text)){
    if(any(match(heat.index, c('swbgt', 'wbgt.shade', 'wbt', 'apparentTemp', 'effectiveTemp', 'humidex', 'discomInd')), na.rm=T)) unit.text <- "\260C"
    if(match(heat.index, 'hi', nomatch = F)) unit.text <- "\260F"
  }

  if(!is.null(td)) xvar <- td
  if(!is.null(hu)) xvar <- hu
  xlim <- range(xvar, na.rm=T)
 
	# *** Prepare data to plot ***
	x <- seq(floor(xlim[1]), ceiling(xlim[2]), length=n.bins) # refers to hu/td
	y <- seq(floor(ylim[1]), ceiling(ylim[2]), length=n.bins) # refers to ta

	mat <- matrix(NA, ncol=length(x), nrow=length(y))
	for(i in 1:length(y)){
		for(j in 1:length(x)){
			if(heat.index=="wbt") mat[i,j]<- wbt.Stull(y[i], x[j])
			if(heat.index=="wbgt.shade") mat[i,j]<- wbgt.Bernard(y[i], x[j])$data
			if(heat.index=="apparentTemp") mat[i,j]<- apparentTemp(y[i], x[j])
			if(heat.index=="effectiveTemp") mat[i,j]<- effectiveTemp(y[i], x[j])
			if(heat.index=="humidex") mat[i,j]<- humidex(y[i], x[j])
			if(heat.index=="discomInd") mat[i,j]<- discomInd(y[i], x[j])
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
	image(df, breaks=breaks.index, col=cols, las=1, cex.axis=1.5, main=title, cex.main=cex.main)

	if(add.contours) contour(df, levels=breaks.index, add=T, col="slategrey", xlim= xlim, ylim=ylim, xaxs="i", yaxs="i" )
	if(add.points)	points(xvar, ta, pch=21, xaxs="i", yaxs="i", xlim= xlim, ylim=ylim, las=1, cex=1)

	# add labels for the axis
	par(mar=c(0.1,2,0.2,0.5))
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.6,0.5,ylab, srt=90,cex=1.5)
	plot(c(0,1),c(0,1),type="n",axes=F, ann=F)
	text(0.55,0.5,xlab, cex=1.5)

	# add color bar
	par(mar=c(3,0.6,3,4.5))
	plot.colorbar(round(breaks.index,4), palette=cols, unit.text, cex.unit, cex.textcbar)

}




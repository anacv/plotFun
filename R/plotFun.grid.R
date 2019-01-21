#' Plot gridded map.
#' 
#' Plot a gridded map, either a single plot or multiplot. A common colorbar is set for a multiplot.
#' 
#' @param data matrix or list of matrices to be plotted. When it is a list, a multiplot is generated (each element of the list is plotted in a subplot).
#' @param lon vector longitudes in strictly ascending order.
#' @param lat vector latitudes in strictly ascending order.
#' @param lonLims 2-elemtent vector with the range of longitudes to be plotted. By default all the data is plotted.
#' @param latLims 2-elemtent vector with the range of latitudes to be plotted. By default all the data is plotted.
#' @param lattice in multiple plots, 2-element vector giving the number of rows and columns to display the multiplot.
#' @param breaks vector of values defining the intervals to be used in the colorbar. 
#' @param palette character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.
#' @param window.width sets the width of the device window. Modify this argument to improve the appearance. Default:8.
#' @param window.height sets the height of the device window.  Modify this argument to improve the appearance. Default:6.
#' @param title.main character string with the main title.
#' @param title.single (optional): in multiple plots, a character vector with the individual titles for single plots.
#' @param unit.text character string to be placed in the colorbar with the units.
#' @param cex.main numeric value giving the title expansion factor. Default: 1.
#' @param cex.single (optional): in multiple plots, numeric value giving the title expansion factor. Default: 1.
#' @param cex.textcbar numeric value giving the expansion factor of the text in the colorbar. Default:1.
#' @param cex.unit numeric value giving the expansion factor of the units text. Default:1.
#' @param width.cbar numeric value giving the fraction of the plot (from 0 to 1) reserved for the colorbar. Modify this argument for a better appearance. Default=0.1
#' @param disp.warnings logical to set if warnings shoul be displayed.
#' @param export logical. If TRUE, the figure is exported as pdf file. Default: False.
#' @param export.path path (including file name) to save the plot.
#' @param export.format "png" or "pdf" format.
#' @param lon.points vector of longitudes for grid points to be marked in the map.
#' @param lat.points vector of latitudes for grid points to be marked in the map.
#' @export plotFun.grid
#' @details Packages classInt and maps needed.
#' @import classInt maps
#' @author Ana Casanueva (24.05.2017)
#' @examples \dontrun{
#' # Generate data
#' x <- seq(-5, 20); m= length(x)
#' y <- seq(35,55); n=length(y)
#' tas <- array(runif(n*m*6, -5, 30), dim=c(6,m,n)) # time, lon lat.
#' # Single plot
#' plotFun.grid(tas[1,,], lon=x, lat = y, lonLims = c(-10,30), latLims = c(35,65), width.cbar = 0.2, 
#' breaks = seq(10,20), title.main = "Mean Temp.", unit.text = "degC", cex.main=1.5)
#' # Multi-plots
#' plotFun.grid(list(tas[1,,], tas[2,,], tas[3,,], tas[4,,], tas[5,,], tas[6,,]), 
#' lon=x, lat = y, lonLims = c(-10,30), latLims = c(35,65), lattice=c(2,3), width.cbar = 0.1, 
#' breaks = seq(10,20), window.height = 8, window.width = 12, 
#' title.single = c("Temp1","Temp2","Temp3","Temp4","Temp5","Temp6"), cex.single = 1.3, 
#' title.main = "Station plot - Multiplot", cex.main = 2, unit.text = "degC")
#' }



plotFun.grid <- function(data, lon, lat, lonLims=c(min(lon),max(lon)), latLims=c(min(lat),max(lat)), lattice=NULL, breaks= NULL, palette=rainbow(10), window.width=8, window.height=6, title.main=NULL, title.single=NULL,  unit.text=NULL, cex.main=1,  cex.single=1, cex.unit=1, cex.textcbar=1, width.cbar=0.1, disp.warnings=FALSE, export=FALSE, export.format=NULL, export.path=NULL, lon.points=NA, lat.points=NA){

	# Verification checks 
	try(if(export & is.null(export.format)) stop("Cannot save plot: missing export.format"))
	try(if(export & is.null(export.path)) stop("Cannot save plot: missing export.path"))
	# Export figure or not
	if (export & !is.null(export.format) & !is.null(export.path)){
		if(export.format=="png") png(export.path,width=window.width, height=window.height, units="in", res=300)  
		if (export.format=="pdf") pdf(export.path,width=window.width, height=window.height) 
	} else {if(options("device")=="RStudioGD") x11(width=window.width, height=window.height) else  dev.new(width=window.width, height=window.height)}

	# Display warning messages or not
	if(!disp.warnings) options(warn=-1)

	# Make single plot or multiplot according to data dimensions
	if(is.matrix(data)){

		# Build the layout for a single plot
		layout(rbind(c(1,2),c(1,2)), widths=c(0.8,0.2)) # plot (0.8) and colorbar (0.2)

		# Set breaks if none
		if(is.null(breaks)) breaks <- set.Breaks(data)
		
		# Plot
		plotFun.grid1(data, lon, lat, lonLims, latLims, breaks, palette, cex.main, title.main, lon.points=lon.points, lat.points=lat.points)

		# Include colorbar
		par(mar=c(3,0.6,3,5))
		plotFun.colorbar(breaks, palette, unit.text, cex.unit, cex.textcbar)

	} else{
		# Number of real plots to make
		n <- length(data) 

		# Build the layout depending of the number of plots (and leave space for colorbar)
		if(is.vector(lattice)){ 
			nrow <- lattice[1]
			ncol <- lattice[2]
			n2 <- nrow*ncol
			matrix <- cbind(matrix(seq(1,n2), nrow=nrow, ncol=ncol, byrow=T), rep(n2+1,nrow))
		} else{
			# If no lattice is given, a plot with 2, 3 or 4 rows is performed.
			if ((n %% 2)!=0 & (n %% 3)!=0 & (n %% 4)!=0) n <- n+1

			if ((n %% 4)==0) {matrix <- cbind(matrix(seq(1,n), nrow=4, byrow=T), rep(n+1,4))
			} else if ((n %% 3)==0) {matrix <- cbind(matrix(seq(1,n), nrow=3, byrow=T), rep(n+1,3))
			} else if ((n %% 2)==0) {matrix <- cbind(matrix(seq(1,n), nrow=2, byrow=T), rep(n+1,2))
			}	 
		}

		# Set column width
		width <- (1-width.cbar)/(ncol(matrix)-1) 
		layout(matrix, widths=c(rep(width,(ncol(matrix)-1)),width.cbar))

		# Leave room for common title at the top (if needed)
		if(!is.null(title.main)) par(oma = c(0, 0, 3, 0)) 

		# Set breaks if none
		if(is.null(breaks)) breaks <- set.Breaks(data[[1]])

		# Plot subplots
		for (i in 1:n){
			plotFun.grid1(data[[i]], lon, lat, lonLims, latLims, breaks, palette, cex.single, title.single[i], lon.points=lon.points, lat.points=lat.points)
		}

		# Fill with empty plots when necessary (i.e. to fill the matrix)
		rest <- 0
		if(length(data) != nrow(matrix)*(ncol(matrix)-1)) rest <-(nrow(matrix)*(ncol(matrix)-1)) %% length(data)
		if(rest>=1){
			for(r in 1:rest){
				plot(1, type="n", axes=F, ann=F)
			}
		}

		# Include colorbar
		par(mar=c(3,0.6,3,4))
		plotFun.colorbar(breaks, palette, unit.text, cex.unit, cex.textcbar)
		# Plot title
		mtext(title.main, outer = TRUE, cex = cex.main)
	}

	if (export & !is.null(export.format) & !is.null(export.path)) dev.off()
}


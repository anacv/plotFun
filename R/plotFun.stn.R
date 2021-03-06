#' Plot map with stations.
#' 
#' Plot a map with point stations, either a single plot or multiplot. A common colorbar is set for a multiplot.
#' 
#' @param data vector or matrix to be plotted. When it is a matrix, a multiplot is generated (each row of data is plotted in a subplot).
#' @param lon vector of longitudes. It must have the same length as data (or number of columns in data).
#' @param lat vector of latitudes. It must have the same length as data (or number of columns in data).
#' @param xlims 2-element vector defining the minimum and maximum longitud for the plotting area.
#' @param ylims 2-element vector defining the minimum and maximum latitude for the plotting area.
#' @param lattice in multiple plots, 2-element vector giving the number of rows and columns to display the multiplot.
#' @param breaks vector of values defining the intervals to be used in the colorbar. 
#' @param palette character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.
#' @param window.width sets the width of the device window. Modify this argument to improve the appearance. Default:8.
#' @param window.height sets the height of the device window.  Modify this argument to improve the appearance. Default:6.
#' @param title.main character string with the main title.
#' @param title.single (optional): in multiple plots, a character vector with the individual titles for single plots.
#' @param unit.text character string to be placed in the colorbar with the units.
#' @param mark.border logical. If black contour for the cicles is plotted. Default=TRUE.
#' @param cex.marker numeric value giving the marker expansion factor. Default: 1.
#' @param cex.main numeric value giving the title expansion factor. Default: 1.
#' @param cex.single (optional): in multiple plots, numeric value giving the title expansion factor. Default: 1.
#' @param cex.textcbar numeric value giving the expansion factor of the text in the colorbar. Default:1.
#' @param cex.unit numeric value giving the expansion factor of the units text. Default:1.
#' @param width.cbar numeric value giving the fraction of the plot (from 0 to 1) reserved for the colorbar. Modify this argument for a better appearance. Default=0.1
#' @param axis.lab display or not lon/lat ticks and labels. Default=FALSE
#' @param text display some text in each point. Length should be the same as lat and lon.
#' @param cex.text numeric value giving the text expansion factor. Default: 1.
#' @param map.fill logical. If map is filled with colour in `map.col'. Default: FALSE
#' @param map.col Colour for the map (continent). Default: grey89.
#' @param disp.warnings logical to set if warnings shoul be displayed.
#' @param export logical. If TRUE, the figure is exported as pdf file. Default: False.
#' @param export.path path (including file name) to save the plot.
#' @param export.format "png" or "pdf" format.
#' @export plotFun.stn
#' @details Packages classInt and maps needed.
#' @import classInt maps 
#' @author Ana Casanueva (17.02.2017)
#' @examples \dontrun{
#' # Generate data
#' tas <- cbind(rnorm(10, mean=15, sd=2), rnorm(10, mean=20, sd=1.5))
#' longitude = c(6,-5)
#' latitude = c(46, 40)
#' # Single plot
#' plotFun.stn(apply(tas,2, mean, na.rm=T), lon=longitude, lat = latitude, xlims = c(-10,30), 
#' ylims = c(35,65), width.cbar = 0.2, breaks = seq(10,20), title.main = "Mean Temp.", 
#' unit.text = "degC", cex.main=1.5, cex.marker = 2)
#' # Multi-plots
#' plotFun.stn(tas[1:2,], lon=longitude, lat = latitude, xlims = c(-10,30), ylims = c(35,65), 
#' width.cbar = 0.15, breaks = seq(10,20), window.height = 10, window.width = 6, 
#' title.single = c("Temp1","Temp2"), cex.single = 1.3, title.main = "Station plot", cex.main = 2, 
#' unit.text = "degC")
#' }

plotFun.stn <- function(data, lon, lat, xlims, ylims, lattice=NULL, breaks= NULL, 
                        palette=rainbow(10), window.width=8, window.height=6,  title.main=NULL, 
                        title.single=NULL,  unit.text=NULL, mark.border=TRUE, cex.marker=1, 
                        cex.main=1,  cex.single=1, cex.unit=1, cex.textcbar=1, width.cbar=0.1, 
                        disp.warnings=FALSE, axis.lab=FALSE, export=FALSE, export.format=NULL, 
                        export.path=NULL, text=NULL, cex.text=1, map.fill=FALSE, map.col="grey89"){

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
	if(is.vector(data)){

		# Build the layout for a single plot
		layout(rbind(c(1,2),c(1,2)), widths=c(0.8,0.2)) # plot (0.8) and colorbar (0.2)

		# Set breaks if none
		if(is.null(breaks)) breaks <- set.Breaks(data)
		
		# Plot
		plotFun.stn1(data, lon, lat, xlims, ylims, breaks, palette, cex.marker, cex.main, title.main, axis.lab, mark.border=mark.border, text=text, cex.text=cex.text, map.fill=map.fill, map.col=map.col)

		# Include colorbar
		par(mar=c(3,0.6,3,5))
		plotFun.colorbar(breaks, palette, unit.text, cex.unit, cex.textcbar)

	} else{
		# Number of real plots to make
		n <- nrow(data) 

		# Build the layout depending of the number of plots (and leave space for colorbar)
		if(is.vector(lattice)){ 
			nrow <- lattice[1]
			ncol <- lattice[2]
			n2 <- nrow*ncol
			matrix <- cbind(matrix(seq(1,n2), nrow=nrow, ncol=ncol, byrow=T), rep(n2+1,nrow))
		} else{
			# If no lattice is given, a plot with 2, 3 or 4 rows is performed.
		  if(n==1) {matrix <- t(as.matrix(c(1,2)))
		  } else if ((n %% 2)!=0 & (n %% 3)!=0 & (n %% 4)!=0) n <- n+1

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
		if(is.null(breaks)) breaks <- set.Breaks(data)

		# Plot subplots
		for (i in 1:nrow(data)){
			plotFun.stn1(data[i,], lon, lat, xlims, ylims, breaks, palette, cex.marker, cex.single, title.single[i], axis.lab, mark.border=mark.border, text=text, cex.text=cex.text)
		}

		# Fill with empty plots when necessary (i.e. to fill the matrix)
		rest <- 0
		if(nrow(data) != nrow(matrix)*(ncol(matrix)-1)) rest <-(nrow(matrix)*(ncol(matrix)-1)) %% nrow(data)
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


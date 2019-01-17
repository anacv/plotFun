#' Sort data.
#' 
#' Sort data is ascending order and coordinates accordingly, in order to plot highest values on top.
#' 
#' @param data vector to be plotted. 
#' @param lon vector of longitudes. It must have the same length as data .
#' @param lat vector of latitudes. It must have the same length as data .
#' @return A list of: data, lon, lat, indices with new order.
#'
#' @author Ana Casanueva (12.12.2017)
#' 

sort4plot <- function(data, lon, lat){

	# new data with NAs and sorted data
	plot.data <- rep(NA, length(data))
	all <- seq(1,length(data))
	
	# look for NAs
	ind.na <- which(is.na(data))

	if(length(ind.na)>0){

		# sort no NA data
		aux.sort <- sort(data[-ind.na], index.return=TRUE)

		# place sorted data
		plot.data[(length(ind.na)+1):(length(ind.na)+length(aux.sort$x))] <- aux.sort$x

		# indices assigned to new order
		ind <- c(ind.na, all[-ind.na][aux.sort$ix])

	} else{
		# sort data
		aux.sort <- sort(data, index.return=TRUE)

		# place sorted data
		plot.data <- aux.sort$x

		# indices assigned to new order
		ind <- aux.sort$ix

	}

	# new coordinates 
	lon.new <- lon[ind]
	lat.new <- lat[ind]

	result <- list(data=plot.data, lon=lon.new, lat=lat.new, indices=ind)
	return(result)
}

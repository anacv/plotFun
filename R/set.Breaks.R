#' Get breaks for plot.
#' 
#' Get breaks for plot based on the data interval.
#' 
#' @param data Vector of matrix from which the breaks are desired.
#' @return A vector with breaks for plot.

set.Breaks <- function(data){
  
  min <- floor(min(data, na.rm=T))
  max <- ceiling(max(data, na.rm=T))
  inc <- (max-min)/10
  breaks <- round(seq(min, max,inc),2)
  
  return(breaks)
}

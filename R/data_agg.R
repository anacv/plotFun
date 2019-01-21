#' Temporal aggregation of daily or hourly data.
#' 
#' Temporal aggregation of data with different possible aggregation functions.#'
#'
#' @param data Matrix with the data to be aggregated (time steps in rows, stations in columns).
#' @param dates Vector of dates (POSIX class) of the original data.
#' @param agg Level of temporal aggregation: daily ("D", only for hourly input data), monthly ("M"), seasonal ("S"), annual ("Y"). 
#' @param aggFun Function for temporal aggregation. Possible values: mean, median, min, max. Default: mean.
#' @param input.mch logical. If the input data is from DWH, the hourly needs to be aggregated differently. Default: FALSE
#' @return A list with aggregated data and dates.
#' @details The result is a multi-year series. The input series is first filled with NA to have full years, i.e. the result will have NAs for the season/month with no data at all. If a season/month has data but is not complete, a warning is shown.
#' @export
#' @examples \dontrun{
#' # Generate some data
#' dates.day <- seq(as.POSIXct(as.Date("20160101", format='%Y%m%d'), tz="UTC"), 
#' as.POSIXct(as.Date("20171231", format='%Y%m%d'), tz="UTC"), by="day") 
#' dates.hour <-seq(as.POSIXlt("201601010000", format='%Y%m%d%H%M'), 
#' as.POSIXlt("201712312350", format='%Y%m%d%H%M'), by="hour") 
#' data.day <- array(runif(length(dates.day)*2, -5, 30), dim=c(length(dates.day),2)) 
#' data.hour <- rnorm(length(dates.hour), mean=15, sd=2)
#' # Aggregate from daily to annual
#' data_agg(data.day, dates.day, agg="Y", aggFun="mean") 
#' # Aggregate from daily to monthly
#' data_agg(data.day, dates.day, agg="M", aggFun="max") 
#' # Aggregate from daily to seasonal
#' data_agg(data.day, dates.day, agg="S", aggFun="min") 
#' # Aggregate from hourly to daily
#' data_agg(data.hour, dates.hour, agg="D", aggFun="mean") 
#' }
 

data_agg <- function(data, dates, agg=NULL, aggFun="mean", input.mch=FALSE){
 

  data <- as.matrix(data) # to avoid problems with 1 station (vector)
  if(!is.null(colnames(data))) stn.name <- colnames(data)
  
  # Check length of plot data and dates
  assertthat::assert_that(nrow(data)==length(dates), msg="Dates and data length do not match")
  
  # Filter by years first
  years <- as.numeric(unique(format(dates, '%Y')))
  ind.year <- lapply(years, function(x) which(as.numeric(format(dates,'%Y')) %in% x))

    # New aggregated dates
  if(agg=="D"){
    dates.agg.full <- unique(format(dates, '%Y-%m-%d'))
    if(input.mch & format(dates[length(dates)], '%H')=="00" & as.numeric(format(dates[length(dates)], '%Y'))>=2018){
      dates.agg <- unique(format(dates[1:(length(dates)-1)], '%Y-%m-%d'))# -1 because the last time step (00:00) is used in the aggregation of the last full day
      print("The aggregation is done from 01 of day D to 00 of day D+1")
    }else{
      dates.agg <- unique(format(dates, '%Y-%m-%d'))
      print("The aggregation is done from 00 to 23 of day D")
    }
  } 
  if(agg=="Y") dates.agg <- unique(format(dates, '%Y'))
  if(agg=="M") dates.agg <- as.character(seq(as.Date(dates[1]), as.Date(format(dates[length(dates)], format= "%Y-12-31")), by="month"))
  if(agg=="S") dates.agg <- as.character(seq(as.Date(dates[1]), as.Date(format(dates[length(dates)], format= "%Y-12-31")), by="quarter"))

  # Aggregate to daily (only from hourly)
  if(agg=="D"){
    data.agg <- matrix(NA, nrow=length(dates.agg), ncol=ncol(data))
    for (i in 1:(length(dates.agg))){ 
      # In DWH hours to days are aggregated differently before and after 2018!!
      if(as.numeric(format(dates[length(dates)], '%Y'))>=2018){
        ind.day.d <- which(format(dates,"%Y-%m-%d")==dates.agg.full[i] & as.numeric(format(dates,"%H"))>=1)
        ind.day.next <- which(format(dates,"%Y-%m-%d")==dates.agg.full[i+1] & as.numeric(format(dates,"%H"))==0) # The aggregation in the DWH for a day D is from 01:00 os day D to 00:00 of day D+1, therefore +1 timestep
        ind.day <- c(ind.day.d, ind.day.next)
        
      }else{
        ind.day <- which(format(dates,"%Y-%m-%d")==dates.agg[i]) # it would take positions from data from 00 to 23 od day D
      }
      #if(sum(ind.day>length(dates))) {remove <- which(ind.day>length(dates)); ind.day <- ind.day[-remove]}

      for(j in 1:ncol(data)){ # if all data is NA, write NA (otherwise it writes Inf)
        if(sum(is.na(data[ind.day,j]))==length(ind.day)){
          data.agg[i,j] <- NA
        } else{
          data.agg[i,j] <- apply(as.matrix(data[ind.day,j]), 2, aggFun, na.rm=T)
        }
      }   
      # warning if a day is not complete
      if(length(ind.day)!=0 & 24!= length(ind.day)) print(paste0("Warning: the input series for day ",dates.agg[i]," is not complete. The aggregation is calculated with the available data"))
      if(length(ind.day)==0)  print(paste0("Warning: the input series for day ",dates.agg[i]," is empty."))
      # close loop over days in the period 
    }

    # when all values in a day are NA, the result is -Inf. Change to NA
    data.agg <- apply(data.agg,2,function(x) replace(x, is.infinite(x),NA))
  } 

  # Aggregate to monthly
  if(agg=="M"){
    mon <- c(1:12) # it attemps to fill the whole month and year aggregated series
    ind.mon <- lapply(mon, function(x) which(as.numeric(format(dates,'%m')) %in% x))
    i <- 1
    data.agg <- matrix(NA, nrow=length(years)*length(mon), ncol=ncol(data))
    for (y in 1:length(years)){
      for(m in 1:length(mon)){
        ind.dates <- intersect(ind.year[[y]], ind.mon[[m]])
        
        # if ind.dates not empty, calculate aggregationn and save dates of the first time step (because the whole month may not be included)  
        if(length(ind.dates)!=0) { 
          #data.agg[i,] <- apply(as.matrix(data[ind.dates,]), 2, aggFun, na.rm=T)
          for(j in 1:ncol(data)){ # if all data is NA, write NA (otherwise it writes Inf)
            if(sum(is.na(data[ind.dates,j]))==length(ind.dates)){
              data.agg[i,j] <- NA
            } else{
              data.agg[i,j] <- apply(as.matrix(data[ind.dates,j]), 2, aggFun, na.rm=T)
            }
          }   
        } else{
          print(paste0("Warning: the input series for month ",mon[m]," and year ", years[y]," is empty."))
        }
        i <- i +1

        # warning if a month is not complete
        ndays <- lubridate::days_in_month(mon[m])
        if(length(ind.dates)!=0 & ndays!= length(ind.dates)) print(paste0("Warning: the input series for month ",mon[m]," and year ", years[y]," is not complete. The aggregation is calculated with the available data"))

      }
    }
  }
 
  # Aggregate to seasonal
  if(agg=="S"){
    seas <- matrix(c(12,1:11), nrow=4, ncol=3, byrow=TRUE) # DJF, MAM, JJA, SON in rows
    ind.seas <- list()
    for(s in 1:nrow(seas)){
      aux <- lapply(seas[s,], function(x) which(as.numeric(format(dates,'%m')) %in% x))
      ind.seas[[s]] <- aux
    }
    i <- 1
 
    data.agg <- matrix(NA, nrow=length(years)*nrow(seas), ncol=ncol(data)) 
    for (y in 1:length(years)){
      for(s in 1:nrow(seas)){
        if(s==1){ # winter, take December from previous year
          if(y==1){ # first year no December
            ind.dates <- c(intersect(ind.year[[y]], sort(unlist(ind.seas[[s]][[2]]))),
                           intersect(ind.year[[y]], sort(unlist(ind.seas[[s]][[3]]))))
          } else{
            ind.dates <- c(intersect(ind.year[[y-1]], sort(unlist(ind.seas[[s]][[1]]))),
                           intersect(ind.year[[y]], sort(unlist(ind.seas[[s]][[2]]))),
                           intersect(ind.year[[y]], sort(unlist(ind.seas[[s]][[3]]))))
          }
        } else{
          ind.dates <- intersect(ind.year[[y]], sort(unlist(ind.seas[[s]])))
        }

        if(length(ind.dates)!=0) { # if not empty, save data and dates of the first time step (because the whole season may not be included)
          for(j in 1:ncol(data)){ # if all data is NA, write NA (otherwise it writes Inf)
            if(sum(is.na(data[ind.dates,j]))==length(ind.dates)){
              data.agg[i,j] <- NA
            } else{
              data.agg[i,j] <- apply(as.matrix(data[ind.dates,j]), 2, aggFun, na.rm=T)
            }
          }         
          
        } else{
          print(paste0("Warning: the input series for season ",seas[s,1],", ",seas[s,2],", ", seas[s,3]," and year ", years[y]," is empty."))
        }
        
        # warning if a month is not complete
        ndays <- lubridate::days_in_month(seas[s,1]) + lubridate::days_in_month(seas[s,2]) + lubridate::days_in_month(seas[s,3])
        if(length(ind.dates)!=0 & ndays!= length(ind.dates)) print(paste0("Warning: the input series for season ",seas[s,1],", ",seas[s,2],", ", seas[s,3]," and year ", years[y]," is not complete. The aggregation is calculated with the available data"))
        
        i <- i +1
      }
    }
  }
  
  # Aggregate to annual
  if(agg=="Y"){
    data.agg <- matrix(NA, nrow=length(dates.agg), ncol=ncol(data))
    for (y in 1:length(years)){
      ind.dates <- ind.year[[y]]
      #data.agg[y,] <- apply(as.matrix(data[ind.dates,]), 2, aggFun, na.rm=T)
      for(j in 1:ncol(data)){ # if all data is NA, write NA (otherwise it writes Inf)
        if(sum(is.na(data[ind.dates,j]))==length(ind.dates)){
          data.agg[y,j] <- NA
        } else{
          data.agg[y,j] <- apply(as.matrix(data[ind.dates,j]), 2, aggFun, na.rm=T)
        }
      }   
    }
  }
  df <- data.frame(data.agg)
  if(!is.null(colnames(data))) colnames(df) <- stn.name
  res <- list(data.agg=df, dates.agg=dates.agg, agg= agg, aggFun=aggFun)
  return(res)

} # end data_agg

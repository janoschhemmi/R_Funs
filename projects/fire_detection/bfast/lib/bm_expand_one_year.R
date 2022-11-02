#' Expand date time series
#'
#' @title Expand date time series
#' @description Appends a year of data to the beginning and end of the time series by duplicating those years.
#' @param dates Date. Vector of dates.
#' @param vals numeric. spectral values
#' @return zoo. time series object
#' @author Jan Hemmerling
#' @export
#' @md
#' 
bm_expand_one_year <- function(dates, vals){
  
  # Fun for expanding time series by doubling edge years
  years      <- 1900 + as.POSIXlt(dates)$year
  year_range <- range(years)
  
  outVals <- c(vals[years == year_range[1]],
               vals,
               vals[years == year_range[2]])
  
  outDates <- c(dates[years == year_range[1]] - 365,
                dates,
                dates[years == year_range[2]] + 365)
  
  dts  <- zoo::zoo(outVals,  
                   1900 + as.POSIXlt(outDates)$year + (lubridate::yday(outDates)-1  )/365, 
                   frequency = 365)
  
  return(dts)
}
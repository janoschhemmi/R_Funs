#' Fix dates for use in Zoo time series
#'
#' @title Fix dates for use in Zoo time series
#' @description Fixes leap year dates and duplicate dates. Otherwise Zoo will throw an error/warning at you.
#' @param dates Date. Vector of dates.
#' @return Date. Vector of dates
#' @author Jan Hemmerling
#' @export
#' @md

bm_fix_date <- function(dates) {
  
  # if we have feb 29 th replace / Feb 29th can cause zoo error 
  dates[lubridate::month(dates)==2 & lubridate::day(dates)==29] <- dates[lubridate::month(dates)==2 & lubridate::day(dates)==29] -1
  dates[lubridate::month(dates)==3 & lubridate::day(dates)==1]  <- dates[lubridate::month(dates)==3 & lubridate::day(dates)==1] +1
  
  # check for same day observation  /  multiple dates can cause zoo error
  if(anyDuplicated(dates)){
    
    print("caught duplicated dates once")
    dates[base::duplicated(dates)] <- dates[base::duplicated(dates)] +1
    
    if(anyDuplicated(dates)){
      print("caught duplicated dates twice")
      dates[base::duplicated(dates)] <- dates[base::duplicated(dates)] +1
      
    }
    if(anyDuplicated(dates)){
      print("caught duplicated dates 3x")
      dates[base::duplicated(dates)] <- dates[base::duplicated(dates)] +1
    }
    
  }
  return(dates)
}
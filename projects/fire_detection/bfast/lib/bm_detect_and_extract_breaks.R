#' Breakpoint detection and extraction for single pixel vector
#'
#' @title Breakpoint detection and extraction for single pixel vector
#' @description Detects breakpoints using bfast/strucchange and extracts related 
#' metrics useful as predictors for classification.
#' @param specval numeric. Vector of spectral values
#' @param dates Date. Vector of dates with same length as `specval`
#' @param index character. Character vector of index codes associated with `specval`
#' @param fit_index character. Index used for fitting bfast/strucchange
#' @param h numeric. Minimal segment size either given as fraction relative to the 
#' sample size or as an integer giving the minimal number of observations in each segment. Defaults to `40`.
#' @param breaks numeric. Maximum number of breakpoint, defaulting to `3`.
#' @param order_harmonic numeric. Order of the harmonic term, defaulting to `1`.
#' @param expand boolean. Expand time series values at beginning and end by one year, defaulting to `TRUE`.
#' @return numeric. Vector containing information on the breakpoints and extracted metrics
#' @seealso [bm_detect_and_extract_tiles()] for creating breakpoint metrics with raster data 
#' and [bm_detect_and_extract_samples()] for sample data.
#' @author Jan Hemmerling
#' @author Dirk Pflugmacher
#' @export
#' @importFrom rlang .data
#' @md
#' 

bm_detect_and_extract_breaks <- function(specval, dates, index, fit_index="nbr",
                                         h=40, breaks=3, order_harmonic=1, 
                                         expand=T){
  
  # specval: spectral values WITHOUT landcover mask
  # dates:   dates vector matching specval
  # index:   vector of index labels matching specval, e.g. NBR, TCW

  # determine the spectral indices
  index_list = unique(index) 
  
  # create empty result vector
  # 1 + 3 * ( 10 + 6 * 27))
  length_fitted <- 10
  length_imetrics <- 27
  l_block <- length_fitted + length_imetrics * length(index_list)
  length_result <- 1 + breaks * (length_fitted + length(index_list) * length_imetrics) # 517
  
  results <- as.integer(rep(-9999, length_result))
  
  # get fitted index vals and dates
  tsVals <- specval[index == toupper(fit_index)]
  tsDates <- dates[index == toupper(fit_index)]
  
  # check for nas time series
  if (all(is.na(tsVals))) return(results)
  
  # remove NA's before building the ts object  
  tsDates <- tsDates[!is.na(tsVals)]
  tsVals <- tsVals[!is.na(tsVals)]
  
  # switch for expansion of dates function
  if (expand) {
    
    dts   <- bm_expand_one_year(tsDates, tsVals)
    
  } else {
    dts  <- zoo::zoo(tsVals,  
                     1900 + as.POSIXlt(tsDates)$year + (lubridate::yday(tsDates)-1  )/365, 
                     frequency = 365)
  }
  
  if ( (class(dts)[1]=="try-error") ) {
    print("zoo error..")
    stop()
    return(results)
  }
  # check if dts is time series
  if(!("zoo" %in% class(dts) )){
    print("no ts object.. ")
    return(results)
  }
  
  # preprocessing of ts, adding harmonic / linear terms
  bfpp <- try(bfast::bfastpp(dts, order=order_harmonic))
  if ( (class(bfpp)[1]=="try-error") ) {
    print("bfastpp error..")
    return(results)
  }
  
  # detecting breaks
  bp <- tryCatch(strucchangeRcpp::breakpoints(response ~ trend + harmon, 
                                              data=bfpp, h=h, breaks= 3),
                 error=function(x) return(NULL))
  
  if (is.null(bp) || is.na(bp$breakpoints[1])) return(results)
  
  # assign segment labels
  bfpp$segment <- strucchangeRcpp::breakfactor(bp)
  
  ## fit linear trend for each segment
  lmd <- stats::lm(response ~ segment + segment:time - 1, data=bfpp)
  # if(class(lmd)[1]=="try-error"){
  #   return(results)
  # }

  # add fitted values
  bfpp$fitted <- stats::fitted(lmd)
  n_bps <- length(bp$breakpoints)
  
  ## get date of detected breaks
  breakdat      <- bfpp[bp$breakpoints,]$time
  breakdat_dat  <- as.Date(lubridate::date_decimal(breakdat))
  breakyear     <- floor(breakdat)
  doy           <- lubridate::yday(breakdat_dat)
  
  #  split segments at breakpoints
  # for each segment gets values per timestep
  segs_split <- unname(split(bfpp$fitted, bfpp$segment))
  
  # slopes of segments from linear model
  slopes_lm <- stats::coefficients(lmd)[(n_bps+2):(2*n_bps+2)] 
  
  # add number of breaks to first layer
  results[1] <- n_bps
  
  
  # loop through breakpoints
  for(b in 1:n_bps){
    
    # get strucchange data
    bp_single <- bp$breakpoints[b]
    
    # slope pre post
    slope_pre  <- slopes_lm[b] * 10
    slope_post <- slopes_lm[b+1] * 10
    
    # pre/post- fitted values
    pre_value_fitted   <- bfpp$fitted[bp_single]
    post_value_fitted  <- bfpp$fitted[bp_single + 1]
    
    # magnitude fitted
    magnitude_fitted <- post_value_fitted - pre_value_fitted
    
    # magnitude segment pre
    magnitude_segment_pre  <- dplyr::last(segs_split[[b]]) -
      dplyr::first(segs_split[[b]])
    magnitude_segment_post <- dplyr::last(segs_split[[b+1]]) -
      dplyr::first(segs_split[[b+1]])
    
    # collect fitted data
    p1 <- (2 + (b-1)*l_block)
    p2 <- (p1 + length_fitted - 1)
    results[p1:p2] <- c(b, 
                        breakyear[b], 
                        doy[b], 
                        pre_value_fitted, 
                        post_value_fitted, 
                        magnitude_fitted,
                        slope_pre, 
                        slope_post, 
                        magnitude_segment_pre, 
                        magnitude_segment_post)
    
    
    # loop over indices
    for (i in 1:length(index_list)){
      
      # choose index
      vec_vals <- specval[index == index_list[i]]
      vec_dates <- dates[index == index_list[i]]
      
      # create nona
      vec_dates <- vec_dates[!is.na(vec_vals)]
      vec_vals  <- vec_vals[!is.na(vec_vals)]
      
      # finds nearest matching date
      i_break <- which.min(abs(vec_dates - breakdat_dat[b]))
      
      # pre and post break value
      pre_val  <- vec_vals[i_break]
      post_val <- vec_vals[i_break+1]

      # mag
      magnitude <- post_val - pre_val
      
      mean_pre_val    <- mean(c(vec_vals[bp_single], vec_vals[bp_single-1], vec_vals[bp_single-2]))
      mean_post_val   <- mean(c(vec_vals[bp_single+1], vec_vals[bp_single+2], vec_vals[bp_single+3]))
      mean_magnitude  <- mean_post_val - mean_pre_val # move-out
      
      # select obs of pre / post / breakyear
      data_break_year       <- vec_vals[lubridate::year(vec_dates) == breakyear[b]]
      quantiles_break_year  <- stats::quantile(data_break_year, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      iqr_breakyear         <- quantiles_break_year[4]-quantiles_break_year[2] # move-out
      
      # data pre break year
      data_pre_break_year       <- vec_vals[lubridate::year(vec_dates)  == (breakyear[b]-1)]
      quantiles_pre_break_year  <- stats::quantile(data_pre_break_year, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      iqr_pre_breakyear         <- quantiles_pre_break_year[4]-quantiles_pre_break_year[2] # move-out
      
      # ## data post break year
      data_post_break_year <- vec_vals[lubridate::year(vec_dates)  == (breakyear[b]+1)]
      quantiles_post_break_year  <- stats::quantile(data_post_break_year, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      iqr_post_breakyear <- quantiles_post_break_year[4]-quantiles_post_break_year[2] # move-out
      
      # get one observation per year
      obs_break_year       <- vec_vals[which.min(abs(vec_dates - as.Date(paste0(breakyear[b],"-07-01"))))]
      obs_pre_break_year   <- vec_vals[which.min(abs(vec_dates - as.Date(paste0((breakyear[b]-1),"-07-01"))))]
      obs_post_break_year  <- vec_vals[which.min(abs(vec_dates - as.Date(paste0((breakyear[b]+1),"-07-01"))))]
      
      # collect index data
      p3 <- (p2+1) + (i-1)*length_imetrics
      p4 <- p3 + length_imetrics - 1
      
      
      results[p3:p4] <- c(pre_val,
                          post_val,
                          magnitude,
                          mean_pre_val,
                          mean_post_val,
                          mean_magnitude,
                          quantiles_break_year,
                          iqr_breakyear,
                          quantiles_pre_break_year,
                          iqr_pre_breakyear,
                          quantiles_post_break_year,
                          iqr_post_breakyear,
                          obs_break_year,
                          obs_pre_break_year,
                          obs_post_break_year) # length = 27
      
    } # loop over index
    
  } # loop over breaks
  
  return(round(results))
  
}


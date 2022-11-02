## Breakdetection with strucchangeRcpp on subtile level ##

### Path to lib
.libPaths("S:/BrandSat/02_Code/R/library_6")

library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(bfastSpatial)
library(svMisc)
library(snow)
library(lubridate)
library(hms)
library(rgdal)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyr)
library(Rcpp)
library(dplyr)
library(randomForest)
library(data.table)
library(randomForest)
library(flextable)
library(terra)
library(strucchangeRcpp)

# update if needed
# old.packages()
# update.packages(ask = FALSE, lib.loc = "S:/BrandSat/02_Code/R/library_6")

Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

## terra options 
terraOptions(memfrac=0.6, tempdir = 'S:/BrandSat/000_TerraTemp', verbose = TRUE,
             progress = 10, datatype = 'Init2s')

			 
################################################################################

## Worker Fun

################################################################################


## main strucchange fun handler
detect_and_extract_breaks <- function(x){
  #.libPaths("S:/BrandSat/02_Code/R/library_6")
  # require(strucchangeRcpp, quietly = TRUE)
  # require(data.table, quietly = TRUE)
  # require(lubridate, quietly = TRUE)
  # require(zoo, quietly=TRUE)
  # require(bfast, quietly = TRUE)
  # require(Rcpp, quietly = TRUE)
  # require(randomForest, quietly = TRUE)
  
  print("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  print("-- Start  --")
  
  ### Settings for Breakdetection
  h              <- 40
  breaks         <- 3
  order_harmonic <- 1
  
  ## Indices stacked in input TS-stack
  index_list <-  c("NBR","NDV","TCB","TCG","TCW","TCD")
  
  
  ## Other subsequent functions ################
  
  
  get_dates <<- function(names) {
    year      <- substr(names, 1, 4)
    month     <- substr(names, 5, 6)
    day       <- substr(names, 7, 8)
    
    date     <- as.Date(paste0(year,"-",month, "-", day))
    
  }

  
  ## -------------------
  ## expand time series with variable number of observations
  ## makes sure qw have n number of observations before 1990 and after 2019; meaning 2020 is used as base year at end
  
  exanding_ts_fun_variabel <<- function(dates_nbr, nbr){
    
    ## in::
    ## dates_nbr
    ## nbr :: nbr vector
    
    ## nbr no NAs
    nbr_nona <- nbr[!is.na(nbr)]
    
    ## define how many observations before 1990 and after 2019
    ## get first and las year
    years_in_nbr      <- 1900 + as.POSIXlt(dates_nbr)$year
    years_in_nbr_nona <- years_in_nbr[!is.na(nbr)]
    
    ## obs to add before 1990
    
    n_before_1990     <- match(1990, years_in_nbr_nona)
    ## observations to add after 31.12.2019
    n_after_2019      <- match(2019, rev(years_in_nbr_nona))
    
    
    ## safety net if no observations in 1990 / 2019
    if(is.na(n_before_1990)){
      n_before_1990 <- match(1991, years_in_nbr_nona)
    }
    
    if(is.na(n_after_2019)){
      n_after_2019<- match(2018, years_in_nbr_nona)
    }
    
    ## min / max years
    min_year <- min(years_in_nbr_nona)
    max_year <- max(years_in_nbr_nona)
    
    ## how many observations need to be added to get 40 Observations?
    n_before_1990_to_add <- 40 - n_before_1990
    n_after_2019_to_add  <- 40 - n_after_2019
    
    ## doys of ts
    dates_nona        <- dates_nbr[!is.na(nbr)]
    doy_in_nbr_nona   <- yday(1900 + as.POSIXlt(dates_nona))
    
    ## how many in nona vector
    n_min_years      <- length(years_in_nbr_nona[years_in_nbr_nona==min_year |
                                                   years_in_nbr_nona==(min_year +1) |
                                                   years_in_nbr_nona==(min_year + 2)])
    
    unique_years <- unique(years_in_nbr_nona[years_in_nbr_nona==min_year |
                                               years_in_nbr_nona==(min_year +1) |
                                               years_in_nbr_nona==(min_year + 2)])
    
    ## add before 1990 ## add from 1985 - 1987
    if (n_before_1990_to_add > 0){
      
      nbr_min_year <- nbr_nona[1:n_min_years]
      dates_min_year <- dates_nona[1:n_min_years]
      
      ## replicate from reversed ts
      rep_nbr_rev <- rep(rev(nbr_min_year), len = n_before_1990_to_add)
      
      ## years to subtract in first iteration
      years_sub_first_it <- year(dates_min_year) - min_year + 1
      
      ## years to subtract to get consequent time series
      sequence <- seq(from =  length(unique_years), to = (ceiling(n_before_1990_to_add/n_min_years) * length(unique_years)), by = length(unique_years))
      years_to_substract <-  rep(sequence, each = n_min_years,  length.out = (n_before_1990_to_add))
      
      ## now add not completed years
      # years_to_substract <- c(years_to_substract, rep(max(years_to_substract) + 1, n_before_1990_to_add - length(years_to_substract)))
      
      ## original rep dates; then subtract additional years
      rep_dates    <- rep(rev(dates_min_year), len = n_before_1990_to_add)
      dates_to_add <- rev(rep_dates - years( years_to_substract))
      
      dates_nbr <- c(dates_to_add, dates_nbr)
      nbr       <- c(rep_nbr_rev, nbr)
      
    }
    
    ## adding after 2019, here just replicate 2020 data
    if (n_after_2019_to_add > 0){
      
      nbr_max_year   <- nbr_nona[years_in_nbr_nona == max_year]
      dates_max_year <- dates_nona[years_in_nbr_nona == max_year]
      
      ## replicate from reversed ts
      rep_nbr_ <- rep((nbr_max_year), len = n_after_2019_to_add)
      years_to_add <- rep( 1:ceiling(n_after_2019_to_add/length(dates_max_year)), each = length(dates_max_year), length.out = n_after_2019_to_add)
      
      ## original rep dates
      rep_dates_ <- rep((dates_max_year), len = n_after_2019_to_add)
      dates_to_add_ <- (rep_dates_ + years(years_to_add))
      
      dates_nbr <- c(dates_nbr, dates_to_add_)
      nbr <- c( nbr, rep_nbr_)
    }
    
    return(list(n_before_1990_to_add,n_after_2019_to_add, dates_nbr, nbr))
    
  } ## expanding ts
  
  ##############################################
  
  ## Fun for expanding time series by doubling edge years
  
  exanding_ts_fun_one_year <<- function(dates_nbr, nbr){
    
    # dates_nbr <- dates
    nbr_nona <- nbr[!is.na(nbr)]
    dates_nona <- dates_nbr[!is.na(nbr)]

    ## get first and last year
    years_in_nbr      <- 1900 + as.POSIXlt(dates_nbr)$year
    years_in_nbr_nona <- years_in_nbr[!is.na(nbr)]
    
    min_year <- min(years_in_nbr_nona)
    max_year <- max(years_in_nbr_nona)
    
    min_year_extra <- min_year -1
    max_year_extra <- max_year +1
    
    ## get min and max year values
    min_year_obs <- nbr_nona[year(dates_nona) == min_year]
    max_year_obs <- nbr_nona[year(dates_nona) == max_year]
    
    ## get min max dates
    min_year_dates <- dates_nona[year(dates_nona) == min_year] -365
    max_year_dates <- dates_nona[year(dates_nona) == max_year] +365
    
    ## bind to extended df
    smp_new <- list( c(min_year_dates, dates_nbr, max_year_dates),c(min_year_obs,nbr,max_year_obs))
    
    return(smp_new)
  }
  
  
  ## Extract data at breaks and classify breaks with given model
  
  extract_data <- function(x,dates, dates_nona,
                           n_bps, breakdat_dat,
                           breakyear,  doy,
                           bfpp, bps, segs_split,
                           f,dts, index_list, index_indicator#,
                           #names_out
  ) {
    
    
    # x intro vector without mask val
    # dates           :: original dates of time series
    # dates_ecpanded  :: expanded dates
    # nbr_expanded    :: nbr ts on which breakpoints were detected on
    # n_bps           :: n breakpoints detected
    # break_dat       :: dates of detected breaks
    # year and doy    :: of breaks
    # bfpp            :: linear and harmonic fitted trend 
    # bps             :: breakpoits
    # segs_split      :: segments of data, splitted at breaks, breakpoint is last of segement
    # f               :: fitted values
    # dts             :: nbr time series
    # index list      :: list of stacked intup raster
    # names_out       :: names for out_matrix
    
    
    
    print(">>>>> ------------------------------------------")
    print(">>>>> -- StartExtract Data at Breakpoints FUN --")
    
    it_break <- 1
    print(">>>>> extracting data from df.. ")
    
    for(number_detected_break in seq(1:n_bps)){
      print(paste0(">>>>> looping over detected break.. ", it_break, " .. of ",n_bps," breaks"))
      
      ## ----------------------
      ## get strucchange data
      bp_single <- bps[number_detected_break]
      
      # slopes of segments from linear model
      slopes_lm <- summary(lm(response ~ segment + segment:time - 1, data=bfpp))$coefficients[(n_bps+2):(2*n_bps+2),1]
      
      ## slope pre post
      slope_pre  <- slopes_lm[number_detected_break]
      slope_post <- slopes_lm[number_detected_break+1]
      
      # pre/post- fitted values
      pre_value_fitted   <- floor(f[bp_single])
      post_value_fitted  <- floor(f[bp_single + 1])
      
      ## magnitude fitted
      magnitude_fitted <- post_value_fitted - pre_value_fitted
      
      ## magnitude segment pre
      magnitude_segment_pre  <- as.integer(tail(segs_split[[number_detected_break]], n=1) -
                                             head(segs_split[[number_detected_break]], n=1))
      magnitude_segment_post <- as.integer(tail(segs_split[[number_detected_break+1]], n=1) -
                                             head(segs_split[[number_detected_break+1]], n=1))
      
      ## -----------------------
      ###### collect fitted data
      fitted_data <- c(number_detected_break, breakyear[number_detected_break], doy[number_detected_break], pre_value_fitted, post_value_fitted, magnitude_fitted,
                       slope_pre, slope_post, magnitude_segment_pre, magnitude_segment_post)
      names_fitted <- c("number_detected_break", "breakyear","doy", "pre_val_fit","post_val_fit","magnitude_fit", "slope_pre","slope_post",
                        "magnitude_segment_pre","magnitude_segment_post")
      
      ## safety_net
      if (!length(fitted_data) == length(names_fitted)){
        fitted_data <- rep(-9999, length(names_fitted))
      }
      print(paste0("fitted data: ", fitted_data))
      
      ## -----------------------------------------------------------------------
      ## loop over indixes
      it_in <- 1
      print(">>>>> Looping over Indices.. ")
      for (Index__ in index_list){
        
        print(paste0("processing Index: ",Index__))
        
        ## choose index
        vec_index <- x[index_indicator == Index__]
        
        ## create nona //
        vec_index_nona <- vec_index[!is.na(vec_index)]
        
        #Jan: Is dates_nona same length as vec_index_nona? dates_nona are expanded time series but vec_index not.
        
        ## pre and post break value
        pre_val  <- vec_index_nona[(which(dates_nona == breakdat_dat[number_detected_break]))]
        ## next non na
        post_val <- vec_index_nona[(which(dates_nona == breakdat_dat[number_detected_break])+1)]
        
        ## mag
        magnitude <- as.integer(post_val) -as.integer(pre_val)
        
        mean_pre_val    <- as.integer(mean(c(vec_index_nona[bp_single],vec_index_nona[bp_single-1],vec_index_nona[bp_single-2])))
        mean_post_val   <- as.integer(mean(c(vec_index_nona[bp_single+1],vec_index_nona[bp_single+2],vec_index_nona[bp_single+3])))
        mean_magnitude  <- mean_post_val - mean_pre_val
        
        ## select obs of pre / post / breakyear
        data_break_year       <- vec_index_nona[year(dates_nona) == breakyear[number_detected_break]]
        quantiles_break_year  <- as.integer(quantile(na.omit(data_break_year), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
        iqr_breakyear         <- quantiles_break_year[4]-quantiles_break_year[2]
        
        # ## data pre break year
        data_pre_break_year       <- vec_index_nona[year(dates_nona)  == (breakyear[number_detected_break]-1)]
        quantiles_pre_break_year  <- as.integer(quantile(na.omit(data_pre_break_year), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
        iqr_pre_breakyear         <- quantiles_pre_break_year[4]-quantiles_pre_break_year[2]
        
        # ## data post break year
        data_post_break_year <- vec_index_nona[year(dates_nona)  == (breakyear[number_detected_break]+1)]
        quantiles_post_break_year  <- as.integer(quantile(na.omit(data_post_break_year), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
        iqr_post_breakyear <- quantiles_post_break_year[4]-quantiles_post_break_year[2]
        
        ## get one observation per year 
        obs_break_year       <- vec_index_nona[which.min(abs(dates_nona - as.Date(paste0(breakyear[number_detected_break],"-07-01"))))]
        obs_pre_break_year   <- vec_index_nona[which.min(abs(dates_nona - as.Date(paste0((breakyear[number_detected_break]-1),"-07-01"))))]
        obs_post_break_year  <- vec_index_nona[which.min(abs(dates_nona - as.Date(paste0((breakyear[number_detected_break]+1),"-07-01"))))]
        
        ## ------------------
        ## collect index data
        
        index_results        <- c(as.integer(pre_val), as.integer(post_val), magnitude, mean_pre_val, mean_post_val, mean_magnitude,
                                  quantiles_break_year, iqr_breakyear,
                                  quantiles_pre_break_year, iqr_pre_breakyear,
                                  quantiles_post_break_year, iqr_post_breakyear,
                                  obs_break_year ,obs_pre_break_year ,obs_post_break_year
        )
        if(it_in == 1){
          index_results_out    <- append(fitted_data,index_results)
        } else {
          index_results_out    <- append(index_results_out, index_results)
        }
        
        ## it of n index
        print(paste0(">>>>> extracted ",Index__," for break.. "))
        it_in <- it_in + 1
      } ## loop over index
      
      
      if(it_break == 1) {
        metrics_out <-   index_results_out
        # names(metrics_out) <- names_out
      } else {
        ## for each sample collect data of all detected breaks
        # names(metrics) <- names_out
        metrics_out    <- append(metrics_out,   index_results_out)
      }
      
      ## n break iterator
      it_break <- it_break + 1
      
    } ## loop over detected breaks
    metrics_out    <- append((it_break-1), metrics_out)
    
    ## expand to 3 times one metric out __ 46
    if (length(metrics_out) < (517)){
      metrics_out <- (append(metrics_out, rep(-9999,(517-length(metrics_out)))))
    }
    if (length(metrics_out) > (517)){
      metrics_out <- as.double(rep(-9999,517))
    }

	## safety
    metrics_out <- as.double(as.numeric(metrics_out))
    
    return(metrics_out)
    
  }  ## function extract classify
  
  # ----------------------------------------------------------------------------------------------------------------------------------
  #### MAIN
  # __________________________________________________________________________________________________________________________________
  

  ## get mask
  mask_val <- x[1]
  print(mask_val)
  
  ## interrupt for no mask
  if (mask_val == 0 || is.na(mask_val)) {
    print("mask == 0")
    out <- as.double(rep(-9999,517))
	## stop MAIN
    return(out)
  }
  
  ## get number of layers per index
  # n_layers <- length(unique(names(x[2:length(x)])))

  ## get dates of stack; requires coherence of stacked raster
  dates    <- try(get_dates(names_input_stack))
 
  if ( (class(dates)[1]=="try-error") ) {
    print("dates error..")
    out <- as.double(rep(-9999,517))
    return(out)
  }
  
  ## if we have feb 29 th replace / Feb 29th can cause zoo error 
  dates[month(dates)==2 & day(dates)==29] <- dates[month(dates)==2 & day(dates)==29] -1
  dates[month(dates)==3 & day(dates)==1]  <- dates[month(dates)==3 & day(dates)==1] +1
  
  ## check for same day observation  /  multiple dates can cause zoo error
  if(anyDuplicated(dates)){
    print("catched duplicated dates once")
    dates[base::duplicated(dates)] <- dates[base::duplicated(dates)] +1
  }
 
  if(anyDuplicated(dates)){
    print("catched duplicated dates twice")
    dates[base::duplicated(dates)] <- dates[base::duplicated(dates)] +1
    
  }
  if(anyDuplicated(dates)){
    print("catched duplicated dates 3x")
    dates[base::duplicated(dates)] <- dates[base::duplicated(dates)] +1
  }
  #print(index_list)
  #print(nlayer_input_stack)
  
  ## create run-length vector for index selection of input vector
  index_indicator <- rep(index_list,each = nlayer_input_stack)

  ## get nbr
  #print(length(x))
  x   <- x[2:length(x)]
  nbr <- x[index_indicator == "NBR"]
  
  ## check for nas in NBR time series
  if (all(is.na(nbr))) {
    print("NAs.. ")
    out <- as.double(rep(-9999,517))
    return(out)
  }
  
  ## do break detection
  if (mask_val == 1) {
    print("mask == 1")
    
    ## -------------------------------------------------------------------------
    ## switch for expansion of dates function
    expansion <- 1
    if ( expansion == 1){
      
      ## dynamically expand time series
      #t__1 <- Sys.time()
      # nbr_expanded   <- try(exanding_ts_fun_variabel(dates, nbr))
      
	  ## expand by one edge year
      nbr_expanded   <- try(exanding_ts_fun_one_year(dates, nbr))
      
      if ( (class(nbr_expanded)[1]=="try-error") ) {
        print("nbr expansion error..")
        out <- as.double(rep(-9999,517))
        return(out)
      }
      
      # t__2 <- Sys.time()
      #print(paste0("expanding took: ", (t__2 - t__1), " seconds"))
      
      ## split into dates and nbr, each expanded ## for variable expansion
      # n_before_1990      <- nbr_expanded[[1]]
      # n_after_2019       <- nbr_expanded[[2]]
      # dates_expanded     <- as.Date(nbr_expanded[[3]])
      # nbr_expanded       <- nbr_expanded[[4]]
      
      ## for one year expansion
      dates_expanded     <- as.Date(nbr_expanded[[1]])
      nbr_expanded       <- unlist(nbr_expanded[[2]])
      n_before_1990      <- 0
      n_after_2019       <- 0
      print(length(dates_expanded))
      print(length(nbr_expanded))
      
      ## dates dont get expanded
    } else {
      n_before_1990      <- 0
      n_after_2019       <- 0
      dates_expanded     <- dates
      nbr_expanded       <- nbr
      
    } ## expansion of ts
    
    
    ## time series of nbr, input for bfast
    dts  <- try(zoo::zoo(nbr_expanded,  1900 + as.POSIXlt(dates_expanded)$year + (yday(dates_expanded)-1  )/365, frequency = 365))
    
    if ( (class(dts)[1]=="try-error") ) {
      print("nbr expansion error..")
      out <- as.double(rep(-9999,517))
      #names(out) <- names_out
      return(out)
    }
    
    
    ## check if dts is time series
    if(!("zoo" %in% class(dts) )){
      print("no ts object.. ")
      out <- as.double(rep(-9999,517))
      return(out)
    }
    
    ### get no NA dates, used for selecting observations per index later
    dates_nona <- (as.Date(date_decimal(time(dts[!is.na(dts)]))))
    ## match with original dates ts
    dates_nona <- dates_nona[(n_before_1990+1):(length(dates_nona)-n_after_2019)]
    
    ## preprocessing of ts, adding harmonuic / linear terms
    bfpp <- try(bfast::bfastpp(dts, order=order_harmonic))
    
	if ( (class(bfpp)[1]=="try-error") ) {
      print("nbr expansion error..")
      out <- as.double(rep(-9999,517))
      return(out)
    }
 
    print("detecting breaks.. ")
    bp <- try(strucchangeRcpp::breakpoints(response ~ trend + harmon, data=bfpp, h=h, breaks= 3))
    
    
    ## if no breakpoints detected
    if ( (class(bp)[1]=="try-error") || (is.na(bp[[1]]))) {
      
      print("no breakpoints detected..")
      out <- as.double(rep(-9999,517))
      return(out)
      
    } else {
      
      ## breakpoints are detected
      print("breakpoints detected...")
      
      bfpp$segment         <- strucchangeRcpp::breakfactor(bp)
      levels(bfpp$segment) <- as.character(1:nlevels(bfpp$segment))
      
      ## fit linear trend for each segment
      f   <- try(fitted(lm(response ~ segment + segment:time - 1, data=bfpp)))
      if(class(f)[1]=="try-error"){
        out <- as.double(rep(-9999,517))
        return(out)
      }
  
      bfpp$fitted <- f
      bps   <- bp$breakpoints
      n_bps <- length(bps)
      print(paste0("deteced ", n_bps, " breakpoints!"))
      
      ## get date of detected breaks
      breakdat      <- bfpp[bps,]$time
      breakdat_dat  <- as.Date(date_decimal(breakdat))
      breakyear     <- floor(breakdat)
      
      #print(paste0("deteced ", breakdat,breakdat_dat, " breakpoints!"))
      doy           <- yday(breakdat_dat)
      
      #  split segments at breakpoints ## for each segment gets values per timestep
      segs_split <- unname(split(f, cumsum(seq_along(f) %in% (bps+1))))
      
	  ## PLOT TS with breakpoints
      # plot(bfpp$time, bfpp$response)
      # abline(v = breakdat, lty = 1, col="blue", lwd=1.2)
      
      # ## Extract data
      print("starting extract and classifiy function.. : ")
      
      out <- try(extract_data(x,dates,dates_nona,
                              n_bps, breakdat_dat,
                              breakyear,  doy,
                              bfpp, bps, segs_split,
                              f,dts, index_list,index_indicator
      ))
      
      
      ## check result
      if(class(out)[1]=="try-error"){
        out <- as.double(rep(-9999,517))
        return(out)
      }
      
      out <- as.double(out)
  
      print(length(out))
      
      return(out)
      
      
    }
  }
} ## fun :: Detect and extract breaks




################################################################################
## TILE LOOP FUN

# tile <- "X0068_Y0045"
breaks_on_tile <- function(tile, cores, ...) {
  
  ## n cores to use 
  cores <- 20
  
  ## packages 
  .libPaths("S:/BrandSat/02_Code/R/library_6")
  require(terra,  quietly = TRUE)
  require(zoo, quietly = TRUE)
  
  ### global Settings 
  Sys.setenv("LANGUAGE"="En")
  Sys.setlocale("LC_ALL", "English")
  terraOptions(memfrac=0.35, tempdir = 'S:/BrandSat/000_TerraTemp', verbose = TRUE,
               progress = 10, datatype = 'Init2s', progress = 2)
  
  ## loop over retiled stacks
  for ( sub_tile in seq(1,16)){ ## Issues :  
    # sub_tile <- 1
  
    #file_retile <- paste0("A:/01_Data_Level3/202202_LANDSAT_retile/",tile,"/",tile,"_stack_croped_",sub_tile,".tif")
    #file_retile <- paste0("A:/01_Data_Level3/202202_LANDSAT_retile_test_X0068_Y0045/",tile,"/",tile,"_stack_croped_",sub_tile,".tif")
    #file_retile <- "A:/01_Data_Level3/202202_LANDSAT_retile_test_X0068_Y0045/X0068_Y0045/X0068_Y0045_stack_croped_1.tif"
    file_retile <- paste0("A:/01_Data_Level3/202202_LANDSAT_retile_new_mask/",tile,"/",tile,"_stack_croped_",sub_tile,".tif")
	
	names_out <- read.table("P:/workspace/jan/fire_detection/break_detection/out_names_index_extraction_full_list.csv")
    names_out <- names_out$V1
    
    print(paste0("loading ", sub_tile,"/16 stack of tile: ", tile ))
    rast_stack <- terra::rast(file_retile)
    
    ## get names of layers for foreach solution
    names_input_stack  <- names(rast_stack)[2:length(names(rast_stack))]
    names_input_stack  <- unique(names_input_stack)
	
	## for kernel export
    nlayer_input_stack <<- length(names_input_stack)
    names_input_stack  <<- names_input_stack
	
	
    ## create dir for output
    # out_cube <- "A:/workspace/fire_detection/tiled_breaks/original_data_one_year/"
    out_cube <- "A:/workspace/fire_detection/tiled_breaks_new_mask/"
    
    if(!dir.exists(paste0(out_cube,"/",tile))){
      dir.create(paste0(out_cube,"/",tile)) }
    if(!dir.exists(paste0(out_cube,"/",tile,"/",sub_tile))){
      dir.create(paste0(out_cube,"/",tile,"/",sub_tile)) }

    file_name_out <- paste0("metrics_breaks_subtile_",sub_tile)
    
    ## delete output if existent 
    fn = paste0(out_cube,"/",tile,"/",sub_tile,"/",
                file_name_out,".tif")
    
    if(file.exists(fn)){
      file.remove(fn)
    }
    
    t1 <- Sys.time()
    ncells_in_stack <<- terra::ncell(rast_stack)
    
    ## init empty raster for output
    out_raster <- terra::rast( )
    res(out_raster)  <- res(rast_stack)
    ncol(out_raster) <- ncol(rast_stack)
    nrow(out_raster) <- nrow(rast_stack)
    
    ext(out_raster)  <- ext(rast_stack)
    crs(out_raster)  <- crs(rast_stack)
    nlyr(out_raster) <- 517
    names(out_raster) <- names_out
    out_raster <- out_raster
    
    
    ## ------------------------------------------------------------------------
   
    
    ## ------------------------------------------------------------------------
    ## for each 
    t1 <- Sys.time()
    
    ## loop over pixel in raster 
    library(foreach)
    library(doParallel)
    
    ## Parallel FUN #### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    print(paste0("begin break function for tile: ", tile, " ",sub_tile))

    #setup parallel backend to use many processors
    cl <- makeCluster(cores)#,outfile="S:/BrandSat/000_TerraTemp/out_log_parallel.txt" ) #
    doParallel::registerDoParallel(cl)
    clusterEvalQ(cl, .libPaths("S:/BrandSat/02_Code/R/library_6"))
    clusterEvalQ(cl, {library(doParallel)
      library(bfast)
      library(zoo)
      library(rlang)
      library(lubridate)
      library(terra)
      library(snow)
      library(lubridate)
      library(strucchangeRcpp)
      library(data.table)
      library(rgdal)
      library(ggplot2)
      library(reshape2)
      library(tidyr)
      library(Rcpp)
      library(dplyr)
      library(foreach)
      #invisible(source("P:/workspace/jan/code/R/BFAST/source_funs_breaks_for_retile.R"))

    })
    
    ## raster into matrix
    rast_stack_matrix <- terra::values(rast_stack)
    
    finalMatrix <- foreach(ii=1:ncells_in_stack, .combine=rbind,  # ii=1:ncells_in_stack
                           #.verbose = TRUE,
                           .export = ls(globalenv())[ls(globalenv()) %in% c("detect_and_extract_breaks","names_input_stack","nlayer_input_stack") ],
                           #.noexport = "detect_and_extract_breaks",
                           .packages = c("doParallel","foreach","dplyr","tidyr","reshape2","rgdal","strucchangeRcpp","zoo","bfast")) %dopar% {
                             
                             tempMatrix = detect_and_extract_breaks(rast_stack_matrix[ii,]) 
     
                           }
    #stop cluster
    # finalMatrix_s <- finalMatrix
    # finalMatrix <<- finalMatrix_s
    
    ## STOP
    stopCluster(cl)
    
	## write to raster 
    values(out_raster) <-finalMatrix
    fn_out <- paste0(out_cube,"/",tile,"/",sub_tile,"/",file_name_out, ".tif")
    writeRaster(out_raster, fn_out, overwrite = TRUE)
	

    # object.size(out_raster)
    # out <- terra::app(rast_stack, fun =detect_and_extract_breaks, cores = cores,
    #                    overwrite = TRUE,
    #                    filename = fn ,datatype = 'INT2S',
    #                    wopt = list(names = names_out)) #
    

    
    ## write 
    # terra::writeRaster(out ,filename =  paste0(out_cube,"/",tile,"/",sub_tile,"/",names(out), ".tif"), overwrite=TRUE,datatype = 'INT2S')
    t2 <- Sys.time()
    print(paste0("completed.. took ", difftime(Sys.time(),t1, units = "mins" ), " minutes.. "))
    
    
  } ## loop over retiled
  
} ## loop over tiles 


## -----------------------------------------------------------------------------
## ## ## ## ## ## ## ## ## ## ## MAIN LOOP ## ## ## ## ## ## ## ## ## ## ## ## #

## list dir of cube for loop 
cube_dir <- "A:/01_Data_Level3/202202_LANDSAT_retile/"

list_tiles <- list.dirs(cube_dir, full.names = FALSE) 
list_tiles <- list_tiles[!list_tiles %in% c("")] 

## filter of tiles 
# list_tiles <- list_tiles[list_tiles %in% c("X0068_Y0045")] 
# list_tiles <- list_tiles[list_tiles %in% c("X0068_Y0041")] 
# list_tiles <- list_tiles[list_tiles %in% c("X0069_Y0044","X0069_Y0045")] 

## output cube 
out_cube <- "A:/workspace/fire_detection/tiled_breaks_new_mask/original_data_one_year/"


for(tile in list_tiles){
  
  ## for loop over tiles; on each tile break detection is performed
  t_3 <- Sys.time()
  breaks_on_tile(tile,cores = 10)
  print(paste0("completed all tile .. overall took ", difftime(Sys.time(),t_3, units = "mins" ), " minutes.. "))
}

##
##


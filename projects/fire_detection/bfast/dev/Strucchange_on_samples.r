##Strucchange_0## 1_break_detection_on_ts_7_new_run_with_one_year_expansion



###############################################################################
## Do Break Detection based on extracted Landsat time series 
## Output are metrics csvs from Indecies at detected breaks 

### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")
# install.packages('Rcpp')
# library(Rcpp)

# install.packages(c("bfast","zoo","rgdal","rlang","stlplus","lubridate","devtools","doParallel","svMisc","snow","lubridate","hms","ggplot2","data.table","reshape2","dplyr","tidyr","Rcpp","randomForest","terra","strucchangeRcpp"))
# install.packages(c("rlang","stlplus","lubridate","devtools"))
# install.packages(c("svMisc","snow","lubridate","hms","ggplot2","data.table"))
# install.packages(c("reshape2","dplyr","tidyr","Rcpp","randomForest","terra","strucchangeRcpp"))
# install.packages("doParallel")

library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(bfastSpatial)
library(raster)
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
library(flextable)
library(terra)
library(strucchangeRcpp)



# old.packages()
# # update if needed 
# update.packages(ask = FALSE, lib.loc = "S:/BrandSat/02_Code/R/library_4")



Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
## terra options 
terraOptions(memfrac=0.3, tempdir = 'S:/BrandSat/000_TerraTemp', verbose = TRUE,
             progress = 10, datatype = 'Init2s')



## -----------------------------------------------------------------------------
## Functions ##

## Get Dates 
## get dates from Landsat Layer name-strings as provided by FORCE LV2 
get_dates <- function(names) {
  year      <- substr(names, 2, 5)
  month     <- substr(names, 6, 7)
  day       <- substr(names, 8, 9)
  
  date     <- paste0(year,"-",month, "-", day) 
  date_as_date <- as.Date(date)
  return(date_as_date)
}

## -------------------
## expand time series 
## expand time series for x years 

exanding_ts_fun <- function(smp){
  
  tt     <-  smp[ ,1:4]
  smp_tt <-  smp[ ,5:ncol(smp)]
  
  ## get first and las year 
  date_tt <- as.data.frame(cbind(as.integer(substr(tt$Force_name, 2,  5)), 
                                 as.integer(substr(tt$Force_name, 6,  7)),
                                 as.integer(substr(tt$Force_name, 8,  9))))
  colnames(date_tt) <- c("year","month","day")
  min_year <- min(date_tt$year)
  max_year <- max(date_tt$year)
  
  min_year_extra <- min_year -1
  max_year_extra <- max_year +1
  
  ## get min and max years  
  min_year_names <- smp[date_tt$year == min_year,]$Force_name ## get Force name where year of smp is min
  max_year_names <- smp[date_tt$year == max_year,]$Force_name
  
  ## get min max df part 
  min_year <- smp[smp$Force_name %in% min_year_names,]
  max_year <- smp[smp$Force_name %in% max_year_names,]
  
  ## change min max dates 
  min_year$date <- min_year$date - 365
  max_year$date <- max_year$date + 365
  
  ## change min max force names 
  min_year$Force_name <- paste0("X", (as.integer(substr(min_year$Force_name, 2,  5)))-1, substr(min_year$Force_name, 6,  15))
  max_year$Force_name <- paste0("X", (as.integer(substr(max_year$Force_name, 2,  5)))+1, substr(max_year$Force_name, 6,  15))
  max_year <<- max_year
  
  
  ## bind to extended df 
  smp_new <- as.data.frame(rbind(min_year, smp, max_year))
  
  return(smp_new)
}

## variable time series expansion
exanding_ts_fun_variabel <<- function(nbr, dates_nbr){
  
  ## in::
  ## dates_nbr 
  ## nbr :: nbr vector  
  #zoo_time_series <- dts
  ## nbr no NAs 
  #nbr_nona <- as.numeric(zoo_time_series[!is.na(zoo_time_series)])
  #dates_nbr <- as.Date(date_decimal(as.numeric(time(zoo_time_series))))
  #nbr <- x 
  #dates_nbr <- dates
  
  first_date <- dates_nbr[1]
  last_date <- dates_nbr[length(dates_nbr)]
  
  #nbr <- as.numeric(zoo_time_series)
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
  
  ## get part that needs to be added 
  
  if(n_before_1990_to_add > 0){
    dates_to_add <- dates_nona[1:n_before_1990_to_add]
    nbr_to_add   <- nbr_nona[1:n_before_1990_to_add]
    dates_to_add <- dates_to_add - ((year(dates_to_add) - year(as.Date(paste(min_year, 1, 1, sep = "-"))))+1) * 365 *2 +365
    
  }
  ## get part to add at end 
  
  ## rev dates per year at end
  if(n_after_2019_to_add > 0){
    dates_nona_rev <- c(rev(dates_nona[year(dates_nona) ==(max_year-3)]),rev(dates_nona[year(dates_nona) ==(max_year-2)]),rev(dates_nona[year(dates_nona) ==(max_year-1)]),rev(dates_nona[year(dates_nona) ==max_year]))
    dates_to_add_at_end <- rev(dates_nona_rev)[1:n_after_2019_to_add]
    nbr_nona_rev <- c(rev(nbr_nona[year(dates_nona) ==(max_year-3)]),rev(nbr_nona[year(dates_nona) ==(max_year-2)]),rev(nbr_nona[year(dates_nona) ==(max_year-1)]),rev(nbr_nona[year(dates_nona) ==max_year]))
    nbr_to_add_at_end <- rev(nbr_nona_rev)[1:n_after_2019_to_add]
    
    
    ## create new dates 
    # dates_to_add_d <<- dates_to_add
    # min_year_d <<- min_year
    dates_to_add_at_end <- dates_to_add_at_end + ( year(as.Date(paste(max_year, 1, 1, sep = "-")))- (year(dates_to_add_at_end) )) * 365 *2 +365
  }
  
  if(n_before_1990_to_add > 0){
    if(n_after_2019_to_add > 0){
      nbr <- c(nbr_to_add, nbr, nbr_to_add_at_end)
      dates_nbr <- c(dates_to_add, dates_nbr, dates_to_add_at_end)
    } else {
      nbr <- c(nbr_to_add, nbr)
      dates_nbr <- c(dates_to_add, dates_nbr)
    }
  } else {
    if(n_after_2019_to_add > 0){
      nbr <- c( nbr, nbr_to_add_at_end)
      dates_nbr <- c( dates_nbr, dates_to_add_at_end)
    }
  }
  
  
  print(dates_nbr)
  return(list(nbr,dates_nbr,first_date,last_date))
  
  
} ## expanding ts 


## ----------------------------
## preprocessing smp references 

preprocessing_dis_smp <- function(dis_smp) { 
  ## exclude distance of less than x years to start and end 
  ## also exclude less than two years in between 
  
  ## globs
  dis_smp <- dis_smp
  distance_to_start_and_end_of_ts <- 1
  distance_to_other_refs <- 0
  write_stats <- 0
  selected_due_to_distance_start_end <- 0
  
  ## exclude distance of less than two years to start and end 
  binded_start <- dis_smp$image_year - 1985
  binded_end   <- 2021 - dis_smp$image_year 
  if (any(binded_end < distance_to_start_and_end_of_ts | binded_start < distance_to_start_and_end_of_ts)){
    
    ## select for stats 
    selected_due_to_distance_start_end <- dis_smp[(binded_end < distance_to_start_and_end_of_ts) | 
                                                    (binded_start < distance_to_start_and_end_of_ts),]
    selected_due_to_distance_start_end <- subset(selected_due_to_distance_start_end,select= c("plotid","date","change_process","image_year"))
    
    ## mnaipulate selected samples 
    dis_smp <- dis_smp[!(binded_end < distance_to_start_and_end_of_ts) | 
                         (binded_start < distance_to_start_and_end_of_ts),]
  }
  
  ## check for distance between ref breakdates
  binded <- c(1985,dis_smp$image_year,2022)
  ## distance to previous 
  binded <- as.data.frame(cbind(binded, append(100,c(binded[2:length(binded)]-binded[1:(length(binded)-1)]))))
  colnames(binded)  <- c("year", "distance")
  
  ## exclude with distance < 2 a 
  if (any(binded$distance < distance_to_other_refs) ) { ## always exclude the second one. first disturbance can stay 
    print("exclude a disturbance.. to close to other ref.. ")
    # excluded_refs <- dis_smp[dis_smp$image_year == binded[(binded$distance < distance_to_other_refs),]$year, ]
    dis_smp <- dis_smp[!dis_smp$image_year == binded[(binded$distance < distance_to_other_refs),]$year, ]
  }
  
  return(dis_smp)
}

## ------------------------------------------------------
##  selection of reference label for detected breakpoints 
select_ref_label <- function(number_of_years_to_look_in,
                             disturbance_of_validation, dates_of_validation, breakdat_dat) {
  require(lubridate)
  l <- 1 # it over detected breaks 
  for (breakdate in breakdat_dat){   ## loop over break dates
    
    ### create time span where reference label is counted as reference label 
    time_span <- lubridate::interval(as.Date(breakdate) - (number_of_years_to_look_in*365),
                                     as.Date(breakdate) + (number_of_years_to_look_in*365) )
    print(time_span)
    if  ( any(dates_of_validation %within% time_span)){  ## if any ref is in time span go here
      
      disturbance_of_validation_checked  <- disturbance_of_validation[dates_of_validation %within% time_span]
      
      ## if two refs are in time span .. take first 
      if(length(disturbance_of_validation_checked) > 1){
        disturbance_of_validation_checked <- disturbance_of_validation_checked[1 ]
      }
      
    } else { ## if not in reference take placeholder 
      disturbance_of_validation_checked  <- "no_ref"
    }
    
    ## store label 
    if ( l == 1) {
      ref_label <- disturbance_of_validation_checked
    } else {
      ref_label <- append(ref_label, disturbance_of_validation_checked)
    }
    
    l <- l + 1
  }
  ## return ref labels for detected breaks
  return(ref_label)
}

## -------------------------------
## preprocessing of comments
## add informatin about harvest after fire 

preprocessing_of_comments <- function(dis_smp, comments) {
  
  ## check if there are comments for smp
  plot_id <- unique(dis_smp$plotid)[1]
  comments_smp <- subset(comments, plotid == plot_id)
  
  ## 
  comment_of_smp  <- comments_smp$comments
  
  if (nchar(comment_of_smp) > 1){
    dis_smp$comment <- comment_of_smp
  } else {
    dis_smp$comment <- NA
  }
  
  ## add empty columns 
  dis_smp$harvest_fire_one_two_years <- NA
  dis_smp$harvest_fire_same_year     <- NA
  dis_smp$start_fire        <- NA
  dis_smp$end_fire          <- NA
  dis_smp$start_harvest     <- NA
  start_fire <- NA
  end_fire <- NA
  start_harvest <- NA
  harvest_fire_same_year     <- NA
  harvest_fire_one_two_years <- NA
  
  ## check for consequtive fire and harvest
  if (any (unique(dis_smp$change_process) == "Fire")){
    
    ## select fire date 
    fire_date <- dis_smp[dis_smp$change_process == "Fire",]$date
    
    ## look two years later for harvest 
    time_span_harvest <- lubridate::interval(as.Date(fire_date),
                                             as.Date(fire_date) + (2*365) )
    
    disturbances_in_ref <- c("Harvest","Wind","Other","Fire", "Hydrology", "Purple")
    
    ## any other disturbances in two year timespan after fire 
    if(any(dis_smp$date %within% time_span_harvest)){
      Harvest <- subset(dis_smp, dis_smp$date %within%  time_span_harvest & 
                          dis_smp$change_process %in% disturbances_in_ref )
      if(nrow(Harvest) > 0){
        dis_smp$harvest_fire_one_two_years <- 1   
        harvest_fire_one_two_years <- 1
      }
    }
  }
  
  ## check for first word in comments 
  comment_of_smp_split <- strsplit(comment_of_smp, ",")
  
  if(!is.na(comment_of_smp_split[[1]][1])){
    if (comment_of_smp_split[[1]][1] == "start" ){
      print("is fire")#
      
      start_fire         <- as.Date(as.integer(comment_of_smp_split[[1]][4]),
                                    origin = paste0(comment_of_smp_split[[1]][3],"-01-01"))
      end_fire           <- as.Date(as.integer(comment_of_smp_split[[1]][5]),
                                    origin = paste0(comment_of_smp_split[[1]][3],"-01-01"))
      start_harvest      <- as.Date(as.integer(comment_of_smp_split[[1]][8]),
                                    origin = paste0(comment_of_smp_split[[1]][7],"-01-01"))
      
      ## write into smp_dis
      print(dis_smp)
      
      harvest_fire_same_year     <- 1
      dis_smp$harvest_fire_same_year     <- 1
      dis_smp$start_fire     <- start_fire
      dis_smp$end_fire       <- end_fire 
      dis_smp$start_harvest  <- start_harvest 
      
      dates_return <- c(start_fire, end_fire, start_harvest)
      print(dates_return)
    }}
  
  return(dis_smp)
}


## --------------------------------
## extract data from ts fun 

extract_data <- function(tile, smp, n_bps, breakdat_dat,
                         year,  doy,
                         bfpp, bps, selected_ref_labels,
                         segs_split,  index_list, bp,
                         dates,  original_dates,
                         two_years_at_start,half_a_year_at_end,
                         f , breakdat  , amps) {
  
  # tile :: name of tile
  # smp  :: nbr smp breakpoints were detected on 
  # n_bps :: number of detected breaks
  # break_dat :: dates of breaks
  # bfpp :: linear and harmonic fitted trend , also with linear fitted values 
  # segs :: segments of data, splitted at breaks, breakpoint is last of segement
  # selected ref labels :: if any 
  
  print(" ")
  print("------------------------------------------")
  print("-- StartExtract Data at Breakpoints FUN --")
  
  
  it_break <- 1
  print("extracting data from df.. ")
  
  for(number_detected_break in seq(1:n_bps)){
    print("looping over detected breaks.. ")
    print(paste0("break iterator:", it_break))
    
    
    print(paste0("extracting break ",number_detected_break, " of ",n_bps," breaks"))
    ## is there a ref? 
    
    ref_label <- selected_ref_labels[number_detected_break]
    
    is_there_a_ref <- 1
    if( ref_label == "no_ref"){
      is_there_a_ref <- 0 
    }
    
    ## -----------------------------
    ## collected meta data for break 
    print("collecting meta information.. ")
    
    smp_id <- as.integer(smp$id[1])
    
    meta <- c(smp_id , tile, smp$x[1], smp$y[1], n_bps, number_detected_break,
              is_there_a_ref,
              ref_label, as.character(as.Date(breakdat_dat[number_detected_break])),
              doy[number_detected_break], year[number_detected_break])
    
    
    names(meta) <- c("smp_id","tile","x","y","n_bps","id_break","is_there_a_ref","ref_label","date","doy","year")
    print(meta)
    
    ## ----------------------
    ## get strucchange data 
    bp_single <- bps[number_detected_break]
    
    # slopes of segments from linear model 
    slopes_lm <- summary(lm(response ~ segment + segment:time - 1, data=bfpp))$coefficients[(n_bps+2):(2*n_bps+2),1]
    
    ## slope pre 
    slope_pre  <- slopes_lm[number_detected_break]
    slope_post <- slopes_lm[number_detected_break+1]
    
    # pre/post- fitted values 
    pre_value_fitted   <- floor(f[bp_single])
    post_value_fitted  <- floor(f[bp_single + 1])
    
    ## magnitude fitted
    magnitude_fitted <- post_value_fitted - pre_value_fitted
    
    ## avoid 0 division 
    if(post_value_fitted == 0){
      post_value_fitted <- 1
    }
    if(pre_value_fitted == 0){
      pre_value_fitted <- 1
    }
    
    relative_magnitude_fitted <- post_value_fitted / pre_value_fitted
    
    ## magnitude segment pre 
    magnitude_segment_pre <- as.integer(tail(segs_split[[number_detected_break]], n=1) -
                                          head(segs_split[[number_detected_break]], n=1))
    magnitude_segment_post <- as.integer(tail(segs_split[[number_detected_break+1]], n=1) -
                                           head(segs_split[[number_detected_break+1]], n=1))
    
    season    <- as.numeric(substr(breakdat[number_detected_break], 5, 7))
    
    ## -----------------------
    ###### collect fitted data 
    fitted_data <- c(pre_value_fitted, post_value_fitted, magnitude_fitted,relative_magnitude_fitted,
                     slope_pre, slope_post, magnitude_segment_pre, magnitude_segment_post, season)
    fitted_data <- unname(fitted_data)
    names(fitted_data) <- c("pre_val_fit","post_val_fit","magnitude_fit", "relativ_magnitude_fit","slope_pre","slope_post",
                            "magnitude_segment_pre","magnitude_segment_post","season")
    print(paste0("fitted data: ", fitted_data))
    ## -----------------
    ## loop over indixes
    it_in <- 1
    for (Index__ in index_list){
      
      ## ------------------------------------------------------  test 
      test_test <- 0
      if (test_test == 1){
        Index__ <- "nbr"
      }
      print(paste0("extracting Index: ", Index__))
      ## load df 
      
      file_index <- paste0("P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_2_with_outliers_till_2022/",tile,"_",Index__,"_extracted.csv")
      # print(file_index)
      df_index   <- read.table(file_index, header = TRUE, sep = ";", dec = ".")
      
      ## into tydi data 
      df_index_long <- as.data.frame(tidyr::pivot_longer(df_index, -c("id","x","y"), values_to = "Index_name" , names_to = "Force_name"))
      
      ## add date
      df_index_long$date <- get_dates(df_index_long$Force_name)
      
      # subset smp 
      df_index_long <- df_index_long[df_index_long$id == smp_id, ]
      
      # into ts 
      dts_index  <- zoo::zoo(df_index_long$Index_name[1:length(df_index_long$Index_name)],   
                             1900 + as.POSIXlt(dates)$year + (yday(dates) - 1)/365, frequency = 365)
      
      ## non nas of ts 
      nonna_list <- which(!is.na(dts_index))
      
      ## pre and post break value 
      pre_val  <- dts_index[nonna_list[bp_single]]
      
      ## next non na 
      post_val       <- dts_index[nonna_list[bp_single + 1]]
      
      ## mag 
      magnitude <- as.integer(post_val) -as.integer(pre_val)
      
      #print(nonna_list)
      ## mean pre 
      if (length(nonna_list) != 0){
        
        if(anyDuplicated(time(dts_index))){
          print("catched duplicated dates once")
          index(dts_index)[duplicated(date_decimal(time(dts_index)))] <- index(dts_index)[duplicated(date_decimal(time(dts_index)))] + 1/365
        }
        if(anyDuplicated(time(dts_index))){
          print("catched duplicated dates twice")
          index(dts_index)[duplicated(date_decimal(time(dts_index)))] <- index(dts_index)[duplicated(date_decimal(time(dts_index)))] + 1/365
        }
        if(anyDuplicated(time(dts_index))){
          print("catched duplicated dates 3x")
          index(dts_index)[duplicated(date_decimal(time(dts_index)))] <- index(dts_index)[duplicated(date_decimal(time(dts_index)))] + 1/365
        }
        
        ## data break year 
        mean_pre_val   <- as.integer(mean(c(dts_index[nonna_list[bp_single]],dts_index[nonna_list[bp_single-1]],dts_index[nonna_list[bp_single-2]])))
        mean_post_val  <- as.integer(mean(c(dts_index[nonna_list[bp_single+1]],dts_index[nonna_list[bp_single+2]],dts_index[nonna_list[bp_single+3]])))
        mean_magnitude <- mean_post_val - mean_pre_val
        
        ## get quartiles of breakyear and pre and post 
        ## data break year 
        
        data_break_year <- dts_index[year(dates) == year[number_detected_break]]
        quantiles_break_year  <- as.integer(quantile(na.omit(data_break_year), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
        iqr_breakyear <- quantiles_break_year[4]-quantiles_break_year[2] 
        
        ## data pre break year 
        data_pre_break_year       <- dts_index[year(dates) == (year[number_detected_break]-1)]
        quantiles_pre_break_year  <- as.integer(quantile(na.omit(data_pre_break_year), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
        iqr_pre_breakyear         <- quantiles_pre_break_year[4]-quantiles_pre_break_year[2] 
        
        ## data post break year 
        data_post_break_year <- dts_index[year(dates) == (year[number_detected_break]+1)]
        quantiles_post_break_year  <- as.integer(quantile(na.omit(data_post_break_year), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
        iqr_post_breakyear <- quantiles_post_break_year[4]-quantiles_post_break_year[2] 
        
        ## get one observation per year 
        obs_break_year       <- dts_index[nonna_list][which.min(abs(dates[nonna_list] - as.Date(paste0(year[number_detected_break],"-07-01"))))]
        obs_pre_break_year   <- dts_index[nonna_list][which.min(abs(dates[nonna_list] - as.Date(paste0((year[number_detected_break]-1),"-07-01"))))]
        obs_post_break_year  <- dts_index[nonna_list][which.min(abs(dates[nonna_list] - as.Date(paste0((year[number_detected_break]+1),"-07-01"))))]
        
        
      } ## ## all NA 
      
      ## ------------------
      ## collect index data 
      if(it_in == 1){
        
        ## set names 
        index_result_names       <- c(paste0(Index__,"_pre_val"), paste0(Index__,"_post_val"), paste0(Index__,"_magnitude"),
                                      paste0(Index__,"_mean_pre_val"), paste0(Index__,"_mean_post_val"), paste0(Index__,"_mean_magnitude"),
                                      paste0(Index__,"_q05_breakyear"), paste0(Index__,"_q25_breakyear"), paste0(Index__,"_q50_breakyear"),
                                      paste0(Index__,"_q75_breakyear"), paste0(Index__,"_q95_breakyear"), paste0(Index__,"_iqr_breakyear"),
                                      paste0(Index__,"_q05_pre_breakyear"), paste0(Index__,"_q25_pre_breakyear"), paste0(Index__,"_q50_pre_breakyear"),
                                      paste0(Index__,"_q75_pre_breakyear"), paste0(Index__,"_q95_pre_breakyear"), paste0(Index__,"_iqr_pre_breakyear"),
                                      paste0(Index__,"_q05_post_breakyear"), paste0(Index__,"_q25_post_breakyear"), paste0(Index__,"_q50_post_breakyear"),
                                      paste0(Index__,"_q75_post_breakyear"), paste0(Index__,"_q95_post_breakyear"), paste0(Index__,"_iqr_post_breakyear"),
                                      paste0(Index__,"_pre_breakyear_single_value"), paste0(Index__,"_breakyear_single_value"), paste0(Index__,"_post_breakyear_single_value"))
        
        
        if(length(nonna_list) != 0){
          index_results_out        <- c(as.integer(pre_val), as.integer(post_val), magnitude, mean_pre_val, mean_post_val, mean_magnitude,
                                        quantiles_break_year, iqr_breakyear, quantiles_pre_break_year, iqr_pre_breakyear,
                                        quantiles_post_break_year, iqr_post_breakyear, 
                                        obs_break_year, obs_pre_break_year, obs_post_break_year)} 
        if(length(nonna_list) == 0){
          index_results_out <- rep( NA, 27) 
          print("substitude with NA")
        }
        ## names of out in first iteration 
        names(index_results_out) <- index_result_names
        #print(index_results_out)
        
      } else { # first break iteration 
        
        ## set names 
        index_result_names      <- c(paste0(Index__,"_pre_val"), paste0(Index__,"_post_val"), paste0(Index__,"_magnitude"),
                                     paste0(Index__,"_mean_pre_val"), paste0(Index__,"_mean_post_val"), paste0(Index__,"_mean_magnitude"),
                                     paste0(Index__,"_q05_breakyear"), paste0(Index__,"_q25_breakyear"), paste0(Index__,"_q50_breakyear"),
                                     paste0(Index__,"_q75_breakyear"), paste0(Index__,"_q95_breakyear"), paste0(Index__,"_iqr_breakyear"),
                                     paste0(Index__,"_q05_pre_breakyear"), paste0(Index__,"_q25_pre_breakyear"), paste0(Index__,"_q50_pre_breakyear"),
                                     paste0(Index__,"_q75_pre_breakyear"), paste0(Index__,"_q95_pre_breakyear"), paste0(Index__,"_iqr_pre_breakyear"),
                                     paste0(Index__,"_q05_post_breakyear"), paste0(Index__,"_q25_post_breakyear"), paste0(Index__,"_q50_post_breakyear"),
                                     paste0(Index__,"_q75_post_breakyear"), paste0(Index__,"_q95_post_breakyear"), paste0(Index__,"_iqr_post_breakyear"),
                                     paste0(Index__,"_pre_breakyear_single_value"), paste0(Index__,"_breakyear_single_value"), paste0(Index__,"_post_breakyear_single_value"))
        
        
        if(length(nonna_list) != 0){
          index_results       <- c(as.integer(pre_val), as.integer(post_val), magnitude, mean_pre_val, mean_post_val, mean_magnitude,
                                   quantiles_break_year, iqr_breakyear, quantiles_pre_break_year, iqr_pre_breakyear,
                                   quantiles_post_break_year, iqr_post_breakyear, 
                                   obs_break_year, obs_pre_break_year, obs_post_break_year) 
        } 
        
        if(length(nonna_list) == 0){
          index_results <- rep( NA, 27)    
          print("substitude with NA")
        }
        
        names(index_results) <- index_result_names
        index_results_out <- unlist(list(index_results_out, index_results))
      } ## else not first iteration 
      
      ## it of n index 
      it_in <- it_in + 1
      
    } # loop over index
    
    ## bind data together for each break  
    smp_data <- list(meta, fitted_data, index_results_out)
    smp_data <- unlist(smp_data)
    
    if(it_break == 1) {
      smp_data_out <- smp_data 
    } else {
      smp_data_out <- rbind(smp_data, smp_data_out)
    }
    
    ## n break iterator 
    it_break <- it_break + 1
    
    
  }## loop over detetced breaks 
  
  smp_data_out <- as.data.frame(smp_data_out)
  
  ## safety net 
  if(ncol(smp_data_out) == 1){
    print("safety again to dataframe-...")
    smp_data_out <- data.frame(as.list(smp_data))
    
  }
  
  row.names(smp_data_out) <- NULL
  return(smp_data_out)
  
} ## function extract data




#### ------------------------------------
#### WORKER FUN 

## function that will detect breaks in ts of reference smp / one at a time  
## if breaks are detected it will extract variables 

do_fun_at_ref_smps <- function(x, dates,original_dates, dates_of_validation , disturbance_of_validation, 
                               dis_smp,smp,  ## whole reference dis sample, for comments and harvest after fire 
                               smp_id, tile,
                               are_there_disturbances, Index_, corrupt,number_of_years_to_look_in,
                               additional_title , h, breaks, index_list,
                               two_years_at_start,half_a_year_at_end, n_smp_finished ,output_name,
                               output_folder_name){
  
  ## load packages   
  require(strucchangeRcpp, quietly = TRUE)
  require(lubridate, quietly = TRUE)
  require(zoo, quietly=TRUE)
  require(bfast, quietly = TRUE)
  require(snow, quietly = TRUE)
  require(ggplot2)
  require(MASS)
  
  
  print("------------------------------")
  print("-- Start Detect Breaks FUN")
  
  ## globals 
  order         <- 1
  plot          <- 1
  
  ## data into ts  
  dts  <- zoo(x[1:length(x)],   
              1900 + as.POSIXlt(dates)$year + (yday(dates) - 1)/365, frequency = 365)
  
  first_date <- dates[1] + 365
  last_date  <- dates[length(dates)] -365
  
  # plot(dts)
  ## ---------------------------------------------------------------------------
  ## count how many observations before 1990 
  
  # dts <<- dts
  
  year          <- year(as.Date(as.yearmon(time(na.omit(dts)))))
  n_before_1990 <- min(which(year == 1990))
  year_40_reached <- year(as.Date(as.yearmon(time(na.omit(dts))))[40])
  # doys_after_1990 <- as.numeric(as.Date(as.yearmon(time(na.omit(dts))))[40] - as.Date("1990-01-01"))
  
  n_after_2020 <- length(year[year == 2021])
  year_40_reached_hinten <- rev(year(as.Date(as.yearmon(time((na.omit((dts)))))))) [40]
  
  out_40s <-data.frame(tile, smp_id, n_before_1990, year_40_reached,n_after_2020,year_40_reached_hinten)
  
  ## write for each tile 
  ifelse(!dir.exists(paste0("P:/workspace/jan/fire_detection/break_detection/meta/", output_folder_name)),
         dir.create(paste0("P:/workspace/jan/fire_detection/break_detection/meta/", output_folder_name)), FALSE)
  ## 
  
  ifelse(!dir.exists(paste0("P:/workspace/jan/fire_detection/break_detection/meta/", output_folder_name,"/",tile)),
         dir.create(paste0("P:/workspace/jan/fire_detection/break_detection/meta/", output_folder_name,"/",tile)), FALSE)
  ## 
  
  # tile <- "X0065_Y0040"
  out_40s_csv <- paste0("P:/workspace/jan/fire_detection/break_detection/meta/", output_folder_name,tile,"/",tile,"_when_40_reached.csv")
  colnames(out_40s) <- c("tile","smp_id","n_before_1990","year_40_reached","n_after_2020","year_40_reached_from_back")
  
  if(file.exists(out_40s_csv) == TRUE){
    
    ## load file 
    out_40s_before <- read.table(out_40s_csv,
                                 sep =";", dec =".", header = TRUE)
    ## join 
    out_40s <- as.data.frame(rbind(out_40s_before, out_40s))
    
  }
  write.table(x = out_40s,  file = out_40s_csv, sep =";", dec = ".", col.names = TRUE, row.names = FALSE)
  
  
  ## ---------------------------------------------------------------------------
  ## expansion of time series
  print("expanding time series")
  # dts <<- dts
  
  # x <<- x
  # dates <<- dates
  
  #expanded <<- exanding_ts_fun_variabel(x,dates)
  
  #print("expanded.. ")
  # expanded <<- expanded
  #x <- expanded[[1]]
  #dates <- expanded[[2]]
  
  # n_before_1990 <- expanded[[3]]
  # n_after_2019 <- expanded[[4]]
  #first_date <- expanded[[3]]
  #last_date <- expanded[[4]]
  
  ## if we have feb 29 th replace 
  dates[month(dates)==2 & day(dates)==29] <- dates[month(dates)==2 & day(dates)==29] -1
  dates[month(dates)==3 & day(dates)==1] <- dates[month(dates)==3 & day(dates)==1] +1
  dates <- as.Date(dates)
  
  dates[duplicated(dates)]
  
  dts  <- zoo(x[1:length(x)],   
              1900 + as.POSIXlt(dates)$year + (yday(dates) - 1)/365, frequency = 365)
  # plot(dts)
  
  ## ---------------------------------------------------------------------------
  ## preprocessing of ts, adding harmonuic / linear terms
  order <- 1
  
  index(dts)[duplicated(date_decimal(time(dts)))] <- index(dts)[duplicated(date_decimal(time(dts)))] + 1/365
  index(dts)[duplicated(date_decimal(time(dts)))] <- index(dts)[duplicated(date_decimal(time(dts)))] + 1/365
  index(dts)[duplicated(date_decimal(time(dts)))] <- index(dts)[duplicated(date_decimal(time(dts)))] + 1/365
  
  bfpp <- bfastpp(dts, order=order)
  
  ## detect breakpoints of time series 
  print("detecting breaks.. ")
  library(strucchangeRcpp)
  # bfpp <<- bfpp 
  # breaks <<- breaks 
  # h <<- h
  
  
  if (corrupt == 0){ ## meaning not only NA in data
    bp <- try(strucchangeRcpp::breakpoints(response ~ trend + harmon, data=bfpp, h=h, breaks=breaks), silent=FALSE)
    print("....................................... ")
    
    
    ## if no breakpoints are detected 
    if (is.na(bp$breakpoints[1]) || (class(bp)[1]=="try-error")) { ## if no breakpoints detected or 
      
      print("no breakpoints detected..") 
      smp_data_out <- NULL
      
      dates.obs <- dates[order(dates)][which(!is.na(dts))]
      obs.val   <- as.numeric(na.omit((dts)))
      
      dat <- data.frame(source=obs.val, date=dates.obs)
      
      min <- min(dat$source)
      max <- max(dat$source)
      
      ## plot without breakpoints
      out.plot <- ggplot(dat, aes(date, source)) +
        geom_point(alpha=.3) +
        xlab("Date") +
        # ylab("NBR") +
        ylab("TCW") +
        theme_bw() +
        ggtitle(paste0("no disturbance detected in smp: ",smp_id, "; tile: ", tile, " with Index: ", "NBR", " ",additional_title))
      
      # set naming
      additional <- "_no_disturbance_detected"
      
      ## create tile folder
      ifelse(!dir.exists(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/", "plots_",output_folder_name,"_no_breaks_detected")),
             dir.create(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/", "plots_", output_folder_name,"_no_breaks_detected")), FALSE)
      ifelse(!dir.exists(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/", "plots_",output_folder_name,"_no_breaks_detected/",tile)),
             dir.create(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/", "plots_", output_folder_name,"_no_breaks_detected/",tile)), FALSE)
      ## 
      
      print(paste0( smp_id, "_plot_",Index_,"_strucchange",additional,".png" ))
      print(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/","plots_",output_folder_name,"_no_breaks_detected/",tile))
      ggsave(filename = paste0( smp_id, "_plot_",Index_,"_strucchange",additional,".png" ),
             path = paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/","plots_",output_folder_name,"_no_breaks_detected/",tile),
             dpi = 150, width = 10, height = 5)
      saveRDS(out.plot, file =paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/","plots_",output_folder_name,"_no_breaks_detected/",tile,"/", smp_id, "_plot_",Index_,"_strucchange",additional,"",additional_title,".rds" ) )
      
      
    } else { ## if breakpoints are detcted or no error in bfast
      
      ## check if breakpoints are detected 
      if (!is.na(bp$breakpoints[1]) && (corrupt ==  0)){
        ## breakpoints detected
        
        bfpp$segment         <- strucchangeRcpp::breakfactor(bp) ## which segement is observation assignt to 
        levels(bfpp$segment) <- as.character(1:nlevels(bfpp$segment))
        # tt <- bfpp
        
        ## fit linear trend for each segment
        f   <- fitted(lm(response ~ segment + segment:time - 1, data=bfpp))
        bfpp$fitted <- f
        
        bps   <- bp$breakpoints
        n_bps <- length(bps)
        print(paste0("deteced ", n_bps, " breakpoints!"))
        
        ## get date of detected breaks 
        breakdat      <- bfpp[bps,]$time
        breakdat_dat  <- as.Date(date_decimal(breakdat)) + 1
        breakyear     <- floor(breakdat)
        print(paste0("deteced ", breakdat,breakdat_dat, " breakpoints!"))
        doy           <- yday(breakdat_dat)
        
        plot(bfpp$time, bfpp$response)
        abline(v = breakdat, lty = 1, col="blue", lwd=1.2)
        
        
        ## get labels for detected breakpoints within 1 year pre and post 
        ## if more than one take the first 
        selected_ref_labels <- select_ref_label(number_of_years_to_look_in,
                                                disturbance_of_validation, dates_of_validation, breakdat_dat)
        
        print(paste0("selected ref labels: ", selected_ref_labels))
        
        #  split segments at breakpoints ## for each segment gets values per timestep
        segs_split <- unname(split(f, cumsum(seq_along(f) %in% (bps+1))))
        # print(segs_split)
        
        ##-------------------------------------------------------------------
        ## fitting model (for plotting)
        m <- rlm (response ~ segment/(trend+harmon), data=bfpp[,], maxit=100)
        # 
        coef <- coefficients(m)
        # Int <- coef[1]
        # ##Extract trends
        # trends <- coef[which(grepl('trend', names(coef)))]
        # ##Extract Amplitudes for each segment. Depending on parameter "order" one gets
        #multiple sine and cosine terms
        
        cosines <- coef[which(grepl('harmoncos', names(coef)))]
        sines <- coef[which(grepl('harmonsin', names(coef)))]
        amps <- sqrt(cosines^2 + sines^2)
        names(amps) <- rep(1:length(unique(bfpp$segment)), order)
        
        # # trend <- bfpp[bfpp$segment == 1,]
        # # trend$days <- as.numeric(substr(trend$time, 6, 8))
        # # dmean <- mean(trend$days)
        # # har <- dmean/365
        # # trend$harmon[] <- rep(
        # #   c(cos(2 * pi * har * 1:order), sin(2 * pi * har * 1:order)),
        # #   each = nrow(trend))
        # # bfpp$trendprediction[bfpp$segment == 1] <- predict(m, newdata = trend)
        # 
        
        # ## extract data at breaks
        # returns data of all breaks of one smp 
        smp_data_out <- extract_data(tile = tile, smp = smp, breakdat_dat = breakdat_dat,
                                     year = breakyear, doy = doy, breakdat  = breakdat,
                                     n_bps = n_bps, bfpp = bfpp, bps = bps, selected_ref_labels = selected_ref_labels,
                                     segs_split = segs_split, index_list = index_list, bp = bp,
                                     dates = dates, two_years_at_start = two_years_at_start,
                                     half_a_year_at_end = half_a_year_at_end, f = f, amps = amps)
        
        
        ## write data of sample; extent existing tile table
        print("extracted data.. ")
        # print(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_tables/",output_folder_name,tile,output_name ))
        if(file.exists(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_tables/",output_folder_name,tile,output_name,".csv" ))){
          
          ## load file 
          smp_before <- read.table(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_tables/",output_folder_name,tile,output_name ,".csv"),
                                   sep =";", dec =".", header = TRUE)
          ## join 
          smp_data_out <- as.data.frame(rbind(smp_before, smp_data_out))
        }
        
        write.table(smp_data_out, paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_tables/",output_folder_name,tile,output_name,".csv" ), sep =";", dec = ".", col.names = TRUE, row.names = FALSE)
        
        ## --------------------------------------------------------------------
        ##                        PLOT
        
        if((plot == 1) == TRUE)  {
          
          print("plotting.. ")
          
          f <- do.call(c, segs_split)
          dates.obs <- dates[order(dates)][which(!is.na(dts))]
          obs.val   <- as.numeric(na.omit((dts)))
          
          slopes <- coef(bp)[,2]
          dat <- data.frame(fitted=fitted(bp),linear=f, source=obs.val, date=dates.obs)
          
          ## min max for plot range 
          min <- min(dat$source)
          max <- max(dat$source) + 200
          
          if (min > 0){
            min <- -500
          } else {
            min <- min - 250
          }
          
          #confidence invervals
          ci <- confint(bp, breakpoints=TRUE)
          ci <- ci 
          bda <- ci[[1]]
          
          cid_ <-as.Date(date_decimal( bfpp$time[c(bda[,1],bda[,3])]))
          print(cid_)
          cid <- data.frame(matrix(cid_, ncol = 2))
          colnames(cid) <- c("min_cid","max_cid")
          cid$min_cid <- as.Date(cid$min_cid, origin ="1970-01-01")
          cid$max_cid <- as.Date(cid$max_cid, origin ="1970-01-01")
          cid <<- cid
          ##--------------------------------
          
          
          ## creating artificial dates for fited values 
          bfpp$date <- as.Date(date_decimal(bfpp$time)) + 1
          ## for each segment get min max dates 
          
          bfpp_split <- unname(split(bfpp, cumsum(seq_along(f) %in% (bps+1))))
          maxs <- as.Date(sapply(bfpp_split, function(x) max(x$date)))
          mins <- as.Date(sapply(bfpp_split, function(x) min(x$date)))
          minmax <- as.data.frame(cbind.data.frame(mins, maxs))
          
          
          constant_dates <- apply(minmax,1,function(x) { seq.Date( as.Date(x['mins']), as.Date(x['maxs']), by='15 days') } )
          
          #constant_datess <<- constant_dates
          # constant_dates <-constant_datess
          
          if("matrix" %in% class(constant_dates)){
            constant_dates <- as.Date(constant_dates)
            constant_dates <- split(constant_dates, c(rep(1,nrow(constant_dates)),rep(2,nrow(constant_dates))))
            
          }
          
          print("here")
          
          constant_dates<- (do.call(c, constant_dates))
          constant_dates <- as.data.frame(constant_dates)
          constant_dates$time <- decimal_date(constant_dates$constant_dates)
          
          
          print("here")
          # add segments 
          constant_dates$segment <- 0
          for ( oo in seq(1:nrow(minmax))){
            constant_dates[constant_dates$constant_dates >= minmax[oo,]$mins & 
                             constant_dates$constant_dates <= minmax[oo,]$maxs,]$segment <- oo
          }
          constant_dates$segment <- as.factor(constant_dates$segment)
          print("here")
          ## we need trend term
          m_linear <- (lm(response ~ segment + segment:time - 1, data=bfpp))
          constant_dates$linear <- predict(m_linear, newdata = constant_dates)
          
          ## we need harmonic term
          # bfpp_split <<- bfpp_split
          # bp <<- bp
          # constant_dates <<- constant_dates
          # 
          # amps <<- amps 
          coefs <- as.data.frame(coef(bp))
          
          y.har.cos.sin <- function(x,y) coefs$harmoncos[y] * cos(2 * pi * x) + coefs$harmonsin[y] * sin(2 * pi * x) # + coefs$trend[y] * x +  coefs$`(Intercept)`[y]
          # added_linear  <- function(x,y) x + amps[y]/2
          
          constant_dates$harmonic <- y.har.cos.sin(constant_dates$time, constant_dates$segment) + constant_dates$linear 
          # constant_dates$harmonic <- y.har.cos.sin(constant_dates$time, constant_dates$segment)
          # added_linear(constant_dates$linear, constant_dates$segmen)
          ggplot(constant_dates, aes(x= constant_dates, y= linear)) + geom_line() +
            geom_line(data = constant_dates, aes(x= constant_dates, y= harmonic))
          
          
          if(are_there_disturbances == 1){ 
            
            ## ----------------------------------------  
            ## recorded disturbances in reference data 
            
            
            ## todo :: add cids :: cid
            
            # dat <<- dat
            # date_s <<- date
            # date <- date_s
            # source_s <<- source
            # source <- source_s
            # Index_ <<- Index_
            # dates_of_validation <<- dates_of_validation
            # disturbance_of_validation <<- disturbance_of_validation
            # breakdat_dat <<- breakdat_dat
            # smp_id <<- smp_id 
            # additional_title <<- additional_title
            # min_s <<- min
            # min <- min_s
            
            print("plotting with ggplot.. ")
            dat$source <- dat$source / 10000 
            dat$fitted <- dat$fitted / 10000
            dat$linear <- dat$linear / 10000
            constant_dates$harmonic <- constant_dates$harmonic/10000
            min_min <- min / 10000
            max_max <- max / 10000
            
            
            out.plot <- ggplot(dat, aes(date, source)) +
              geom_point(alpha=.3) +
              geom_line(aes(date, linear), colour="firebrick4") + 
              # geom_line(aes(date, fitted), colour="grey40", alpha = 0.3) + 
              geom_line(data = constant_dates, aes(x=constant_dates, y=harmonic), colour="grey40", alpha = 0.8) +
              
              xlab("Date") +
              ylab(Index_) +
              theme_bw() +
              ## disturbance ref 
              geom_vline(xintercept = dates_of_validation, linetype = "dashed", size = 0.8, colour = "grey12") + ## validated breaks 
              # geom_text(data=data.frame(x= dates_of_validation, y=(min_min ) ), aes(x, y), label= disturbance_of_validation, 
              #           vjust=-0.4, hjust = 0.0120, angle = 90) + ## label line 
              geom_text(data=data.frame(x= dates_of_validation, y=(min_min -0.1 ) ), aes(x, y), label= disturbance_of_validation,
                        vjust=-0.44, hjust = 1.30220, angle = 270, colour = "black")  + ## label line
              
              ## detected break
              geom_vline(xintercept = breakdat_dat, linetype = "dashed", size = 0.8, colour = "red2") + ## validated breaks 
              # geom_text(data=data.frame(x= breakdat_dat, y=(min_min + 0.02 ) ), aes(x, y), label= "detected", 
              #           vjust=-0.54, hjust = 1.90220, angle = 270, colour = "red") + ### label line
              ggtitle(paste0("smp: ",smp_id, "; tile: ", tile, " Index: ", Index_, " ",additional_title)) +
              
              
              ## add ci 
              geom_vline(xintercept = cid_, linetype = "dotted", size = 0.45, colour = "dodgerblue4", alpha = 0.45) +
              annotate("rect",xmin = cid$min_cid, xmax = cid$max_cid, ymin = -Inf, ymax = Inf,
                       alpha = .1, fill = "dodgerblue4") +  #, color = "dodgerblue1") +
              
              ## add expandet dates 
              geom_vline(xintercept = first_date, linetype = "twodash", size = 0.55, colour = "darkseagreen4", alpha = 0.65) +
              geom_vline(xintercept = last_date, linetype = "twodash", size = 0.55, colour = "darkseagreen4", alpha = 0.65) +
              # 
              
              ylim(min_min - 0.1, max_max + 0.1) 
            
            
            # geom_text(data=data.frame(x= as.Date("2000-01-01"), y=(min ) ), aes(x, y), label= 
            #             paste(unname(slopes[1]),unname(slopes[2]),unname(slopes[3])), 
            #           vjust=-0.5, hjust = 0.20220, angle = 0, colour = "red", size = 3.8) +
            # geom_text(data=data.frame(x= as.Date("2000-01-01"), y=(min ) ), aes(x, y), label= 
            #             paste(unname(slopes_2[1]),unname(slopes_2[2]),unname(slopes_2[3])), 
            #           vjust=0.6, hjust = 0.20220, angle = 0, colour = "blue", size = 3.8)
            # 
            
            # set naming 
            additional <- ""
            
            if ("Fire" %in% disturbance_of_validation){
              additional <- "_fire"
            }
            
            ## fire and harvest in next two years 
            if(nrow(dis_smp) > 0){ ## safety if there are 
              if (!is.na(dis_smp$harvest_fire_one_two_years) ){
                additional <- "_fire_with_harvest_in_next_two_years"
                
              }
              ## fire and harvest in next two years 
              if (!is.na(dis_smp$harvest_fire_same_year) ){
                additional <- "_fire_with_harvest_in_same_year"
                
                out.plot <- out.plot +
                  geom_vline(xintercept = dis_smp$start_harvest , linetype = "dashed", size = 0.6, colour = "blue") +
                  geom_text(data=data.frame(x= dis_smp$start_harvest, y=(min +20) ), aes(x, y), label= "Start Harvest", 
                            vjust=-0.4, hjust = 1.0120, angle = 90, size = 0.4, colour = "blue") 
                
                
              } ## fire and harvest in same year 
            } ## if dis_smp nrow > 0 
          }
          
          ## ----------------------------------------
          else { ## no disturbances in validation data 
            
            print("plotting with ggplot.. ")
            dat$source <- dat$source / 10000 
            dat$fitted <- dat$fitted / 10000
            dat$linear <- dat$linear / 10000
            constant_dates$harmonic <- constant_dates$harmonic/10000
            min_min <- min / 10000
            max_max <- max / 10000
            
            
            out.plot <- ggplot(dat, aes(date, source)) +
              geom_point(alpha=.3) +
              geom_line(aes(date, linear), colour="firebrick4") + 
              # geom_line(aes(date, fitted), colour="grey40", alpha = 0.3) + 
              geom_line(data = constant_dates, aes(x=constant_dates, y=harmonic), colour="grey40", alpha = 0.8) +
              
              xlab("Date") +
              ylab(Index_) +
              theme_bw() +
              #geom_vline(xintercept = dates_of_validation, linetype = "dashed", size = 0.8, colour = "grey39") + ## validated breaks 
              #geom_text(data=data.frame(x= dates_of_validation, y=(min +20) ), aes(x, y), label= disturbance_of_validation, 
              #          vjust=-0.4, hjust = 0.0120, angle = 90) + ## label line 
              geom_vline(xintercept = breakdat_dat, linetype = "dotted", size = 0.8, colour = "red") + ## validated breaks 
              # geom_text(data=data.frame(x= breakdat_dat, y=(min ) ), aes(x, y), label= "detected",
              #           vjust=-0.54, hjust = 1.90220, angle = 270, colour = "red") + ### label line
              ggtitle(paste0("undisturbed smp: ",smp_id, "; tile: ", tile, "Index: ", Index_, " ",additional_title)) +
              ## add ci 
              geom_vline(xintercept = cid_, linetype = "dotted", size = 0.45, colour = "dodgerblue4", alpha = 0.45) +
              annotate("rect",xmin = cid$min_cid, xmax = cid$max_cid, ymin = -Inf, ymax = Inf,
                       alpha = .1, fill = "dodgerblue4") +  #, color = "dodgerblue1") +
              
              ## add expandet dates 
              geom_vline(xintercept = first_date, linetype = "twodash", size = 0.55, colour = "darkseagreen4", alpha = 0.65) +
              geom_vline(xintercept = last_date, linetype = "twodash", size = 0.55, colour = "darkseagreen4", alpha = 0.65) +
              # 
              
              ylim(min_min - 0.1, max_max + 0.1) 
            
            # set naming 
            additional <- paste0("_no_disturbance_in_reference" )
            
          }
          
          ## plot comment 
          if ( are_there_disturbances == 1){
            if (!is.na(dis_smp$comment)) {
              out.plot <- out.plot + 
                labs( subtitle = unique(dis_smp$comment))
            }} ## if there disturbances and comments 
          
          
          print(out.plot)
          
          ## -------------------------------------------------------------------
          ## Safe PLOT 
          
          ## create tile folder 
          ifelse(!dir.exists(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/plots_",output_folder_name)),
                 dir.create(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/plots_",output_folder_name)), FALSE)
          
          ifelse(!dir.exists(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/plots_",output_folder_name,tile,"/")),
                 dir.create(paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/plots_",output_folder_name,tile,"/")), FALSE)
          ## safe plot # smp_id 2112
          ggsave(filename = paste0( smp_id, "_plot_",Index_,"_strucchange",additional,"_",additional_title,".png" ), path = paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/plots_",output_folder_name,tile,"/"),
                 dpi = 300, width = 10, height = 5)
          saveRDS(out.plot, file =paste0("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/","plots_",output_folder_name,tile,"/", smp_id, "_plot_",Index_,"_strucchange",additional,"",additional_title,".rds" ) )
          
          
        } #plot
        
        
        # return(out)
        # 
      }
      
    }
    
    
  }## if strucchange has no error 
  
  print("finished sample.. ")
  return(smp_data_out)
} ## function 


## ------------------------------------------------------------------------------
### MAIN FUN 

## loop over tiles
#for(tile in list_tiles[1:10] ){ # length(list_tiles)]){
main_fun  <- function(tile){
  
  # tile <- "X0068_Y0045"
  ## load data df / index to base detection on 
  Index_ <- "TCW"
  df_file <- paste0("P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_2_with_outliers_till_2022/",tile,"_",Index_,"_extracted.csv")
  print("------------------------------")
  print("------- Start Main FUN -------")
  print("                              ")
  ## check if excists 
  if (file.exists(df_file)){
    print(paste0("tile: ", tile, "loading extracted ts df.. "))
    # load file 
    df_ts <- read.table(df_file, header = TRUE, dec = ".", sep = ";")
    data_excists_in_tile  <- 1
  } else {
    print(paste0("tile: ", tile, " has no data"))
    data_excists_in_tile <- 0
  }
  
  n_smp_finished_in_tile <- 0
  n_smp_finished <- 0
  
  ## ----------------------------------------------------------------------
  ## nach prozessierung 
  if (file.exists(df_file)){
    ids_in_tile <- unique(df_ts$id)
    
    # ids_nach    <- read.table("P:/workspace/jan/fire_detection/nachprocessing_ids.csv", sep =";", dec =".", header = TRUE)
    # ids_in_tile <<- ids_in_tile
    # ids_in_tile <- ids_in_tile[ids_in_tile %in% ids_nach$x]
    
    if(length(ids_in_tile) == 0) {
      print(paste0("tile: ", tile, " has no data"))
      data_excists_in_tile <- 0
    }}
  
  ##------------------------------------------------------------------------
  
  ## check again if data for tile is there 
  if(data_excists_in_tile == 1){
    
    ## get dates  
    date_names <- names(df_ts)[4:length(df_ts)]
    date_dates <- get_dates(date_names)
    
    ## into tidy data 
    df_ts_long <- as.data.frame(tidyr::pivot_longer(df_ts, -c("id","x","y"), values_to = "Index_name" , names_to = "Force_name"))
    
    ## add date
    df_ts_long$date <- get_dates(df_ts_long$Force_name)
    
    ## get unique ids of tile from saved df 
    
    ## !!
    # ids_in_tile <- unique(df_ts$id)
    
    print(paste0("","tile: ", tile, " has ",length(ids_in_tile)," samples.. "))
    print(ids_in_tile)
    
    ## loop over smps 
    n_smp_finished_in_tile <- 1
    n_smp_finished <- 1
    
    for (smp_id in ids_in_tile){
      #smp_id <- 37
      # smp_id <- 863
      ## exclude duplicated smps 
      double <-  c(3142,3144,1882)
      if (!(smp_id %in% double)){
        
        print(paste0("processing smp with id ", smp_id))
        smp <- df_ts_long %>% subset(id == smp_id)
        
        ## check if samp is broken (only NAs)
        if(all(is.na(smp$Index_name))==TRUE) {
          print("sample is corrupted.. skip..  ")
          corrupt <- 1
        } else {
          print("sample is not corrupted ")
          corrupt <- 0
          
          make_ts_longer <- 1
          ## add additional years at begning and end 
          if (make_ts_longer == 1){
            print("expanding ts..  ")
            smp_old <<- smp
            
            smp <- exanding_ts_fun(smp)
            print("expanded.. ")
            
            ## how many obs added at start
            n_obs_added_start <- which(smp$date == as.Date(smp_old$date[1][1]),  arr.ind=TRUE) - 1
            
            ## how many added at end 
            n_obs_added_end   <- nrow(smp)  - which(smp$date == as.Date(smp_old$date[nrow(smp_old)][1]),  arr.ind=TRUE) - 1
            
            ## create flag for observation plots 
            flag_added <- c(rep(1,n_obs_added_start), rep(0,nrow(smp)),rep(1,n_obs_added_end))
            
          }  else {
            n_obs_added_start <- 0 
          }
        }
        
        ## get disturbances validation of smp 
        dis_smp <- interpretations %>% subset(plotid == smp_id & disturbance == 1)
        
        if (nrow(dis_smp) == 0) {
          are_there_disturbances <- 0 
          print("no disurbances in sample ref ")
        } else {
          are_there_disturbances <- 1 
          
          ## preprocessing of interpreations
          ## -----------------------------------
          ## exclusion of refs to close to start and end or to each other
          dis_smp_1 <<- dis_smp
          
          dis_smp <- preprocessing_dis_smp(dis_smp) ## exclusion of references to close to start and finsish 
          dis_smp_2 <<- dis_smp 
          
          ## find way to safe both in meta 
          
          # print("processed reference.. ")
          #### inclusion of comments as reference
          dis_smp <- preprocessing_of_comments(dis_smp, comments)
          dis_smp_3 <<- dis_smp 
          
          print("comments preprocessed.. ")
        }
        
        
        ## returns break data of one smp
        # require(Rcpp)
        break_data_smp <- do_fun_at_ref_smps(x = smp$Index_name, dates = smp$date, dates_of_validation = dis_smp$date , 
                                             disturbance_of_validation = dis_smp$change_process, dis_smp = dis_smp, smp = smp,
                                             smp_id, tile, are_there_disturbances = are_there_disturbances,
                                             Index_ = Index_, corrupt = corrupt, number_of_years_to_look_in = number_of_years_to_look_in, 
                                             additional_title = additional_title, h = h, breaks = breaks, original_dates = date_dates,
                                             two_years_at_start = two_years_at_start,half_a_year_at_end = half_a_year_at_end, index_list = index_list,
                                             n_smp_finished  = n_smp_finished,output_name = output_name,
                                             output_folder_name  = output_folder_name)## for expandng of ts dfs 
        # break_data_smp_2 <<- break_data_smp
        
        
        ## combine break data of smps 
        if (n_smp_finished_in_tile == 1){
          smp_data_tile <- break_data_smp
          
        } 
        
        
        
        
      }  else {## if else for ecluded ids 3142 3144 
        # data_excists_in_tile <- 0 ## gib NA output 
        # print("excluding sample, one of 3142 or 3144")
      }
      
      print("----------------------------------------------------------------------------------------------------------")
      #   print("----------------------------------------------------------------------------------------------------------")
      # print(smp_id)  
    }  ## loop over smps in tile
    
  } ## if data excists 
  
  print("                      ------------------  finished tile ------------------")
  print("----------------------------------------------------------------------------------------------------------")
  print(" ")
  print(" ")
  
  
  ## ---------------------------------------------------------------------------
  
  
  ## ---------------------------------------------------------------------------
  
  # return(smp_data_tile_2)
} ## main loop 



####   ---------------------------------------------------------------------------------------------------------------------------------------------------------------

##                                           Globals                                 ###
# index to base break detection on  
index <- "TCW"

# cube_dir <- "A:/01_Data_Level3/20210622_LANDSAT_monthly_TC/"
cube_dir <- "A:/01_Data_Level3/202202_LANDSAT/"

## list dir of cube for loop 
list_tiles <- list.dirs(cube_dir, full.names = FALSE)
list_tiles <- list_tiles[!list_tiles %in% c("")]
# list_tiles <- c("X0070_Y0039","X0072_Y0044","X0072_Y0045","X0072_Y0046")

## fore tiles 
force_tiles <- readOGR("P:/workspace/jan/FORCE/tiles/Tiles_Extent_Brandenburg_3035.shp")
force_tiles <- force_tiles[force_tiles@data$Name %in% list_tiles, ]

## -----------------------------------------------------------------------------
## load samples 

## load id bb
bb_id <- read.table("P:/timesync/bb/tsync_plots_bb.csv", 
                    sep = ",", dec = ".", header = TRUE)
bb_interpretations <- read.table("P:/timesync/bb/tsync_plots_bb_jan_interpretations.csv",
                                 sep = ",", dec = ".", header = TRUE)
bb_comment <- read.table("P:/timesync/bb/tsync_plots_bb_jan_interpretations_comment.csv",
                         sep = ",", dec = ".", header = TRUE)

## id fires 
fires_id <- read.table("P:/timesync/bb/tsync_fires_4_distance_to_all_center.csv",
                       sep = ",", dec = ".", header = TRUE)
fires_interpretations <- read.table("P:/timesync/bb/tsync_fires_interpretations.csv",
                                    sep = ",", dec = ".", header = TRUE)
fires_comments <- read.table("P:/timesync/bb/tsync_fires_interpretations_comment.csv",
                             sep = ",", dec = ".", header = TRUE)

only_do_fires <- 0 
if (only_do_fires == 1){
  ids             <- fires_id
  interpretations <- fires_interpretations
  comments        <- fires_comments
} else {
  ## merge 
  ids             <- rbind(fires_id, bb_id)
  interpretations <- as.data.frame(rbind(bb_interpretations, fires_interpretations))
  comments        <- as.data.frame(rbind(bb_comment, fires_comments))
}

## add dis flag 
disturbances <- c("Harvest","Wind","Other","Fire", "Hydrology", "Purple")
interpretations$disturbance <- ifelse(interpretations$change_process %in% disturbances, 1, 0)
## add date 
interpretations$date <-  as.Date(interpretations$image_julday, origin = paste0(interpretations$image_year,"-01-01"))

## switches 
plot_ts <- 1
# 
# ### add years to ts / 0 == no years added 
make_ts_longer <- 0
# 
# ## switch for two additional years at start of time series / 0 == only one additional 
two_years_at_start <- 1
# 
# ## switch for only appending half a year at end / 0 == a whole year at end
half_a_year_at_end <- 0
# 
# ## -------
# ## Globals 
# 
# ## number of years to look in for reference labels for detected breakpoints 
# ## ( pre and post (2 years --> timespan of 4)) 
number_of_years_to_look_in <- 1.1
# 
# ## ---------------------
# ## strucchange parameters
h             <- 40 # should be rougly two years / 40 min
breaks        <- 3
# 
# ## Indeces dfs to get data from 
index_list <- c("NBR","NDV","TCB","TCW","TCG","TCD")
# 
# ## additonal title for plots 
# additional_title <- "years_added_"
additional_title <- ""
# 
# ## n samples finished 
# n_smp_finished <- 0
# 
# ## Output folder names
output_name         <- "_output_extracted_data_h40_till_2022_tcw"
output_folder_name  <- "TCW_h40_till_2022_one_year_added/"

####   ---------------------------------------------------------------------------------------------------------------------------------------------------------------

##                                           Main                                  ###


library(doParallel)
cl <- makeCluster(2) #not to overload your computer
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("S:/BrandSat/02_Code/R/library_4"))
clusterEvalQ(cl, {library(doParallel)
  library(bfast)
  library(zoo)
  library(rlang)
  # library(stlplus)
  library(lubridate)
  # library(devtools)
  #library(bfastSpatial)
  library(terra)
  #library(svMisc)
  library(snow)
  
  # library(hms)
  library(rgdal)
  library(ggplot2)
  library(data.table)
  library(reshape2)
  library(tidyr)
  library(Rcpp)
  library(dplyr)
  #library(MASS)
})
#list_tiles[49]
# list_tiles <- list_tiles[16:17]
list_tiles <-list_tiles[!list_tiles %in% c("X0069_Y0041","X0071_Y0043")]
# tile <- "X0069_Y0041"

system.time(
  foreach (ooo  = 1:52, .combine = rbind) %dopar% { ## 54
    
    print(paste0("process tile n = ", ooo))
    print(paste0("process tile n = ", list_tiles[ooo]))
    main_fun(list_tiles[ooo])
    
    
  }
)
#stop cluster
stopCluster(cl)


print("finished.. ")


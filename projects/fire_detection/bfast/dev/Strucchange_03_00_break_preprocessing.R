
### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

# install.packages("hms")
## lib
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(bfastSpatial)
library(raster)
#library(svMisc)
#library(snow)
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
#library(flextable)

## -----------------------------------------------------------------------------
## check meta if all samples got processed 
# setwd("P:/workspace/jan/fire_detection/break_detection/h40_tiled_2_meta_meta/")
# temp    <- list.files(pattern="*.csv")
# myfiles <- lapply(temp, fread)  
# meta    <- rbindlist(myfiles, use.names=TRUE, fill=TRUE)
# length(unique(meta$smp_id))  ## looks good 

## load preprocessed refs 
refs <- read_csv2("P:/workspace/jan/fire_detection/disturbance_ref/preprocessed_disturbance_ref.csv")

## global id for refs
## START 
## -----------------------------------------------------------------------------


# ---------------------
# load extracted breaks
p <- "P:/workspace/jan/fire_detection/break_detection/break_detection_tables/h40_till_2022_with_expansion/"
temp    <- list.files(p, pattern=".csv", full.names = T)
myfiles <- lapply(temp, data.table::fread)
breaks  <- do.call(rbind, myfiles)
breaks <- breaks %>% dplyr::select(-season)
breaks  <- as.data.frame(breaks)
breaks <- breaks[!duplicated(breaks),]
nachprocess_ <- breaks[rowSums(is.na(breaks)) > 0,]



## write smp ids with nas
nachprocess_ <- breaks[rowSums(is.na(breaks)) > 0,]$smp_id
# write.table(nachprocess_, "P:/workspace/jan/fire_detection/nachprocessing_ids.csv", sep =";", dec =".", col.names = TRUE, row.names = FALSE)

## later redo 
# X0071_Y0043
# X0069_Y0041

## for now drop 
breaks <- breaks[!rowSums(is.na(breaks)) > 0,]

# breaks[breaks$smp_id==3231,]
# --------------------
# bind with refs
### loop over breaks


rm(result)
rm(safe_out)

t1 <- Sys.time()
for(b  in seq(1:nrow(breaks))){

  # b <- 1620
  print(b)
  single_break <- breaks[b,]
  bid          <- single_break$smp_id

  ##
  refs_of_bid <- refs %>% subset(plotid == bid)

  ## create time span
  time_span <- lubridate::interval(as.Date(single_break$date) - (1*365),
                                   as.Date(single_break$date) + (1*365) )
  ## get break of time span
  ref_in_timespan <- refs_of_bid[as.Date(refs_of_bid$date_of_interpretation) %within% time_span,]
  print("da")

  ## flag if there are refs in timespan
  single_break$meta_is_there_ref <- 0
  single_break$meta_how_many_ref <- 0

  single_break$meta_is_there_ref <- 1
  single_break$meta_how_many_ref <- nrow(ref_in_timespan)
  single_break$meta_days_difference_to_closest_break  <- NA
  single_break$meta_days_difference_to_second_closest_break <- NA
  single_break$meta_closest_ref_id <- NA


  if (nrow(ref_in_timespan > 0)){



    ## select the ref with the smallest temporal distance
    if (nrow(ref_in_timespan > 1)){

      for(u in seq(1:nrow(ref_in_timespan))) {

        time_span_single_ref <- as.integer(difftime(as.Date(single_break$date), as.Date(ref_in_timespan[u,]$date_of_interpretation)))

        ## safe
        if (u == 1){
          safe_single_time_span <- as.data.frame(cbind(time_span_single_ref, ref_in_timespan[u,]$dis_id))

        } else {
          time_span_cbind             <- as.data.frame(cbind(time_span_single_ref, ref_in_timespan[u,]$dis_id))
          print("here")
          safe_single_time_span       <- as.data.frame(rbind(safe_single_time_span,(time_span_cbind)))
          print("here")
        }

        ##  if loop looped through all detected refs of one break; sort them
        if(u == nrow(ref_in_timespan)) {
          print("set")
          safe_time_spans_of_one_ref <- safe_single_time_span[order(abs(as.integer(safe_single_time_span$time_span_single_ref))),]
          print("set")
          single_break$meta_days_difference_to_closest_break        <- safe_time_spans_of_one_ref$time_span_single_ref[1]
          single_break$meta_days_difference_to_second_closest_break <- safe_time_spans_of_one_ref$time_span_single_ref[2]
          print("det")
          single_break$meta_closest_ref_id <- safe_time_spans_of_one_ref$V2[1]
          print("set")
          #single_ref$second_closest_break_global_id <- safe_time_spans_of_one_ref$V2[2]
        }
      }

        #smp_id

      } # loop over refs in timespan
    }
      if(nrow(ref_in_timespan) == 1){# if only one ref in timespan

      ## safe distance to single detected break
      
 
      time_span_single_break_2 <- as.integer(difftime(single_break$date, as.Date(ref_in_timespan[,]$date_of_interpretation)))
      single_break$meta_days_difference_to_closest_break <- time_span_single_break_2
      single_break$meta_closest_ref_id <- ref_in_timespan$dis_id

  }

  result <- single_break
  ## see which one is closer
  print("have result")

  ## store
  result <- as.data.frame(result)
  if (b == 1){
    safe_out <- result

  } else {
    print("safe out")
    safe_out <- rbind(safe_out,result)
  }

}
time_span_single_break_2
t2 <- Sys.time()
print(t2-t1)
write.table(safe_out, "P:/workspace/jan/fire_detection/break_detection/preprocessed_breaks/h40_till_2022_with_expansion/preprocessed_breaks.csv", sep =";", dec = ".",
           row.names = FALSE, col.names = TRUE)

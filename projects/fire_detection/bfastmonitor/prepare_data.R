.libPaths("S:/BrandSat/02_Code/R/library_7")


library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
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
library(readr)
library(purrr)
#install.packages("rlang")

#install_github("bfast2/strucchangeRcpp")
#install_github("bfast2/bfast")
#install.packages(c("dplyr", "ggplot2"))

#remove.packages("rlang")
install.packages("rlang")
library(rlang)
library(dplyr)
library(ggplot2)
library(lubridate)




## read references 
tsync <- read.csv2("P:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_wind.csv")

## read ts 
ts_ori <- read.csv2("P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_Sentinel2_ts_with_outliers_till_2022_post2.csv")
head(ts)

## cross ids 
id_ts <- unique(ts_ori$id)
id_ref <- unique(tsync$plotid)
ids <- intersect(id_ts, id_ref)

## ids that have disturbances either not all or have in 2018-2020 and no before
ts    <- ts_ori[ts_ori$id %in% ids,]
tsync <- tsync[tsync$plotid %in% ids,]

## tsync disturbed in years 2018-2020 
tsync_2 <- tsync %>% filter(image_year %in% c(2018,2019,2020)) %>% filter(change_process %in% c("Harvest","Fire"))
table(tsync_2$image_year, tsync_2$change_process)

## tsync not disturbed in years 2010-2020 
tsync_3 <- tsync %>% filter(image_year %in% seq(2010,2021)) %>% dplyr::group_by(plotid)
  filter(!change_process %in% c("Growth","Harvest","Insect","Wind","Fire","Fire_salvage","Harvest_salvage","Hydrology"))
table(tsync_3$image_year, tsync_3$change_process)

## if included in ts 2 than exclude in ts 3 
tsync_3 <- tsync_3[!tsync_3$plotid %in% tsync_2$plotid]


tsync_2 <- rbind(tsync_2, tsync_3)
tsync_2[tsync_2$plotid == 1776,]

tsync_2 <- tsync_2[!duplicated(tsync_2$plotid),]


tsync_2[!duplicated(tsync_2$plotid),]
#tsync_2 <- tsync_2 %>% filter(change_process %in% "Stable")

table(tsync_2$image_year, tsync_2$change_process)

## filter by id
ts    <- ts[ts$id %in% tsync_2$plotid,]

## filter time
ts_2 <- ts %>% filter(date > 2015 & date < 2021)
uni_id <- unique(ts_2$id)

## 
pl <- ts_2[ts_2$id %in% uni_id[3],]
pl %>% filter(index == "NBR") %>% ggplot(.,aes(x=date, y= value)) + geom_point()

## delete non unique entries
pl <- pl %>% filter(index == "NBR")
pl <- pl[!duplicated(pl$date),]

#### Loop over ids #####
counter <- 1
for(id in uni_id[1:20]){
  
  #id <- 10
  print(paste0("id: ", id))
  pl_all <- ts_2[ts_2$id %in% id,]
  
  ## filter index
  ## delete non unique entries
  pl <- pl_all %>% filter(index == "NBR")
  pl <- pl[!duplicated(pl$date),]

  ## into ts
  options("digits" = 10)

  #pl <- pl[!duplicated(pl$date_float),]
  pl_zoo <- as.ts( zoo::zoo(pl$value,  
                         1900 + as.POSIXlt(pl$date)$year + (lubridate::yday(pl$date)-1  )/365, 
                         frequency = 365))


  pl_plot <- bfast::bfastmonitor(pl_zoo, start = 2018 , history = 2015)
  
  plot(pl_plot)
  # Sys.sleep(3)
  
  ## select data
  ## get references
  id_ref <- tsync_2[tsync_2$plotid %in% id,][c("plotid","image_year","change_date","change_process")]
  print(id_ref)
  
  ## IF NOT NA THAN
  if(is.na(pl_plot$breakpoint)){
    id_ref$detected_breakpoint <- 0
    id_ref$break_date_dec <- NA
    id_ref$break_date     <- NA
    id_ref$magnitude <- NA
    id_ref$pre_val_nbr  <- NA
    id_ref$post_val_nbr <-  NA
    id_ref$magnitude_nbr <-  NA

    id_ref$pre_val_ndv  <- NA
    id_ref$post_val_ndv <-  NA
    id_ref$magnitude_ndv <-  NA
    
  } else {
    id_ref$detected_breakpoint <- 1
    id_ref$break_date_dec <- pl_plot$breakpoint
    id_ref$break_date <- as.Date(date_decimal(pl_plot$breakpoint))
    id_ref$magnitude <- pl_plot$magnitude
    
    # finds nearest matching date
    i_break <- which.min(abs(as.Date(pl$date) - id_ref$break_date))
    
    # pre and post break value
    id_ref$pre_val_nbr  <- pl[i_break,"value"]
    id_ref$post_val_nbr <- pl[i_break+1,"value"]
    id_ref$magnitude_nbr <- id_ref$post_val_nbr - id_ref$pre_val_nbr
    
    pl_ndv  <- pl_all %>% filter(index == "NDV")
    id_ref$pre_val_ndv  <- pl_ndv[i_break,"value"]
    id_ref$post_val_ndv <- pl_ndv[i_break+1,"value"]
    id_ref$magnitude_ndv <- id_ref$post_val_ndv - id_ref$pre_val_ndv
    
  }
  
  # print(id_ref)
  # 
  # print(pl_plot$breakpoint)
  # 
 
  if(counter == 1){
    break_ref <- id_ref
  } else {
    break_ref <- rbind(break_ref, id_ref)  
  }
 
  counter <- counter + 1
  }


 

# 
# 
# pl_zoo <- ts(zoo::zoo(pl$value, ((as.integer(strftime(pl$date, format = "%y"))-15) * 365 + as.integer(strftime(pl$date, format = "%j")))), frequency=365)
# 
# tt <- bfast::bfastmonitor(pl_zoo, start = (3 * 365 ), history = ( 1))
# plot(tt)
# tt

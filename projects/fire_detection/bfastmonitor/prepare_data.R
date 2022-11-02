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

install_github("bfast2/strucchangeRcpp")
install_github("bfast2/bfast")
install.packages(c("dplyr", "ggplot2"))

remove.packages("rlang")
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
tsync_2 <- tsync %>% filter(image_year %in% c(2018,2019,2020)) %>% filter(change_process %in% c("Harvest","Insect","Wind","Fire"))
# table(tsync_2$image_year, tsync_2$change_process)

## tsync not disturbed in years 2010-2020 
tsync_3 <- tsync %>% filter(image_year %in% seq(2010,2021)) %>% filter(!change_process %in% c("Growth","Harvest","Insect","Wind","Fire","Fire_salvage","Harvest_salvage","Hydrology"))
table(tsync_3$image_year, tsync_3$change_process)


tsync_2 <- rbind(tsync_2, tsync_3)
tsync_2 <- tsync_2[!duplicated(tsync_2$plotid),]
tsync_2 <- tsync_2 %>% filter(change_process %in% "Stable")
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
for(id in uni_id[1:40]){
  
  print(paste0("id: ", id))
  pl <- ts_2[ts_2$id %in% id,]
  
  ## filter index
  ## delete non unique entries
  pl <- pl %>% filter(index == "NBR")
  pl <- pl[!duplicated(pl$date),]

  ## into ts
  options("digits" = 10)
  #pl$date_float <- year(pl$date)+day(pl$date)/366
  #pl <- pl[!duplicated(pl$date_float),]
  pl_zoo <- as.ts( zoo::zoo(pl$value,  
                         1900 + as.POSIXlt(pl$date)$year + (lubridate::yday(pl$date)-1  )/365, 
                         frequency = 365))


  pl_plot <- bfast::bfastmonitor(pl_zoo, start = 2018 , history = 2015)
  plot(pl_plot)
  # Sys.sleep(3)
  
  ## select data
  i## IF NOT NA THAN
  
  print(pl_plot$breakpoint)
  
  pl_plot$breakpoint
  
  
  #tsync_2 
 
  }


 

# 
# 
# pl_zoo <- ts(zoo::zoo(pl$value, ((as.integer(strftime(pl$date, format = "%y"))-15) * 365 + as.integer(strftime(pl$date, format = "%j")))), frequency=365)
# 
# tt <- bfast::bfastmonitor(pl_zoo, start = (3 * 365 ), history = ( 1))
# plot(tt)
# tt

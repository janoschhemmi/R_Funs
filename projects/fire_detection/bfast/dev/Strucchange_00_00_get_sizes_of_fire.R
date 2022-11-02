
### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_3")

# install.packages("hms")
## lib
library(bfast)
library(zoo)
library(rlang)
# library(stlplus)
library(lubridate)
library(devtools)
#(bfastSpatial)
library(raster)
#library(svMisc)
library(snow)
library(lubridate)
library(hms)
library(ggplot2)
#library(doSnow)
library(foreach)
library(rgdal)
library(sp)
library(dplyr)
library(reshape2)
library(Rcpp)

rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

get_size_of_fire <- function(){
## load samples 
samples <- shapefile("P:/workspace/fire_digitize/fire_samples_Brandsat/smp_fires_brandsat_4_distance_to_all.shp")
plot(samples)
length(unique((samples@data$UID)))

## load digitized fires 
fire <- shapefile("P:/workspace/fire_digitize/fires_digitized_Brandsat/fires_digitized_BrandSat_3035_adj_area_effis_added.shp")

plot(fire, add = TRUE, colour = "red")

## per sample get all fire ids
out <- sp::over(samples, fire, returnList=TRUE)

## rbind with id / with element of list 
tmp <- plyr::ldply(out, data.frame)
tmp$.id <- as.integer(tmp$.id)
samples@data$seq <- seq(1:nrow(samples@data))
tmp     <- left_join(tmp, samples@data, by = c(".id" = "seq"))
tmp <- tmp[,-15]

## now load fire inform<tion 
fire_ref <- shapefile("P:/data/burned_area/brandenburg/Wabra_Bbg_2006-2019_koordinates_epsg25833_gt0_1ha.shp")
fire_ref_select <- fire_ref@data[,c("datume","fl","flw","fireid","year","ws")]
fire_ref_select$datume <- as.Date(fire_ref_select$datume)
fire_ref_select[fire_ref_select$fireid == 20190167,]

## look for other fire refs 
fire_ref_2 <- read.table("P:/data/burned_area/brandenburg/Wabra_2012-2019_noack/Wabra_Bbg_2012-2019_polygon/fires.csv", sep = ";", dec = ".", header = TRUE)
fire_ref_2_select <- fire_ref_2[,c("DatumE","Fl","FlW","fireid","year","WS")]
fire_ref_2_select$DatumE <- as.Date(fire_ref_2_select$DatumE )
colnames(fire_ref_2_select) <- colnames(fire_ref_select)
## rbind 
fire_ref <- rbind(fire_ref_select, fire_ref_2_select)
fire_ref[fire_ref$fireid == 20190167,]
fire_ref$fireid.x <- as.integer(fire_ref$fireid)

## merge with 
tmp$fireid.x <- as.integer(tmp$fireid.x)
tmp <- left_join(tmp,fire_ref, by = c("fireid.x" = "fireid"))

## clean up colnames 
tmp <- tmp[,-which(names(tmp) %in% c(".id","review","excl_Jan","fireid.x.y"))]
colnames(tmp) <- c("date","source","comment","confidence","fireid","clearance","Non.Forest","no_crown","Area_polygon","HA_effis","UID","datume","fl","flw","year","ws")

## safe 
write.table(tmp, "P:/workspace/jan/fire_detection/disturbance_ref/fire_ref_per_sample.csv", sep = ";", dec = ".", col.names = TRUE, row.names = FALSE)

}


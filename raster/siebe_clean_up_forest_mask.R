## nach dem sieben die 0 in waldflaechen behalten 
## load 

.libPaths("S:/BrandSat/02_Code/R/library_3")
# pp <- str(allPackage <- installed.packages(.Library, priority = "high"))
# allPackage [, c(1,3:5)]
# install.packages("strucchange")
## lib

library(raster)
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(bfastSpatial)
# library(raster)
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


## load raster 
summary(r)
r <-  terra::rast("P:/workspace/jan/masks/RSE_Full_Brandenburg.tif")
m <- c(0, 3, 0,
       3.9, 6, 1,
       6.9, 12, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
summary(rc1)

writeRaster(rc1, "P:/workspace/jan/masks/RSE_Full_Brandenburg_Forest.tif", datatype ="INT2S", overwrite = TRUE )

## reclassify gesiebten layer
r_gesiebt <-  raster("P:/workspace/jan/masks/RSE_Full_Brandenburg_Forest_mmu12.tif")
r <-  raster("P:/workspace/jan/masks/RSE_Full_Brandenburg_Forest.tif")

## keep all 0 from r 
r_gesiebt_2 <- r_gesiebt
r_gesiebt_2[r_gesiebt == 1 &  r == 0]  <- 0
#r_gesiebt_3 <- r_gesiebt[]
#r_gesiebt_2[,1]
#values(r_gesiebt_3) <- as.numeric(r_gesiebt_2[,1])
writeRaster(r_gesiebt_2, "P:/workspace/jan/masks/RSE_Full_Brandenburg_Forest_mmu12_2.tif", datatype ="INT2S", overwrite = TRUE )

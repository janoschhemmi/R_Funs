## Sript for vrt Layer 

### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

# install.packages("randomForest")
# urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
# install.packages(urlPackage, repos=NULL, type="source") 

# install.packages("hms")
## lib
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(bfastSpatial)
library(terra)
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
library(caret)



## ## ## ## ## ## ## ## ## ## MAIN LOOP ## ## ## ## ## ## ## ## ## ## ## ## #

## list dir of break cube for loop 
cube_dir <- "A:/01_Data_Level3/202203_LANDSAT_S2_missing_pixel"

index_list <- c("NBR","NDV","TCB","TCD","TCG","TCW")
for (index in index_list){
  
  # index <- "NBR"
  path <- paste0(cube_dir,"/",index,"/")

  print(path)
  files <- list.files(path, full.names = TRUE, pattern = ".tif")
  rast(files[1])
  #files_tif <- apply(files, rast())
  
  zu <- terra::vrt(
    files,
    paste0(cube_dir,"/",index,".vrt"),
    overwrite = TRUE)
  plot(zu)
  }

rr <- read.table("P:/workspace/jan/fire_detection/disturbance_ref/preprocessed_ref_all_smp.csv", sep = ";", dec = ".",
                 header  = TRUE)
rr[rr$change_process == "Other",]$plotid

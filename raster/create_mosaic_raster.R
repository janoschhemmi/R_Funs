################################################################################




## ---------------------------------------------------------------------------##
##                                Create Raster Mosaic                          ##
## --                           --------------                            --  ##    

# set lib path 
### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_3")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
set.seed(101)

library(dplyr)
library(parallel)
library(cluster)
library(ggplot2)
library(scales)
library(gridExtra)
library(reshape2)
library(formattable)
library(maptools)
library(sp)
library(rgdal)
library(raster)
library(sf)
library(rgeos)
library(randomForest)
library(caret)
library(matrixStats)
library(fortify)
library(mapac)
library(e1071)
library(gt)
library(tidyverse)
library(tibble)

# -- Features -- Environmental -- features + LSM -- features + LSM + Environmental 

## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

wd <- "A:/workspace/tree_species/10_Classi/Classi_550_features_txt_review_no_mask/"
## tile list 
## TILES
tilenames <-  c(
  #"X0065_Y0040", 
  "X0065_Y0041","X0066_Y0040",
  "XX0066_Y0041", "X0066_Y0042",  "X0067_Y0040", "X0067_Y0041", "X0067_Y0042",
  "X0067_Y0043", "X0067_Y0044", "X0067_Y0045",  "X0068_Y0040", "X0068_Y0041",
  "X0068_Y0042", "X0068_Y0043", "X0068_Y0044", "X0068_Y0045",  "X0069_Y0040",
  "X0069_Y0041", "X0069_Y0042", "X0069_Y0043", "X0069_Y0044", "X0069_Y0045", "X0069_Y0046",
  "X0069_Y0047", "X0070_Y0040", "X0070_Y0041", "X0070_Y0042", "X0070_Y0043",
  "X0070_Y0044", "X0070_Y0045", "X0070_Y0046", "X0070_Y0047",  "X0071_Y0040",
  "X0071_Y0041", "X0071_Y0042", "X0071_Y0043", "X0071_Y0044", "X0071_Y0045", "X0071_Y0046",
  "X0071_Y0047", "X0072_Y0040", "X0072_Y0042", "X0072_Y0043", "X0072_Y0044",
  "X0072_Y0045", "X0072_Y0046", "X0073_Y0044", "X0073_Y0045", "X0073_Y0046")# [1:4]
tilenames <- list.dirs(wd, full.names = FALSE) 
tilenames <- tilenames[!tilenames %in% ""]

##### GLOBALS

## PATH to TILES
# Tile_Path <- "A:/10_Classi/Classi_New_Int_txt/05_full_all_forest"
Tile_Path  <- "A:/workspace/tree_species/10_Classi/Classi_550_features_txt_review_no_mask/"
# Tile_Path  <- wd
## Name of File, equal in each Tile
# File_Name <- "PREDICTION_RF_350_Features_HL_ML_MLP.tif"
File_Name <- "PREDICTION_RF_550_Features_txt_HL_ML_MLP_paper_int.tif"
## Path to Safe to
# Out_Path  <- "A:/10_Classi/Classi_New_Int_txt/RF_Predict_05_Full_model_all_Forest_Mosaic.tif"
Out_Path  <- "A:/workspace/tree_species/10_Classi/_mosaic/"

### loop over tiles for paths to files 
t <- 1
paths_to_file = c()
for ( tile in tilenames) {
  print(tile)
  
  path_to_file <- paste0(Tile_Path,"",tile,"/",File_Name)
  print(path_to_file)

  if (t == 1){
    #mosaic <- raster(path_to_file)
    
  } else {
  # raster_to_add <- raster(path_to_file)
  paths_to_file <- append(paths_to_file, path_to_file)
    print(path_to_file)
  #moasic <- raster::mosaic(mosaic,raster_to_add, fun = max)
  }
 
  t <- t + 1 
  }


## make cluster 
# ncl     <- detectCores() - 20
# cl <- makeCluster(ncl)
# e <- new.env()
# e$libs <- c("S:/BrandSat/02_Code/R/library", .libPaths())
# clusterExport(cl, "libs", envir = e)#,
# clusterEvalQ(cl,.libPaths(libs))
# clusterEvalQ(cl, sapply(c('raster', 'gdalUtils',"rgdal"), library, char=TRUE))

rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')

## first make lst of all classi tifs 
rastlist <- list.files(recursive = TRUE, path = Tile_Path, pattern=File_Name, all.files=TRUE, full.names=TRUE)
rlist    <- lapply(rastlist, raster)
mos      <- do.call(raster::merge, rlist)



# names(rastlist) <- NULL
# rastlist <- as.list(rastlist)
# raster.list$fun <- mean
# 
# mos <- do.call(raster::mosaic, list(rastlist))
# stopCluster(cl)     
# plot(output_raster)

raster::writeRaster(mos , paste0(Out_Path,"/","RF_Predict_550_features_txt_Forest_no_mask.tif") , overwrite = TRUE)


model <<- readRDS("P:/workspace/jan/fire_detection/model_disturbance_classification/model_equal_fit_nbr_tcb.rds")

??bfast::bfastpp

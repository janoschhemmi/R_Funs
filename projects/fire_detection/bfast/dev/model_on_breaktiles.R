## Model on Subtiles Dirk redo ---- 

## prediction of pretrained model on extracted breakfeatures 
## on subtile level (16 subtiles per force tile)

## global Settings---- 
.libPaths("S:/BrandSat/02_Code/R/library_4")
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
library(data.table)
library(randomForest)
library(flextable)
library(terra)
library(strucchangeRcpp)

# old.packages()
# # update if needed 
# update.packages(ask = FALSE, lib.loc = "S:/BrandSat/02_Code/R/library_3")

Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

## terra options 
terraOptions(memfrac=0.6, tempdir = 'S:/BrandSat/000_TerraTemp', verbose = FALSE,
             progress = 10, datatype = 'Init2s')


## Worker Fun ----

classi_on_subtile <- function(tile) {
  
  #subtile <- 16
  print(paste0("classifying tile: ",tile))
  
  # Input cube of Breakfeature 
  # cube_dir     <- "P:/workspace/jan/fire_detection/break_detection/break_detection_cube/"
  cube_dir     <- "A:/workspace/fire_detection/tiled_breaks_new_mask_dp/"
  
  ## Output cube of classified tiles 
  #out_cube     <- "P:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/"
  out_cube      <- "A:/workspace/fire_detection/tiled_classified_breaks_new_mask_dp/"
  
  ## set path extension for subfolders (if needed)
  #file_name    <- "metrics_breaks_subtile_"
  model_name   <- "model_38_features_equal"
  
  ## load model 
  model <- readRDS(paste0("P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022_one_year_expansion/",model_name,".rds"))
  
  ## create new output folder for each applied model 
  if(!dir.exists(paste0(out_cube,"/",model_name,"/"))){
    dir.create(paste0(out_cube,"/",model_name,"/"))
  }
  
  ## create folder for each tile 
  out_tile_dir <- paste0(out_cube,"/",model_name,"/",tile,"/")
  
  if(!dir.exists(out_tile_dir)){
    dir.create(out_tile_dir)
  }
  
  ## Loop over subtiles----
  for(subtile in seq(1:16)){ ## :16
    
    #subtile <- 16
    print(paste0("on subtile ",subtile))
    
    # ## create subtile out dir
    # if(!dir.exists(paste0(out_tile_dir,subtile,"/"))){
    #   dir.create(paste0(out_tile_dir,subtile,"/"))
    # }
    
    ## load break feature stack ----
    rast_stack <- terra::rast(paste0(cube_dir,tile,"/",tile,"_stack_croped_",subtile,".tif"))
    
    ## rename in case of first round of breakdetection by bp ;) 
    names_break_stack <- read.table("P:/workspace/jan/fire_detection/break_detection/out_names_index_extraction_full_list.csv")
    names(rast_stack) <- names_break_stack$V1
    
    names_model <- row.names(model$importance)
    
    ## classify 3x breaks loop ---- 
    for(layer in seq(1:3)){
      #layer <- 3
      print(paste("classifying break ", layer))
      
      ## join wit n break
      names_model_break <- paste0("break_",layer,"_",names_model)
      
      ## pull required  layer from subtile raster // except for relative magnitude fit 
      rast_stack_break <- rast_stack[[names_model_break[!names_model_break %in% 
                                                          paste0("break_",layer,"_","relativ_magnitude_fit")] ]] 
      
      ## calculate realtive magnitude fit ---- 
      if(paste0("break_",layer,"_","relativ_magnitude_fit") %in% names_model_break ){
        
        ## avoid 0 division 
        post_layer <- rast_stack_break[ paste0("break_",layer,"_","post_val_fit")] 
        post_layer[post_layer == 0] <- 10.001
        pre_layer <- rast_stack_break[ paste0("break_",layer,"_","pre_val_fit")] 
        pre_layer[pre_layer == 0] <- 10.001
        
        ## raltive 
        relative_layer <- post_layer / pre_layer
        names(relative_layer) <- paste0("break_",layer,"_","relativ_magnitude_fit") 
        
        ## stack 
        rast_stack_break <- c(rast_stack_break, relative_layer)
      }
      
      ## adapt slopes to old version 
      slope_pre <- rast_stack_break[[names(rast_stack_break) == paste0("break_",layer,"_","slope_pre")]] /10
      slope_post <- rast_stack_break[[names(rast_stack_break) == paste0("break_",layer,"_","slope_post")]] / 10
      
      rast_stack_break <- terra::subset(rast_stack_break, 
                                        names(rast_stack_break)[!names(rast_stack_break) %in% c(paste0("break_",layer,"_","slope_pre"),
                                                                                                paste0("break_",layer,"_","slope_post"))])
      rast_stack_break <- c(rast_stack_break,slope_pre,slope_post)
      
      ## any NAs to -9999
      rast_stack_break[is.na(rast_stack_break)] <- -9999
      
      ## if relative magnitude was calculated bring to end of model names, needed for next step 
      names_model <- c(names_model[!names_model %in% "relativ_magnitude_fit"], "relativ_magnitude_fit")
      
      ## same for slopes  
      names_model <- c(names_model[!names_model %in% c("slope_pre","slope_post")], "slope_pre","slope_post")
      
      ## set to names of model for prediction; here magnitude is last layer  
      names(rast_stack_break) <- names_model
      
      ## run prediction ---- 
      library(randomForest)
      print("predicting.. ")
      predicted_raster <- predict(model=model, object=rast_stack_break)
      
      ## mask prediction whith forest mask 
      prediction_masked <- terra::mask(predicted_raster,rast_stack[[1]],maskvalues = -9999, updatevalue = -9999)
      
      ## get disturbances
      fires_one_layer   <-  prediction_masked
      fires_one_layer[fires_one_layer != 1] <- 0
      harvest_one_layer <- prediction_masked
      harvest_one_layer[harvest_one_layer != 2] <-  0
      harvest_one_layer[harvest_one_layer == 2] <- 1
      
      fires_year_one_layer   <- terra::mask(rast_stack[[paste0("break_",layer,"_","breakyear")]], fires_one_layer, maskvalue = FALSE, updatevalue =-9999) 
      harvest_year_one_layer <- terra::mask(rast_stack[[paste0("break_",layer,"_","breakyear")]], harvest_one_layer, maskvalue = FALSE, updatevalue =-9999) 
      fires_doy_one_layer    <- terra::mask(rast_stack[[paste0("break_",layer,"_","doy")]], fires_one_layer, maskvalue = FALSE, updatevalue =-9999) 
      harvest_doy_one_layer  <- terra::mask(rast_stack[[paste0("break_",layer,"_","doy")]], harvest_one_layer, maskvalue = FALSE, updatevalue =-9999) 
      
      ## stack ----  
      if(layer == 1){
        fires <-       fires_one_layer
        fires_years <- fires_year_one_layer
        fires_doys <-  fires_doy_one_layer
        
        harvests <-       harvest_one_layer
        harvests_years <- harvest_year_one_layer
        harvests_doys <-  harvest_doy_one_layer
        
      } else {
        print("stacking fires.. ")
        fires <- c(fires,fires_one_layer)
        fires_years <- c(fires_years,fires_year_one_layer)
        fires_doys <- c(fires_doys, fires_doy_one_layer)
        print("stacking harvests.. ")
        harvests       <- c(harvests,harvest_one_layer) 
        harvests_years <- c(harvests_years,harvest_year_one_layer)
        harvests_doys  <- c(harvests_doys, harvest_doy_one_layer)
      }
    } ## loop over layers 
    
    ## combine for output ----
    fires_sum     <- sum(fires)
    harvests_sum  <- sum(harvests)
    breaks_sum    <- sum(fires_sum, harvests_sum)
    
    out_stack <- c(breaks_sum, fires_sum, fires_years, fires_doys, 
                   harvests_sum,harvests_years, harvests_doys )
    # names(out_stack) <- c("n_disturbance","n_fires","fires_years","fires")
    writeRaster(out_stack,paste0(out_tile_dir, "/",tile,"_classified_break_",subtile,".tif"), 
                datatype = "Init2s", overwrite = TRUE)
    
    
  } ## loop over subtile
  
  # 
  # ## load as stack (## way faster with terra)
  # print(paste0("loading stack of tile: ", tile))
  # rast_stack <- terra::rast("A:/01_Data_Level3/202202_LANDSAT_retile/X0068_Y0045/X0068_Y0045_stack_croped_16.tif")
  # #plot(rast_stack[[1]])
  # print(terra::ext(rast_stack))
  # 
  # out_tile_dir <- paste0("A:/01_Data_Level3/202202_LANDSAT_retile_test_X0068_Y0045/",tile,"/")
  # ## create out dir 
  # if(!dir.exists(out_tile_dir)){
  #   dir.create(out_tile_dir) }
  
  # ## mosaik results for tile 
  classified_tifs <- c(paste0(out_tile_dir,list.files(out_tile_dir, recursive = TRUE, pattern = ".tif$")))
  model_name
  terra::vrt(
    classified_tifs,
    paste0(out_tile_dir,"/classification_mosaik_",tile,"_",model_name,".vrt"),
    overwrite = TRUE)
  
  ## moasik output ----
  # classified_tifs <- lapply(classified_tifs, rast)
  # mos <- do.call(mosaic, classified_tifs)
  # names(mos) <- c("n_disturbances","n_fires","year_fire_1","year_fire_2","year_fire_3",
  #                 "doy_fire_1","doy_fire_2","doy_fire_3","n_harvest","year_harvest_1","year_harvest_2","year_harvest_3",
  #                 "doy_harvest_1","doy_harvest_2","doy_harvest_3")
  # print("write mosaik..")
  # writeRaster(mos, paste0(out_tile_dir,"tile_",tile,"_classified_breaks_mosaik.tif"), overwrite = TRUE)
}

## ## ## ## ## ## ## ## ## ## ## MAIN LOOP ## ## ## ## ## ## ## ## ## ## ## ## #
## Main ----

## list dir of break cube for loop 
cube_dir <- "P:/workspace/jan/fire_detection/break_detection/break_detection_cube/"

## all tiles in breakcube
list_tiles <- list.dirs(cube_dir, full.names = FALSE, recursive = FALSE) 
list_tiles <- list_tiles[!list_tiles %in% c("")] 

list_tiles <- "X0068_Y0045" ## Test tile

for(tile in list_tiles){
  
  print("working on tile:: ")
  print(tile)
  
  t_1 <- Sys.time()
  ## for loop over tiles;
  result_stack <- classi_on_subtile(tile)
  
  print(paste0("completed all tile .. overall took ", difftime(Sys.time(),t_1, units = "mins" ), " minutes.. "))
  
}

## overall mosaik ----
classified_vrt <- c(paste0(out_cube,"/",model_name,"/",list.files(paste0(out_cube,"/",model_name,"/"), recursive = TRUE, pattern =
                                                                    glob2rx("*.vrt$"))))

terra::vrt(
  classified_vrt,
  paste0(out_cube,"/",model_name,"/",model_name,"_classified_breaks_mosaik.vrt"),
  overwrite = TRUE)

##
# classified_tifs <- lapply(classified_tifs, rast)
# mos <- do.call(mosaic, classified_tifs)
# names(mos) <- c("n_disturbances","n_fires","year_fire_1","year_fire_2","year_fire_3",
#                 "doy_fire_1","doy_fire_2","doy_fire_3","n_harvest","year_harvest_1","year_harvest_2","year_harvest_3",
#                 "doy_harvest_1","doy_harvest_2","doy_harvest_3")
# print("write mosaik..")
# writeRaster(mos, paste0(out_cube,"/",model_name,"/",model_name,"_classified_breaks_mosaik.tif"), overwrite = TRUE)
# 
# 



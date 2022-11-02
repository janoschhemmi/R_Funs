#' Classify bfast breakpoints and metrics
#'
#' @title Classify bfast breakpoints metrics
#' @description Applies a random forest model to bfast breakpoint metrics to 
#' classify disturbance types.
#' @param in_files Vector of file names. Input are raster files holding the 
#' bfast metrics derived from `bm_detect_and_extract_tiles()`
#' @param rf_model randomForest model object. Names of the predictors 
#' must match the layernames of the raster.
#' @param outpath character. Directory name for the output.
#' @param breaks numeric. Maximum number of breakpoint, defaulting to `3`.
#' @param overwrite boolean. Overwrite output file if it exists, defaulting to `FALSE`.
#' @param cores numeric. Number of parallel worker processes.
#' @return Nothing. Writes raster file to `outpath`.
#' @seealso [bm_detect_and_extract_tiles()] for creating breakpoint metrics.
#' @author Dirk Pflugmacher
#' @author Jan Hemmerling
#' @importFrom magrittr %>%
#' @import randomForest
#' @export
#' @md
#' 
#
cores <- 1

### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")
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
library(strucchangeRcpp)
library(doParallel)
library(rgdal)
library(foreach)

in_files = fns; rf_model; outpath = out_path

bm_classify_bfast_output <- function(in_files, rf_model, outpath, breaks=3, overwrite=T, cores=1) {
  
  
  if (cores==0) return(invisible(NULL))
  
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    clusterEvalQ(cl, .libPaths("S:/BrandSat/02_Code/R/library_6"))
    doParallel::registerDoParallel(cl)
    `%doit%` <- foreach::`%dopar%`
  } else {
    `%doit%` <- foreach::`%do%`
  }
  

  foreach::foreach(in_file=in_files, 
                   .packages=c('randomForest')) %doit% {

    
    out_file <- file.path(outpath, basename(dirname(in_file)), basename(in_file))
    
    if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file))
    
    if (file.exists(out_file) & !overwrite) {
      message(paste(out_file, "exists. Use overwrite keyword."))   
      return(invisible(NULL))
      } 

    # load break feature stack
    rast_stack <- terra::rast(in_file)
    
    metric_names <- row.names(rf_model$importance)
    
    
    out_layer_names <- c("n_disturbances","n_fires", "n_harvest","n_insect",
                         "year_fire_1","year_fire_2","year_fire_3",
                         "doy_fire_1","doy_fire_2","doy_fire_3",
                         "mag_fire_1","mag_fire_2","mag_fire_3",
                         "prob_fire_1","prob_fire_2","prob_fire_3",
                         "year_harvest_1","year_harvest_2","year_harvest_3",
                         "doy_harvest_1","doy_harvest_2","doy_harvest_3",
                         "mag_harvest_1","mag_harvest_2","mag_harvest_3",
                         "prob_harvest_1","prob_harvest_2","prob_harvest_3",
                         "year_insect_1","year_insect_2","year_insect_3",
                         "doy_insect_1","doy_insect_2","doy_insect_3",
                         "mag_insect_1","mag_insect_2","mag_insect_3",
                         "prob_insect_1","prob_insect_2","prob_insect_3")
    
    # create empty raster
    outras <- terra::init(terra::subset(rast_stack, 1:length(out_layer_names)), -9999)
    names(outras) <- out_layer_names

    # set layers for counts to zero
    terra::values(outras[[2]]) <- 0
    terra::values(outras[[3]]) <- 0
    terra::values(outras[[4]]) <- 0
    
    i <- 1  
    # loop through breaks
    for (i in 1:breaks) {
  
      # create
      layer_names     <- paste0("break_", i, "_", metric_names)
      not_in_raster   <- paste0("break_", i, "_", "relativ_magnitude_fit")
      
      layer_names <- layer_names[!layer_names %in% not_in_raster]
      
      # pull required  layer except for relative magnitude fit 
      rast_stack_break <- rast_stack[[layer_names]]
      
      
      # calculate relative magnitude fit - avoid 0 division 
      post_layer <- rast_stack_break[ paste0("break_",i,"_","post_val_fit")] 
      pre_layer <- rast_stack_break[ paste0("break_",i,"_","pre_val_fit")] 
      
      relative_layer <- post_layer / pre_layer
      relative_layer[pre_layer == -9999 | post_layer == -9999] <- 0
      relative_layer[post_layer == 0 | pre_layer == 0] <- 0
      names(relative_layer) <- paste0("break_",i,"_","relativ_magnitude_fit")
      
      # stack 
      rast_stack_break <- c(rast_stack_break, relative_layer)
      
      
      # any NAs to -9999
      rast_stack_break[is.na(rast_stack_break)] <- -9999
      
      
      # remove prefix from layer names to match names of predictor variables
      names(rast_stack_break) <- sub(paste0("break_",i,"_"), "", names(rast_stack_break))
      
      
      # make prediction
      set.seed(42)
      predictions <- terra::predict(model=rf_model, object=rast_stack_break)
      
      probs <- terra::predict(model=rf_model, object=rast_stack_break, type="prob")

      
      # mask prediction with forest mask 
      prediction_masked <- terra::mask(predictions, 
                                       rast_stack[[paste0("break_",i,"_","breakyear")]] < 1900, 
                                       maskvalues = 1, updatevalue = 0)
      #plot(prediction_masked)
      ln_breakyear <- paste0("break_",i,"_","breakyear")
      ln_doy <- paste0("break_",i,"_","doy")
      ln_magnitude <- paste0("break_",i,"_","magnitude_fit")
      
      fires   <- prediction_masked == 1
      harvest <- prediction_masked == 2
      insect  <- prediction_masked == 3

      # count fires and harvests
      outras[[2]]     <- outras[[2]] + fires
      outras[[3]]     <- outras[[3]] + harvest
      outras[[4]]     <- outras[[4]] + insect

      # fire year and doy
      outras[[4+i]][fires] <- rast_stack[[ln_breakyear]][fires]
      outras[[7+i]][fires] <- rast_stack[[ln_doy]][fires]
      outras[[10+i]][fires] <- rast_stack[[ln_magnitude]][fires]
      outras[[13+i]]       <- probs[[1]] * 1000

      # harvest year and doy
      outras[[16+i]][harvest] <- rast_stack[[ln_breakyear]][harvest]
      outras[[19+i]][harvest] <- rast_stack[[ln_doy]][harvest]
      outras[[22+i]][harvest] <- rast_stack[[ln_magnitude]][harvest]
      outras[[25+i]]          <- probs[[2]] * 1000
      
      # insect year and doy
      outras[[28+i]][insect] <- rast_stack[[ln_breakyear]][insect]
      outras[[31+i]][insect] <- rast_stack[[ln_doy]][insect]
      outras[[34+i]][insect] <- rast_stack[[ln_magnitude]][insect]
      outras[[37+i]]          <- probs[[4]] * 1000
      
    } # loop over layers 

    # combine for output
    outras[[1]]     <- outras[[2]] + outras[[3]]  + outras[[4]]
    names(outras) <- out_layer_names
    
    # [[c(out_layer_names[1:3], sort(out_layer_names[-(1:3)]))]]

    terra::writeRaster(outras, out_file, datatype = "INT2S", overwrite = TRUE)
    
    rm(fires, harvest, insect, rast_stack, outras) 
    
  }
  
   
  if (cores > 1) parallel::stopCluster(cl)
  
}
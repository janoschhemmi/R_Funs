source("bfast/lib_bfast_utils.R")
library(tictoc)

# 
# in_path      <- "p:/workspace/jan/fire_detection/break_detection/break_detection_cube/landsat-Sentinel2-nbr-h40-o1-bp3-expansion" #/X0068_Y0045"
# outpath_root <- "p:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/landsat-S2-nbr-h40-o1-bp3-expansion"
# path_models  <- "p:/workspace/jan/fire_detection/break_detection/classification_model/landsat-S2-nbr-h40-o1-bp3-expansion_revisited"
# model_name   <- "model_basic_mtry_15"


in_path      <- "p:/workspace/jan/fire_detection/break_detection/break_detection_cube/landsat-nbr-h40-o1-bp3-expansion" #/X0068_Y0045"
outpath_root <- "p:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/landsat-nbr-h40-o1-bp3-expansion"
path_models  <- "p:/workspace/jan/fire_detection/break_detection/classification_model/landsat-nbr-h40-o1-bp3-expansion_revisited"
model_name   <- "model_basic_mtry_10_redo_3"


#--- classify cube --------------------
rf_model <- readRDS(file.path(path_models, paste0(model_name, ".rds")))
rf_model$confusion

out_path <- file.path(outpath_root,model_name)
if(!dir.exists(out_path)) dir.create(out_path)

fns <- list.files(in_path, "^X.*tif$", full.names = T, recursive=T)
bm_classify_bfast_output(fns, rf_model, out_path, cores=20)


in_path      <- "p:/workspace/jan/fire_detection/break_detection/break_detection_cube/landsat-nbr-h40-o1-bp3-expansion" #/X0068_Y0045"
outpath_root <- "p:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/landsat-nbr-h40-o1-bp3-expansion"
path_models  <- "p:/workspace/jan/fire_detection/break_detection/classification_model/landsat-nbr-h40-o1-bp3-expansion_revisited"
model_name   <- "model_basic_mtry_15"

#--- classify cube --------------------
rf_model <- readRDS(file.path(path_models, paste0(model_name, ".rds")))
rf_model$confusion
# out_path <- file.path(outpath_root,model_name)
# if(!dir.exists(out_path)) dir.create(out_path)
# 
# fns <- list.files(in_path, "^X.*tif$", full.names = T, recursive=T)
# bm_classify_bfast_output(fns, rf_model, out_path, cores=15)




#--- run single files for testing -----

if (F) {
  tic()
  
  in_file <- "p:/workspace/jan/fire_detection/break_detection/break_detection_cube/landsat-nbr-h40-o1-bp3-expansion/X0068_Y0045/X0068_Y0045_NBR-NDV-TCB-TCG-TCW-TCD_4.tif"
  bm_classify_bfast_output(in_file, rf_model, out_path, overwrite=T)
  toc()
}
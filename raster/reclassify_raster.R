### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_3")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
set.seed(101
)
## lib
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(raster)
library(lubridate)
library(remotes)
library(mapac)
library(dplyr)
library(randomForest)
library(remotes)
library(foreach)
library(igraph)
library(bfastSpatial)
library(parallel)


# classi_name <- "PREDICTION_RF_150_Features_HL_ML_MLP.tif"
# reclassi_name <- "PREDICTION_RF_150_Features_HL_ML_MLP_paper_int.tif"
# 
# classi_name <- "PREDICTION_RF_475_Features_HL_ML_MLP.tif"
# reclassi_name <- "PREDICTION_RF_475_Features_HL_ML_MLP_paper_int.tif"

classi_name <- "PREDICTION_RF_550_Features_HL_ML_MLP.tif"
reclassi_name <- "PREDICTION_RF_550_Features_txt_HL_ML_MLP_paper_int.tif"

wd <- "A:/10_Classi/Classi_150_features_review/"
wd <- "A:/10_Classi/Classi_550_features_review_all_forest/"
wd <- "A:/workspace/tree_species/10_Classi/Classi_550_features_txt_review_no_mask/"

tile_list <- list.dirs(wd, full.names = FALSE)# [1:3]
tile_list <- tile_list[!tile_list %in% ""]

int_tree_species_split <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/old/00_area_stats_int_split_paper_update.csv", header=TRUE, sep = ";", dec=".")
int_tree_species_split <- int_tree_species_split[,c(3,14)]

from_to_matrix <- data.matrix(int_tree_species_split)

## ------------------------------------------------------------------------- ##

pclass <- function(r, cores=20, from_to_matrix) {
  
  require(randomForest)
  require(doParallel)
  
  beginCluster(cores)

  rc1 <- clusterR(r, reclassify, 
                  args=list(rcl= from_to_matrix, right=FALSE),
                  filename=rasterTmpFile(), overwrite = TRUE)

  message(stime/60.)
  endCluster()
  
  #registerDoSEQ()
  return(rc1)
}


# bin <- raster("A:/07_ForestMask/Landcover/europe_landcover_2015_RSE-Full3_masked_10m_Forest_8bit.tif")
# NAvalue(bin) <- 0
# writeRaster(bin, "A:/07_ForestMask/Landcover/europe_landcover_2015_RSE-Full3_masked_10m_Forest_8bit_na0.tif")

for (tile in tile_list) {
  
  # tile <- "X0070_Y0045"
  print(paste0("doing tile: ", tile))
    ## load raster 
    if ( file.exists(paste0(wd, tile, "/", classi_name)) == TRUE){
    classi   <- raster(paste0(wd, tile, "/", classi_name))
    reclassi <- reclassify(classi,rcl = from_to_matrix,  filename=rasterTmpFile(), overwrite = TRUE)
    ## parallel classify 
    print("writing Raster.. ")
    writeRaster(reclassi, file = paste0(wd,tile,"/",reclassi_name), overwrite = TRUE,
              format = "GTiff")
  } else {
    print("no classi found")
  }
  # reclassi <- pclass(classi, from_to_matrix=from_to_matrix
}


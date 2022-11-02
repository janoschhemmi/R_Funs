
## global Settings ----
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

# old.packages()
# # update if needed 
# update.packages(ask = FALSE, lib.loc = "S:/BrandSat/02_Code/R/library_3")

Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
terraOptions(memfrac=0.6, tempdir = 'S:/BrandSat/000_TerraTemp', verbose = TRUE,
             progress = 10, datatype = 'Init2s')

# set index list 
index_list <- c("NBR", "NDV","TCB","TCG","TCW","TCD")

# index <- "cro" ## loading cropped stack of nbr
stack_forest_mask == 0 ## not stack forest mask if cropped is used

# cube_dir <- "A:/01_Data_Level3/20210622_LANDSAT_monthly_TC/"
cube_dir <- "A:/01_Data_Level3/202208_LS_dl/"
out_dir  <- "A:/01_Data_Level3/202208_LS_dl/"

## Forest mask 
mask_dir  <- "P:/workspace/jan/masks/tiled/"
mask_file <- "forest_mask_landcover_map_30m.tif"
# mask_file <- "forest_mask_test.tif"

## list dir of cube for loop 
list_tiles <- list.dirs(cube_dir, full.names = FALSE)
list_tiles <- list_tiles[!list_tiles %in% c("")]
# list_tiles <- list_tiles[list_tiles %in% c("X0071_Y0043")]

## fore tiles; for subsetting of smps 
force_tiles <- vect("P:/workspace/jan/FORCE/tiles/Tiles_Extent_Brandenburg_3035.shp")
force_tiles <- force_tiles[force_tiles$Name %in% list_tiles, ]


## load samples fun ----
load_samples <- function(){
  ## load id bb
  bb_id <- read.table("P:/timesync/bb/tsync_plots_bb.csv", 
                      sep = ",", dec = ".", header = TRUE)
  bb_interpretations <- read.table("P:/timesync/bb/tsync_plots_bb_interpretations.csv",
                                   sep = ",", dec = ".", header = TRUE)
  bb_comment <- read.table("P:/timesync/bb/tsync_plots_bb_interpretations_comment.csv",
                           sep = ",", dec = ".", header = TRUE)
  
  ## id fires 
  fires_id <- read.table("P:/timesync/bb/tsync_fires_4_distance_to_all_center.csv",
                         sep = ",", dec = ".", header = TRUE)
  fires_interpretations <- read.table("P:/timesync/bb/tsync_fires_interpretations.csv",
                                      sep = ",", dec = ".", header = TRUE)
  fires_comments <- read.table("P:/timesync/bb/tsync_fires_interpretations_comment.csv",
                               sep = ",", dec = ".", header = TRUE)
  
  ids <- rbind(fires_id, bb_id)
  return(ids)
}

## load ids; here coordinates needed; --> load samples from timesync output 
ids <- load_samples()

## get force tiles that have smps in ----
ss <- 1
for(tile in list_tiles){
  
  ## subset tile shp  
  tile_shp <- force_tiles[force_tiles$Name %in% tile,]
  ## get extent of tile 
  tile_extent <- ext(tile_shp)
  
  print(tile_extent)
  ## subset x y lists
 
  if(length(tile_shp) > 0){ ## if tile has data 
    points_tile  <- subset(ids, x >= tile_extent[1] & x <= tile_extent[2] &
                             y >= tile_extent[3] & y <= tile_extent[4] )
    
    ## load stack 
    # index_file <- paste0(cube_dir,tile,"/",file_raw)
    nrow_points_tile <- nrow(points_tile)
    
    if(ss == 1){
      tile_list_with_smp <- c(tile)
    }    else {
      tile_list_with_smp <- append(tile_list_with_smp, tile)
    }
    ss <- ss + 1
  } 
  
}

tile_list_with_smp <- unique(tile_list_with_smp)

## extract fun ----
# tile <- ("X0070_Y0040")
extract_fun <- function(tile) {
  
  print(paste0("doing tile: ",tile))
  s <- 1
  
  ## subset tile shp 
  # force_tiles <- rgdal::readOGR("P:/workspace/jan/FORCE/tiles/Tiles_Extent_Brandenburg_3035.shp")
  force_tiles <- terra::vect("P:/workspace/jan/FORCE/tiles/Tiles_Extent_Brandenburg_3035.shp")
  
  tile_shp <- force_tiles[force_tiles$Name %in% tile,]
  
  ## load sample ------------
  ids <- load_samples()
  
  ## get extent of tile 
  
  tile_extent <- ext(tile_shp)
  print(tile_extent)
  
  points_tile  <- subset(ids, x >= tile_extent[1] & x <= tile_extent[2] &
                           y >= tile_extent[3] & y <= tile_extent[4] )
  print("points in tile: ")
  print(points_tile)
  
  ## subset x y lists ----
  if(length(tile_shp) > 0){ ## if tile has data
    nrow_points_tile <- nrow(points_tile)
    
  } else { ## tile has no data .. set to zero so not all points get exracted 
    ## set to zero 
    nrow_points_tile <- 0
  }
  
  if (nrow_points_tile > 0 ){ ## there are smps 
    
    ## loop over indecies 
    for(index in index_list){
      ## load stack 
      # index <- "NBR"
      print(paste0("loading ", index, " stack..."))
      
      test_test <- 0 
      if(test_test == 1){
        index <- "TCG"
      }
      index_file <- paste0("A:/01_Data_Level3/202202_LANDSAT/",tile,"/1985-2021_001-365_HL_TSA_LNDLG_",index, "_TSS.tif")
      
      ## load stack 
      print(paste0("loading ",index))
      print(paste0("loading ",index_file))
      index_stack <- terra::rast(index_file)
      
      # ## extract smps
      print(paste0("extracting n=",nrow(points_tile)," samples"))
      
      ## extract smps ------------
      smps_tile <- cbind(points_tile$x, points_tile$y, points_tile$plotid,
                         terra::extract(index_stack, cbind(points_tile$x,points_tile$y)))
      smps_tile <- as.data.frame(smps_tile)
      
      # smps_tile <<- smps_tile
      print("smps_tile")
      print(smps_tile)
      colnames(smps_tile)[1:3] <- c("x","y","id")
      # ## store 
      write.table(smps_tile, paste0("P:/workspace/jan/fire_detection/plots/LPS/breaks_ts/treuenbrietzen/",tile,"_",index,"_extracted.csv"), sep = ";", dec = ".",
                  col.names = TRUE, row.names = FALSE)
      
    } ## loop over indecies 
    
  }
  
  print(paste0("S: ",s))
  
  
  s <- s + 1
}


##
## Main ----

library(parallel)
library(doParallel)
library(foreach)

detectCores()
cl <- makeCluster(2) #not to overload your computer
doParallel::registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("S:/BrandSat/02_Code/R/library_6"))
clusterEvalQ(cl, {library(doParallel)
  library(bfast)
  library(zoo)
  library(rlang)
  library(lubridate)
  library(devtools)
  library(terra)
  library(snow)
  library(lubridate)
  #library(hms)
  library(rgdal)
  library(ggplot2)
  library(data.table)
  library(reshape2)
  library(tidyr)
  library(Rcpp)
  library(dplyr)
  library(foreach)
  library(rgdal)
  #library(doSnow)
  
})


system.time(
  foreach (ooo  = 1:2, .packages="foreach") %do% { ## 51
    
    print(paste0("process tile n = ", ooo))
    extract_fun(tile_list_with_smp[ooo])
    # extract_fun(tiles_nach_process[ooo])
    
    # print("here")
    # print(res)
  }
)
#stop cluster
stopCluster(cl)
print("finished")




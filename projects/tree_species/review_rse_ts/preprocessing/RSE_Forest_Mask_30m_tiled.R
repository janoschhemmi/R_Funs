

### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_2")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")


## lib
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(raster)
library(lubridate)



### read LC RSE 
LC <- raster("A:/07_ForestMask/Landcover/europe_landcover_2015_RSE-Full3.tif")

## read LANDSAT example 
LS <- raster("A:/01_Data_Level3/")

##  dir in which cube is 
cube_dir <- "P:/workspace/jan/masks/tiled/"

# dir in which to store out csv's
out_dir <- "P:/workspace/jan/masks/tiled/"

## list dir of cube for loop 
list_tiles <- list.dirs(cube_dir, full.names = FALSE)

## check for empty characters 
list_tiles_checked <- c()
for (tile_name in list_tiles) {
  print(length(tile_name[1]))
  if (nchar(tile_name[1]) > 1) {
    list_tiles_checked <- append(list_tiles_checked, tile_name)    
  }
}

## loop over tiles 
for (tile in list_tiles_checked) {
  
  time_1 <- Sys.time()
  print(paste0("doing tile: ", tile))
  ## read 10m 
  file <- paste0(cube_dir,tile,"/","forest_mask_landcover_map_10m.tif")
  
  if (file.exists(file) == TRUE){ 
    # load file
    tif_10m <- raster(file)

    tif_30m <- aggregate(tif_10m, fact = 3)
    writeRaster(tif_30m,paste0(out_dir,tile,"/","forest_mask_landcover_map_30m.tif"), overwrite = TRUE)
  } else {
    print(paste0(file, "does not exist.. skip"))
  }
  time_2 <- Sys.time()
  time_dif <- time_2 - time_1
  print(paste0("took.. ",time_dif))
  
  }
  

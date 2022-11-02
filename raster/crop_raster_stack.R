
### unrelated test 

stack <- stack( "A:/01_Data_Level3/20210622_LANDSAT_monthly_TC/X0068_Y0045/1985-2020_001-365_HL_TSA_LNDLG_NBR_TSS.tif")

coo <- as.data.frame(matrix(1,ncol = 2, nrow = 1))
coo[1,1] <- x
coo[2,1] <- y
tt <- extract(stack, y = coo)

install.packages("svMisc")
library(svMisc)

for (o in seq(1:101)){
  
  progress(o, progress.bar = TRUE)
  Sys.sleep(0.01)
}



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
library(lubridate)
library(devtools)
library(bfastSpatial)
library(raster)
library(svMisc)
library(snow)
library(lubridate)

#install.packages("lubridate")
# install.packages("devtools")
# install.packages("bfastSpatial")
# install_github('dutri001/bfastSpatial')

bfastSpatial::
  
  ## globals ###################################################################
## index to base cals on
index <- "NBR"
file_raw <- paste0("1985-2020_001-365_HL_TSA_LNDLG_",index,"_TSS.tif")

# cube_dir <- "A:/01_Data_Level3/20210622_LANDSAT_monthly_TC/"
cube_dir <- "A:/01_Data_Level3/202106_LANDSAT/"
out_dir  <- "A:/01_Data_Level3/202106_BFAST/"

## Forest mask 
mask_dir  <- "P:/workspace/jan/masks/tiled/"
mask_file <- "forest_mask_landcover_map_30m.tif"
mask_file <- "forest_mask_test.tif"

## list dir of cube for loop 
list_tiles <- list.dirs(cube_dir, full.names = FALSE)

## TEST /// write fake Forest_mask 
# op <- par(mfrow=c(1, 2))
# plot(forest_mask_2)
# plot(forest_mask)
# writeRaster(forest_mask, "P:/workspace/jan/masks/tiled/X0068_Y0045/forest_mask_test.tif")
# 

## tile names 
list_tiles_checked <- c()
for (tile_name in list_tiles) {
  print(length(tile_name[1]))
  if (nchar(tile_name[1]) > 1) {
    list_tiles_checked <- append(list_tiles_checked, tile_name)    
  }
}
## for testing 
list_tiles_checked <- list_tiles_checked[2]


tile <- "X0068_Y0045"
print(paste0("doing tile: ", tile))

## load forest mask 
forest_mask <- raster(paste0(mask_dir,tile,"/",mask_file))
plot(forest_mask)

## load tif stack 
print("stacking.. ")
stack <- stack(paste0(cube_dir,tile,"/",file_raw))

## get dates
names <- names(stack)
dates <- get_dates(names)

## add forest mask as first layer 
stack <- stack(forest_mask, stack) 

## get min vals of stack 
minx <- extent(stack)[1]
miny <- extent(stack)[3]

## extract xy of mask 
xy     <- as.data.frame(xyFromCell(forest_mask, which(forest_mask[] == 1))) 
n_pixel <- nrow(xy)  
print(paste0("in this tile are ",n_pixel," pixels"))

to_crop_e <- extent( 5500 * 30 + extent(stack)[1] ,700 * 30 + extent(stack)[1],
                     800 * 30 + extent(stack)[3] ,900 * 30 + extent(stack)[3])
croped_s  <- crop(stack, to_crop_e)

writeRaster(croped_s, "A:/01_Data_Level3/202106_LANDSAT/X0068_Y0045/1985-2020_001-365_HL_TSA_LNDLG_cro_TSS.tif")

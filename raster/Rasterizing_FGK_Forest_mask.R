


library(dplyr)
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
library(parallel)
require(gdalUtils)
require(rgdal)
require(rgeos)
require(cluster)
require(parallel)
require(raster)
library(gdalUtils)
library(cluster)

## --> do the naehrstoffstufe 

setwd("S:/")

#---------------
# Switch 1 --> 10, 2 --> 20m 
switch <- 1 


#### load data ######################

# shapefile
# Test
# my.shapefile <- readOGR(dsn="C:/Uni/Masterarbeit/Landesforst_Brandenburg/Data/Data_Forst_Brandenburg/FGK_2reviere.shp")
my.shapefile <- readOGR(dsn="S:/Data_Forst_Brandenburg/02_ForstGrundKarte_Shapes/01_Forstgrundkarte_ArcGisExport/FGK_Soil_unique.shp")


# force tiles 
# force_tiles <- readOGR(dsn="C:/Uni/Masterarbeit/03_Classification/KMZ_zwei_tiles.shp")
force_extent            <- readOGR(dsn="Processing/02_Sampling/KMZ_Brandenburg_tiles_extent.shp")
force_tiles_germany     <- readOGR(dsn="Processing/02_Sampling/KMZ_Germany_tiles.shp")
force_tiles_rasterizing <- readOGR(dsn="Processing/02_Sampling/KMZ_Brandenburg_tiles.shp")

# force layer (for projection)
if (switch == 1){ # 10m 
force_layer <- raster("Force_Data/Force_Tile_Raster/2016-2019_001-365_LEVEL4_TSA_LNDLG_NIR_TSI.tif", band = 1)
} 
if (switch == 2){ # 20m 
force_layer <- raster("Force_Data/Force_Tile_Raster/2018-2020_001-365_HL_TSA_SEN2L_EVI_TSI.tif", band = 1)
}


# load inventur df 
Brandenburg_preprocessed_2 <- read.table("BrandSat/01_Data/01_FGK/2020-08-25_Brandenburg_preprocessed_unique_more_classes_per_tree_species.csv",  sep=";", dec = ".", header = TRUE)

# plot(my.shapefile) # does it look ok?
#plot(force_tiles_rasterizing)
#projection(force_tiles_rasterizing)
#force_tiles_rasterizing@data

## set projections  
sr_utm <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
sr_latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
sr_3035 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 
sr_force_layer <- projection(force_layer)

#####
### built raster for rasterization of FGK

# initiate raster 
r.raster <- raster()

# Define same projection as tiles to raster 
projection(r.raster) <- sr_force_layer

## define extent 
extent(r.raster) <- extent(force_extent)

## fuer Brandenburg 10x 9x 
difx <- extent(force_layer)[2] - extent(force_layer)[1]
difx <- difx * 10
extent(r.raster)[2] <- extent(r.raster)[1] + difx

dify <- extent(force_layer)[4] - extent(force_layer)[3]
dify <- dify * 9
extent(r.raster)[3] <- extent(r.raster)[4] - dify

## same resolutin 
res(r.raster) <- res(force_layer)
# plot(r.raster)

#####
## transform shapefile to force projection for rasterization 
print("transforming shp")
my.shapefile_sub     <- spTransform(my.shapefile, sr_force_layer)

### buffer in order to avoid broken geometries 
shp_buffered <- buffer(my.shapefile_sub, width=-0.00001, dissolve=FALSE) ## distance to border in meter 



###################################################################
### parallelise per tile 

force_tiles_rasterizing <- shapefile("S:/Processing/02_Sampling/KMZ_Brandenburg_tiles.shp")
plot(force_tiles_rasterizing)

## for each tile in in shape create raster and safe to disk 
l = 1
for (i in 1:length(force_tiles_rasterizing)){
  ## initiate new raster 
  r.raster <- raster()
  print(i) 
  
  ## transform tile 
  tile     <- spTransform(force_tiles_rasterizing[i,], sr_force_layer)
  
  # Define same projection as tiles to raster 
  projection(r.raster) <- sr_force_layer
  
  ## define extent 
  extent(r.raster) <- extent(tile)
  print(extent(r.raster))
  
  # set resolution
  res(r.raster) <- res(force_layer)
  
  ## mask treestands in tile and write to new shapefile
  intersection <- intersect(shp_buffered, tile)
  
  if (length(intersection) == 0) {
    print(paste("tile:", i, "has no intersections"))} 
  else {
    ## give consequent number to tiles 
    writeOGR(intersection, "S:\\BrandSat\\01_Data\\01_FGK\\01_Rasterization\\04_parts_per_tiles_fgk_STAO_naehrstoff", paste0("treestands_p",l), driver="ESRI Shapefile", overwrite=TRUE)
    writeRaster(r.raster,paste0("S:/BrandSat/01_Data/01_FGK/01_Rasterization/04_tile_raster_fgk_STAO_naehrstoff/gdal_p",l,".tif"), format="GTiff", overwrite=TRUE)
    l = l + 1}
}

## list all files in folder 
intersection_list  <- list.files("S:/BrandSat/01_Data/01_FGK/01_Rasterization/03_tile_raster_fgk_STAO/")
list_of_tilenumber <- vector("list",length(intersection_list))
k = 1
for (string in intersection_list){
  t <- substring(string, regexpr("_p", string) + 2)
  t <- substr(t,1,nchar(t)-4)
  list_of_tilenumber[k] <- t
  print(t)
  k = k + 1
}


# set up cluster and pass required packages and objects to cluster
ncl     <- detectCores() - 10
npar <- length(intersection_list)
cl <- makeCluster(ncl)
clusterEvalQ(cl, sapply(c('raster', 'gdalUtils',"rgdal"), require, char=TRUE))
clusterExport(cl, list("raster","npar"))

# parallel apply the rasterize function against the vector parts that were written, 


parLapply(cl = cl, X = 1:npar, fun = function(x) raster::rasterize(x=readOGR(paste0("S:/BrandSat/01_Data/01_FGK/01_Rasterization/03_parts_per_tiles_fgk_STAO/treestands_p",x,".shp")),
                                                                   y=raster(paste0("S:/BrandSat/01_Data/01_FGK/01_Rasterization/03_tile_raster_fgk_STAO/gdal_p",x,".tif")),field= 1,
                                                                   filename =paste0("S:/BrandSat/01_Data/01_FGK/01_Rasterization/04_tile_raster_fgk_STAO_naehrstoff/gdal_out_mask",x,".tif")))

# There are now n rasters representing the n segments of the original vector file
# read in the rasters as a list, merge and write to a new tif. 
s <- lapply(X=1:46, function(x) raster(paste0("S:/BrandSat/01_Data/01_FGK/01_Rasterization/04_tile_raster_fgk_STAO_naehrstoff/gdal_out_mask",x,".tif")))
s$filename <- "fgk_mask.tif"
s$overwrite <- TRUE
output_raster <- do.call(merge,s)
stopCluster(cl)     

### write raster
writeRaster(output_raster, "S:/BrandSat/01_Data/01_FGK/01_Rasterization/fgk_mask.tif", format ="GTiff")

print("finished")


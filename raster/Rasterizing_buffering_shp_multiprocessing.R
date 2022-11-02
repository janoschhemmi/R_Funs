
install.packages("parallel")
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

install.packages("gdalUtils")
library(gdalUtils)

install.packages("cluster")
library(cluster)



setwd("V:/")

#### load data ######################

# shapefile
# Test
# my.shapefile <- readOGR(dsn="C:/Uni/Masterarbeit/Landesforst_Brandenburg/Data/Data_Forst_Brandenburg/FGK_2reviere.shp")
my.shapefile <- readOGR(dsn="Data_Forst_Brandenburg/02_ForstGrundKarte_Shapes/01_Forstgrundkarte_ArcGisExport/FGK_join_ArcPro.shp")

# force tiles 
# force_tiles <- readOGR(dsn="C:/Uni/Masterarbeit/03_Classification/KMZ_zwei_tiles.shp")
force_extent        <- readOGR(dsn="Processing/01_Sampling/KMZ_Brandenburg_tiles_extent.shp")
force_tiles_germany <- readOGR(dsn="Processing/01_Sampling/KMZ_Germany_tiles.shp")
force_tiles_rasterizing <- readOGR(dsn="Processing/01_Sampling/KMZ_Brandenburg_tiles.shp")

# force layer (for projection)
force_layer <- raster("Processing/01_01_raster_data/Force_Tiles/2016-2019_001-365_LEVEL4_TSA_LNDLG_NIR_TSI.tif", band = 1)

# load df 
Brandenburg_preprocessed_2 <- read.table("Data_Forst_Brandenburg/03_DataTable_Preprocessed/Brandenburg_preprocessed_unique.csv",  sep=";", dec = ".", header = TRUE)


# plot(my.shapefile) # does it look ok?
plot(force_tiles_rasterizing)
projection(force_tiles_rasterizing)
force_tiles_rasterizing@data

## set projections  
sr_utm <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
sr_latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
sr_3035 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 
sr_force_layer <- projection(force_layer)

## merge shapefile with preprocessed data 
print("joining shapefile with data table")
shp_merged <- merge(my.shapefile, Brandenburg_preprocessed_2,all.x=FALSE, by.x=c("adresse","Baumart","Mischung","Schichtung","Mischung1","Schicht_Nr","Zeilen_Nr"), by.y=c("adresse","Baumart","Mischung","Schichtung","Mischung1","Schicht_Nr","Zeilen_Nr"))


## filter for sampling areas 
shp_sample <- subset(shp_merged, sample_criterium_es_rb1_unique == 1) ## only leaves tree stands with one layer and one tree species 
# shp_sample <- subset(shp_sample, Baumart_int <= 20) ## sampling in the twenty most common 
shp_sample <- subset(shp_sample, Baumart_int <= 60) ## sampling in the sixty most common 
# plot(shp_sample)
# shp_sample@data

#####
### built raster for rasterization of tree species  

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
# values(r.raster) <- 0
plot(r.raster)


#####

## transform shapefile to force projection for rasterization 
print("transforming shp")
shp_sample     <- spTransform(shp_sample, sr_force_layer)

## innerbuffer shapefile ## 30m
print("buffering shp")
shp_sample_buffered <- buffer(shp_sample, width=-15, dissolve=FALSE) ## distance to border in meter 
shp_sample_buffered@polygons

### write buffered shapefile 
writeOGR(shp_sample_buffered,layer = 1, "Data_Forst_Brandenburg/02_ForstGrundKarte_Shapes/02_Shape-Buffered/brandenburg_buffered_15m_60ts.shp", driver = "ESRI Shapefile")


#### prallel shp_sample_buffered force_tiles_rasterizing force_extent  force_tiles_germany force_tiles_rasterizing



## print baumart int in raster
r_sample           <- rasterize(shp_sample_buffered, r.raster,field = 'Baumart_int')
# r_sample_b2       <- rasterize(shp_sample_buffered, r.raster,field = 'StratoAge_int')
# r_sample_stack    <- stack(r_sample, r_sample_b2)

writeRaster(r_sample, filename=file.path("Data_Forst_Brandenburg/04_Rasterized_ForstGrundKarte/Brandenburg_15mbuffer_60ts.tif"), format="GTiff", overwrite=TRUE)
print("finished writing raster")

## script speichern nach 30m buffer_40ts

shp_buffered <- readOGR(dsn="Data_Forst_Brandenburg/02_ForstGrundKarte_Shapes/02_Shape-Buffered/brandenburg_buffered_15m_60ts.shp")
##
shp_buffered <- shp_sample_buffered

### number of cores 
no_cores <- detectCores() -20

features <- 1:nrow(shp_buffered[,])

# Split features in n parts
n <- 20
parts <- split(features, cut(features, n))

# Initiate cluster (after loading all the necessary object to R environment: BRA_adm2, parts, r.raster, n)
cl <- makeCluster(no_cores, type = "PSOCK")
#?? makeCluster
print(cl)

# Parallelize rasterize function
#plot(r.raster)
#xx <-  function(x,shp, raster) { rasterize (shp[parts[[x]],], raster,field = 'Baumart_int')}
#x <- 1:n
#system.time(rParts <- parLapply(cl = cl, X = 1:n, fun = function(x) {rasterize(shp_buffered[parts[[x]],])},  r.raster,field = 'Baumart_int')))

#system.time(rParts <- parLapply(cl = cl,x = 1:n, fun = xx(x,shp_buffered,r.raster)))

# Finish
#stopCluster(cl)

# Merge all raster parts
#rMerge <- do.call(merge, rParts)

# Plot raster
#plot(rMerge)


###### alles nochmal nach tiles parallelisieren 

# split vector data into n parts, the same as number of processors (minus 1)
npar     <- detectCores() - 10
features <- 1:nrow(shp_buffered[,])
parts    <- split(features, cut(features, npar))

# write the vector parts out
setwd("V:/Data_Forst_Brandenburg/02_ForstGrundKarte_Shapes/04_parts/15m_60ts/")
for(n in 1:npar){
  print(n)
  writeOGR(shp_buffered[parts[[n]],], ".\\parts", paste0("mydata_p",n), driver="ESRI Shapefile")
}

# set up and write a blank raster for gdal_rasterize for EACH vector segment created above
for(n in 1:npar){
  print(n)
  writeRaster(r.raster, filename=paste0(".\\gdal_p",n,".tif"), format="GTiff", overwrite=TRUE)
}

# set up cluster and pass required packages and objects to cluster
cl <- makeCluster(npar)
clusterEvalQ(cl, sapply(c('raster', 'gdalUtils',"rgdal"), require, char=TRUE))
clusterExport(cl, list("r.raster","npar"))

# parallel apply the gdal_rasterize function against the vector parts that were written, 
# same number as processors, against the pre-prepared rasters
parLapply(cl = cl, X = 1:npar, fun = function(x) gdal_rasterize(src_datasource=paste0(".\\parts\\mydata_p",x,".shp"),
                                                                dst_filename=paste0(".\\gdal_p",x,".tif"),b=1,a="code",verbose=F,output_Raster=T))

# There are now n rasters representing the n segments of the original vector file
# read in the rasters as a list, merge and write to a new tif. 
s <- lapply(X=1:npar, function(x) raster(paste0(".\\gdal_p",n,".tif")))
s$filename <- "myras_final.tif"

  #do.call(merge,s)
stopCluster(cl)



s$filename <- 'test2.tif'
s$overwrite <- TRUE
output_raster <- do.call(merge, s)
stopCluster(cl)

## write raster 
# writeRaster(myras_final.tif, "myras_final.tif", format ="GTiff")
writeRaster(output_raster, "myras_final_3.tif", format ="GTiff")


###################################################################
### parallelise per tile 


force_tiles_rasterizing <- shapefile("V:/Processing/01_Sampling/KMZ_Brandenburg_tiles.shp")
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
  writeOGR(intersection, ".\\parts_per_tiles", paste0("treestands_p",l), driver="ESRI Shapefile", overwrite=TRUE)
  writeRaster(r.raster,paste0(".\\tile_raster\\gdal_p",l,".tif"), format="GTiff", overwrite=TRUE)
  l = l + 1}
}

## list all files in folder 
intersection_list  <- list.files("tile_raster/")
list_of_tilenumber <- vector("list",length(intersection_list))
k = 1
for (string in intersection_list){
  t <-substring(string, regexpr("_p", string) + 2)
  t <- substr(t,1,nchar(t)-4)
  list_of_tilenumber[k] <- t
  print(t)
  k = k + 1
}


# set up cluster and pass required packages and objects to cluster
ncl     <- detectCores() - 10
cl <- makeCluster(ncl)
clusterEvalQ(cl, sapply(c('raster', 'gdalUtils',"rgdal"), require, char=TRUE))
clusterExport(cl, list("r.raster","npar"))

# parallel apply the rasterize function against the vector parts that were written, 
npar <- length(intersection_list)

parLapply(cl = cl, X = 1:npar, fun = function(x) raster::rasterize(x=readOGR(paste0(".\\parts_per_tiles\\treestands_p",x,".shp")),
                                                                y=raster(paste0(".\\tile_raster\\gdal_p",x,".tif")),field='Bmrt_nt',
                                                                filename =paste0(".\\tile_raster\\gdal_out_p",x,".tif")))

# There are now n rasters representing the n segments of the original vector file
# read in the rasters as a list, merge and write to a new tif. 
s <- lapply(X=1:npar, function(x) raster(paste0(".\\tile_raster\\gdal_out_p",x,".tif")))
s$filename <- "myras_final_4.tif"
s$overwrite <- TRUE
do.call(merge,s)
stopCluster(cl)         
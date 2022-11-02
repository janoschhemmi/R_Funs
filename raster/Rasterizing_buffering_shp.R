install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("formattable")
install.packages("maptools")
install.packages("sp")
install.packages("rgdal")
install.packages("raster")
install.packages("sf")
install.packages("rgeos")


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


setwd("V:/")

#### load data ######################

# shapefile
# Test
# my.shapefile <- readOGR(dsn="C:/Uni/Masterarbeit/Landesforst_Brandenburg/Data/Data_Forst_Brandenburg/FGK_2reviere.shp")
my.shapefile <- readOGR(dsn="Data_Forst_Brandenburg/02_ForstGrundKarte_Shapes/01_Forstgrundkarte_ArcGisExport/FGK_join_ArcPro.shp")

# force tiles 
# force_tiles <- readOGR(dsn="C:/Uni/Masterarbeit/03_Classification/KMZ_zwei_tiles.shp")
force_tiles <- readOGR(dsn="Processing/01_Sampling/KMZ_Brandenburg_tiles_extent.shp")

# force layer (for projection)
force_layer <- raster("Processing/01_01_raster_data/Force_Tiles/2016-2019_001-365_LEVEL4_TSA_LNDLG_NIR_TSI.tif", band = 1)

# load df 
Brandenburg_preprocessed_2 <- read.table("Data_Forst_Brandenburg/03_DataTable_Preprocessed/Brandenburg_preprocessed_unique.csv",  sep=";", dec = ".", header = TRUE)


# plot(my.shapefile) # does it look ok?
# plot(force_layer)

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
extent(r.raster) <- extent(force_layer)

## fuer Brandenburg 10x 9x 
difx <- extent(force_layer)[2] - extent(force_layer)[1]
difx <- difx * 10
extent(r.raster)[2] <- extent(r.raster)[1] + difx

dify <- extent(force_layer)[4] - extent(force_layer)[3]
dify <- dify * 9
extent(r.raster)[3] <- extent(r.raster)[4] - dify

## same resolutin 
res(r.raster) <- res(force_layer)


#####

## transform shapefile to force projection for rasterization 
print("transforming shp")
shp_sample     <- spTransform(shp_sample, sr_force_layer)

## innerbuffer shapefile ## 30m
print("buffering shp")
shp_sample_buffered <- buffer(shp_sample, width=-15, dissolve=FALSE) ## distance to border in meter 

??rasterize

## print baumart int in raster
r_sample           <- rasterize(shp_sample_buffered, r.raster,field = 'Baumart_int')
# r_sample_b2       <- rasterize(shp_sample_buffered, r.raster,field = 'StratoAge_int')
# r_sample_stack    <- stack(r_sample, r_sample_b2)

writeRaster(r_sample, filename=file.path("Data_Forst_Brandenburg/04_Rasterized_ForstGrundKarte/Brandenburg_15mbuffer_60ts.tif"), format="GTiff", overwrite=TRUE)
print("finished writing raster")

## script speichern nach 30m buffer_40ts


###### alles nochmal nach tiles parallelisieren 

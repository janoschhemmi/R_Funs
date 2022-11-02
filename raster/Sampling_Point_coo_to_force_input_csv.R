

# set lib path 
.libPaths("S:/BrandSat/02_Code/R/library")

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
library(randomForest)
library(caret)
library(matrixStats)
library(fortify)
library(mapac)
library(e1071)
library(gt)


## load point shapefile for sampling 

## // shp includes x and y col added in qgis via add geometry attribute 
shp <- readOGR("A:/04_Sampling/00_SamplingPoints/2021-01-18_after_distance_reduction/Included_smp_points_Brandenburg.shp")
shp <- readOGR("P:/workspace/fire_digitize/fire_samples_Brandsat/fire_smp_1_tile_X0068_Y0045_reduced.shp")

x <- as.numeric(shp@data$x_coo)
x <- round(x)
y <- as.numeric(shp@data$y_coo)
y <- round(y)
int <- shp@data$UID
int <- as.numeric(na.omit(int))
data <- as.data.frame(cbind(x,y,int))


## alternatively with sf 
shp <- st_read("A:/04_Sampling/2021-01-25_S2_original/Sample_points_X0070_Y0041_2.shp")
x <- as.numeric(shp$coo_x)
x <- round(x)
y <- as.numeric(shp$coo_y)
y <- round(y)
int <- shp$int_split
int <- as.numeric(na.omit(int))
data <- as.data.frame(cbind(x,y,int))

data_tally <- data %>%
  group_by(int) %>%
  tally()

write.table(data, "S:/BrandSat/01_Data/02_Sampling/2021-06_smp_Landsat_fire_1tile/force_input_1tile_reduced.csv",sep = " ", dec = ".", col.names = FALSE, row.names = FALSE)


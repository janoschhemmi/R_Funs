install.packages("ggplot2")
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



###
path = "V:/Evironmental_Data/07_DWD/soil_moisture/"
tif_path = paste0(path,"tif/")

setwd(path)
list_files <- list.files(path, pattern = "\\.asc$")
end_tif <- 'tif'

for (file in list_files){
  print(file)
  
  ## read ascii 
  ascii <- raster(paste0(path,file))
  crs(ascii) <- CRS('+init=EPSG:31467')
  
  #print(ascii@crs)
  print("reproject...")
  ascii_re <- projectRaster(ascii, crs = CRS('+init=EPSG:3035'))
  
  print("filename:")
  file_name <- paste0(path, substr(file, 1, nchar(file)-4),".tif")
  file_short <- paste0(substr(file, 1, nchar(file)-4),".tif")
  print(file_short)
  
  print("write Raster..")
  setwd(tif_path)
  writeRaster(ascii_re,file_short ,datatype = 'INT2S', format = 'GTiff')
  
}



path = "V:/Evironmental_Data/07_DWD/precipitation/"
setwd(path)
list_files <- list.files(path, pattern = "\\.asc$")
end_tif <- 'tif'

for (file in list_files){
  print(file)
  
  ## read ascii 
  ascii <- raster(paste0(path,file))
  crs(ascii) <- CRS('+init=EPSG:31467')
  
  #print(ascii@crs)
  print("reproject...")
  ascii_re <- projectRaster(ascii, crs = CRS('+init=EPSG:3035'))
  
  print("filename:")
  file_name <- paste0(path, substr(file, 1, nchar(file)-4),".tif")
  file_short <- paste0(substr(file, 1, nchar(file)-4),".tif")
  print(file_short)
  
  print("write Raster..")
  writeRaster(ascii_re,file_short ,datatype = 'INT2S', format = 'GTiff')
  
}

  
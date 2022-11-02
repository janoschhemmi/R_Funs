
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
path =  "U:/"
setwd(path)

tilenames = c('X0065_Y0040', 'X0065_Y0041', 'X0066_Y0040', 'X0066_Y0041', 'X0066_Y0042', 'X0067_Y0040', 'X0067_Y0041',
             'X0067_Y0042', 'X0067_Y0043', 'X0067_Y0044', 'X0067_Y0045', 'X0068_Y0040', 'X0068_Y0041', 'X0068_Y0042',
             'X0068_Y0043', 'X0068_Y0044', 'X0068_Y0045', 'X0069_Y0040', 'X0069_Y0041', 'X0069_Y0042', 'X0069_Y0043',
             'X0069_Y0044', 'X0069_Y0045', 'X0069_Y0046', 'X0069_Y0047', 'X0070_Y0039', 'X0070_Y0040', 'X0070_Y0041',
             'X0070_Y0042', 'X0070_Y0043', 'X0070_Y0044', 'X0070_Y0045', 'X0070_Y0046', 'X0070_Y0047', 'X0071_Y0039',
             'X0071_Y0040', 'X0071_Y0041', 'X0071_Y0042', 'X0071_Y0043', 'X0071_Y0044', 'X0071_Y0045', 'X0071_Y0046',
             'X0071_Y0047', 'X0072_Y0040', 'X0072_Y0042', 'X0072_Y0043', 'X0072_Y0044', 'X0072_Y0045', 'X0072_Y0046',
             'X0072_Y0047', 'X0073_Y0044', 'X0073_Y0045', 'X0073_Y0046',
             'X0070_Y0050', 'X0071_Y0049', 'X0070_Y0048','X0070_Y0049')[1:2]

## crs 
ra_pro <- raster("U:/X0065_Y0040/2016-2019_001-365_LEVEL4_TSA_LNDLG_BLU_TSI.tif")
ra_pro@crs

anc_list_8bit = c("bgl5000_v20.tif","buek1000de_v21.tif")
anc_list_16bit = c("bgl5000_v20_16bit.tif","buek1000de_v21_16bit.tif")
anc_list_16bit_repro = c("bgl5000_v20_16bit_repro.tif","buek1000de_v21_16bit_repro.tif")

for (tile in tilenames){
  print(tile)
  
  for (i in 1:length(anc_list_8bit)){
    print(i)
    
    in_tif <- paste0(path,tile,"/",anc_list_16bit[i])
    out_tif <- paste0(path,tile,"/",anc_list_16bit_repro[i])
    
    print(in_tif)
    print(out_tif)
    
    raster_in <- raster(in_tif)
    projection(raster_in) <- ra_pro@crs
    print("writing_raster... ")
    writeRaster(raster_in, out_tif, datatype="INT2S", format = 'GTiff')
    
  }
  
}






## Preprocessing Master Code ----
## This script simply runs two other scripts for preprocessing of 
## Timesync References and Comments 

## So if any comments get changed in TymeSync; 
## the master Code needs to be run once.. 


## global Settings ----
.libPaths("S:/BrandSat/02_Code/R/library_3")

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
library(hms)
library(ggplot2)
library(doSnow)
library(foreach)
library(rgdal)

rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")


setwd("P:/workspace/jan/code/R/BFAST/00_reference_preprocessing")
## run fire preprocessing
source(file = "Strucchange_00_00_get_sizes_of_fire.R")
get_size_of_fire()
source(file = "Strucchange_00_00_reprocess_interpretations.R")
preprocess_inters()

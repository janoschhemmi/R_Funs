### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_3")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
set.seed(101
)
## lib
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(raster)
library(lubridate)
library(remotes)
library(mapac)
library(dplyr)
library(randomForest)
library(remotes)
library(foreach)
library(igraph)

install.packages("igraph")
remotes::install_github("juoe/spatialtools")


#' Apply minimum mapping unit to categorical raster
#'
#' @param x Raster object
#' @param mmu Minimum mapping unit as number of pixels
#' @param ncores Number of cores to use if parallel processing is wanted
#' @param filename Optional. filename for writing raster to disk.
#'
#' @return A raster layer with the designated mmu
#' @export

minimum_mapping_unit <- function(x, mmu, ncores, filename = NULL) {
  
  # layers <- raster::unstack(raster::layerize(x, falseNA = TRUE))
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  patches <- foreach::foreach(i=layers, .packages=c("raster")) %dopar% {
    
    patches <- raster::clump(i)
    f <- as.data.frame(raster::freq(patches))
    
    exclude_id <- f$value[which(f$count < mmu)]
    
    rcl <- matrix(c(exclude_id, rep(NA, length(exclude_id))), ncol = 2)
    reclassify(patches, rcl)
    
  }
  raster::clu
  parallel::stopCluster(cl)
  
  stacked <- do.call(raster::cover, patches)
  
  masked <- raster::mask(x, stacked)
  
  # apply majority filter
  
  focal_fun <- function(v) {
    
    mode_val <- as.numeric(names((which.max(table(v)))))
    
    if (!purrr::is_empty(mode_val)) {
      return(mode_val)
    } else {
      return(NA)
    }
    
  }
  
  additional_nas <- raster::freq(masked, value = NA)- raster::freq(x, value = NA)
  wsize <- 3
  
  while(additional_nas > 0){
    
    filled <- raster::focal(masked, matrix(1, nrow=wsize, ncol=wsize), fun=focal_fun, pad=TRUE, NAonly = TRUE)
    
    filled_mask <- raster::mask(filled, x)
    
    additional_nas <- raster::freq(filled_mask, value = NA) - raster::freq(x, value = NA)
    wsize <- wsize + 2
    
  }
  
  if(!is.null(filename)) {
    raster::writeRaster(filled_mask, filename, overwrite = TRUE)
  }
  
  return(filled_mask)
  
}




################################################################################

## load mask 
forest_mask <- raster("A:/07_ForestMask/Landcover/europe_landcover_2015_RSE-Full3_masked_10m_Forest_8bit.tif")

## mmu 
mmu <- 3

patches <- raster::clump(forest_mask, directions=4, gaps = FALSE)
writeRaster(patches, "P:/workspace/jan/masks/europe_landcover_2015_RSE-Full_BB_SA_10m_mmu_3_patches.tif")

f <- as.data.frame(raster::freq(patches))

exclude_id <- f$value[which(f$count < mmu)]

rcl <- matrix(c(exclude_id, rep(NA, length(exclude_id))), ncol = 2)
reclassified <- reclassify(patches, rcl)
writeRaster(reclassified, "P:/workspace/jan/masks/europe_landcover_2015_RSE-Full_BB_SA_10m_mmu_3.tif")

plot(reclassified)


rcl
patches
fm_3 <- minimum_mapping_unit(forest_mask, 3, 5, filename = NULL)

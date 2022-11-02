
### Model with all features 

### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_3")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
set.seed(101)

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
library(snow)
library(glcm)



## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

df_features <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_spec_reduced.csv", 
                         sep = ";", dec = ".", header = TRUE)
df_env     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_env_red.csv", 
                         sep = ";", dec = ".", header = TRUE)
df_txt     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_txt_reduced.csv", 
                         sep = ";", dec = ".", header = TRUE)

### hist srtm 
hist_1 <- hist(df_env$srtm)
hist_2 <- hist(df_env$aspect_10m)
hist_3 <- hist(df_env$slope_10m)
hist_4 <- hist(df_env$RSMS_1981_17_10m)
hist_5 <- hist(df_env$TAMM_1981_17_10m)
hist_6 <- hist(df_env$nutrient)

par(mfrow=c(3,2))
plot(hist_1, xlab = "elevation [m]", main =  "" )
plot(hist_2, xlab = "aspect [?]", main = " ")
plot(hist_3, xlab = "slope [?]", main =  "" )
plot(hist_4, xlab = "Yearly sum of precipitation [mm]", main =  "" )
plot(hist_5, xlab = "Yearly mean air temp. [1/10 ?C.]", main =   "" )
plot(hist_6, xlab = "Nutrient Level", main =  "" )


## ----------------------------------------------------------------------------
## hist of variables 

# 1 nutrient level 
wd <- ("S:/BrandSat/01_Data/01_FGK/01_Rasterization/06_fgk_soil_rasterized/")
wd_out <- ("P:/workspace/treespecies/_review/histograms_samples/plot")
env_list <- list.files(wd,pattern = ".tif" ) # [1:2]
env_up <- c("nutrient","soilwater", "mineral","organic","seasonal","flood", "wet","wet flood","Stao max a")
smp_xy <- as.data.frame(df_env[,1:2])  

i <- 1
for (env in env_list){
  
  # load raster
  env_raster <- raster(paste0(wd,env))
  # env_raster[env_raster== -9999] <- -Inf
  NAvalue(env_raster) <- -9999
  plot(env_raster)
  
  # pal <- colorRampPalette(c("darkorange4","blue4"))
  # plot_env <- plot(env_raster, col = pal(5))
  unique <- unique(env_raster, progress = "text")
  n_vals <- length(unique)
  
  hist_ras <- hist(env_raster, maxpixels = ncell(env_raster),
                   main = paste0("Distribution of ", env_up[i], " levels"),
                   xlab = paste0(env_up[i], " levels", mids = n_vals)
                   )
  ## safe table
  df_out <- data.frame(breaks = hist_ras$mids, counts = hist_ras$counts)
  write.table(df_out, paste0(wd_out,"/_",i,"_table_ras_",env,".csv"), sep = ";", dec = ".",
              col.names = TRUE, row.names = FALSE)
  
  ## safe 
  options(scipen=999)
  jpeg(file=paste0(wd_out ,"/",i,"_hist_ras_",env,".jpeg"))
  plot(hist_ras,  main = paste0("Distribution of ", env_up[i], " levels"),
       xlab = paste0(env_up[i], " levels"))
  dev.off()
  
  smp_env <- extract(env_raster, smp_xy)
  hist_smp <- hist(smp_env,
                   main = paste0("Distribution of sampled ", env_up[i], " levels"),
                   xlab = paste0(env_up[i], " levels")
  )
  ## safe 
  jpeg(file=paste0(wd_out ,"/",i,"_hist_smp_",env,".jpeg"))
  plot(hist_smp,        main = paste0("Distribution of sampled ", env_up[i], " levels"),
       xlab = paste0(env_up[i], " levels"))
  dev.off()
  
  i <- i + 1
    }


### do it for rain, temp, srtm , slope
wd <- "A:/08_Environmental/01_Multiannual/04_Precipitation"
tif_list <- c("/03_MeanAirTemp/TAMM_17_1981_30.tif","/04_Precipitation/RSMS_17_1981_30.tif")



################################################################################

Brandenburg <- shapefile("P:/workspace/jan/shapes/Borders/Brandenburg.shp")
## Histograms FGK vs. All Forst

## load precipitation 
preci <- raster("A:/08_Environmental/01_Multiannual/04_Precipitation/RSMS_17_1981_30.tif")
plot(preci)

## to brandeburg
b_re <- spTransform(Brandenburg, crs(preci))
plot(b_re)
r2 <- crop(preci, extent(b_re))
r3 <- mask(r2, b_re)
plot(r3)

## load elevation 
srtm <- raster("A:/01_Data_Level3/BRB_ENV/srtm_10m_mosaic_tif.tif")
plot(srtm)
srtm_2 <- crop(srtm, extent(b_re))
srtm_3 <- mask(srtm_2, b_re)


## load all forest 
forest <- raster("A:/07_ForestMask/Forest_Mask_Combined/forest_mask_rse_cropped.tif")
#forest <- projectRaster(forest, crs(srtm))
forest <- crop(forest, extent(b_re))
#plot(forest)
# forest <- mask(forest, b_re)
# writeRaster(forest, "A:/07_ForestMask/Forest_Mask_Combined/forest_mask_rse_cropped.tif", overwrite = TRUE)

## load fgk mask 
fgk <- raster("S:/BrandSat/01_Data/01_FGK/01_Rasterization/fgk_mask.tif")

### mask to forest 
crs(srtm_3) 
crs(forest)

# srtm_c <- crop(srtm_3, extent(forest))
# r3_c    <- crop(r3, extent(forest))

## clip extent in qgis 
writeRaster(srtm_c , "A:/08_Environmental/_old/srtm_bb_for_cropping_inqgis.tif")
writeRaster(srtm_cq , "A:/08_Environmental/_old/srtm_bb_other_extent_try.tif")


## load raster clipped in qgis
srtm_cq <- raster("A:/08_Environmental/_old/srtm_bb_for_cropping_inqgis_cropped_to_forest_mask_2.tif")
preci_cq <- raster("A:/08_Environmental/_old/preci_bb_for_cropping_inqgis_cropped_to_forest_mask_3.tif")
fgk      <- raster("A:/08_Environmental/_old/fgk_mask_cropped.tif")

## resample preci
r_resam <- resample(preci_cq,fgk,method='bilinear')

## mask
srtm_forest  <- mask(srtm_cq,  forest, maskvalue = 0)
preci_forest <- mask(r_resam, forest, maskvalue = 0)

srtm_fgk  <- mask(srtm_cq,fgk)
preci_fgk <- mask(r_resam, fgk)

plot(srtm_fgk)

## freqs
freq_srtm_forest <- freq(srtm_forest, progress = "text")
freq_preci_forest <- freq(preci_forest, progress = "text")
freq_srtm_fgk <- freq(srtm_fgk, progress = "text")
freq_preci_fgk <- freq(preci_fgk, progress = "text")
??freq

## as df 
freq_srtm_forest <- as.data.frame(freq_srtm_forest)
freq_preci_forest <- as.data.frame(freq_preci_forest)
freq_srtm_fgk <- as.data.frame(freq_srtm_fgk)
freq_preci_fgk <- as.data.frame(freq_preci_fgk)

## safe  

# write.table(freq_srtm_forest, "P:/workspace/treespecies/_review/histograms_samples/df_01_freq_srtm_forest.csv", sep = ";", dec = ".", col.names = TRUE,row.names = FALSE)
# write.table(freq_preci_forest, "P:/workspace/treespecies/_review/histograms_samples/df_02_freq_preci_forest.csv", sep = ";", dec = ".", col.names = TRUE,row.names = FALSE)
# write.table(freq_srtm_fgk, "P:/workspace/treespecies/_review/histograms_samples/df_03_freq_srtm_fgk.csv", sep = ";", dec = ".", col.names = TRUE,row.names = FALSE)
# write.table(freq_preci_fgk, "P:/workspace/treespecies/_review/histograms_samples/df_04_freq_preci_fgk.csv", sep = ";", dec = ".", col.names = TRUE,row.names = FALSE)

freq_preci_forest$type <- "forest"
freq_preci_fgk$type    <- "fgk"
preci_join <- rbind(na.omit(freq_preci_forest),na.omit(freq_preci_fgk))

## rebuilt 
preci_fo <- as.vector(rep(freq_preci_forest$value, freq_preci_forest$count))
preci_fgk <- as.vector(rep(freq_preci_fgk$value, freq_preci_fgk$count))
hist_1 <- hist(preci_fo, bins=15, main = " ", xlab = "Dis. precipitation forest")
hist_2 <- hist(preci_fgk, bins=15, main = " ", xlab = "Dis. precipitation inventory")

plot(hist_1, main = " ", xlab = "Dis. precipitation forest")
plot(hist_2, main = " ", xlab = "Dis. precipitation inventory")

rm(preci_fgk)
rm(preci_fo)

srtm_fo <- as.vector(rep(freq_srtm_forest$value, freq_srtm_forest$count))
srtm_fo[srtm_fo <= -1] <- 0

srtm_fgk <- as.vector(rep(freq_srtm_fgk$value, freq_srtm_fgk$count))
srtm_fgk[srtm_fgk <= -1] <- 0

hist_3 <- hist(srtm_fo, bins=15, main = " ", xlab = "Dis. elevation forest")
hist_4 <- hist(srtm_fgk, bins=15, main = " ", xlab = "Dis. elevation inventory")

plot(hist_3, main = " ", xlab = "Dis. elevation forest [m]")
plot(hist_4, main = " ", xlab = "Dis. elevation inventory [m]")

rm(srtm_fgk)
rm(srtm_fo)

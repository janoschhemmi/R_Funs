
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
library(tidyverse)
library(tibble)
library(foreign)


## -----------------------------------------------------------------
## extract smp points
# 
# pts <- shapefile("A:/04_Sampling/00_SamplingPoints/2021-01-18_after_distance_reduction/Included_smp_points_Brandenburg_3_tweeck.shp")
# over <- over(pts, my.shapefile_sub)
# over <- cbind(over, pts@data)
# # ## write data 
# 
# write.table(over, "A:/04_Sampling/2021-02-05_S2_new_interpolation/env/df_env_new_fgk_dirk.csv", sep =";", dec =".", col.names = TRUE)
over <- read.table("A:/04_Sampling/2021-02-05_S2_new_interpolation/env/df_env_new_fgk_dirk.csv", sep =";", dec =".", header = TRUE)

## load df spec txt env 
df_spec <- read.table("A:/04_Sampling/2021-02-05_ts_review/df_features_rse_review_winter_sub.csv", sep =";", dec =".", header = TRUE)
df_txt  <- read.table("A:/04_Sampling/2021-02-05_ts_review/df_txt_summer_20m.csv", sep =";", dec =".", header = TRUE)
df_txt_2019  <- read.table("A:/04_Sampling/2021-02-05_ts_review/smp_txt/df_txt_2019.csv", sep =";", dec =".", header = TRUE)
# df_env  <- read.table("A:/04_Sampling/2021-02-05_S2_new_interpolation/env/df_env.csv", sep =";", dec =".", header = TRUE)

## old env df already reduced
df_env_red<- read.table("A:/04_Sampling/2021-02-05_S2_new_interpolation/env/df_env_new_fgk_sub_with_stao_max.csv", sep =";", dec =".", header = TRUE)

# 
# over$ycoord
# ## filter all by coordinates from over 
# over_in <- over[over$nutrient != -9999,]
# nrow(over_in)
# over_in$cooxy <- paste0(round(over_in$xcoord), round(over_in$ycoord))
# 
# ## coordinates df_spec
# df_spec_coo <- df_spec
# df_spec_coo$cooxy <- paste0(df_spec_coo$coo_x, df_spec_coo$coo_y)
# 
# ## --> reduced by coordinates still included 
# df_spec_red <- df_spec[df_spec_coo$cooxy %in% over_in$cooxy,]
# df_txt_red  <- df_txt [df_spec_coo$cooxy %in% paste0(df_env_red$coo_x,df_env_red$coo_y),]
# df_env_red  <- df_env [df_spec_coo$cooxy %in% over_in$cooxy,]
# 
# ## educe smp 
# pts@data$xy_coo <- paste0(round(pts@data$xcoord), round(pts@data$ycoord))
# pts_dat <- pts@data
# pts_dat <- pts_dat[pts_dat$xy_coo %in% over_in$cooxy, ] 
# pts_dat <- pts_dat[!duplicated(pts_dat),]
# 
# 
# ## exclude old soil 12 13 14 
# df_env_red <- df_env_red[,c(1:11,15:48)]
# 
# ## include new soil fgk 
# df_env_red$coo_xy <- paste0(df_env_red$coo_x, df_env_red$coo_y)
# df_env_red <- left_join(df_env_red, over_in[,108:124], by =c("coo_xy"="cooxy"))
# ## reduce env again 
# df_env_red  <- df_env_red[df_env_red$coo_xy %in% over_in$cooxy,]
# rm_cols <- c("coo_xy","int_split.y", "int.y" ,"int_redifi" ,"int_testin", "int_paper.y",  "xcoord",  "ycoord")
# df_env_red <- df_env_red[,!(colnames(df_env_red) %in% rm_cols), drop =FALSE]
# df_env_red <- df_env_red[!duplicated(df_env_red),]
# df_txt_red <- df_txt_red[!duplicated(df_txt_red),]
# df_spec_red <- df_spec_red[!duplicated(df_spec_red),]


## reduce by df_env
df_spec_red <- df_spec_red[paste0(df_spec_red$coo_x,df_spec_red$coo_y) %in% paste0(df_env_red$coo_x,df_env_red$coo_y),]
df_spec_red <- df_spec[paste0(df_spec$coo_x,df_spec$coo_y) %in% paste0(df_env_red$coo_x,df_env_red$coo_y),]
df_spec_red <- df_spec_red[!duplicated(df_spec_red),]

## reduce txt 
# df_spec_red <- df_spec_red[paste0(df_spec_red$coo_x,df_spec_red$coo_y) %in% paste0(df_env_red$coo_x,df_env_red$coo_y),]
df_txt_red <- df_txt[paste0(df_txt$coo_x,df_txt$coo_y) %in% paste0(df_env_red$coo_x,df_env_red$coo_y),]
df_txt_red <- df_txt_red[!duplicated(df_txt_red),]

df_txt_2019_red <- df_txt_2019[paste0(df_txt_2019$coo_x,df_txt_2019$coo_y) %in% paste0(df_env_red$coo_x,df_env_red$coo_y),]
df_txt_2019_red <- df_txt_2019_red[!duplicated(df_txt_2019_red),]


nrow(df_spec_red)
nrow(df_txt_red)
nrow(df_env_red)

## check if all have same order 


## write new dfs 
write.table(df_spec_red, "A:/04_Sampling/2021-02-05_ts_review/df_spec_reduced.csv", sep =";",dec=".", col.names = TRUE)
write.table(df_txt_red, "A:/04_Sampling/2021-02-05_ts_review/df_txt_reduced.csv", sep =";",dec=".", col.names = TRUE)
write.table(df_env_red, "A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_env_red.csv", sep =";",dec=".", col.names = TRUE)

write.table(df_txt_2019_red, "A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_txt_2019_red.csv", sep =";",dec=".", col.names = TRUE)

# nrow(over_in)
# pt_xy <- as.data.frame(cbind(pts@data$xcoord, pts@data$ycoord))
# pt_xy$cooxy <- paste0(pt_xy$V1, pt_xy$V2)

#################################################################################
## sample stats 


### tally how many sample points per split 
pts_dat[pts_dat$int_split == 111,]$int_paper <- 1
pts_dat[pts_dat$int_split == 111,]$int <- 1
pts_dat[pts_dat$int_split == 112,]$int <- 1
pts_dat[pts_dat$int_split == 113,]$int <- 1
pts_dat[pts_dat$int_split == 122,]$int <- 1
pts_dat[pts_dat$int_split == 133,]$int <- 1
pts_dat[pts_dat$int_split == 123,]$int <- 1

pts_dat_tally <- pts_dat%>%
  group_by(int_split, int_paper,int) %>%
  tally()
pts_dat_tally <- pts_dat_tally[order(pts_dat_tally$int_split),]
pts_dat_tally <- as.data.frame(pts_dat_tally)

write.table(pts_dat_tally, "A:/04_Sampling/00_SamplingPoints/2021_with_new_interpolation/smp_points_included_in_paper_stats_per_split_with_old_int.csv", sep =";", dec=".", col.names = TRUE)

## only int paper

pts_dat_tally <- pts_dat%>%
  group_by(int_paper) %>%
  tally()

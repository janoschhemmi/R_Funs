




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


## -----------------------------------------------------------------------------
## load data 


data.path <- 'A:/04_Sampling/2021-02-05_ts_review/'
setwd(data.path)

df_features <- read.table("df_features_rse_review.csv", sep = ";", dec =".", header = TRUE)


## sub bands 
df_features     <- df_features[, grepl("coo", colnames(df_features))| 
                                 grepl("int", colnames(df_features))|
                                 grepl("BLU", colnames(df_features))|
                                 grepl("GRN", colnames(df_features))|
                                 grepl("RED", colnames(df_features))|
                                 grepl("RE1", colnames(df_features))|
                                 grepl("RE2", colnames(df_features))|
                                 grepl("RE3", colnames(df_features))|
                                 grepl("NIR", colnames(df_features))|
                                 grepl("SW1", colnames(df_features))|
                                 grepl("SW2", colnames(df_features))|
                                 grepl("NDV", colnames(df_features))]
## sub winter (Mar - Nov in)  32_2018 - 86_2018 ;  105_2019 - 159_2019 

# 7 anfang 
# 7 * 146 
colnames(df_features)[1:8]
c <- 1
select <- c(1,2,3,4,5,6,7)
for (b in seq (1:10)) {
  
  print(b)
  for (d in seq ( 1: 146)){
    
    print(d)
    
    if ((d >= 13 & d <= 67) |(d >= 86 & d <= 140)  ){
      
    select <- append(select, (c+7)) # + 2017 + start 
    }
    c <- c + 1
  
    }
}

df_features_sub <- df_features[,select]
colnames(df_features_sub)
write.table(df_features_sub, "df_features_rse_review_winter_sub.csv", sep =";", dec=".", col.names = TRUE)


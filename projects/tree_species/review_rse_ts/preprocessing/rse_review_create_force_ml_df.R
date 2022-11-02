## 
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


from_split_age_to_paper <- function(vector_in, lookup){
  
  ## vector of split ints in 
  ## lookup table with structure: c1:in c2:out 
  df_in <- as.data.frame(vector_in)
  df_in$vector_in <- as.numeric(as.character(df_in$vector_in))
  int_tree_species_split <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/old/00_area_stats_int_split_paper_update.csv", header=TRUE, sep = ";", dec=".")
  tt_sub <- int_tree_species_split[,c("Baumart_int_age","Baumart_int_Paper")]
  tt_sub <- tt_sub[!duplicated(tt_sub),]
  col_1 <- colnames(df_in)[1]
  colnames(tt_sub)
  df_ing <- left_join(df_in, tt_sub, by =c("vector_in"="Baumart_int_age"))
  return(df_ing[,2])
  
}

## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

df_features <- read.table("A:/workspace/tree_species/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_spec_reduced.csv", 
                          sep = ";", dec = ".", header = TRUE)
df_env     <- read.table("A:workspace/tree_species/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_env_red.csv", 
                         sep = ";", dec = ".", header = TRUE)
df_txt     <- read.table("A:workspace/tree_species//04_Sampling/2021-02-05_ts_review/reduced_fgk/df_txt_reduced.csv", 
                         sep = ";", dec = ".", header = TRUE)

## --------------------------------------------------------------------------ää
## int age 

## -----------------------------------------------------------------------------
## int split in int split age for paper test 
int_split_age <- read.table("P:/workspace/treespecies/_review/tables/ts_int_split_paper.csv", 
                            sep =";", dec=".", header = TRUE)
int_split_age <- int_split_age[,colnames(int_split_age) %in% c("Baumart_int_age_density", "Baumart_int_age")] 
df_festures_pre <- df_features[,1:7]
df_festures_pre <- left_join(df_festures_pre, int_split_age, by = c("int_split" = "Baumart_int_age_density"))
df_features_int_age <- cbind(df_festures_pre,df_features[,8:ncol(df_features)] )


## load model 
model_spec_2018_2019 <- readRDS("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2018_2019/550/04_model_spec_550_env_txt.rds")
## alle bands .. brin in band layer order 
for (t in seq(1:10)){
  print(t)
  
  if ( t < 10) {
    
  band_store <- paste0("Band","0",t)
  if ( t == 1) {
      bands <- band_store
    } else {
      bands <- append(bands, band_store)
    }
  
  } else {
    band_store <- paste0("Band",t)
    bands <- append(bands, band_store)
  }
  }
levels(bands) <- bands 


imp_550 <- as.data.frame(importance(model_spec_2018_2019))
# exclu <- row.names(imp_550)[551:594]
imp_550 <- imp_550[!rownames(imp_550) %in% exclu,]
## exclude environmental 
             
imp_550       <- imp_550[ order(imp_550$MeanDecreaseGini, decreasing = TRUE), ]
imp_550_names <- row.names(imp_550)# [1:550]
df_550  <- df_features[,colnames(df_features) %in% imp_550_names]
colnames(df_txt)
colnames(df_550)
responce <- as.data.frame(df_features_int_age[,8])
df_550 <- cbind(df_550, df_txt[,9:78])

## sort df 550 to band order 



## ----------------------------------------------------------------------------
### write for force ml 
write.table(df_550, "A:/workspace/tree_species/_ts_review/force_ml/550_features_txt/features.csv", sep = " ",
            dec = ".", col.names = FALSE, row.names = FALSE)
write.table(responce, "A:/workspace/tree_species/_ts_review/force_ml/550_features_txt/responce.csv", sep = " ",
            dec = ".", col.names = FALSE, row.names = FALSE)

### for each band get band numbers used 

for (band in bands) {
  
  print(band)
  ## subset 
  used <- c()
  used <- imp_550_names[grepl(band,imp_550_names)]
  print(length(used))
 
  if (length(used) > 0){
    splitted <- ( strsplit(used, "_"))
    splitted <- (do.call(rbind, splitted))
    splitted <- as.data.frame(splitted)
    
    number_bands <- splitted[,4]
    
    ## add 2017 offset to 2018 
    number_bands <- as.numeric(number_bands)
    number_bands <- number_bands + 19 
    
    name <- splitted[1,2]
    
    } else {
    number_bands <- c()
  
    }
   
  ## merge for forece output 
  print(paste0(cat(sort.int(as.numeric(number_bands)))))
  sorted <- (sort.int(as.numeric(number_bands)))
  
  row_ <- c(band, name, sorted)
  
  if(band == "Band01"){
    all_out <- as.data.frame(matrix(row_, ncol = length(row_)))
  } else {
     #<- plyr::cbind.fill(as.data.frame(all_out), as.data.frame(row_))
    n_1 <- ncol(all_out) 
    n_2 <- length(row_)
    
    if ( (n_1 > n_2) == TRUE) { ## more Na s to row
      
       row_ <- c(row_, rep (NA, (n_1 - n_2)))
    } 
    
    print("here")
    if ( n_1 < n_2) { ## more Na s to store df
      print("morte NA to df out")
      all_out <- cbind(all_out, as.data.frame(matrix(nrow = nrow(all_out), ncol = (n_2-n_1), NA)))
    }
    
    ## merge
    print("merging")
    all_out <- as.data.frame(rbind(as.data.frame(all_out),(row_)))
  }
}

all_out
write.table(all_out, "A:/workspace/treespecies/_review/force_ml/550_features/bands_for_ml.csv", sep = " ", dec = ".", col.names = FALSE, row.names = FALSE)


# ioio <- read.table("P:/workspace/jan/fire_detection/break_detection/h40_tiled_3/X0068_Y0045_output_extracted_data_h40.csv", sep = ";", dec = ".", header = TRUE)

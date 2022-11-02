
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
df_txt_2019     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_txt_2019_red.csv", 
                         sep = ";", dec = ".", header = TRUE)



# ------------------------------------------------------------------------------
## 
## -----------------------------------------------------------------------------
## Function 

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
aa_split <- function(model, vector_ref_int, vector_ref_split){
  
  
  ## global levels 
  ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir",
                           "Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )
  
  map      <- as.data.frame(as.integer(as.character(model$predicted)))
  ref      <- as.data.frame(as.integer(vector_ref_int))
  strats   <- vector_ref_split
  
  ## get map from split to ts ints 
  map_int <- as.data.frame(from_split_age_to_paper(map[,1]))
  
  colnames(map) <- "predicted"
  colnames(ref) <- "ref"
  
  ## change reference ints into ts names
  stats_split <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/old/00_area_stats_int_split_paper_update.csv", sep = ";", dec = ".", header = TRUE)
  length(unique(stats_split$Baumart_int_age))
  stats_ts <- stats_split %>% 
    group_by(TreeSpecies) %>% dplyr::select(TreeSpecies, Baumart_int_Paper)
  stats_ts <- as.data.frame(stats_ts[!duplicated(stats_ts),])
  
  freq_all <- stats_split[,c("Baumart_int_age", "TreeSpecies","Baumart_int_Paper")]#,"TreeSpecies")]
  freq_all <- freq_all[freq_all$Baumart_int_age < 999, ]
  freq_all <- freq_all[!duplicated(freq_all$Baumart_int_age),]
  
  ## int --> character .. ts names 
  aa_map    <-  map %>%
    left_join(., freq_all, by = c("predicted"="Baumart_int_age")) %>%
    dplyr::select(TreeSpecies)
  
  aa_ref   <-   ref %>%
    left_join(., stats_ts, by = c("ref"="Baumart_int_Paper")) %>%
    dplyr::select(TreeSpecies)
  
  
  ## set factor levels -- for order 
  ts_names_17 <- factor(ts_names_17_levels, levels = ts_names_17_levels)
  aa_ref$TreeSpecies      <- factor(aa_ref$TreeSpecies, levels = ts_names_17_levels)
  aa_map$TreeSpecies      <- factor(aa_map$TreeSpecies, levels = ts_names_17_levels)
  
  ## area for each strat 
  unique_strats <- sort(unique(strats))
  areas_strats <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/percent_area_split_for_paper.csv", sep =";", dec =".", header = TRUE)
  areas_strats <- as.data.frame(areas_strats %>% group_by(Baumart_int_age_density) %>% summarise(area = percent_area))
  
  ## into factor 
  stratum_levels <- areas_strats$Baumart_int_age_density
  strat <- factor(strats, levels = stratum_levels)
  stats_split_factor <- factor(areas_strats$Baumart_int_age_density, levels = stratum_levels)
  
  levels(aa_ref$TreeSpecies)
  levels(aa_map$TreeSpecies)
  length(levels(strat))
  length(levels(areas_strats$Baumart_int_age_density))
  length(levels(areas_strats$area))
  
  result <-  aa_stratified(strat,               ## strat splits for each smp
                           aa_ref$TreeSpecies,      ## ts int of ref                                     --> Ts_Name
                           aa_map$TreeSpecies,      ## ts int of pre                             --> Prediction as Ts name 
                           areas_strats$Baumart_int_age_density,     ## list of strata ints                            --> seq(1:n(ts)) ## oder nene hier die anzahl von samples
                           areas_strats$area        ## list of strata area           --> ShapeArea FGK
                           
  )
  return(result)  
}

prandomForest <- function(x,y, data=NULL, ntree=500,classwt, 
                          cores=30) {
  .libPaths("S:/BrandSat/02_Code/R/library_3")
  require(randomForest)
  require(doParallel)
  cl <- makeCluster(cores)
  registerDoParallel(cl)  
  clusterEvalQ(cl, .libPaths("S:/BrandSat/02_Code/R/library_3"))
  
  gtree <- ceiling(ntree / cores)
  
  stime <- system.time({
    rfm <- foreach(n_tree=rep(gtree, cores), .combine=randomForest::combine, .multicombine=TRUE, 
                   .packages='randomForest') %dopar% {
                     randomForest(x,y, data=data, ntree=n_tree, classwt = classwt, 
                                  importance = TRUE)
                   }
  })[3]
  message(stime/60.)
  stopCluster(cl)
  #registerDoSEQ()
  return(rfm)
}



## -----------------------------------------------------------------------------
## int split in int split age for paper test 
int_split_age <- read.table("P:/workspace/treespecies/_review/tables/ts_int_split_paper.csv", 
                            sep =";", dec=".", header = TRUE)
int_split_age <- int_split_age[,colnames(int_split_age) %in% c("Baumart_int_age_density", "Baumart_int_age")] 
df_festures_pre <- df_features[,1:7]
df_festures_pre <- left_join(df_festures_pre, int_split_age, by = c("int_split" = "Baumart_int_age_density"))
df_features_int_age <- cbind(df_festures_pre,df_features[,8:ncol(df_features)] )
(unique(df_features_int_age$Baumart_int_age))
# ------------------------------------------------------------------------------
## weights antiproportional to smp frequency age split 
tally_smp <- as.data.frame(df_features_int_age %>%
                             group_by(Baumart_int_age) %>%
                             tally())

tally_smp$anti <- 10 / ((100 * tally_smp$n) / sum(tally_smp$n))
tally_smp <- tally_smp[order(tally_smp$Baumart_int_age, decreasing = FALSE),]
tally_smp
##


## MODELs
#-------------------------------------------------------------------------------
# 001 features 

data.path <- 'P:/workspace/treespecies/_review/model/'
setwd(data.path)

## with old bands 
df_old_bands <- as.data.frame(cbind(df_features_int_age[,1:8],
                                    df_features_int_age[, grepl("GRN", colnames(df_features_int_age)) |
                                                          grepl("RED", colnames(df_features_int_age)) |
                                                          grepl("RE1", colnames(df_features_int_age)) |
                                                          grepl("RE2", colnames(df_features_int_age)) |
                                                          grepl("NIR", colnames(df_features_int_age)) |
                                                          grepl("SW1", colnames(df_features_int_age)) |
                                                          grepl("SW2", colnames(df_features_int_age)) 
                                    ]))


#--------------------------------------------------------------------------------
#  2 Model 2019 
df_2019_new <- as.data.frame(cbind(df_features_int_age[,1:8],
                                   df_features_int_age[, grepl("2019", colnames(df_features_int_age))]))
model_2019_new      <- readRDS("02_model_2019_new_bands.rds")

## get feature importance 
importance <- as.data.frame(importance(model_2019_new))
importance <- importance[order(importance$MeanDecreaseGini, decreasing = TRUE),]
imp_ <- row.names(importances[1:400,])

## subset df 
df_features_400 <- as.data.frame(cbind(df_features_int_age[,1:8], 
                                       df_features[,colnames(df_features) %in% imp_]))

## model 400 
model_400 <- readRDS("P:/workspace/treespecies/_review/model/feature_reduction/04_model_2019_new_bands_reduced_to_400_bands.rds")


# 2 model env 
df_env_ <- as.data.frame(cbind(df_features_int_age[,1:8],  
                                       df_env[,9:length(df_env)]))
df <- df_env
col_int <- 8
model_env <- readRDS("P:/workspace/treespecies/models/01_model_paper/02_01_model_env_age_split.rds")

# 3 model env + spec 
df_spec_400_env <- cbind(df_features_400, df_env[,9:ncol(df_env)]) 
df <- df_spec_400_env
col_int <- 8
model_spec_400_env <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                classwt = tally_smp$anti )
# 4 model env + spec + txt 
df_spec_400_env_txt <- cbind(df_features_400, df_env[,9:ncol(df_env)], df_txt_2019[,9:ncol(df_txt)]) 
df <- df_spec_400_env_txt
col_int <- 8
model_spec_400_env_txt <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                    classwt = tally_smp$anti )

## safe models 
setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2019/")
saveRDS(model_400,"01_model_spec_400_2019.rds")
saveRDS(model_env,"02_model_env_2019.rds")
saveRDS(model_spec_400_env,"03_model_spec_400_env_2019.rds")
saveRDS(model_spec_400_env_txt,"04_model_spec_400_env_txt_2019.rds")
# do aa somewhere else


##############################################################################

## 475 
setwd("P:/workspace/treespecies/_review/model/")
model_spec_split <- readRDS("P:/workspace/treespecies/_review/model/01_model_spec_all_bands.rds")
importances <- as.data.frame(importance(model_spec_split))
importances <- importance[order(importance$MeanDecreaseGini, decreasing = TRUE),]

## --> go with 475 bands 
imp_ <- row.names(importances[1:475,])
## subset df 
df_features_475 <- as.data.frame(cbind(df_old_bands[,1:8], 
                                       df_features[,colnames(df_features) %in% imp_]))

## 1 model df 475 feat
df <- df_features_475
col_int <- 8
model_spec_475 <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                classwt = tally_smp$anti )

# 2 model env 
df_env_ <- as.data.frame(cbind(df_old_bands[,1:8], 
                               df_env[,9:length(df_env)]))
df <- df_env
col_int <- 8
model_env <- readRDS("P:/workspace/treespecies/models/01_model_paper/02_01_model_env_age_split.rds")

# 3 model env + spec 
df_spec_475_env <- cbind(df_features_475, df_env[,9:ncol(df_env)]) 
df <- df_spec_475_env
col_int <- 8
model_spec_475_env <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                    classwt = tally_smp$anti )
# 4 model env + spec + txt 
df_spec_475_env_txt <- cbind(df_features_475, df_env[,9:ncol(df_env)], df_txt[,9:ncol(df_txt)]) 
df <- df_spec_475_env_txt
col_int <- 8
model_spec_475_env_txt <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                        classwt = tally_smp$anti )

## safe models 
setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2018_2019/")
saveRDS(model_spec_475,"01_model_spec_475.rds")
saveRDS(model_env,"02_model_env.rds")
saveRDS(model_spec_475_env,"03_model_spec_475_env.rds")
saveRDS(model_spec_475_env_txt,"04_model_spec_475_env_txt.rds")


## ------------------------------------------------------------------------- ##
### model int  old bands 

## weights antiproportional to smp frequency age split 
tally_smp_int <- as.data.frame(df_features %>%
                             group_by(int_paper) %>%
                             tally())

tally_smp_int$anti <- 10 / ((100 * tally_smp_int$n) / sum(tally_smp_int$n))
tally_smp_int <- tally_smp_int[order(tally_smp_int$Baumart_int_age, decreasing = FALSE),]
tally_smp_int

## subset df 
df_old_bands_sub <- df_old_bands[,-8]


## 1 model df 475 feat
df <- df_old_bands_sub
col_int <- 7
model_spec_old_int <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                classwt = tally_smp_int$anti )

# 2 model env 
df_env_ <- as.data.frame(cbind(df_old_bands[,1:7], 
                               df_env[,9:length(df_env)]))
df <- df_env_
col_int <- 7
model_env_int <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                        classwt = tally_smp_int$anti )

# model_env <- readRDS("P:/workspace/treespecies/models/01_model_paper/02_01_model_env_age_split.rds")

# 3 model env + spec 
df_spec_old_int_env <- cbind(df_old_bands_sub, df_env[,9:ncol(df_env)]) 
df <- df_spec_old_int_env
col_int <- 7
model_spec_old_int_env <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                    classwt = tally_smp_int$anti )
# 4 model env + spec + txt 
df_spec_old_int_env_txt <- cbind(df_old_bands_sub, df_env[,9:ncol(df_env)], df_txt[,9:ncol(df_txt)]) 
df <- df_spec_old_int_env_txt
col_int <- 7
model_spec_old_int_env_txt <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                        classwt = tally_smp_int$anti )

## safe models 
setwd("P:/workspace/treespecies/_review/model/old_bands_int/")
saveRDS(model_spec_old_int,"01_model_spec_int.rds")
saveRDS(model_env_int,         "02_model_env_int.rds")
saveRDS(model_spec_old_int_env,"03_model_spec_int_env.rds")
saveRDS(model_spec_old_int_env_txt,"04_model_spec_old_bands_env_txt.rds")







# aa_model_new_spec[["accuracy"]]
# aa_model_old_spec[["accuracy"]]
# 
# aa_model_2018_old[["accuracy"]]
# aa_model_2019_old[["accuracy"]]
# 
# 
# 
# 
# 
# 
# ## get 150 --------------------------------------------------------------------
# imp_150 <- row.names(importances[1:150,])
# ## subset df
# df_features_150 <- as.data.frame(cbind(df_old_bands[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_150]))
# 
# 
# ## model 150
# col_int <- 8
# df      <- df_features_150
# model_spec_150 <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]))
# model_spec_150
# 
# ## AA
# vector_ref_int   <-  df_features$int_paper
# vector_ref_split <-  df[,col_int]
# ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )
# model            <- model_spec_150
# 
# ## AA
# result_01_split_model_150_bands <- aa_split(model,df_features$int_paper, df_features$int_split)
# 
# #------------------------------------------------
# ## get 200 -------------------------------------------------------------------
# imp_200 <- row.names(importances[1:200,])
# ## subset df
# df_features_200 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_200]))
# ## model 200
# col_int <- 8
# df      <- df_features_200
# model_spec_200 <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]))
# result_01_split_model_200_bands <- aa_split(model_spec_200,df_features$int_paper, df_features$int_split)
# 
# # -----------------------------------------------
# ## get 250 -------------------------------------------------------------------
# imp_250 <- row.names(importances[1:250,])
# ## subset df
# df_features_250 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_250]))
# ## model 250
# col_int <- 8
# df      <- df_features_250
# 
# model_spec_250 <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]))
# result_01_split_model_250_bands <- aa_split(model_spec_250,df_features$int_paper, df_features$int_split)
# 
# # -----------------------------------------------
# ## get 100 -------------------------------------------------------------------
# imp_100 <- row.names(importances[1:100,])
# ## subset df
# df_features_100 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_100]))
# ## model 250
# col_int <- 8
# df      <- df_features_100
# 
# model_spec_100 <- prandomForest(x = df[,(col_int+1):ncol(df)],  y= as.factor(df[,col_int]))
# result_01_split_model_100_bands <- aa_split(model_spec_100,df_features$int_paper, df_features$int_split)
# 
# ## get 175 -------------------------------------------------------------------
# imp_175 <- row.names(importances[1:175,])
# ## subset df
# df_features_175 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_175]))
# ## model 250
# col_int <- 8
# df      <- df_features_175
# 
# model_spec_175 <- prandomForest(x = df[,(col_int+1):ncol(df)],  y= as.factor(df[,col_int]))
# result_01_split_model_175_bands <- aa_split(model_spec_175,df_features$int_paper, df_features$int_split)
# 
# ## get 225 -------------------------------------------------------------------
# imp_225 <- row.names(importances[1:225,])
# ## subset df
# df_features_225 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_225]))
# ## model 250
# col_int <- 8
# df      <- df_features_225
# 
# ## get 275 -------------------------------------------------------------------
# imp_275 <- row.names(importances[1:275,])
# ## subset df
# df_features_275 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_275]))
# ## model 250
# col_int <- 8
# df      <- df_features_275
# 
# model_spec_275 <- prandomForest(x = df[,(col_int+1):ncol(df)],  y= as.factor(df[,col_int]))
# 
# ## get 300 -------------------------------------------------------------------
# imp_300 <- row.names(importances[1:300,])
# ## subset df
# df_features_300 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_300]))
# ## model 250
# col_int <- 8
# df      <- df_features_300
# 
# model_spec_300 <- prandomForest(x = df[,(col_int+1):ncol(df)],  y= as.factor(df[,col_int]))
# 
# ## get 325 -------------------------------------------------------------------
# imp_325 <- row.names(importances[1:325,])
# ## subset df
# df_features_325 <- as.data.frame(cbind(df_features_int_age[,1:8],
#                                        df_features[,colnames(df_features) %in% imp_325]))
# ## model 325
# col_int <- 8
# df      <- df_features_325
# 
# model_spec_325 <- prandomForest(x = df[,(col_int+1):ncol(df)],  y= as.factor(df[,col_int]))
# 
# 
# result_01_split_model_225_bands <- aa_split(model_spec_225,df_features$int_paper, df_features$int_split)
# result_01_split_model_275_bands <- aa_split(model_spec_275,df_features$int_paper, df_features$int_split)
# result_01_split_model_300_bands <- aa_split(model_spec_300,df_features$int_paper, df_features$int_split)
# result_01_split_model_325_bands <- aa_split(model_spec_325,df_features$int_paper, df_features$int_split)
# 
# 
# ##
# aa_n_feature <- c(100,150,175,200,225,250,275)
# aa_list <- c(
# result_01_split_model_100_bands[["accuracy"]][1],
# result_01_split_model_150_bands[["accuracy"]][1] ,
# result_01_split_model_175_bands[["accuracy"]][1] ,
# result_01_split_model_200_bands[["accuracy"]][1] ,
# result_01_split_model_225_bands[["accuracy"]][1],
# result_01_split_model_250_bands[["accuracy"]][1] ,
# result_01_split_model_275_bands[["accuracy"]][1] )
# (result_01_split_model_275_bands)
# s <- 1
# for( aa in aa_list){
# 
#   accu <- aa
#   print(accu)
#   accu_oa <- accu[1]
#   n_f <- aa_n_feature[s]
#   print(accu_oa)
# 
#   if(s == 1){
#     store <- c(accu_oa, n_f)
#     print("first")
#   } else {
#     row <- c(accu_oa, n_f)
#     store <- rbind(store, row)
#   }
#   s <- s + 1
# }
# store <- as.data.frame(store)
# plot(x=store$V2, y=store$V1)
# 
# 

store_2018_2019_new_all
write.table(store_2018_2019_new_all, "P:/workspace/treespecies/_review/model/feature_reduction_env_txt/feature_reduction_2018_2019_oa_models.csv",
            sep = ":", dec = ".", col.names = TRUE, row.names = FALSE)
df_tst <- data.frame(error = tst_2018_2019$error.cv,nvar = tst_2018_2019_n_var)
write.table(df_tst,"P:/workspace/treespecies/_review/model/feature_reduction_env_txt/feature_reduction_rfcv_2018_2019.csv",
            sep = ";", dec = ".", col.names = TRUE, row.names = FALSE)           
           
           
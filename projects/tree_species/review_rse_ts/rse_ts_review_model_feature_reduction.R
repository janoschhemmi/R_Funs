
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

nrow(df_features)
nrow(df_env)
nrow(df_txt)

colnames(df_features)[1:9]
## --> same --> good 


data.path <- 'A:/04_Sampling/2021-02-05_S2_new_interpolation/'
setwd(data.path)

## check for ts paper 
length(unique(df_features$int_split))

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

df <- df_old_bands
colnames(df)[1:10]

col_train <- 8
col_int   <- 8

y= as.factor(df[,col_int])
levels(tally_smp$anti) <- levels(y)

########## train model with bands of paper 
# model_spec_old_bands <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
#                                       classwt = tally_smp$anti )

# model_spec_old_bands      <-  randomForest(y=as.factor(df[,col_train]),x=df[,(col_int+1):ncol(df)],
#                                      ntree=500, importance = T, na.action =na.omi,
#                                      classwt = (1000/freq_17_per)) #, sampsize = sampsize)
# saveRDS(model_spec_old_bands   , paste0("01_model_spec_old_bands", ".rds"))
# aa_model_old_spec <- aa_split(model_spec_old_bands,df_features$int_paper, df_features$int_split) 
# 
# 
# #------------------------------------------------
# ### train model with bands suggested + NDVI 
# df <- df_features_int_age
# colnames(df)[1:10]
# 
# col_train <- 8
# col_int   <- 8
# 
# model_spec <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
#                                       classwt = tally_smp$anti )
# 
# saveRDS(model_spec   , paste0("01_model_spec_all_bands", ".rds"))
# aa_model_new_spec <- aa_split(model_spec,df_features$int_paper, df_features$int_split) 
# 
# 
# # ### load 
# # model_spec <- readRDS("01_model_spec_all_features.rds")
# # model_spec_old_bands  <- readRDS("01_model_spec_old_bands.rds")
# # length(unique(model_spec$predicted))
# 
# #--------------------------------------------------------------------------------
# # Feature Reduction 
# 
# 
# # Feature reduction
# library(randomForest)
# 
# tst <- rfcv(trainx = df[,(col_int+1):ncol(df)], trainy = df[,col_train], 
#             scale = "log", step=0.8, cv.fold = 3)
# tst_n_var <- tst$n.var
# tst_error_cv <- tst$error.cv
# tst$predicted
# 
# par(mfrow=c(1,1))
# plot(tst_error_cv, x= names(tst_error_cv), xlab = "number of variables", ylab = "error rate")
# 
# ## --> go with 150 
# ## --> go with 200
# ## --> go with 250 
# 
# 
# ## mean decrease GINI
# importances <- as.data.frame(importance(model_spec))
# importances <- importances[order(importances$MeanDecreaseGini, decreasing = TRUE),]
# 
# 
# ## --------------------------------------------------------------------------
# ## old and new bands with splitted years 
# 
# #--------------------------------------------------------------------------------
# #  1 Model 2018 
# 
# df_2018_old <- as.data.frame(cbind(df_old_bands[,1:8],
#                                df_old_bands[, grepl("2018", colnames(df_old_bands))]))
# 
# col_int <- 8 
# df      <- df_2018_old
# 
# model_2018_old    <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
#                                    classwt = tally_smp$anti )
# saveRDS(model_2018_old   , paste0("02_model_2018_old_bands", ".rds"))
# 
# aa_model_2018_old <- aa_split(model_2018_old,df_features$int_paper, df_features$int_split) 
# 
# #--------------------------------------------------------------------------------
# #  1 Model 2018 
# df_2019_old <- as.data.frame(cbind(df_old_bands[,1:8],
#                                df_old_bands[, grepl("2019", colnames(df_old_bands))]))
# 
# 
# col_int <- 8 
# df      <- df_2019_old
# 
# model_2019_old   <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
#                                   classwt = tally_smp$anti )
# saveRDS(model_2019_old   , paste0("02_model_2019_old_bands", ".rds"))
# 
# aa_model_2018_old
# aa_model_2019_old <- aa_split(model_2019_old,df_features$int_paper, df_features$int_split) 
# 
# ## -----------------------------------------------------------------------------
# ## model years with new bands
# 
# #  1 Model 2018 
# 
# df_2018_new <- as.data.frame(cbind(df_old_bands[,1:8],
#                                    df_features_int_age[, grepl("2018", colnames(df_features_int_age))]))
# 
# col_int <- 8 
# df      <- df_2018_new
# 
# model_2018_new    <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
#                                    classwt = tally_smp$anti )
# saveRDS(model_2018_new   , paste0("02_model_2018_new_bands", ".rds"))
model_2018_new <- readRDS("02_model_2018_new_bands.rds")

# aa_model_2018_new <- aa_split(model_2018_new,df_features$int_paper, df_features$int_split) 

#--------------------------------------------------------------------------------
#  2 Model 2019 
df_2019_new <- as.data.frame(cbind(df_old_bands[,1:8],
                                   df_features_int_age[, grepl("2019", colnames(df_features_int_age))]))

# col_int <- 8 
# df      <- df_2019_new
# 
# model_2019_new    <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
#                                    classwt = tally_smp$anti )
# saveRDS(model_2019_new   , paste0("02_model_2019_new_bands", ".rds"))
model_2019_new <- readRDS("old/02_model_2019_new_bands.rds")
model_2018_2019_new <- readRDS("01_model_spec_all_bands.rds")
# aa_model_2019_new <- aa_split(model_2019_new,df_features$int_paper, df_features$int_split) 

aa_model_2019_new[["accuracy"]]
aa_model_2018_new[["accuracy"]]

# ---------------------------------------------
##  Loop over some number of variables 
variable_length_list <- c(25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525)
# variable_length_list <- c(550,575,600,650,700,750,800,850,900,950,1000,1050,1100)

## mean decrease GINI
importances <- as.data.frame(importance(model_2018_2019_new))
importances <- importances[order(importances$MeanDecreaseGini, decreasing = TRUE),]
# saveRDS(model_2018_new, "P:/workspace/treespecies/_review/model/")

z <- 1
for (length in variable_length_list){
  print(length)
  imp_ <- row.names(importances[1:length,])
  ## subset df 
  df_features_imp <- as.data.frame(cbind(df_old_bands[,1:8], 
                                         df_features[,colnames(df_features) %in% imp_]))
  

  ## model 
  col_int <- 8 
  df      <- df_features_imp
  model_spec_imp <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                 classwt = tally_smp$anti )
  saveRDS(model_spec_imp   , paste0("feature_reduction/04_model_2018_2019_new_bands_reduced_to_",length ,"_bands.rds"))
  # model_spec_imp <- readRDS(paste0("03_model_2018_old_bands_reduced_to_",length ,"_bands.rds"))
  # 
  ## aa 
  result_imp_aa <- aa_split(model_spec_imp,df_features$int_paper, df_features$int_split) 
  oa_imp    <- result_imp_aa[["accuracy"]][1]
  error_imp <- result_imp_aa[["accuracy"]][2]
 
  accu_oa <- oa_imp[1]
  n_f <- length
  print(accu_oa)
   
  if(z == 1){
    store_2018_2019_new <- c(accu_oa,error_imp, n_f)
    print("first")
  } else {
    row <- c(accu_oa, error_imp,  n_f)
    store_2018_2019_new <- rbind(store_2018_2019_new, row)
  }
  z <- z + 1
  
}

## run weekend 2018 
df  <- (df_2019_new)[]
col_int <- 8 

## join two durchgaenge zusammen
store_2018_2019_new_all <- rbind(store_2018_2019_new,store_2018_2019_new_1)
# tst_2018 <- rfcv(trainx = df[,(col_int+1):ncol(df)], trainy = as.factor(df[,col_train]), 
#             scale = "log", step=0.6, cv.fold = 3)
# 
# tst_2018_n_var    <- tst_2018$n.var
# tst_2018_error_cv <- tst_2018$error.cv
# plot_rfcv_2018 <- plot(y= tst_2018_error_cv, x= tst_2018_n_var, type = "line", 
#                        ylab = "error rate", xlab = "n var", main = "age split model all bands 2018")
# 
# 
# 
# 
# ## run weekend 2018 
# df  <- (df_features)[]
# col_int <- 8 
# # tst_2019 <- rfcv(trainx = df[,(col_int+1):ncol(df)], trainy = as.factor(df[,col_train]), 
# #             scale = "log", step=0.6, cv.fold = 3)
# tst_2019_n_var    <- tst_2019$n.var
# tst_2019_error_cv <- tst_2019$error.cv
# plot_rfcv_2019 <- plot(y= tst_2019_error_cv, x= tst_2019_n_var, type = "line", 
#                        ylab = "error rate", xlab = "n var", main = "age split model all bands 2019")

#################### 2018 / 2019 
df  <- (df_features_int_age)[]
col_int <- 8 
tst_2018_2019 <- rfcv(trainx = df[,(col_int+1):ncol(df)], trainy = as.factor(df[,col_train]), 
            scale = "log", step=0.6, cv.fold = 3)
tst_2018_2019_n_var    <- tst_2018_2019$n.var
tst_2018_2019_error_cv <- tst_2018_2019$error.cv
plot_rfcv_2018_2019 <- plot(y= tst_2018_2019_error_cv, x= tst_2018_2019_n_var, #type = "po", 
                       ylab = "error rate", xlab = "n var", main = "age split model all bands 2019")

# 
# 
# ####################
# #store_store <- store
# store_all <- as.data.frame(rbind(store_store, store))
# store_df <- as.data.frame(store)
# store_2019 <- as.data.frame(store_2019)

#### preprocessing for error plot feature reduction 
store_2018_new <- as.data.frame(store_2018_new)
store_2018_new$min_y <- store_2018_new$V1 - store_2018_new$V2
store_2018_new$max_y <- store_2018_new$V1 + store_2018_new$V2

#### preprocessing for error plot feature reduction 
store_2019_new <- as.data.frame(store_2019_new)
store_2019_new$min_y <- store_2019_new$V1 - store_2019_new$V2
store_2019_new$max_y <- store_2019_new$V1 + store_2019_new$V2

#### preprocessing for error plot feature reduction 
store_2018_2019_new_all
store_2018_2019_new_all <- as.data.frame(store_2018_2019_new_all)
store_2018_2019_new_all$min_y <- store_2018_2019_new_all$V1 - store_2018_2019_new_all$V2
store_2018_2019_new_all$max_y <- store_2018_2019_new_all$V1 + store_2018_2019_new_all$V2


# 
library(ggplot2)
plot_2018_all <- ggplot(store_2018_new, aes(x = V3, y = V1)) +  geom_point() +
  xlab ("n_features") +
        ylab("oa_accuracy") +
  ggtitle("feature reduction all bands, 2018") +
  ylim(0.93,0.960) +
  geom_linerange(mapping = aes(x = store_2018_new$V3, ymin = store_2018_new$min_y,
                               ymax = store_2018_new$max_y)) 

plot_2018_all

plot_2019_all <- ggplot(store_2019_new, aes(x = V3, y = V1)) +  geom_point() +
  xlab ("n_features") +
  ylab("oa_accuracy") +
  ggtitle("feature reduction all bands, 2018") +
  ylim(0.93,0.960) +
  geom_linerange(mapping = aes(x = store_2019_new$V3, ymin = store_2019_new$min_y,
                               ymax = store_2019_new$max_y)) 

plot_2019_all


################################################################################
plot_2018_2019_all <- ggplot(store_2018_2019_new_all, aes(x = V3, y = V1)) +  geom_point() +
  xlab ("n_features") +
  ylab("oa_accuracy") +
  ggtitle("feature reduction all bands, 2018_2019") +
  ylim(0.93,0.965) +
  geom_linerange(mapping = aes(x = V3, ymin = min_y,
                               ymax = max_y)) 

plot_2018_2019_all

## --> go with 150 bands 
imp_ <- row.names(importances[1:150,])
## subset df 
df_features_150 <- as.data.frame(cbind(df_old_bands[,1:8], 
                                       df_features[,colnames(df_features) %in% imp_]))

## 1 model df 150 feat
df <- df_features_150
col_int <- 8
model_spec_150 <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                    classwt = tally_smp$anti )

# 2 model env 
df_env_ <- as.data.frame(cbind(df_old_bands[,1:8], 
                                       df_env[,9:length(df_env)]))
df <- df_env
col_int <- 8
model_env <- readRDS("P:/workspace/treespecies/models/01_model_paper/02_01_model_env_age_split.rds")

# 3 model env + spec 
df_spec_150_env <- cbind(df_features_150, df_env[,9:ncol(df_env)]) 
df <- df_spec_150_env
col_int <- 8
model_spec_150_env <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                classwt = tally_smp$anti )
# 4 model env + spec + txt 
df_spec_150_env_txt <- cbind(df_features_150, df_env[,9:ncol(df_env)], df_txt[,9:ncol(df_txt)]) 
df <- df_spec_150_env_txt
col_int <- 8
model_spec_150_env_txt <- prandomForest(x = df[,(col_int+1):ncol(df)], y= as.factor(df[,col_int]),
                                    classwt = tally_smp$anti )

## safe models 
setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/")
saveRDS(model_spec_150,"01_model_spec_150.rds")
saveRDS(model_env,"02_model_env.rds")
saveRDS(model_spec_150_env,"03_model_spec_150_env.rds")
saveRDS(model_spec_150_env_txt,"04_model_spec_150_env_txt.rds")
# do aa somewhere else


##############################################################################

## 475 


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
setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/")
saveRDS(model_spec_475,"01_model_spec_475.rds")
saveRDS(model_env,"02_model_env.rds")
saveRDS(model_spec_475_env,"03_model_spec_475_env.rds")
saveRDS(model_spec_475_env_txt,"04_model_spec_475_env_txt.rds")

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

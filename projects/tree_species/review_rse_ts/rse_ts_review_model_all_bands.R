
### Model with all features 

### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_3")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

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

## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

df_features <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_spec_reduced.csv", 
                         sep = ";", dec = ".", header = TRUE)
df_env     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_env_red.csv", 
                         sep = ";", dec = ".", header = TRUE)
df_txt     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_txt_reduced.csv", 
                         sep = ";", dec = ".", header = TRUE)

df_features[,1:9]
"Sitka spruce"
df_features_tally <- df_txt %>%
  group_by(int_paper) %>%
  tally()

nrow(df_features)
nrow(df_env)
nrow(df_txt)

colnames(df_features)[1:9]
## --> same --> good 


data.path <- 'A:/04_Sampling/2021-02-05_S2_new_interpolation/'
setwd(data.path)

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
  
  result <-  aa_stratified(strat,               ## strat splits for each smp
                           aa_ref$TreeSpecies,      ## ts int of ref                                     --> Ts_Name
                           aa_map$TreeSpecies,      ## ts int of pre                             --> Prediction as Ts name 
                           areas_strats$Baumart_int_age_density,     ## list of strata ints                            --> seq(1:n(ts)) ## oder nene hier die anzahl von samples
                           areas_strats$area        ## list of strata area           --> ShapeArea FGK
                           
  )
  return(result)  
}


# ------------------------------------------------------------------------------
## weights antiproportional to smp frequency age split 
tally_smp <- as.data.frame(df_features %>%
                             group_by(Baumart_int_age) %>%
                             tally())



tally_smp$anti <- 10 / ((100 * tally_smp$n) / sum(tally_smp$n))
tally_smp <- tally_smp[order(tally_smp$Baumart_int_age, decreasing = FALSE),]

model_spec_split


## -----------------------------------------------------------------------------
## int split in int split age for paper test 
int_split_age <- read.table("P:/workspace/treespecies/_review/tables/ts_int_split_paper.csv", 
                            sep =";", dec=".", header = TRUE)
int_split_age <- int_split_age[,colnames(int_split_age) %in% c("Baumart_int_age_density", "Baumart_int_age")] 
df_festures_pre <- df_features[,1:7]
df_festures_pre <- left_join(df_festures_pre, int_split_age, by = c("int_split" = "Baumart_int_age_density"))
df_features_int_age <- cbind(df_festures_pre,df_features[,8:ncol(df_features)] )

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

df <- df_features_int_age
colnames(df)[1:10]

col_train <- 8
col_int   <- 8
model_spec      <-  randomForest(y=as.factor(df[,col_train]),x=df[,(col_int+1):ncol(df)],
                                     ntree=500, importance = T, na.action =na.omi,
                                     classwt = (tally_smp$anti)) #, sampsize = sampsize)
saveRDS(model_spec   , paste0("01_model_spec_all_features", ".rds"))


## AA join together 
# ## class weights for model from areas of area fgk
classes_model <- as.numeric(model_spec$classes)
classes_df    <- unique(df_env$int_split.x)
setdiff(classes_model, classes_df)
setdiff(classes_df, classes_model)

areas_split <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/percent_area_split_for_paper.csv", sep =";", dec =".", header = TRUE)


## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Function AA for split models 
vector_ref_int   <- df_features$int_paper
vector_ref_split <- df_features$int_split
model            <- model_spec
ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )

## AA
result_01_split_model_spec_all_bands <- aa_split(model_spec,df_features$int_paper, df_features$int_split) 

##

## ----------------------------------------------------------------------------

## feature importance 
importances <- as.data.frame(importance(model_spec))
varImpPlot(model_spec, n.var = 1000 )
??varImpPlot

## order
importances <- importances[ order(importances$MeanDecreaseGini, decreasing = TRUE), ]
plot(x = importances$MeanDecreaseGini, y= importances$MeanDecreaseAccuracy)

## calc percent
summe <- sum(importances$MeanDecreaseGini)

importances$percent_gini <- 0 
importances$id <- seq_along(importances$MeanDecreaseGini)
for (i in seq(1:nrow(importances))) {
  if ( i == 1){ 
  value <- importances$MeanDecreaseGini[1]
  } else {
    value <- value + importances$MeanDecreaseGini[i]
  }
  ## percent 
  importances[i,36] <- value * 100 / summe
}

## plot
plot(x=importances$id, y = importances$percent_gini)

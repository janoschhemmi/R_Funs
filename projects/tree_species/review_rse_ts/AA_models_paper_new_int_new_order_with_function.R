################################################################################




## ---------------------------------------------------------------------------##
##                                Model Paper 17 ts                           ##
## --                           --------------                            --  ##    

# set lib path 
.libPaths("S:/BrandSat/02_Code/R/library")
install_gitlab(repo='pflugmad/mapac', host='scm.cms.hu-berlin.de', quiet=F, force=T)


remove.packages("mapac", .libPaths("S:/BrandSat/02_Code/R/library"))



library(devtools)
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
library(extrafont)
library(officer)
library(flextable)

#install.packages("officer")
#install.packages("flextable")

??mapac
# -- Features -- Environmental -- features + LSM -- features + LSM + Environmental 

## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

data.path <- 'A:/04_Sampling/2021-02-05_S2_new_interpolation/'
setwd(data.path)

df_features        <- read.table("Kernel_6/df_features_winter_sub_fgk_sub.csv", sep = ";", header = TRUE)
## subset features
df_features     <- df_features[,grepl("coo", colnames(df_features))| grepl("int", colnames(df_features))|
                                 grepl("GRN", colnames(df_features))|grepl("RED", colnames(df_features))|
                                 grepl("RE1", colnames(df_features))|grepl("RE2", colnames(df_features))|
                                 # grepl("RE3", colnames(df_features))|
                                 grepl("NIR", colnames(df_features))|grepl("SW1", colnames(df_features))|
                                 grepl("SW2", colnames(df_features))]

df_features                   <- df_features[,grepl("coo", colnames(df_features))| 
                                               grepl("int", colnames(df_features))|
                                               grepl("2018", colnames(df_features))|
                                               grepl("2019", colnames(df_features)) ]



# ## class weights for model from areas of area fgk
areas_split <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/percent_area_split_for_paper.csv", sep =";", dec =".", header = TRUE)


# ------------------------------------------------------------------------------

                                   # Models #

## ------------------------------------------------------------------------------
model_spec         <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/01_02_model_spec_sub_winter_sub_fgk.rds")
model_env          <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/02_model_env_stao.rds")
model_spec_env     <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/03_model_spec_env_stao.rds")
model_spec_env_txt <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/05_model_spec_env_stao_txt_pre.rds")

## with split  
model_spec_split         <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/split_age/01_01_model_spec_age_split.rds")
model_env_split          <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/split_age/02_01_model_env_age_split.rds")
model_spec_env_split     <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/split_age/03_01_model_spec_env_age_split.rds")
model_spec_env_txt_split <- readRDS("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/split_age/04_01_model_full_age_split.rds")

# model_spec_cm <- model_spec_sub$confusion
# write.table(model_spec_cm, "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/cm_count_01_model_spec.csv", sep =";", dec =".", col.names = TRUE)
# model_env_cm <- model_env$confusion
# write.table(model_env_cm, "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/cm_count_02_model_env.csv", sep =";", dec =".", col.names = TRUE)
# model_spec_env_cm <- model_spec_env$confusion
# write.table(model_spec_env_cm, "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/cm_count_03_model_spec_env.csv", sep =";", dec =".", col.names = TRUE)
# model_spec_env_txt_cm <- model_spec_env_txt$confusion
# write.table(model_spec_env_txt_cm, "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/cm_count_04_model_full.csv", sep =";", dec =".", col.names = TRUE)


## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Function AA for split models 



vector_ref_int <- df_features$int_paper
vector_ref_split <- df_features$int_split
model <- model_spec_split
ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )




aa_split <- function(model, vector_ref_int, vector_ref_split){
  
  ## global levels 
  ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )
  
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
levels( aa_ref$TreeSpecies)
      ## ts int of ref                                     --> Ts_Name
levels(aa_map$TreeSpecies)

## results 
library(mapac)
result_01_split_model_spec <- aa_split(model_spec_split,df_features$int_paper, df_features$int_split) ## 0.96094 0.00208
result_02_split_model_env <- aa_split(model_env_split,df_features$int_paper, df_features$int_split)
result_03_split_model_spec_env <- aa_split(model_spec_env_split,df_features$int_paper, df_features$int_split)
result_04_split_model_spec_env_txt <- aa_split(model_spec_env_txt_split,df_features$int_paper, df_features$int_split)

## 01 split 
cm_spec_split <- aa_confusion_matrix_gtable(result_01_split_model_spec, title="Confusion matrix (area proportion)",
                                      type = "percent",
                                      zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                      out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/01_01_cm_model_spec_split.png")
cm_spec_split

## 02 split 
cm_env_split <- aa_confusion_matrix_gtable(result_02_split_model_env, title="Confusion matrix (area proportion)",
                                            type = "percent",
                                            zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                            out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/02_01_cm_model_env_split.png")
cm_env_split

## 03 split 
cm_spec_env_split <- aa_confusion_matrix_gtable(result_03_split_model_spec_env, title="Confusion matrix (area proportion)",
                                           type = "percent",
                                           zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                           out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/03_01_cm_model_spec_env_split.png")
cm_spec_env_split

## 04 split 
cm_spec_env_txt_split <- aa_confusion_matrix_gtable(result_04_split_model_spec_env_txt, title="Confusion matrix (area proportion)",
                                                type = "percent",
                                                zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                                out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/04_01_cm_model_spec_env_txt_split.png")
cm_spec_env_txt_split


model <- model_spec
aa_ts <- function(model, vector_ref_int, vector_ref_split){
  
  ## global levels 
  ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )
  
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
    left_join(., stats_ts, by = c("predicted"="Baumart_int_Paper")) %>%
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


## ts 
result_01_model_spec        <- aa_ts(model_spec,df_features$int_paper, df_features$int_split) ## 0.96094 0.00208
result_02_model_env         <- aa_ts(model_env,df_features$int_paper, df_features$int_split)
result_03_model_spec_env    <- aa_ts(model_spec_env,df_features$int_paper, df_features$int_split)
result_04_model_spec_env_txt <- aa_ts(model_spec_env_txt,df_features$int_paper, df_features$int_split)

## 01 split 
cm_spec <- aa_confusion_matrix_gtable(result_01_model_spec, title="Confusion matrix (area proportion)",
                                            type = "percent",
                                            zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                            out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/01_02_cm_model_spec_no_split.png")
cm_spec

??`mapac-package`


## 02 split 
cm_env <- aa_confusion_matrix_gtable(result_02_model_env, title="Confusion matrix (area proportion)",
                                           type = "percent",
                                           zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                           out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/02_02_cm_model_env_no_split.png")
cm_env

## 03 split 
cm_spec_env <- aa_confusion_matrix_gtable(result_03_model_spec_env, title="Confusion matrix (area proportion)",
                                                type = "percent",
                                                zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                                out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/03_02_cm_model_spec_env_no_split.png")
cm_spec_env

## 04 split 
cm_spec_env_txt <- aa_confusion_matrix_gtable(result_04_model_spec_env_txt, title="Confusion matrix (area proportion)",
                                                    type = "percent",
                                                    zero.rm=T, removeX = TRUE,colNumbering = TRUE,
                                                   # out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/04_01_cm_model_spec_env_txt_no_split.png",
 out_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/04_01_cm_model_spec_env_txt_no_split.html")

cm_spec_env_txt



## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## try to export

## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


?? aa_confusion_matrix_docx
?? aa_confusion_matrix_flextable
?? aa_confusion_matrix_gtable

## ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


aa_flex <- function(result) {
  
  flex <- aa_confusion_matrix_flextable(  aalist = result, 
                                          #docx_file = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/01_01_cm_model_spec_split.docx",
                                          #append = F,
                                          proportion = T,
                                          format.body = "%.03f",
                                          format.accuracy = "%.02f",
                                          accuracy.multiplier = 1,
                                          col.width = NULL,
                                          diagonal = NULL,
                                          format.int = "%.07g",
                                          area.percent = T,
                                          fontsize = 8,
                                          fontname = "Calibri",
                                          firstUp = F,
                                          height.header = NULL,
                                          rotate.header = F,
                                          zero.rm = T
  )
  
  return(flex)
}


 ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


result_02_split_model_env  ##  0.693719920 0.007462735
result_03_split_model_spec_env ### 0.960696447 0.002076025
result_04_split_model_spec_env_txt ##  0.961155405 0.002082327

flex_01_cm_split_spec          <- aa_flex(result_01_split_model_spec)
flex_02_cm_split_env           <- aa_flex(result_02_split_model_env)
flex_03_cm_split_spec_env      <- aa_flex(result_03_split_model_spec_env)
flex_04_cm_split_spec_env_txt  <- aa_flex(result_04_split_model_spec_env_txt)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Function for export of results in ptp, because that works 

# result <- result_01_split_model_spec
# path   <- "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/01_04_cm_model_spec_split_cm.csv"


export_result_to_cm <- function(result, path){
  
  ## takes input from MAPAC::aa_straified and exports cm csv of lengthly format with F1 Score

  
  options("scipen"=100, "digits"=4)
  
  UA <- as.data.frame(paste(round(result$stats$ua * 100, digits = 2), 
                            round(result$stats$ua_se * 100, digits = 2),
                            sep = " ± "))
  colnames(UA) <-  "User's"
  
  PA <- as.data.frame(paste(round(result$stats$pa * 100, digits = 2), 
              round(result$stats$pa_se * 100, digits = 2),
              sep = " ± ") )
  colnames(PA) <-  "Producer's"
  
  ## get F1 
  f1 <- as.data.frame(paste(round(result$stats$f1 * 100, digits = 2), 
                            round(result$stats$f1_se * 100, digits = 2),
                            sep = " ± "))
  colnames(f1) <- "F-score"  
  
  ## Full Matrix 
  bind_full <- aa_confusion_matrix_bind(result,proportion = T, accuracy.multiplier = 100)
  bind_full <- as.data.frame(bind_full)
  
  cm <- as.data.frame(bind_full[1:17,1:17])
  
  ## Get totals 
  MAP_total <- as.data.frame(bind_full$Total[1:17]*100)
  colnames(MAP_total) <- "Map"
  REF_total <- as.data.frame(as.numeric(as.vector(bind_full[18,1:17])*100))
  colnames(REF_total) <- "Ref"
  
  ## Bind all
  cm_bind <- as.data.frame(cbind(cm,MAP_total, REF_total, PA, UA, f1))
  
  ## write to ptp 
  #write.table(cm_bind, path,sep =";flextable::save_as_pptx(flex_01_cm_split_spec, path = "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/01_03_cm_model_spec_split.pptx")
  #)
  write.table(cm_bind, path, sep =";", dec =".", col.names = TRUE, row.names = TRUE)
  return(cm_bind)
}

result_01_split_model_spec ## 0.96094 0.00208
result_02_split_model_env  ##  0.693719920 0.007462735
result_03_split_model_spec_env ### 0.960696447 0.002076025
result_04_split_model_spec_env_txt ##  0.961155405 0.002082327

export_result_to_cm(result_01_split_model_spec,
                    "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/01_04_cm_model_spec_split_cm.csv")
export_result_to_cm(result_02_split_model_env,
                    "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/02_04_cm_model_env_split_cm.csv")
export_result_to_cm(result_03_split_model_spec_env,
                    "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/03_04_cm_model_spec_env_split_cm.csv")
export_result_to_cm(result_04_split_model_spec_env_txt,
                    "A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/split_vs_species/04_04_cm_model_spec_env_txt_split_cm.csv")



## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




## ----------------------------------------------------------------------------## 

##                                AA PLOTS                                     ## 

## --------------------------------------------------------------------------  ##
## Accuarcy Plots 


result_01_split_model_spec ## 0.96094 0.00208
result_02_split_model_env  ##  0.693719920 0.007462735
result_03_split_model_spec_env ### 0.960696447 0.002076025
result_04_split_model_spec_env_txt ##  0.961155405 0.002082327


## stats #1 pur env 
aaStats <- result_02_split_model_env$stats

tmp1 <- tidyr::gather(aaStats[,c('class', 'ua', 'pa')], src, accuracy, -class)
tmp2 <- tidyr::gather(aaStats[,c('class', 'ua_se', 'pa_se')], src, se, -class)
tmp2$src <- gsub('_se', '', tmp2$src)
result <- merge(tmp1, tmp2)
result$se_low <- result$accuracy - result$se
result$se_upp <- result$accuracy + result$se
result$src <- gsub('pa', "Producer's accuracy", result$src)
result$src <- gsub('ua', "User's accuracy", result$src)
result[is.na(result)] <- 0


#-------------------------------------
## stats #2 result_spec
aaStats_2 <- result_01_split_model_spec$stats

tmp1_2 <- tidyr::gather(aaStats_2[,c('class', 'ua', 'pa')], src, accuracy, -class)
tmp2_2 <- tidyr::gather(aaStats_2[,c('class', 'ua_se', 'pa_se')], src, se, -class)
tmp2_2$src <- gsub('_se', '', tmp2_2$src)
result_2 <- merge(tmp1_2, tmp2_2)

## SE_difference = sqrt(SE_UA2^2 + SE_UA1^2)
result_2$se_low <- result_2$accuracy - result$se
result_2$se_upp <- result_2$accuracy + result$se
result_2$src <- gsub('pa', "Producer's accuracy", result_2$src)
result_2$src <- gsub('ua', "User's accuracy", result_2$src)
result_2[is.na(result_2)] <- 0

## calc difference df 
## first basis df 
result_2_spec_dif <- cbind(result_2[,1:2],result_2[,3:6] - result[,3:6])
## calculate SE 
result_2_spec_dif[,4] <- sqrt(result_2$se^2 + result$se^2)
## cal se low and se upp 
result_2_spec_dif$se_low <- result_2_spec_dif$accuracy - result_2_spec_dif$se
result_2_spec_dif$se_upp <- result_2_spec_dif$accuracy + result_2_spec_dif$se


#-------------------------------------
## stats #3 result spec env 
aaStats_3 <- result_03_split_model_spec_env$stats

tmp1_3 <- tidyr::gather(aaStats_3[,c('class', 'ua', 'pa')], src, accuracy, -class)
tmp2_3 <- tidyr::gather(aaStats_3[,c('class', 'ua_se', 'pa_se')], src, se, -class)
tmp2_3$src <- gsub('_se', '', tmp2_3$src)
result_3 <- merge(tmp1_3, tmp2_3)
result_3$se_low <- result_3$accuracy - result_3$se
result_3$se_upp <- result_3$accuracy + result_3$se
result_3$src <- gsub('pa', "Producer's accuracy", result_3$src)
result_3$src <- gsub('ua', "User's accuracy", result_3$src)
result_3[is.na(result_3)] <- 0

################# Calc dif 
## first basis df 
result_3_spec_env_dif <- cbind(result_3[,1:2],result_3[,3:6] - result_2[,3:6])
## calculate SE 
result_3_spec_env_dif[,4] <- sqrt(result_3[,4]^2 + result_2[,4]^2)
## cal se low and se upp 
result_3_spec_env_dif$se_low <- result_3_spec_env_dif$accuracy - result_3_spec_env_dif$se
result_3_spec_env_dif$se_upp <- result_3_spec_env_dif$accuracy + result_3_spec_env_dif$se


#-------------------------------------
## stats #4 result_spec_txt_env
aaStats_3 <- result_04_split_model_spec_env_txt$stats


tmp1_3 <- tidyr::gather(aaStats_3[,c('class', 'ua', 'pa')], src, accuracy, -class)
tmp2_3 <- tidyr::gather(aaStats_3[,c('class', 'ua_se', 'pa_se')], src, se, -class)
tmp2_3$src <- gsub('_se', '', tmp2_3$src)
result_4 <- merge(tmp1_3, tmp2_3)
result_4$se_low <- result_4$accuracy - result_4$se
result_4$se_upp <- result_4$accuracy + result_4$se
result_4$src <- gsub('pa', "Producer's accuracy", result_4$src)
result_4$src <- gsub('ua', "User's accuracy", result_4$src)
result_4[is.na(result_4)] <- 0

################# Calc dif 
## first basis df 
result_4_spec_txt_env_dif <- cbind(result_4[,1:2],result_4[,3:6] - result_3[,3:6])
## calculate SE 
result_4_spec_txt_env_dif[,4] <- sqrt(result_4[,4]^2 + result_3[,4]^2)
## cal se low and se upp 
result_4_spec_txt_env_dif$se_low <- result_4_spec_txt_env_dif$accuracy - result_4_spec_txt_env_dif$se
result_4_spec_txt_env_dif$se_upp <- result_4_spec_txt_env_dif$accuracy + result_4_spec_txt_env_dif$se


## -------------------------------------------------------------------------------
# PLOTS 
result

#2
result_2_spec_dif
#3
result_3_spec_env_dif
#4
result_4_spec_txt_env_dif

## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
set_text <-  element_text(size=15,  family="Calibri")

plot_margins <- unit(c(0.1,0.1,0.1,0.1), "cm")
scale_fill <- scale_fill_manual(values=c("#474a4b", "#D2B48C", "#7570B3","#E69F00")) 

#-------------------------------------
#1 pure spectra 
result$class <- factor(result$class, levels = rev(ts_names_17_levels))
p_env <- ggplot2::ggplot(result, aes(class, accuracy, fill=src)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9)) +
  ggplot2::xlab('') + ggplot2::ylab('Accuracy') +
  ggplot2::coord_flip() + # scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1)) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  # scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3","#E69F00")) +
  scale_fill +
  ggtitle("Env") +
  ggplot2::theme_bw( base_size = 11) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.margin=plot_margins,
                 
                 legend.title=ggplot2::element_blank(), legend.position= "NONE",
                 text=set_text,
                 axis.text.y = element_text(size=17,  family="Calibri"),
                 plot.title = element_text(family = "Calibri", size = 15, 
                                           face = "bold", hjust = 0.5) ) 

p_env


result_2_spec_dif$class <- factor(result_2_spec_dif$class, levels = rev(ts_names_17_levels))
result_2_spec_dif <- na.omit(result_2_spec_dif)


# windowsFonts()
# loadfonts(device = "win")
#--only env 

p_dif_spec <- ggplot2::ggplot(result_2_spec_dif, aes(class, accuracy, fill=src)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9)) +
  ggplot2::xlab('') + ggplot2::ylab('?? Accuracy') +
  ggplot2::coord_flip() + #  scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=seq(-0.8,0.8,0.4), limits=c(-1.0, 1.0)) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  scale_fill +
  ggtitle("Env ??? Spec") +
  
  ggplot2::theme_bw( base_size = 11) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.ticks.y = element_blank(),
                 legend.title=ggplot2::element_blank(), legend.position= "NONE",
                 axis.text.y=element_blank(),
                 plot.margin=plot_margins,
                 plot.title = element_text(family = "Calibri", size = 15, 
                                           face = "bold", hjust = 0.5),
                 text=set_text
  ) 
p_dif_spec


result_3_spec_env_dif$class <- factor(result_3_spec_env_dif$class, levels = rev(ts_names_17_levels))
result_3_spec_env_dif <- na.omit(result_3_spec_env_dif)
result_3_spec_env_dif$labeli <- " "
result_3_spec_env_dif[result_3_spec_env_dif$class =="European ash" & result_3_spec_env_dif$src == "Producer's accuracy" ,]$labeli <- "*"

#-------------------------------------
#3 spectral env
# plot_margins <- unit(c(1,0.01,1,0.01), "cm")

## specific parameter for third and fourth model 
scale_y_breaks <- seq(-0.2,0.2,0.2)
scale_y_limits <- c(-0.25, 0.25)
result_spec_env_diff$class <- factor(result_3_spec_env_dif$class, levels = rev(ts_names_17_levels))
p_dif_spec_env <- ggplot2::ggplot(result_3_spec_env_dif, aes(class, accuracy, fill=src, label = labeli)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9)) +
  ggplot2::xlab('') + ggplot2::ylab('?? Accuracy') +
  ggplot2::coord_flip() + #  scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=scale_y_breaks, limits=scale_y_limits) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  scale_fill + 
  ggtitle("Spec ??? SpecEnv") +
  
  ggplot2::theme_bw( base_size = 11) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 #panel.grid.major = element_line(colour="blue", size=0.5),
                 panel.background = ggplot2::element_blank(),
                 axis.ticks.y = element_blank(),
                 plot.margin=plot_margins,
                 legend.title=ggplot2::element_blank(), legend.position= "Bottom",
                 axis.text.y=element_blank(),
                 plot.title = element_text(family = "Calibri", size = 15, 
                                           face = "bold", hjust = 0.5),
                 text=set_text 
  ) + 
  geom_text(size = 7,  vjust = 0.34, hjust = 1.2)
p_dif_spec_env

result_4_spec_txt_env_dif$class <- factor(result_4_spec_txt_env_dif$class, levels = rev(ts_names_17_levels))
result_4_spec_txt_env_dif <- na.omit(result_4_spec_txt_env_dif)
options("scipen"=100, "digits"=4)
colnames(df_env)
#----------------------------------------------------------------
#4 spec_txt_env
scale_y_limits <- c(-0.2, 0.2)
scale_y_breaks <- seq(-0.2,0.2,0.2)
result_4_spec_txt_env_dif$labeli <- " "
result_4_spec_txt_env_dif[result_4_spec_txt_env_dif$class =="European ash" & result_4_spec_txt_env_dif$src == "Producer's accuracy" ,]$labeli <- "**"
# plot_margins <- unit(c(1,0.01,1,0.01), "cm")
p_dif_spec_txt_env <- ggplot2::ggplot(result_4_spec_txt_env_dif, aes(class, accuracy, fill=src, label = labeli)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9)) +
  ggplot2::xlab('') + ggplot2::ylab('?? Accuracy') +
  ggplot2::coord_flip() + #  scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=scale_y_breaks, limits=scale_y_limits) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  scale_fill + 
  ggtitle("SpecEnv ??? Full") + 
  ggplot2::theme_bw( base_size = 11) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.margin=plot_margins,
                legend.title=ggplot2::element_blank(), legend.position= "NONE",
                 axis.ticks.y = element_blank(),
                 axis.text.y=element_blank(),
                plot.title = element_text(family = "Calibri", size = 15, 
                                          face = "bold", hjust = 0.5),
                 text=set_text) +
#  scale_x_discrete() +
geom_text(size = 7,  vjust = 0.34, hjust =  0.051)
p_dif_spec_txt_env

# ---------------------------------------------------------------------------
p_legend_P <- ggplot2::ggplot(result_4_spec_txt_env_dif[result_4_spec_txt_env_dif$src == "Producer's accuracy",], aes(class, accuracy, fill=src)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::coord_flip() + #  scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=scale_y_breaks, limits=c(0,0)) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  scale_fill + 
  ggplot2::theme_bw( base_size = 11) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.margin=plot_margins,
                 legend.title=ggplot2::element_blank(), #legend.position= "NONE",
               #  legend.position = c(0.155,.13), 
               legend.position = "bottom",  
                legend.text = element_text(size = 14),
                 axis.ticks.y = element_blank(),
                 axis.text.y=element_blank(),
                 plot.title = element_text(family = "Calibri", size = 15, 
                                           face = "bold", hjust = 0.5),
                 text=set_text) 
p_legend_P

scale_fill <- scale_fill_manual(values=c( "#D2B48C", "#7570B3","#E69F00")) 
p_legend_U <- ggplot2::ggplot(result_4_spec_txt_env_dif[result_4_spec_txt_env_dif$src == "User's accuracy",], aes(class, accuracy, fill=src)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::coord_flip() + #  scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=scale_y_breaks, limits=c(0,0)) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  scale_fill + 
  ggplot2::theme_bw( base_size = 11) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.margin=plot_margins,
                 legend.title=ggplot2::element_blank(), #legend.position= "NONE",
                 #  legend.position = c(0.155,.13), 
                 legend.position = "bottom",  
                 legend.text = element_text(size = 14),
                 axis.ticks.y = element_blank(),
                 axis.text.y=element_blank(),
                 plot.title = element_text(family = "Calibri", size = 15, 
                                           face = "bold", hjust = 0.5),
                 text=set_text) 
p_legend_U

legend
library(cowplot)
legend_p <- get_legend(p_legend_P)
legend_u <- get_legend(p_legend_U)
library(cowplot)
#-------------------------------------

p_env
p_dif_spe
p_dif_spec_env


lay <- rbind(c(1,1,1,2,3),
             c(1,1,1,4,5),
             c(6,7,8,9,9))

nischt <- ggplot() + theme_void()
grid_plot <- grid.arrange(p_env,
                          p_dif_spec,p_dif_spec_env,
                          p_dif_spec_txt_env,nischt, legend_p,legend_u,
                          ncol = 4, widths = c(2.5,1,1,1),
                          heights = c(1,0.1))
grid_plot
# png("A:/09_Model/17_model_paper/__paper_final/06_for_real/GRID_Ua_Pa.png",width = 10, height = 6)
# print(grid_plot)
# dev.off()

#lay <- rbind(c(1,1,1,2,3,4))
#g <- arrangeGrob(p_spec,p_env_dif,p_dif_lsm, p_dif_spec_stm,ncol =4,layout_matrix = lay)
ggsave("A:/09_Model/17_model_paper/paper_final_final_new_int/models_fgk_sub/AA/grid_ts_aa_3.png",grid_plot, width = 12, height = 6,dpi = 600
)

## p_spec , p_dif_spec_soil_srtm , p_dif_spec_env_lsm , p_dif_spec_srtm



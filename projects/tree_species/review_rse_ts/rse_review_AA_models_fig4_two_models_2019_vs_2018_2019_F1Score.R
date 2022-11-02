################################################################################




## ---------------------------------------------------------------------------##
##                                Model Paper 17 ts                           ##
## --                           --------------                            --  ##    

# set lib path 
# .libPaths("S:/BrandSat/02_Code/R/library")
# install_gitlab(repo='pflugmad/mapac', host='scm.cms.hu-berlin.de', quiet=F, force=T)
# 
# 
# library(devtools)
# library(dplyr)
# library(ggplot2)
# library(scales)
# library(gridExtra)
# library(reshape2)
# library(formattable)
# library(maptools)
# library(sp)
# library(rgdal)
# library(raster)
# library(sf)
# library(rgeos)
# library(randomForest)
# library(caret)
# library(matrixStats)
# library(fortify)
# library(mapac)
# library(e1071)
# library(gt)
# library(tidyverse)
# library(tibble)
# library(extrafont)
# library(officer)
# library(flextable)

#install.packages("officer")
#install.packages("flextable")


# -- Features -- Environmental -- features + LSM -- features + LSM + Environmental 

## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

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
library(gt)
library(tidyr)
library(ggtext)

################################################################################

### FUNS
aa_int <- function(model, vector_ref_int, vector_ref_split, test = FALSE){
  
  if (test) {
    model <- model_spec_split
    vector_ref_int <- df_features$int_paper
    vector_ref_split <- df_features$int_split
  }
  
  ## global levels 
  ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )
  
  map      <- as.data.frame(as.integer(as.character(model$predicted)))
  ref      <- as.data.frame(as.integer(vector_ref_int))
  strats   <- vector_ref_split
  
  ## get map from split to ts ints 
  map_int <- map
  
  
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
  freq_all_int <- freq_all[!duplicated(freq_all$TreeSpecies),]
  ## int --> character .. ts names 
  
  aa_map    <-  map %>%
    left_join(., freq_all_int, by = c("predicted"="Baumart_int_Paper")) %>%
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


# model <- model_spec
aa_ts <- function(model, vector_ref_int, vector_ref_split, test = FALSE){
  
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

## ---------------------------------------------------------------------------##
##                                     Data                                   ## 

df_features <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_spec_reduced.csv", 
                          sep = ";", dec = ".", header = TRUE)
df_env     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_env_red.csv", 
                         sep = ";", dec = ".", header = TRUE)
df_txt     <- read.table("A:/04_Sampling/2021-02-05_ts_review/reduced_fgk/df_txt_reduced.csv", 
                         sep = ";", dec = ".", header = TRUE)



# ## class weights for model from areas of area fgk
areas_split <- read.table("S:/BrandSat/01_Data/01_FGK/04_Stats/percent_area_split_for_paper.csv", sep =";", dec =".", header = TRUE)


# ------------------------------------------------------------------------------

                                   # Models #

## ------------------------------------------------------------------------------
# setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/")
# 
# ## with split  
# model_spec_split         <- readRDS("01_model_spec_150.rds")
# model_env_split          <- readRDS("02_model_env.rds")
# model_spec_env_split     <- readRDS("03_model_spec_150_env.rds")
# model_spec_env_txt_split <- readRDS("04_model_spec_150_env_txt.rds")
# 
# 
setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2018_2019/550/")
model_spec_split         <- readRDS("01_model_spec_550.rds")
# model_env_split          <- readRDS("02_model_env.rds")
# model_spec_env_split     <- readRDS("03_model_spec_475_env.rds.rds")
# model_spec_env_txt_split <- readRDS("04_model_spec_475_env_txt.rds.rds")

setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2019/")
"P:/"
model_spec_2019         <- readRDS("01_model_spec_400_2019.rds")
# model_env_split          <- readRDS("02_model_env_int.rds")
# model_spec_env_split     <- readRDS("03_model_spec_int_env.rds")
# model_spec_env_txt_split <- readRDS("04_model_spec_old_bands_env_txt.rds")

setwd("P:/workspace/treespecies/_review/model/new_bands_int/")
"P:/"
model_spec_int         <- readRDS("01_model_spec_550_int_.rds")

## gegenueberstellung 





## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Function AA for split models 
vector_ref_int   <- df_features$int_paper
vector_ref_split <- df_features$int_split
ts_names_17_levels  <- c("Scots pine","Norway spruce","Common & Red alder","Common & Sessile oak","European beech","Silver birch","European & Japanese larch","Red oak", "Douglas fir","Robinia","Weymouth pine","Black pine","Small-leaved lime","Great maple","European ash","Poplar", "Hornbeam" )

result_01_split_model_spec       <- aa_split(model_spec_split,df_features$int_paper, df_features$int_split) ## 0.96094 0.00208
result_02_int_model_spec         <- aa_split(model_spec_2019 ,df_features$int_paper, df_features$int_split)
result_03_int_model_spec_new         <- aa_int(model_spec_int ,df_features$int_paper, df_features$int_split)

result_01_split_model_spec  ## model 2018 2019 550 features 
result_02_split_model_2019 <- result_02_int_model_spec
result_03_int_model_spec_new
## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## try to export cm

## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

 ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


###############################################################################
# setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2018_2019/550/")
# export_result_to_cm(result_01_split_model_spec,
#                     "01_04_cm_model_split_550_original.csv")
# 
# setwd("P:/workspace/treespecies/_review/model/new_bands_int/")
# export_result_to_cm(result_03_int_model_spec_new ,
#                     "01_04_cm_model_int_550_features.csv")
# export_result_to_cm(result_02_split_model_env,
#                     "02_04_cm_model_env_split_cm.csv")
# export_result_to_cm(result_03_split_model_spec_env,
#                     "03_04_cm_model_spec_env_split_cm.csv")
# export_result_to_cm(result_04_split_model_spec_env_txt,
#                     "04_04_cm_model_spec_env_txt_split_cm.csv")



## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




## ----------------------------------------------------------------------------## 

##                                AA PLOTS                                     ## 

## --------------------------------------------------------------------------  ##
## Accuarcy Plots 


result_01_split_model_spec ## 0.96094 0.00208
result_02_split_model_2019 ##  0.693719920 0.007462735

## stats #1 pur spec
aaStats <- result_01_split_model_spec$stats

tmp1 <- tidyr::gather(aaStats[,c('class', 'ua', 'pa','f1')], src, accuracy, -class)
tmp2 <- tidyr::gather(aaStats[,c('class', 'ua_se', 'pa_se','f1_se')], src, se, -class)
tmp2$src <- gsub('_se', '', tmp2$src)
result <- merge(tmp1, tmp2)
result$se_low <- result$accuracy - result$se
result$se_upp <- result$accuracy + result$se
result$src <- gsub('pa', "Producer's accuracy", result$src)
result$src <- gsub('ua', "User's accuracy", result$src)
result$src <- gsub('f1', "F1-score", result_2$src)
result[is.na(result)] <- 0


#-------------------------------------
## stats #4 spec env --> spec enf txt 
aaStats_2 <-result_02_split_model_2019$stats
tmp1_2 <- tidyr::gather(aaStats_2[,c('class', 'ua', 'pa','f1')], src, accuracy, -class)
tmp2_2 <- tidyr::gather(aaStats_2[,c('class', 'ua_se', 'pa_se','f1_se')], src, se, -class)
tmp2_2$src <- gsub('_se', '', tmp2_2$src)
result_2   <- merge(tmp1_2, tmp2_2)
result_2$se_low <- result_2$accuracy - result_2$se
result_2$se_upp <- result_2$accuracy + result_2$se
result_2$src <- gsub('pa', "Producer's accuracy", result_2$src)
result_2$src <- gsub('ua', "User's accuracy", result_2$src)
result_2$src <- gsub('f1', "F1-score", result_2$src)
result_2[is.na(result_2)] <- 0

################# Calc dif 
## first basis df 
result_2_split_int_dif <- cbind(result_2[,1:2],result_2[,3:6] - result[,3:6])
## calculate SE 
result_2_split_int_dif[,4] <- sqrt(result_2[,4]^2 + result[,4]^2)
## cal se low and se upp 
result_2_split_int_dif$se_low <- result_2_split_int_dif$accuracy - result_2_split_int_dif$se
result_2_split_int_dif$se_upp <- result_2_split_int_dif$accuracy + result_2_split_int_dif$se

## -------------------------------------------------------------------------------
# PLOTS 
result

#2
result_2_split_int_dif


## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library("ggplot2")
set_text <-  element_text(size=15,  family="Calibri")
plot_margins <- unit(c(0.1,0.1,0.1,0.1), "cm")
scale_fill <- scale_fill_manual(values=c( "#D2B48C", "#7570B3","#E69F00")) # "#474a4b", 

#-------------------------------------
#1 pure spec
result$class <- factor(result$class, levels = rev(ts_names_17_levels))
p_spec <- ggplot2::ggplot(result %>% filter(src == "F1-score")
                          , aes(class, accuracy, fill=src)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9),
                    width = 0.5) +
  # ggplot2::xlab('') + ggplot2::ylab('Accuracy') +
  ggplot2::xlab('') + ggplot2::ylab('F1-score') +
  ggplot2::coord_flip() + # scale_y_continuous(limits=c(0, 0.8)) +
  ggplot2::scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1), expand = c(0,0.02)) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.4, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  # scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3","#E69F00")) +
  scale_fill +
  ggtitle("Spec-Model") +
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

p_spec


#################################################################################
## result 2 env 
result_2_split_int_dif$class <- factor(result_2_split_int_dif$class, levels = rev(ts_names_17_levels))
result_2_env <- na.omit(result_2_split_int_dif)

ymin <- min(result_2_env[result_2_env$src == "F1-score",]$se_low) - 0.01
ymax <- max(result_2_env[result_2_env$src == "F1-score",]$se_upp) + 0.01

min_yscale <- which.min(abs(divide_list - abs(ymin)))
max_yscale <- which.min(abs(divide_list - ymax))

## which has lowest rest if dicvided 
scale_y_limits <- c(ymin, ymax) ## 2018-2019
scale_y_breaks <- c(-divide_list[min_yscale],divide_list[max_yscale],0.2) ## 2018-2019


# scale_y_breaks <- seq(-1,0.4,0.2) ## 2019
# scale_y_limits <- c(-1.01, 0.42) 
p_dif <- ggplot2::ggplot(result_2_env %>% filter(src == "F1-score"), aes(class, accuracy, fill=src)) + 
  ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(.9),
                    width = 0.5) +
  ggplot2::xlab('') + ggplot2::ylab(' ?? F1-score') +
  ggplot2::coord_flip() + #  scale_y_continuous(limits=c(0, 0.8)) +
  # ggplot2::scale_y_continuous(breaks= c(-0.2, 0.0), limits=c(-0.3,0.15)) +
  ggplot2::geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                         width=.5, size =0.3,                   # Width of the error bars
                         position=ggplot2::position_dodge(.9)) +
  #scale_fill_brewer(palette="Dark2") + # Paired Dark2
  #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
  scale_fill +
  ggplot2::scale_y_continuous(breaks=scale_y_breaks, limits=scale_y_limits) +
  # ggtitle("Spec-NoSplit-Model") +
  ggtitle("Spec-Model 2019") +
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
p_dif


p_legend_P

scale_fill <- scale_fill_manual(values=c( "#D2B48C", "#7570B3","#E69F00")[2:3]) 
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
                 plot.title = element_text(family = "Calibri", size = 14, 
                                           face = "bold", hjust = 0.5),
                 text=set_text) 
p_legend_U


# library(cowplot)
# legend_p <- get_legend(p_legend_P)
# legend_u <- get_legend(p_legend_U)

#-------------------------------------

p_spec
p_dif

lay <- rbind(c(1,1,1,2,3),
             c(1,1,1,4,5),
             c(6,7,8,9,9))

nischt <- ggplot() + theme_void()
# install.packages("gridExtra")#
library(gridExtra)
grid_plot <- grid.arrange(p_spec, p_dif,
                         # nischt, 
                         # legend_p,legend_u,
                          ncol = 2, widths = c(2.7,0.9), ## 2019 
                          heights = c(1,0.1))
grid_plot
# png("A:/09_Model/17_model_paper/__paper_final/06_for_real/GRID_Ua_Pa.png",width = 10, height = 6)
# print(grid_plot)
# dev.off()

#lay <- rbind(c(1,1,1,2,3,4))
#g <- arrangeGrob(p_spec,p_env_dif,p_dif_lsm, p_dif_spec_stm,ncol =4,layout_matrix = lay)
setwd("P:/workspace/treespecies/_review/model/feature_reduction_env_txt/2019/")
# ggsave("split_2018_2019_vs_int_2018_2019_bars_f1score_2.png",grid_plot, width = 12, height = 6,dpi = 600
ggsave("split_2018_2019_vs_2019_bars_f1score_2.png",grid_plot, width = 12, height = 6,dpi = 600
       
) ## width == 12 

## p_spec , p_dif_spec_soil_srtm , p_dif_spec_env_lsm , p_dif_spec_srtm



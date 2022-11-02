## Model


### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")
rasterOptions()
dirname(rasterTmpFile()) 
rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

# install.packages("randomForest")
# urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
# install.packages(urlPackage, repos=NULL, type="source") 

# install.packages("hms")
## lib
library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(bfastSpatial)
library(terra)
library(svMisc)
library(snow)
library(lubridate)
library(hms)
library(rgdal)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyr)
library(Rcpp)
library(dplyr)
library(randomForest)
library(data.table)
library(randomForest)
library(flextable)
library(caret)


## load refernce data 
## load samples 
load_samples <- function(){
  ## load id bb
  bb_id <- read.table("P:/timesync/bb/tsync_plots_bb.csv", 
                      sep = ",", dec = ".", header = TRUE)
  bb_comment <- read.table("P:/timesync/bb/tsync_plots_bb_interpretations_comment.csv",
                           sep = ",", dec = ".", header = TRUE)
  
  ## id fires 
  fires_id <- read.table("P:/timesync/bb/tsync_fires_4_distance_to_all_center.csv",
                         sep = ",", dec = ".", header = TRUE)
  fires_comments <- read.table("P:/timesync/bb/tsync_fires_interpretations_comment.csv",
                               sep = ",", dec = ".", header = TRUE)
  
  ids <- rbind(fires_id, bb_id)
  return(ids)
}

load_inters <- function(){
  ## load id bb
  bb_interpretations <- read.table("P:/timesync/bb/tsync_plots_bb_interpretations.csv",
                                   sep = ",", dec = ".", header = TRUE)
  ## id fires 
  fires_interpretations <- read.table("P:/timesync/bb/tsync_fires_interpretations.csv",
                                      sep = ",", dec = ".", header = TRUE)
  
  inters <- rbind(fires_interpretations, bb_interpretations)
  return(inters)
}

## load references 
ids <- load_samples()
ids <- ids[!duplicated(ids$plotid),] ## 3142  3144
length(ids$plotid)
# ids_in_ts <- ids[duplicated(ids_in_ts$id),]

## load preprocessed refs 
refs <- read.table("P:/workspace/jan/fire_detection/disturbance_ref/preprocessed_disturbance_ref.csv", sep = ";", dec =".", header = TRUE)

## for each id one year 
ids_raw <- (ids$plotid)
ids_years <- merge(as.data.frame(ids_raw),as.data.frame(seq(1990,2020)))
# ids_years$disturbance <- 0

colnames(ids_years) <- c("plotid","year")
## ----------------------------------------------------------------------------

## do we have reference for id and year?
## filter refs 
refs <- refs %>% select(.,c("plotid","image_year","change_process_brandsat","image_julday","date_of_interpretation",
                            "harvest_fire_same_year","start_fire","end_fire","start_harvest",
                            "harvest_one_year_after_fire","days_harvest_after_fire","source",
                            "fireid","fl","flw","HA_effis","tsync_id","dis_id"))
colnames(refs) <-c("plotid","image_year","reference", paste0("reference_",c("image_julday","date_of_interpretation",
                                                                            "harvest_fire_same_year","start_fire","end_fire","start_harvest",
                                                                            "harvest_one_year_after_fire","days_harvest_after_fire","source",
                                                                            "fireid","fl","flw","HA_effis","tsync_id","dis_id")))
ids_years_ref <- left_join(ids_years, refs, by = c("plotid", "year" = "image_year"))

## where we have no reference -- > no Disturbance in that year 
ids_years_ref[is.na(ids_years_ref$reference),]$reference <- "Undisturbed"
#ids_years_ref$reference <- as.factor(ids_years_ref$reference)
#summary(ids_years_ref)

# ## 1== no expansion
# pro_breaks_1 <- read.table("P:/workspace/jan/fire_detection/break_detection/preprocessed_breaks/h40_till_2022/preprocessed_breaks.csv", sep =";", dec = ".", 
#                          header = TRUE)
## 2== one year added 
pro_breaks_2 <- read.table("P:/workspace/jan/fire_detection/break_detection/preprocessed_breaks/h40_till_2022_one_year_added/preprocessed_breaks.csv", sep =";", dec = ".", 
                           header = TRUE)
# ## 3== variable expansion
# pro_breaks_3 <- read.table("P:/workspace/jan/fire_detection/break_detection/preprocessed_breaks/h40_till_2022_with_expansion/preprocessed_breaks.csv", sep =";", dec = ".", 
#                            header = TRUE)

## we are setteled for one year expansion
pro_breaks_2 <- pro_breaks_2[!duplicated(pro_breaks_2),]
pro_breaks_2 <- pro_breaks_2[!rowSums(is.na(pro_breaks_2[,12:70])) > 0,]

## left join with refs, and bring to 3 ref level 
refs <- read.table("P:/workspace/jan/fire_detection/disturbance_ref/preprocessed_disturbance_ref.csv", sep = ";", dec =".", header = TRUE)
pro_breaks_2 <- left_join(pro_breaks_2, refs, by = c("meta_closest_ref_id" = "dis_id") )
pro_breaks_2[is.na(pro_breaks_2$change_process_brandsat),]$change_process_brandsat <- "No_dis"
pro_breaks_2[pro_breaks_2$change_process_brandsat== "Other",]$change_process_brandsat <- "Harvest"
pro_breaks_2[pro_breaks_2$change_process_brandsat== "Wind",]$change_process_brandsat <- "Harvest"

## -____________________________________________________________________________
##                                 Model
## same model for each version ----
## _____________________________________________________________________________
## Ref Model with all features 

library(randomForest)
names_for_model <- names(pro_breaks_2)[c(12:15,16:25,47:52, 74:79,101:106,128:133)]
length(names_for_model)

source("P:/workspace/jan/code/R/BFAST/source_funs.R")
set.seed(202)
t1 <- Sys.time()
model <- randomForest(x=pro_breaks_2[,colnames(pro_breaks_2) %in% names_for_model], y= as.factor(pro_breaks_2$change_process_brandsat), do.trace = TRUE)
t2 <- Sys.time()
difftime(t2, t1, units = "mins")

## get predicted data
pro_breaks_2$predicted <- model$predicted

## AA for model 
#saveRDS(model, "P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022_one_year_expansion/model_38_features_unequal.rds")
model <- readRDS("P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022_one_year_expansion/model_38_features_unequal.rds")
varImpPlot(model)

### MODEL with subsample
## whatis the smallest group? 1ß2ß in breaks 2
pro_breaks_2 %>% group_by(change_process_brandsat) %>% tally()
pro_breaks_2_sub <- pro_breaks_2 %>% group_by(change_process_brandsat) %>% sample_n(1020) %>% as.data.frame()

set.seed(202)
t1 <- Sys.time()
model_sub <- randomForest(x=pro_breaks_2_sub[,colnames(pro_breaks_2_sub) %in% names_for_model], y= as.factor(pro_breaks_2_sub$change_process_brandsat), do.trace = TRUE)

t2 <- Sys.time()
difftime(t2, t1, units = "mins")
saveRDS(model_sub, "P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022_one_year_expansion/model_38_features_equal.rds")

## get predicted data
pro_breaks_2_sub$predicted <- model_sub$predicted


cm        <- get_cm(model,"Basic 38")
cm_sub    <- get_cm(model_sub,"Basic 38, equal samplesize")

cm$Predicted <- as.integer(cm$Predicted)
cm_sub$Predicted <- as.integer(cm_sub$Predicted)
cm_all <- rbind(cm,cm_sub)
cm.group <- as_grouped_data(x = cm_all, groups = "model")

library(webshot)

flex_models <- as_flextable(cm.group) %>%
  theme_booktabs() %>%
  hline(part = 'header', border = officer::fp_border(color = "black", width = 3)) %>%
  vline(j=c("Disturbance","No_Dis","Predicted"),   border = officer::fp_border(color= "black", style = "solid", width = 1.45)) %>%
  vline(j=c("Fire","Harvest","Reference","Commission"),   border = officer::fp_border(color= "gray", style = "dashed", width = 1.45)) %>%
  colformat_double(j = c("Predicted"), digits = 0) %>%
  colformat_double(j = c("Omission","Commission","Sensitivity","Specificity","OA"), digits = 2) %>%
  
  hline(i = ~Disturbance %in% c("Fire","Harvest"), border = officer::fp_border(color= "gray", style = "dashed")) %>%
  #
  # bg(i = ~ Disturbance == "Fire" ,j = ~ Fire, bg=bg_picker_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" ,j = ~ Harvest, bg=bg_picker_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ No_Dis, bg=bg_picker_no_dis) %>%
  # ## Omission
  # bg(i = ~ Disturbance == "Fire" , j = ~ Omission, bg=bg_picker_omi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Omission, bg=bg_picker_omi_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ Omission,  bg=bg_picker_omi_no_dis) %>%
  # ## commission
  # bg(i = ~ Disturbance == "Fire" ,  j = ~ Commission, bg=bg_picker_comi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Commission,  bg=bg_picker_comi_harvest) %>%
# bg(i = ~ Disturbance == "No_Dis" , j = ~ Commission, bg=bg_picker_comi_no_dis) %>%
save_as_image("P:/workspace/jan/fire_detection/break_detection/classification_model_aa/01_model_basic_38_feature_aa_vs_equal_sample_size.png",
              zoom = 3, webshot = "webshot")

#dir.create("P:/workspace/jan/fire_detection/break_detection/classification_model_aa/")

####################################################################################
## look at some boxplots values 
colnames(pro_breaks_2)
pro_breaks_2_filtered <- pro_breaks_2 %>% select(.,c("smp_id","year","n_bps","id_break",
                                                     "post_val_fit","magnitude_fit","slope_pre",
                                                     "slope_post","predicted"))
# install.packages("gridExtra")
library(gridExtra)
# pro_breaks_2_filtered$predicted <- as.factor(pro_breaks_2_filtered$predicted)
magnitude_fit_plot <- ggplot(pro_breaks_2_filtered, aes (x = predicted, y= magnitude_fit)) + geom_boxplot()+
  ggtitle("magnitude_fit")+ theme_minimal()
post_val_fit_plot <- ggplot(pro_breaks_2_filtered, aes (x = predicted, y= post_val_fit)) + geom_boxplot()+
  ggtitle("post_val_fit")+ theme_minimal()
slope_pre_plot    <- ggplot(pro_breaks_2_filtered, aes (x = predicted, y= slope_pre)) + geom_boxplot()+
  ggtitle("slope_pre_fit")+ theme_minimal()
slope_post_plot   <- ggplot(pro_breaks_2_filtered, aes (x = predicted, y= slope_post)) + geom_boxplot()+
  ggtitle("slope_post_fit") + theme_minimal()
grid <- grid.arrange(magnitude_fit_plot, post_val_fit_plot,slope_pre_plot,slope_post_plot, nrow = 1)

ggsave(grid, filename = "=01_plot_grid_boxplots_fitted_values_model_basic_38.png",
       path= "P:/workspace/jan/fire_detection/break_detection/classification_model_aa/",
       dpi = 300, width = 12, height = 6)


####################################################################################
## bring predicted breaks together with refs per year 

ids_years_ref[ids_years_ref$reference== "Other",]$reference <- "Harvest"
ids_years_ref[ids_years_ref$reference== "Wind",]$reference <- "Harvest"

ids_years_ref_predictions <- left_join(ids_years_ref, pro_breaks_2_filtered, by = c("plotid" = "smp_id","year"))
ids_years_ref_predictions$predicted <- as.character(ids_years_ref_predictions$predicted)

ids_years_ref_predictions[is.na(ids_years_ref_predictions$predicted),]$predicted <- "Undisturbed"
ids_years_ref_predictions[ids_years_ref_predictions$predicted == "No_dis", ]$predicted <- "Undisturbed"
ids_years_ref_predictions$predicted <- as.factor(ids_years_ref_predictions$predicted)
ids_years_ref_predictions$reference <- as.factor(ids_years_ref_predictions$reference)

####################################################################################
## overall AA 

source("P:/workspace/jan/code/R/BFAST/source_funs.R")
aa_1990_2020 <- get_cm_two_cols(ids_years_ref_predictions$reference,
                                ids_years_ref_predictions$predicted, "1990-2020 / basic-model")
ids_years_ref_predictions_tt <- ids_years_ref_predictions %>% subset(., year >= 2006 & year <= 2019)
aa_2006_2019 <- get_cm_two_cols(ids_years_ref_predictions_tt$reference,
                                ids_years_ref_predictions_tt$predicted, "2006-2019 / basic-model")
ids_years_ref_predictions_tt <- ids_years_ref_predictions %>% subset(., year >= 2017 & year <= 2018)
aa_2017_2018 <- get_cm_two_cols(ids_years_ref_predictions_tt$reference,
                                ids_years_ref_predictions_tt$predicted, "2017-2018 / basic-model")

## flextable
### bind and group dfs 
cms <- rbind(aa_1990_2020,aa_2006_2019,aa_2017_2018)
cms$Predicted <- as.integer(cms$Predicted)
cms.group <- as_grouped_data(x = cms, groups = "Time span / model")
library(webshot)

flex_models <- as_flextable(cms.group) %>% 
  theme_booktabs() %>%
  hline(part = 'header', border = officer::fp_border(color = "black", width = 3)) %>% 
  vline(j=c("Disturbance","Undisturbed","Predicted"),   border = officer::fp_border(color= "black", style = "solid", width = 1.45)) %>%
  vline(j=c("Fire","Harvest","Reference","Commission"),   border = officer::fp_border(color= "gray", style = "dashed", width = 1.45)) %>%
  colformat_double(j = c("Predicted"), digits = 0) %>%
  colformat_double(j = c("Omission","Commission","Sensitivity","Specificity","OA"), digits = 2) %>%
  
  hline(i = ~Disturbance %in% c("Fire","Harvest"), border = officer::fp_border(color= "gray", style = "dashed")) %>%
  # 
  # bg(i = ~ Disturbance == "Fire" ,j = ~ Fire, bg=bg_picker_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" ,j = ~ Harvest, bg=bg_picker_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ No_Dis, bg=bg_picker_no_dis) %>%
  # ## Omission 
  # bg(i = ~ Disturbance == "Fire" , j = ~ Omission, bg=bg_picker_omi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Omission, bg=bg_picker_omi_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ Omission,  bg=bg_picker_omi_no_dis) %>%
  # ## commission 
  # bg(i = ~ Disturbance == "Fire" ,  j = ~ Commission, bg=bg_picker_comi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Commission,  bg=bg_picker_comi_harvest) %>%
# bg(i = ~ Disturbance == "No_Dis" , j = ~ Commission, bg=bg_picker_comi_no_dis) %>%
save_as_image("P:/workspace/jan/fire_detection/break_detection/classification_model_aa/01_model_basic_38_feature_3_time_Spans.png",
              zoom = 3, webshot = "webshot")



####################################################################################
## FRIEDHOF 

## left join with refs, and bring to 3 ref level 
pro_breaks_1 <- left_join(pro_breaks_1, refs, by = c("meta_closest_ref_id" = "dis_id") )
pro_breaks_1[is.na(pro_breaks_1$change_process_brandsat),]$change_process_brandsat <- "No_dis"
pro_breaks_1[pro_breaks_1$change_process_brandsat== "Other",]$change_process_brandsat <- "Harvest"
pro_breaks_1[pro_breaks_1$change_process_brandsat== "Wind",]$change_process_brandsat <- "Harvest"

## left join with refs, and bring to 3 ref level 
pro_breaks_2 <- left_join(pro_breaks_2, refs, by = c("meta_closest_ref_id" = "dis_id") )
pro_breaks_2[is.na(pro_breaks_2$change_process_brandsat),]$change_process_brandsat <- "No_dis"
pro_breaks_2[pro_breaks_2$change_process_brandsat== "Other",]$change_process_brandsat <- "Harvest"
pro_breaks_2[pro_breaks_2$change_process_brandsat== "Wind",]$change_process_brandsat <- "Harvest"

## left join with refs, and bring to 3 ref level 
pro_breaks_3 <- left_join(pro_breaks_3, refs, by = c("meta_closest_ref_id" = "dis_id") )
pro_breaks_3[is.na(pro_breaks_3$change_process_brandsat),]$change_process_brandsat <- "No_dis"
pro_breaks_3[pro_breaks_3$change_process_brandsat== "Other",]$change_process_brandsat <- "Harvest"
pro_breaks_3[pro_breaks_3$change_process_brandsat== "Wind",]$change_process_brandsat <- "Harvest"


## replace infinite 
pro_breaks_1[mapply(is.infinite, pro_breaks_1)] <- 0
pro_breaks_2[mapply(is.infinite, pro_breaks_2)] <- 0
pro_breaks_3[mapply(is.infinite, pro_breaks_3)] <- 0


## subsample if wanted

## whatis the smallest group? 1ß2ß in breaks 2
# pro_breaks_3 %>% group_by(change_process_brandsat) %>% tally()
# 
# pro_breaks_1 <- pro_breaks_1 %>% group_by(change_process_brandsat) %>% sample_n(1020) %>% as.data.frame()
# pro_breaks_2 <- pro_breaks_2 %>% group_by(change_process_brandsat) %>% sample_n(1020) %>% as.data.frame()
# pro_breaks_3 <- pro_breaks_3 %>% group_by(change_process_brandsat) %>% sample_n(1020) %>% as.data.frame()
# 

## -____________________________________________________________________________
##                                 Model
## same model for each version ----
## _____________________________________________________________________________
## Ref Model with all features 

library(randomForest)
names_for_model <- names(pro_breaks_1)[c(12:25,47:52, 74:79,101:106,128:133)]
length(names_for_model)

source("P:/workspace/jan/code/R/BFAST/source_funs.R")
set.seed(202)
t1 <- Sys.time()
model_1 <- randomForest(x=pro_breaks_1[,colnames(pro_breaks_1) %in% names_for_model], y= as.factor(pro_breaks_1$change_process_brandsat), do.trace = TRUE)
model_2 <- randomForest(x=pro_breaks_2[,colnames(pro_breaks_2) %in% names_for_model], y= as.factor(pro_breaks_2$change_process_brandsat), do.trace = TRUE)
model_3 <- randomForest(x=pro_breaks_3[,colnames(pro_breaks_3) %in% names_for_model], y= as.factor(pro_breaks_3$change_process_brandsat), do.trace = TRUE)
t2 <- Sys.time()
difftime(t2, t1, units = "mins")

## get predicted data
pro_breaks_1$predicted <- model_1$predicted
pro_breaks_2$predicted <- model_2$predicted
pro_breaks_3$predicted <- model_3$predicted

## safe model for calculations on tile
saveRDS(model_1, "P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022/model_min_test_equal_sample.rsd")
saveRDS(model_2, "P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022_one_year_expansion/model_min_test_equal_sample.rds")
saveRDS(model_3, "P:/workspace/jan/fire_detection/model_disturbance_classification/h40_till_2022_variable_expansion/model_min_test_equal_sample.rsd")

varImpPlot(model_1)
varImpPlot(model_2)
varImpPlot(model_3)


# ___________________________________________________________________________________________
#

#### Auswertung 
breaks_1_aa <- pro_breaks_1[,c(1:12,68:ncol(pro_breaks_1))]
breaks_1_aa %>%
  filter(ref_label == "Fire") %>%
  filter(predicted != "Fire") %>%
  arrange(flw)
ggplot(breaks_1_aa, aes(date.x,date.y)) + geom_point()

breaks_2_aa <- pro_breaks_2[,c(1:12,68:ncol(pro_breaks_2))]
breaks_2_aa %>%
  filter(ref_label == "Fire") %>%
  filter(predicted != "Fire") %>%
  arrange(flw)
ggplot(breaks_2_aa, aes(date.x,date.y)) + geom_point()

breaks_3_aa <- pro_breaks_3[,c(1:12,68:ncol(pro_breaks_3))]
breaks_3_aa %>%
  filter(ref_label == "Fire") %>%
  filter(predicted != "Fire") %>%
  arrange(flw)
ggplot(breaks_3_aa, aes(date.x,date.y)) + geom_point()


# ___________________________________________________________________________________________
#
## Model aa 
source("P:/workspace/jan/code/R/BFAST/source_funs.R")
#install.packages("caret")
library(caret)
# install.packages("flextable")
library(flextable)
cm_1    <- get_cm(model_1,"No expansion")
cm_2    <- get_cm(model_2,"One year expansion")
cm_3    <- get_cm(model_3,"Variable expansion")


### bind and group dfs 
cms <- rbind(cm_1,cm_2,cm_3)
cms$Predicted <- as.integer(cms$Predicted)
cms.group <- as_grouped_data(x = cms, groups = "model")
library(webshot)

flex_models <- as_flextable(cms.group) %>% 
  theme_booktabs() %>%
  hline(part = 'header', border = officer::fp_border(color = "black", width = 3)) %>% 
  vline(j=c("Disturbance","No_Dis","Predicted"),   border = officer::fp_border(color= "black", style = "solid", width = 1.45)) %>%
  vline(j=c("Fire","Harvest","Reference","Commission"),   border = officer::fp_border(color= "gray", style = "dashed", width = 1.45)) %>%
  colformat_double(j = c("Predicted"), digits = 0) %>%
  colformat_double(j = c("Omission","Commission","Sensitivity","Specificity","OA"), digits = 2) %>%
  
  hline(i = ~Disturbance %in% c("Fire","Harvest"), border = officer::fp_border(color= "gray", style = "dashed")) %>%
  # 
  # bg(i = ~ Disturbance == "Fire" ,j = ~ Fire, bg=bg_picker_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" ,j = ~ Harvest, bg=bg_picker_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ No_Dis, bg=bg_picker_no_dis) %>%
  # ## Omission 
  # bg(i = ~ Disturbance == "Fire" , j = ~ Omission, bg=bg_picker_omi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Omission, bg=bg_picker_omi_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ Omission,  bg=bg_picker_omi_no_dis) %>%
  # ## commission 
  # bg(i = ~ Disturbance == "Fire" ,  j = ~ Commission, bg=bg_picker_comi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Commission,  bg=bg_picker_comi_harvest) %>%
# bg(i = ~ Disturbance == "No_Dis" , j = ~ Commission, bg=bg_picker_comi_no_dis) %>%
save_as_image("P:/workspace/jan/fire_detection/break_detection/break_detection_plots/vergleich_3_versions_same_model_equal_sample_size.png",
              zoom = 3, webshot = "webshot")
library(webshot)
#install.packages("webshot")
## ----------------------------------------------------------------------------
##                          Recombine Model Results 
##                              With Ref data

## that way a global df is created
## should be possible to derive all reference level information from there

## subset pro breaks 
pro_breaks_1_sub <- pro_breaks_1 %>% 
  select("smp_id", "tile","x","y", "n_bps", "id_break", "is_there_a_ref", "ref_label",
         "date.x", "doy", "year", "pre_val_fit", "post_val_fit", "magnitude_fit",
         "relativ_magnitude_fit",    "slope_pre" ,"slope_post","meta_is_there_ref", "meta_how_many_ref","tsync_id", "predicted")
pro_breaks_2_sub <- pro_breaks_2 %>% 
  select("smp_id", "tile","x","y", "n_bps", "id_break", "is_there_a_ref", "ref_label",
         "date.x", "doy", "year", "pre_val_fit", "post_val_fit", "magnitude_fit",
         "relativ_magnitude_fit",    "slope_pre" ,"slope_post","meta_is_there_ref", "meta_how_many_ref","tsync_id", "predicted")
pro_breaks_3_sub <- pro_breaks_3 %>% 
  select("smp_id", "tile","x","y", "n_bps", "id_break", "is_there_a_ref", "ref_label",
         "date.x", "doy", "year", "pre_val_fit", "post_val_fit", "magnitude_fit",
         "relativ_magnitude_fit",    "slope_pre" ,"slope_post","meta_is_there_ref", "meta_how_many_ref","tsync_id", "predicted")

pro_breaks_1_sub$is_detected_break <- 1   
pro_breaks_2_sub$is_detected_break <- 1  
pro_breaks_3_sub$is_detected_break <- 1  

refs_with_breaks_1 <- left_join(refs, pro_breaks_1_sub, by = "tsync_id") 
refs_with_breaks_1[is.na(refs_with_breaks_1$is_detected_break),]$is_detected_break <- 0
refs_with_breaks_1 <- refs_with_breaks_1[!duplicated(refs_with_breaks_1),]
refs_with_breaks_1$is_predicted_as_fire <- ifelse(refs_with_breaks_1$predicted == "Fire",1,0)
refs_with_breaks_1[is.na(refs_with_breaks_1$is_predicted_as_fire),]$is_predicted_as_fire <- 0
refs_with_breaks_1$is_predicted_as_harvest <- ifelse(refs_with_breaks_1$predicted == "Harvest",1,0)
refs_with_breaks_1[is.na(refs_with_breaks_1$is_predicted_as_harvest),]$is_predicted_as_harvest <- 0

refs_with_breaks_2 <- left_join(refs, pro_breaks_2_sub, by = "tsync_id") 
refs_with_breaks_2[is.na(refs_with_breaks_2$is_detected_break),]$is_detected_break <- 0
refs_with_breaks_2 <- refs_with_breaks_2[!duplicated(refs_with_breaks_2),]
refs_with_breaks_2$is_predicted_as_fire <- ifelse(refs_with_breaks_2$predicted == "Fire",1,0)
refs_with_breaks_2[is.na(refs_with_breaks_2$is_predicted_as_fire),]$is_predicted_as_fire <- 0
refs_with_breaks_2$is_predicted_as_harvest <- ifelse(refs_with_breaks_2$predicted == "Harvest",1,0)
refs_with_breaks_2[is.na(refs_with_breaks_2$is_predicted_as_harvest),]$is_predicted_as_harvest <- 0

refs_with_breaks_3 <- left_join(refs, pro_breaks_3_sub, by = "tsync_id") 
refs_with_breaks_3[is.na(refs_with_breaks_3$is_detected_break),]$is_detected_break <- 0
refs_with_breaks_3 <- refs_with_breaks_3[!duplicated(refs_with_breaks_3),]
refs_with_breaks_3$is_predicted_as_fire <- ifelse(refs_with_breaks_3$predicted == "Fire",1,0)
refs_with_breaks_3[is.na(refs_with_breaks_3$is_predicted_as_fire),]$is_predicted_as_fire <- 0
refs_with_breaks_3$is_predicted_as_harvest <- ifelse(refs_with_breaks_3$predicted == "Harvest",1,0)
refs_with_breaks_3[is.na(refs_with_breaks_3$is_predicted_as_harvest),]$is_predicted_as_harvest <- 0

## look which fire refs got not detected? 
fires_not_detected <- refs_with_breaks_2 %>%
  filter(is_detected_break == 0) %>%
  filter(change_process_brandsat == "Fire")

# plot_not_detected <- ggplot(fires_not_detected) + geom_bar(aes(x=image_year)) +
#   theme_minimal() +
#   labs(title = "Count of fire references not detected, per year")
# ggsave(plot_not_detected, filename = "plot_not_detected_fire_reference_per_year.png",
#        path= "P:/workspace/jan/fire_detection/Accuracy_assesement/AA_Break_Detection/01_not_detected_references",
#        dpi = 300, width = 12, height = 8)


## realtive not detected per year 
ee_1 <- refs_with_breaks_1 %>% 
  filter(change_process_brandsat == "Harvest")%>%
  group_by(image_year) %>%
  summarise(n_all_harvest_ref = n(),n_is_detected = sum (is_detected_break),
            n_is_predicted_as_harvest = sum(is_predicted_as_harvest)) %>% as.data.frame() 
ee_2 <- refs_with_breaks_2 %>% 
  filter(change_process_brandsat == "Harvest")%>%
  group_by(image_year) %>%
  summarise(n_all_harvest_ref = n(),n_is_detected = sum (is_detected_break),
            n_is_predicted_as_harvest = sum(is_predicted_as_harvest)) %>% as.data.frame() 
ee_3 <- refs_with_breaks_3 %>% 
  filter(change_process_brandsat == "Harvest")%>%
  group_by(image_year) %>%
  summarise(n_all_harvest_ref = n(),n_is_detected = sum (is_detected_break),
            n_is_predicted_as_harvest = sum(is_predicted_as_harvest)) %>% as.data.frame() 

ee_1$year <- as.Date(as.yearmon((ee_1$image_year)))  
ee_1_melt <- melt(ee_1, id.vars = c('image_year','year'))
ee_2$year <- as.Date(as.yearmon((ee_2$image_year)))  
ee_2_melt <- melt(ee_2, id.vars = c('image_year','year'))
ee_3$year <- as.Date(as.yearmon((ee_3$image_year)))  
ee_3_melt <- melt(ee_3, id.vars = c('image_year','year'))


# plot_detected_and_predicted <- ggplot(ee_3_melt, aes(x = image_year, y = value, fill = variable)) + geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.9) + theme_minimal() +
#    scale_y_continuous(expand = c(0, 0)) +  scale_fill_manual("legend", values = c("n_all_harvest_ref" = "black", "n_is_detected" = "darkslategray4", "n_is_predicted_as_fire" = "orange4")) +
#   theme(text = element_text(size=20),
#         axis.text.x = element_text(angle=90, hjust=1), legend.position = 'top')+
#         ggtitle ( "Predicted Fire ref / year", subtitle = "variable expansion") +
#   scale_x_continuous(breaks=seq(1985, 2020, 1)) 
# 
# # plot_detected_and_predicted
# ggsave(plot_detected_and_predicted, filename = "plot_not_detected_and_not_clsssified_fire_reference_per_year_ovariable_expansion.png",
#        path= "P:/workspace/jan/fire_detection/plots/",
#        dpi = 300, width = 12, height = 8)
# 

##  Difference Plot predicted 
ee_melt_all <- left_join(ee_1_melt, ee_2_melt, by = c("year", "variable", "image_year"))
ee_melt_all <- left_join(ee_melt_all, ee_3_melt, by = c("year", "variable",  "image_year"))
colnames(ee_melt_all) <- c("image_year","year","variable","no_expansion","one_year_expansion","variable_expansion")
ee_melt_all$dif_1_2 <- ee_melt_all$one_year_expansion - ee_melt_all$no_expansion
ee_melt_all$dif_1_3 <- ee_melt_all$variable_expansion - ee_melt_all$no_expansion

plot_detected_and_predicted_compare <- ggplot(ee_melt_all, aes(x = image_year, y = dif_1_3, fill = variable)) + geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.9) + theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +  scale_fill_manual("legend", values = c("n_all_fire_ref" = "black", "n_is_detected" = "darkslategray4", "n_is_predicted_as_harvest" = "orange4")) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1), legend.position = 'top')+
  ggtitle ( "Predicted HArvest ref / year", subtitle = "Comparison No Ex / variable") +
  scale_x_continuous(breaks=seq(1985, 2020, 1)) 

plot_detected_and_predicted_compare
ggsave(plot_detected_and_predicted_compare, filename = "plot_not_detected_and_not_clsssified_harvest_reference_per_year_compare_1_3.png",
       path= "P:/workspace/jan/fire_detection/plots/",
       dpi = 300, width = 12, height = 8)
## FRIEDHOF 

# 
# 
# ## bind with other samples 
# pro_breaks$predicted <- NA
# pro_breaks$used_in_model <- 0
# breaks_sample$used_in_model <- 1
# ## recombine samples used in equal model with other samples 
# breaks_re_combined <- rbind(breaks_sample, pro_breaks[!paste0(pro_breaks$global_break_id) %in% paste0(breaks_sample$global_break_id),])
# 
# ## do prediction for others 
# breaks_re_combined[breaks_re_combined$used_in_model == 0,]$predicted <- predict(model_equal_fit_nbr_tcb, 
#                                                                                 cbind (breaks_re_combined[breaks_re_combined$used_in_model == 0,c(12:18)], breaks_re_combined[breaks_re_combined$used_in_model == 0,grepl("NBR", colnames(breaks_re_combined))], 
#                                         breaks_re_combined[breaks_re_combined$used_in_model == 0,grepl("TCB", colnames(breaks_re_combined))]))
# 
# ## combine with refs that have no breaks 
# colnames(breaks_re_combined)[colnames(breaks_re_combined) == "meta_closest_ref_id"] <- "dis_id"
# 
# 
# 
# refs_sub <- refs[,!colnames(refs) == "date"]
# refs_sub$predicted <- "not_detected"
# breaks_re_combined_sub <- breaks_re_combined[,colnames(breaks_re_combined) %in% colnames(refs_sub)]
# 
# ## 
# breaks_with_all_refs <- rbind(breaks_re_combined_sub, refs_sub[!refs_sub$dis_id %in% breaks_re_combined_sub$dis_id,])
# unique(breaks_with_all_refs$predicted)
# 
# ## look at predictions for fires we have sizes for
# breaks_with_all_refs[breaks_with_all_refs$change_process_brandsat== "Other",]$change_process_brandsat <- "Harvest"
# breaks_with_all_refs[breaks_with_all_refs$change_process_brandsat== "Wind",]$change_process_brandsat <- "Harvest"
# breaks_with_all_refs_fire <- breaks_with_all_refs[breaks_with_all_refs$change_process_brandsat == "Fire" & !is.na(breaks_with_all_refs$flw),]
# breaks_with_all_refs_harvest <- breaks_with_all_refs[breaks_with_all_refs$change_process_brandsat == "Harvest",]
# 
# unique(breaks_with_all_refs_fire$predicted)
# 
# ## ok now do a plot ... what happened to fire refs?
# zz <- breaks_with_all_refs_fire %>% 
#   count(predicted) %>% 
#   group_by(predicted) %>% as.data.frame() 
# zz$percent <- (zz$n * 100) / sum(zz$n)
# zz$Reference <- "fire"
# zzh <- breaks_with_all_refs_harvest %>% 
#   count(predicted) %>% 
#   group_by(predicted) %>% as.data.frame() 
# zzh$percent <- (zzh$n * 100) / sum(zzh$n)
# zzh$Reference <- "harvest"
# zz <- rbind(zz, zzh)
# 
# # remotes::install_github("coolbutuseless/ggpattern")
# library(ggpattern)
# 
# oo <- ggplot(zz) + geom_bar(aes(x = Reference, y = percent, fill = predicted, pattern = predicted), stat = "identity", size = 0.5, width = 0.3, alpha = 0.8) + 
#   geom_text(data = zz,aes(x = Reference, y = as.integer(percent),  fill = predicted, label = paste0(sprintf("%1.1f", percent),"%")), size = 5,
#             position=position_stack(vjust=0.7) ) + scale_fill_manual(values = c("red4","olivedrab","steelblue4","bisque2")) +
# theme_bw() + theme(text = element_text(size = 20))  
# ggsave(oo, filename = "plot_percent_classified_and_detected.png", path = "P:/workspace/jan/fire_detection/Accuracy_assesement/AA_Break_Classification/", height = 7, width = 6)
# 
# 
# 
# ## safe selected model 
# model_equal_fit_nbr_tcb$call
# saveRDS(model_equal_fit_nbr_tcb, "P:/workspace/jan/fire_detection/model_disturbance_classification/model_equal_fit_nbr_tcb.rds")
# colnames(ii)
# ncol(ii)

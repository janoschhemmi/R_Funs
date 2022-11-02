source("bfast/dev/source_funs_model.R")
## make model for bfast breaks
require(randomForest)

# file names and paths
version_tag <- "landsat-S2-nbr-h40-o1-bp3-expansion_revisited"
fn_refs <- "P:/workspace/jan/fire_detection/break_detection/break_detection_tables/landsat-S2-nbr-h40-o1-bp3-expansion_revisited/bfast_metrics_with_tsync_revisited.csv"
outpath <- "p:/workspace/jan/fire_detection/break_detection/classification_model/"


## read bfast - timesync combined
breaks <- read.csv2(fn_refs)
breaks <- na.omit(breaks)
breaks_L_S2 <- breaks
breaks <- breaks %>% mutate(brandsat_ref = ifelse(abs(tsync1_datediff) >= 365,"No-Dis", 
                                            ifelse(tsync1_change != "Harvest_salvage", tsync1_change,
                                             ifelse(abs(tsync2_datediff) <= 365,tsync2_change, tsync1_change ))))        
                                                 
breaks$brandsat_ref[breaks$brandsat_ref == "Harvest_salvage"] <- "Harvest"                              
breaks$brandsat_ref[breaks$brandsat_ref == "Fire_salvage"] <- "Fire"     
breaks[breaks$brandsat_ref == "Fire_salvage",]
breaks[breaks$id == 3000,]
# breaks$brandsat_ref[breaks$brandsat_ref == "Wind"] <- "Harvest"


table(breaks$brandsat_ref)
breaks[breaks == -Inf] <- -1
breaks[breaks == Inf] <- 1

## filter foe 2021
breaks <- breaks[breaks$breakyear <= 2020,  ]

## model ----

# construct file names
fn_model <- file.path(outpath, version_tag, "model_01_basic_mtry_15.rds")
# filter columns
names_for_model <- colnames(breaks)[c(17:185)]
names_for_model <- names_for_model[!grepl("q",names_for_model)]
names_for_model <- names_for_model[!grepl("single",names_for_model)]
names_for_model <- names_for_model[!grepl("rel",names_for_model)]

set.seed(101)
# model_basic_mtry_10 <-  randomForest(x=breaks[,colnames(breaks) %in% names_for_model], y= as.factor(breaks$brandsat_ref), do.trace = FALSE, mtry = 10)
model_basic_mtry_15 <-  randomForest(x=breaks[,colnames(breaks) %in% names_for_model], y= as.factor(breaks$brandsat_ref), do.trace = FALSE, mtry = 15)
## safe 
fn_model <- file.path(outpath, version_tag, "model_basic_mtry_15.rds")
saveRDS(model_basic_mtry_15,fn_model)
model_basic_mtry_15_L_S2 <- readRDS(fn_model)

breaks_S2 <- breaks
breaks_S2$predicted <- model_basic_mtry_15_L_S2$predicted



## ## ## ## ## ## ## ## ## 
## model for landsat 


# file names and paths
version_tag <- "landsat-nbr-h40-o1-bp3-expansion_revisited"
fn_refs <- "P:/workspace/jan/fire_detection/break_detection/break_detection_tables/landsat-nbr-h40-o1-bp3-expansion_revisited/bfast_metrics_with_tsync_revisited.csv"
outpath <- "p:/workspace/jan/fire_detection/break_detection/classification_model/"


## read bfast - timesync combined
breaks <- read.csv2(fn_refs)
breaks <- na.omit(breaks)
breaks <- breaks %>% mutate(brandsat_ref = ifelse(abs(tsync1_datediff) >= 365,"No-Dis", 
                                                  ifelse(tsync1_change != "Harvest_salvage", tsync1_change,
                                                         ifelse(abs(tsync2_datediff) <= 365,tsync2_change, tsync1_change ))))        

breaks$brandsat_ref[breaks$brandsat_ref == "Harvest_salvage"] <- "Harvest"                              
breaks$brandsat_ref[breaks$brandsat_ref == "Fire_salvage"] <- "Fire"     
# breaks$brandsat_ref[breaks$brandsat_ref == "Wind"] <- "Harvest"

table(breaks$brandsat_ref)
breaks[breaks == -Inf] <- -1
breaks[breaks == Inf] <- 1

## model ----
breaks$pre_val_fit[breaks$pre_val_fit != breaks_L_S2$pre_val_fit]
cbind(breaks$pre_val_fit, breaks_L_S2$pre_val_fit)
# construct file names
fn_model <- file.path(outpath, version_tag, "model_01_basic_mtry_15.rds")
# filter columns
names_for_model <- colnames(breaks)[c(17:185)]
names_for_model <- names_for_model[!grepl("q",names_for_model)]
names_for_model <- names_for_model[!grepl("single",names_for_model)]
names_for_model <- names_for_model[!grepl("rel",names_for_model)]

set.seed(101)
model_basic_mtry_15 <-  randomForest(x=breaks[,colnames(breaks) %in% names_for_model], y= as.factor(breaks$brandsat_ref), do.trace = TRUE, mtry = 15)
model_basic_mtry_15_L <- model_basic_mtry_15
## safe 
fn_model <- file.path(outpath, version_tag, "model_basic_mtry_15.rds")
saveRDS(model_basic_mtry_15,fn_model)
model_basic_mtry_15_L <- readRDS(fn_model)

breaks_l <- breaks
breaks_l$predicted <- model_basic_mtry_15_L$predicted
model_basic_mtry_15_L

##
model_basic_mtry_15_L_S2
model_basic_mtry_15_L 

##

## get cms
cm_model_L_S2          <- get_cm(model_basic_mtry_15_L_S2, "Landsat + Sentinel-2")
cm_model_L          <- get_cm(model_basic_mtry_15_L, "Landsat")

 

cm_all <- rbind(cm_model_L_S2,cm_model_L)
cm.group <- as_grouped_data(x = cm_all, groups = "model")
print_grouped_as_flex(cm.group,"P:/workspace/jan/fire_detection/plots/LPS/AA_model_15mtry_Sentinel_vs_Landsat.png" )

# ------------------------------------------------------------------------------

## do cm for 2017 - 2021 
breaks_S2_sub <- breaks_S2[breaks_S2$breakyear >= 2017 & breaks_S2$breakyear <= 2020,]  
breaks_l_sub <- breaks_l[breaks_l$breakyear >= 2017 & breaks_l$breakyear <= 2020,]  

cm_S2 <- get_cm_two_cols(breaks_S2_sub$brandsat_ref,breaks_S2_sub$predicted,"Landsat + Sentinel-2")
cm_l <- get_cm_two_cols(breaks_l_sub$brandsat_ref,breaks_l_sub$predicted,"Landsat")


cm_all <- rbind(cm_S2,cm_l)
cm.group <- as_grouped_data(x = cm_all, groups = "Time span / model")
print_grouped_as_flex(cm.group,"P:/workspace/jan/fire_detection/plots/LPS/AA_model_15mtry_Sentinel_vs_Landsat_2017-2020.png" )


## do compare time difference for detected 
breaks_S2_sub$sensor <- "Sentinel-2"
breaks_l_sub$sensor <- "Landsat"
dev.off()
breaks_sub <- rbind(breaks_S2_sub, breaks_l_sub)
unique(breaks_sub$brandsat_ref)
breaks_sub[breaks_sub == -Inf] <- -1
breaks_sub[breaks_sub == Inf] <- 1

dens_plot <- ggplot( data = breaks_sub %>% filter(brandsat_ref == "Fire" & predicted == "Fire"), aes(x = tsync1_datediff, color = sensor, group = sensor)) + 
  geom_density(adjust=0.4, kernel = c("rectangular")) + 
  theme_minimal() + 
  xlim(-200,200) +
  xlab("days")
  ggtitle("distance of detected fires to referenced fires 2017 - 2021")
  
  
ggsave(dens_plot, filename = "plot_distance_to_fire_refs_2017_2020.png", path = "P:/workspace/jan/fire_detection/plots/LPS/", height = 7, width = 6)
  
hist_plot <- ggplot( data = breaks_sub %>% filter(brandsat_ref == "Fire" & predicted == "Fire"), aes(x = tsync1_datediff)) + 
  geom_histogram(aes(color = sensor, fill = sensor), 
                 position = "identity", bins = 25, alpha = 0.4) +
  theme_minimal() + 
  xlim(-200,200) +
  xlab("days")
ggtitle("distance of detected fires to referenced fires 2017 - 2021")

ggsave(hist_plot, filename = "plot_distance_to_fire_refs_2017_2020_hist.png", path = "P:/workspace/jan/fire_detection/plots/LPS/", height = 7, width = 6)


## how many percent of fires not detected 
refs <- read.csv2("P:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_post2_revisited.csv")
#refs_l <- read.csv2("P:/workspace/jan/fire_detection/break_detection/break_detection_tables/landsat-nbr-h40-o1-bp3-expansion_revisited/bfast_metrics_with_tsync_revisited.csv")
#refs_s <- read.csv2("P:/workspace/jan/fire_detection/break_detection/break_detection_tables/landsat-S2-nbr-h40-o1-bp3-expansion_revisited/bfast_metrics_with_tsync_revisited.csv")

refs[refs$plotid == 1022,]
breaks_l[breaks_l$id == 1022,]
breaks_S2[breaks_S2$id == 103,]

table(breaks_S2$brandsat_ref)
table(breaks_l$brandsat_ref)

## check if refs have breakpoint
refs_l <- left_join(refs, breaks_l, by = c("plotid" = "id"))
refs_l$has_break_point <- ifelse(is.na(refs_l$break_date), "No", "Yes")

## create ref change id
refs_l$change_id <- as.integer(paste0(refs_l$plotid,refs_l$image_year))
refs_l$time_difference <- ifelse(is.na(refs_l$break_date), NA,as.integer(difftime(refs_l$break_date,refs_l$change_date,units = "days" )))

## look per change id if any breakpoint within 1 year 
refs_l$time_difference_less_one_year <- ifelse(abs(refs_l$time_difference) < 365,1,0)


## sort refs to keep 
#rr <- refs_l %>% group_by(change_id) %>% slice_min(n = 1, time_difference)
refs_l_single <- rbind(refs_l[is.na(refs_l$time_difference),],refs_l %>% group_by(change_id) %>% slice_min(n = 1, abs(time_difference)))
refs_l_single$time_difference_less_one_year <- ifelse(refs_l_single$has_break_point == "No", 0, refs_l_single$time_difference_less_one_year)
refs_l_single <- refs_l_single[refs_l_single$disturbance == 1,]

## filter for excluded plots 
refs_l_single <- refs_l_single[!refs_l_single$plotid %in% c(3142, 3144, 1882, 3038, 1166, 1626, 1819, 2121, 1826, 2059, 2192, 75,424, 1031, 1136, 928, 966),]

## filter time window 
refs_l_single_2017_2020 <- refs_l_single[refs_l_single$image_year <= 2020 & refs_l_single$image_year >= 2017,]
refs_l_single_2017_2020[is.na(refs_l_single_2017_2020$predicted),]$predicted <- "No-Dis"

refs_l_single_2017_2020[is.na(refs_l_single_2017_2020$predicted),]$predicted <- "No-Dis"
refs_l_single_2017_2020[is.na(refs_l_single_2017_2020$brandsat_ref),]$brandsat_ref <- "No-Dis"

## percent detected breakpoints 
nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$brandsat_ref == "Fire",])
nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$brandsat_ref == "Fire" & refs_l_single_2017_2020$time_difference_less_one_year == 1,]) /
  nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$brandsat_ref == "Fire",])  
## 94.99 % of fire have breakpoint with Landsat  
refs_l_single_2017_2020[refs_l_single_2017_2020$plotid == 1022,]

### Sentinel

refs <- read.csv2("P:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_post2_revisited.csv")

## check if refs have breakpoint 
refs_s <- left_join(refs, breaks_S2, by = c("plotid" = "id"))
refs_s$has_break_point <- ifelse(is.na(refs_s$break_date), "No", "Yes")

## create ref change id
refs_s$change_id <- as.integer(paste0(refs_s$plotid,refs_s$image_year))
refs_s$time_difference <- ifelse(is.na(refs_s$break_date), NA,as.integer(difftime(refs_s$break_date,refs_s$change_date,units = "days" )))

## look per change id if any breakpoint within 1 year 
refs_s$time_difference_less_one_year <- ifelse(abs(refs_s$time_difference) < 365,1,0)

## sort refs to keep 
#rr <- refs_l %>% group_by(change_id) %>% slice_min(n = 1, time_difference)
refs_s_single <- rbind(refs_s[is.na(refs_s$time_difference),],refs_s %>% group_by(change_id) %>% slice_min(n = 1, abs(time_difference)))
refs_s_single$time_difference_less_one_year <- ifelse(refs_s_single$has_break_point == "No", 0, refs_s_single$time_difference_less_one_year)
refs_s_single <- refs_s_single[refs_s_single$disturbance == 1,]

## filter for excluded plots 
refs_s_single <- refs_s_single[!refs_s_single$plotid %in% c(3142, 3144, 1882, 3038, 1166, 1626, 1819, 2121, 1826, 2059, 2192, 75,424, 1031, 1136, 928, 966),]

## filter time window 
refs_s_single_2017_2020 <- refs_s_single[refs_s_single$image_year <= 2020 & refs_s_single$image_year >= 2017,]
refs_s_single_2017_2020[is.na(refs_s_single_2017_2020$predicted),]$predicted <- "No-Dis"
refs_s_single_2017_2020[is.na(refs_s_single_2017_2020$brandsat_ref),]$brandsat_ref <- "No-Dis"

## percent detected breakpoints 
refs_s_single_2017_2020 <- na.omit(refs_s_single_2017_2020)
refs_l_single_2017_2020 <- na.omit(refs_l_single_2017_2020)



nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$brandsat_ref == "Fire" & refs_l_single_2017_2020$time_difference_less_one_year == 1,]) /
  nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$brandsat_ref == "Fire",])  
## 94.99

nrow(refs_s_single_2017_2020[refs_s_single_2017_2020$brandsat_ref == "Fire" & refs_s_single_2017_2020$time_difference_less_one_year == 1,]) /
  nrow(refs_s_single_2017_2020[refs_s_single_2017_2020$brandsat_ref == "Fire",])  
## 99.07 % of fire have breakpoint with Landsat  

nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$predicted == "Fire" & refs_l_single_2017_2020$time_difference_less_one_year == 1,]) /
  nrow(refs_l_single_2017_2020[refs_l_single_2017_2020$brandsat_ref == "Fire",])  
## 91.61 % predicted as fire
nrow(refs_s_single_2017_2020[refs_s_single_2017_2020$predicted == "Fire" & refs_s_single_2017_2020$time_difference_less_one_year == 1,]) /
  nrow(refs_s_single_2017_2020[refs_s_single_2017_2020$brandsat_ref == "Fire",]) 
## 94.04 % predicted as fire

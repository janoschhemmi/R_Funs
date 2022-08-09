
### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")


library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
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
library(terra)
library(readr)
library(purrr)

source("data_handling/create_dl_dfs/bm_tsync_prepare.R")


## load timesync

# file names and paths
tsync_p <- "p:/timesync/bb/"
out_csv_file <- "p:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_post3_revisited.csv"

# read data
bb_comment <- read_csv(file.path(tsync_p, "tsync_plots_bb_jan_interpretations_comment.csv"), show_col_types = FALSE)
fires_comments <- read_csv(file.path(tsync_p, "tsync_fires_interpretations_comment.csv"), show_col_types = FALSE)
fires_post1_comments <- read_csv(file.path(tsync_p, "tsync_fires_post_training1_interpretations_comment.csv"), show_col_types = FALSE)
fires_post2_comments <- read_csv(file.path(tsync_p, "tsync_fires_post_training2_interpretations_comment.csv"), show_col_types = FALSE)

df_comments <- rbind(bb_comment, fires_comments, fires_post1_comments,fires_post2_comments)


bb_interpretations <- read_csv(file.path(tsync_p, "tsync_plots_bb_jan_interpretations.csv"), show_col_types = FALSE)
fires_interpretations <- read_csv(file.path(tsync_p, "tsync_fires_interpretations.csv"), show_col_types = FALSE)
fires_post1interpretations <- read_csv(file.path(tsync_p, "tsync_fires_post_training1_interpretations.csv"), show_col_types = FALSE)
fires_post2interpretations <- read_csv(file.path(tsync_p, "tsync_fires_post_training2_interpretations.csv"), show_col_types = FALSE)

df_inters <- rbind(fires_interpretations, fires_post1interpretations,fires_post2interpretations, bb_interpretations)
df_inters[df_inters$plotid == "3067",]
# 
rm(fires_interpretations, bb_interpretations,fires_post1interpretations,fires_post2interpretations,
   bb_comment, fires_comments,fires_post1_comments, fires_post2_comments)
# 
# bm_tsync_prepare(df_inters, df_comments, out_csv_file, overwrite = T)

## df_inters 
table(df_inters$change_process)

##
classes_to_include <- c("Fire","Harvest","Other","Stable")
number_of_samples  <- 200
path_LandSat_ts <- "P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_2_with_outliers_till_2022_post1.csv"
padding_days    <- 10
years_prior_stable <- 4
ids_to_exclude <-  c(3142, 3144, 1882, 3038, 1166, 1626, 1819, 2121, 1826, 2059, 2192, 75,424, 1031, 1136, 928, 966, 539, 123, 329, 712, 2110)
index_list <- c("NBR", "NDV", "TCW", "TCG", "TCB")

L_ts            <- read.csv2(path_LandSat_ts)

bm_select_ref_and_extract_dl_df <- function(L_ts, df_inters,classes_to_include, number_of_samples, padding_days = padding_days, 
                          years_prior_stable, ids_to_exclude = ids_to_exclude, index_list
                          ){
  
  set.seed(102)
  ## filter_ref
  df_inters_filtered <- df_inters %>% 
    filter(change_process %in% classes_to_include) %>%
    group_by(plotid) %>%
    mutate(dist_prior = image_year - lag(image_year)) %>%
    ungroup() %>%
    mutate(instance = row_number())
  
  ## exclude refs that are always exlcuded
  df_inters_filtered <- df_inters_filtered[!df_inters_filtered$plotid %in% ids_to_exclude,]
  df_inters_filtered <- df_inters_filtered[df_inters_filtered$plotid %in% L_ts$id,]
  
  ## sample disturbances
  df_disturbances <- df_inters_filtered %>%
    filter(change_process %in% classes_to_include[classes_to_include != "Stable"]) %>%
    filter(image_year > 1989) %>%
    group_by(change_process) %>%
    do(sample_n(.,number_of_samples))
    
  ## sample stables (randomly in stable time window)
  df_stable <- df_inters_filtered %>%
    filter(change_process %in% c ("Stable")) %>%
    mutate(dist_prior = replace_na(dist_prior, 0)) %>%
    filter(dist_prior >= years_prior_stable) %>%
    group_by(change_process) %>%
    do(sample_n(.,number_of_samples)) %>%
    ungroup() %>%
    mutate(image_year =  image_year - as.integer(runif(nrow(.), min = 1, max = dist_prior)),
           image_julday = as.integer(runif(nrow(.), min = 0, max = 365)))
  
  ## combine
  df <- rbind(df_disturbances, df_stable) %>%
    mutate(date_ref = strptime(paste(image_year, image_julday), format="%Y %j"))
  
  ## selct closest observation for each ref 
  closest_observations <- L_ts %>% filter(.$id %in% df$plotid) %>% 
    filter(index == "NBR") %>%
    left_join(df, by = c("id"="plotid")) %>%
    mutate(diff = abs(as.numeric(difftime(date, date_ref, units = "days")))) %>%
    group_by(id, instance) %>%
    slice(which.min((diff))) %>%
    ungroup() %>%
    select(id,date,change_process,diff, instance) %>%
    as.data.frame()
  
 
  ## get padding days for each index 
  ## left_join_by date 
  selected_ts <- L_ts %>% filter(index %in% index_list) %>%
    filter(id %in% df$plotid) %>% 
    left_join(.,closest_observations, by = c("id" = "id", "date" = "date")) #%>%
  
  GetIDsBeforeAfter <- function(x) {
    v = (x-padding_days) : (x+padding_days)
  }
  
  ## row ids with padding
  selected_row_ids <- selected_ts %>%
    mutate(row_id = row_number()) %>%
    filter((!is.na(change_process))) %>%
    pull(row_id) %>%
    map(GetIDsBeforeAfter) %>%
    unlist()
  
  selected_ts_2 <- selected_ts[selected_row_ids,]
  
  
  ## expant instances and change process rows
  GetrepsBeforeAfter <- function(x) {
    times = 2 * padding_days + 1  ## how many times replicate pulled instance 
    replicated <- rep(x, times)
    return(replicated)
    }
  
  selected_reps <- selected_ts %>%
    mutate(row_id = row_number()) %>%
    filter((!is.na(.$instance))) %>%
    pull(instance) %>%
    map(GetrepsBeforeAfter) %>%
    unlist()
    
  selected_ts_2$instances_rep <- selected_reps
  
  ## add sequence
  Getsequence <- function(x) {
    times = 2 * padding_days + 1  ## how many times replicate pulled instance 
    seq <- seq(1, times)
    return(seq)
  }
  selected_sequence <- selected_ts %>%
    mutate(row_id = row_number()) %>%
    filter((!is.na(.$instance))) %>%
    pull(instance) %>%
    map(Getsequence) %>%
    unlist()
  
  selected_ts_2$time_seq <- selected_sequence
  
  selected_changes <- selected_ts %>%
    mutate(row_id = row_number()) %>%
    filter((!is.na(.$instance))) %>%
    pull(change_process) %>%
    map(GetrepsBeforeAfter) %>%
    unlist()
  selected_ts_2$changes_rep <- selected_changes

 
  return(selected_ts_2)
}




## Apply 

df_for_dl <- bm_select_ref_and_extract_dl_df(L_ts=L_ts,
                                             df_inters = df_inters,
                                classes_to_include = classes_to_include,
                                number_of_samples = number_of_samples, 
                                padding_days = padding_days, 
                                years_prior_stable = years_prior_stable, 
                                ids_to_exclude = ids_to_exclude, 
                                index_list = index_list)
table(df_for_dl$change_process)

#write.csv2(df_for_dl, "P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/01_df_for_dl_200_4_classes.csv")
#df_for_dl <- read.csv2("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/01_df_for_dl_200_4_classes.csv")

df_for_dl_wide <- df_for_dl %>% select(!c(date,change_process,diff,instance, sensor)) %>% pivot_wider(names_from = c(time_seq),values_from = value) %>%
  arrange(.,id,index) %>% as.data.frame()

y_safe <- df_for_dl_wide$changes_rep[seq(1,nrow(df_for_dl_wide),length(index_list))]
x_safe <-  df_for_dl_wide %>% select(c(seq(ncol(df_for_dl_wide)- (2 * padding_days ), ncol(df_for_dl_wide))))
## 799 samples
## anforderungen -- [samples, features, timeseries per feature]

y_safe[y_safe == "Fire"] <- 1
y_safe[y_safe == "Harvest"] <- 2
y_safe[y_safe == "Other"] <- 3
y_safe[y_safe == "Stable"] <- 4
y_safe <- as.integer(y_safe)

write.csv2(y_safe, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/02_df_y_",padding_days,".csv"),row.names = FALSE)
write.table(x_safe, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/02_df_x_",padding_days,".csv"), row.names = FALSE, col.names = TRUE, dec = ".", sep = ";")




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




## FUNS
bm_select_ref_and_extract_dl_df_moving_window <- function(L_ts, df_inters,classes_to_include, number_of_samples, padding_days = padding_days, 
                                                          years_prior_stable, ids_to_exclude = ids_to_exclude, index_list
){
  
  set.seed(85)
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
    filter(change_process %in% classes_to_include[!classes_to_include %in% c("Stable","Growth")]) %>%
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
  
  ## sample Growth (halfway in growth window)
  df_growth <- df_inters_filtered %>%
    filter(change_process %in% c ("Growth")) %>%
    mutate(dist_prior = replace_na(dist_prior, 0)) %>%
    filter(dist_prior >= (years_prior_stable-1)) %>%
    group_by(change_process) %>%
    do(sample_n(.,number_of_samples)) %>%
    ungroup() %>%
    mutate(image_year =  image_year - as.integer(dist_prior / 2),
           image_julday = as.integer(runif(nrow(.), min = 0, max = 365)))
  
  ## combine
  df <- rbind(df_disturbances, df_stable, df_growth) %>%
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




## output all time series 
bm_extract_dl_df_all_ts <- function(L_ts, df_inters,classes_to_include = c("Harvest","Other","Fire","Growth","Wind","Decline","Hydrology"), number_of_samples, ids_to_exclude = ids_to_exclude, index_list
){
  
  set.seed(102)
  df_inters_filtered <- df_inters %>% 
    filter(change_process %in% classes_to_include) %>%
    #group_by(plotid) %>%
    #mutate(dist_prior = image_year - lag(image_year)) %>%
    #ungroup() %>%
    mutate(instance = row_number())
  # 
  
  df_inters_filtered <- df_inters_filtered[!df_inters_filtered$plotid %in% ids_to_exclude,]
  df_inters_filtered <- df_inters_filtered[df_inters_filtered$plotid %in% L_ts$id,]
  
  df_inters_filtered <- df_inters_filtered %>%
    mutate(date_ref = strptime(paste(image_year, image_julday), format="%Y %j"))
  
  #head(L_ts)
  #L_ts_s <- L_ts
  #L_ts <- L_ts_s[1:400000,]
  
  ## selct closest observation for each ref 
  closest_observations <- L_ts %>% filter(.$id %in% df_inters_filtered$plotid) %>% 
    filter(index == "NBR") %>%
    left_join(df_inters_filtered, by = c("id"="plotid")) %>%
    mutate(diff = abs(as.numeric(difftime(date, date_ref, units = "days")))) %>%
    group_by(id, instance) %>%
    slice(which.min((diff))) %>%
    ungroup() %>%
    select(id,date,change_process,diff, instance) %>%
    as.data.frame()
  
  #table(closest_observations$change_process)
  
  ## get padding days for each index 
  ## left_join_by date 
  selected_ts <- L_ts %>% filter(index %in% index_list) %>%
    filter(id %in% df_inters_filtered$plotid) %>% 
    left_join(.,closest_observations, by = c("id" = "id", "date" = "date")) #%>%
  
  # GetIDsBeforeAfter <- function(x) {
  #   v = (x-padding_days) : (x+padding_days)
  # }
  
  ## row ids with padding
  # selected_row_ids <- selected_ts %>%
  #   mutate(row_id = row_number()) %>%
  #   filter((!is.na(change_process))) %>%
  #   pull(row_id) %>%
  #   map(GetIDsBeforeAfter) %>%
  #   unlist()
  # 
  # selected_ts_2 <- selected_ts[selected_row_ids,]
  
  
  ## expant instances and change process rows
  # GetrepsBeforeAfter <- function(x) {
  #   times = 2 * padding_days + 1  ## how many times replicate pulled instance 
  #   replicated <- rep(x, times)
  #   return(replicated)
  # }
  # 
  # selected_reps <- selected_ts %>%
  #   mutate(row_id = row_number()) %>%
  #   filter((!is.na(.$instance))) %>%
  #   pull(instance) %>%
  #   map(GetrepsBeforeAfter) %>%
  #   unlist()
  # 
  # selected_ts_2$instances_rep <- selected_reps
  # 
  # ## add sequence
  # Getsequence <- function(x) {
  #   times = 2 * padding_days + 1  ## how many times replicate pulled instance 
  #   seq <- seq(1, times)
  #   return(seq)
  # }
  # selected_sequence <- selected_ts %>%
  #   mutate(row_id = row_number()) %>%
  #   filter((!is.na(.$instance))) %>%
  #   pull(instance) %>%
  #   map(Getsequence) %>%
  #   unlist()
  # 
  # selected_ts_2$time_seq <- selected_sequence
  # 
  # selected_changes <- selected_ts %>%
  #   mutate(row_id = row_number()) %>%
  #   filter((!is.na(.$instance))) %>%
  #   pull(change_process) %>%
  #   map(GetrepsBeforeAfter) %>%
  #   unlist()
  # selected_ts_2$changes_rep <- selected_changes
  
  # add sequence number 
  selected_ts <- selected_ts %>%
    group_by(id, index) %>%
    mutate(time_sequence = row_number()) %>%
    ungroup() %>%
    as.data.frame()
  
  
  return(selected_ts)
}


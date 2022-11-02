## preprocessing S1 vor BA


### Functions 
create_s1_path <- function(path){
  all_path_as <- as.data.frame(list.files(path, pattern = ".tif"))
  colnames(all_path_as) <- "file"
  dd      <- as.data.frame(cbind(all_path_as$file,  str_split_fixed(all_path_as$file, pattern  = "_", 9)))
  dd$year  <- substring(dd$V9, first = 1, last = 4)
  dd$month <- substring(dd$V9, first = 5, last = 6)
  dd$day   <- substring(dd$V9, first = 7, last = 8)
  dd$date  <- paste0(dd$year,"-",dd$month,"-",dd$day)
  dd$date_as_date <- as.Date(dd$date)
  dd$orbit   <- substring(dd$V10, first = 3, last = 3)
  path_store <- dd[,c(1,7,8,11,12,13,14,15,16)]
  colnames(path_store) <- c("file","Pol","other","year","month","day","date","date_as_date", "orbit") 
  return(path_store)
}

### set date ranges 
date_1 <- as.Date("2018-07-19")
date_2 <- as.Date("2018-08-21")
date_3 <- as.Date("2018-08-31")
date_4 <- as.Date("2018-10-01")



## load all file names in folder 
path <- "P:/workspace/jan/S1/S1_data/2018_asc_resample/"
path_store_as <- create_s1_path(path)
path_des <- "P:/workspace/jan/S1/S1_data/2018_dec_resample/"
path_store_des <- create_s1_path(path_des)
path_asc_resample_ratio <- "P:/workspace/jan/S1/S1_data/2018_asc_resample_ratio/"
path_store_asc_resample_ratio <- create_s1_path(path_asc_resample_ratio)
path_des_resample_ratio <- "P:/workspace/jan/S1/S1_data/2018_dec_resample_ratio/"
path_store_des_resample_ratio <- create_s1_path(path_des_resample_ratio)

##-----------------------------------------------------------------------------
## Time range 1 
list_asc_VH_1 <- path_store_as %>%
  filter(year == "2018") %>%
  filter(Pol == "BVH") %>%
  filter(date_as_date > date_1 & date_as_date < date_2) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path,file)) %>% 
  dplyr::select(path)

list_des_VH_1 <- path_store_des %>%
  filter(year == "2018") %>%
  filter(Pol == "BVH") %>%
  filter(date_as_date > date_1 & date_as_date < date_2) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_des,file)) %>% 
  dplyr::select(path)

list_asc_VV_1 <- path_store_as %>%
  filter(year == "2018") %>%
  filter(Pol == "BVV") %>%
  filter(date_as_date > date_1 & date_as_date < date_2) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path,file)) %>% 
  dplyr::select(path)

list_des_VV_1 <- path_store_des %>%
  filter(year == "2018") %>%
  filter(Pol == "BVV") %>%
  filter(date_as_date > date_1 & date_as_date < date_2) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_des,file)) %>% 
  dplyr::select(path)


## Time range 2 
list_asc_VH_2 <- path_store_as %>%
  filter(year == "2018") %>%
  filter(Pol == "BVH") %>%
  filter(date_as_date > date_3 & date_as_date < date_4) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path,file)) %>% 
  dplyr::select(path)

list_des_VH_2 <- path_store_des %>%
  filter(year == "2018") %>%
  filter(Pol == "BVH") %>%
  filter(date_as_date > date_3 & date_as_date < date_4) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_des,file)) %>% 
  dplyr::select(path)

list_asc_VV_2 <- path_store_as %>%
  filter(year == "2018") %>%
  filter(Pol == "BVV") %>%
  filter(date_as_date > date_3 & date_as_date < date_4) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path,file)) %>% 
  dplyr::select(path)

list_des_VV_2 <- path_store_des %>%
  filter(year == "2018") %>%
  filter(Pol == "BVV") %>%
  filter(date_as_date > date_3 & date_as_date < date_4) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_des,file)) %>% 
  dplyr::select(path)

## list ratio pre fire 
list_des_ratio_1 <- path_store_des_resample_ratio %>%
  filter(year == "2018") %>%
  #filter(Pol == "BVV") %>%
  filter(date_as_date > date_1 & date_as_date < date_2) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_des_resample_ratio,file)) %>% 
  dplyr::select(path)

list_des_ratio_2 <- path_store_des_resample_ratio %>%
  filter(year == "2018") %>%
  #filter(Pol == "BVV") %>%
  filter(date_as_date > date_3 & date_as_date < date_4) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_des_resample_ratio,file)) %>% 
  dplyr::select(path)

list_asc_ratio_1 <- path_store_asc_resample_ratio %>%
  filter(year == "2018") %>%
  #filter(Pol == "BVV") %>%
  filter(date_as_date > date_1 & date_as_date < date_2) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_asc_resample_ratio,file)) %>% 
  dplyr::select(path)

list_asc_ratio_2 <- path_store_asc_resample_ratio %>%
  filter(year == "2018") %>%
  # filter(Pol == "BVV") %>%
  filter(date_as_date > date_3 & date_as_date < date_4) %>%
  dplyr::select(file) %>% 
  mutate('path' = paste0(path_asc_resample_ratio,file)) %>% 
  dplyr::select(path)


## ----------------------------------------------------------------------------
## stack images 
stack_asc_VH_1 <- stack(list_asc_VH_1$path) 
stack_des_VH_1 <- stack(list_des_VH_1$path)
stack_asc_VV_1 <- stack(list_asc_VV_1$path) 
stack_des_VV_1 <- stack(list_des_VV_1$path)
stack_asc_VH_2 <- stack(list_asc_VH_2$path) 
stack_des_VH_2 <- stack(list_des_VH_2$path)
stack_asc_VV_2 <- stack(list_asc_VV_2$path) 
stack_des_VV_2 <- stack(list_des_VV_2$path)

stack_ratio_asc_1 <- stack(list_asc_ratio_1$path)
stack_ratio_asc_2 <- stack(list_asc_ratio_2$path)
stack_ratio_des_1 <- stack(list_des_ratio_1$path)
stack_ratio_des_2 <- stack(list_des_ratio_2$path)

stack_list <- c(stack_asc_VH_1, stack_des_VH_1, stack_asc_VV_1, stack_des_VV_1,
                stack_asc_VH_2, stack_des_VH_2, stack_asc_VV_2, stack_des_VV_2,
                stack_ratio_asc_1, stack_ratio_asc_2,
                stack_ratio_des_1, stack_ratio_des_2)

stack_names <- c("stats_asc_VH_timewindow1", "stats_des_VH_timewindow1",
                "stats_asc_VV_timewindow1", "stats_des_VV_timewindow1",
                "stats_asc_VH_timewindow2", "stats_des_VH_timewindow2",
                "stats_asc_VV_timewindow2", "stats_des_VV_timewindow2",
                "stats_asc_ratio_timewindow1", "stats_asc_ratio_timewindow2",
                "stats_des_ratio_timewindow1", "stats_des_ratio_timewindow2",
                "stats_asc_VH_dif", "stats_des_VH_dif",
                "stats_asc_VV_dif", "stats_des_VV_dif",
                "stats_asc_ratio_dif", "stats_des_ratio_dif")

## stat fun
stat_fun <- function(x) {
  mean <- mean(x)

  q <- calc(x ,fun = function(x) quantile(x,
       probs = c(0.0,1,0.25,0.50,0.75),na.rm=T))
  range <- q[[2]] - q[[1]]
  range_q <- q[[5]] - q[[3]]
  stack <- stack(mean, q, range, range_q)
  return(stack)
  }

## ## Radar Burnt Difference (Post - Pre Fire Difference)
stack_list_stat <- sapply( stack_list, stat_fun)
stack_list_stat[[13]]  <- stack_list_stat[[5]] - stack_list_stat[[1]]
stack_list_stat[[14]] <- stack_list_stat[[6]] - stack_list_stat[[2]]
stack_list_stat[[15]] <- stack_list_stat[[7]] - stack_list_stat[[3]]
stack_list_stat[[16]] <- stack_list_stat[[8]] - stack_list_stat[[4]]
stack_list_stat[[17]] <- stack_list_stat[[9]] - stack_list_stat[[11]]
stack_list_stat[[18]] <- stack_list_stat[[10]] - stack_list_stat[[12]]




## write raster stacks
for(i in 1:length(stack_list_stat)){
  print(i)
  to_write <- stack_list_stat[[i]]
  
  ## set names of stack layers
  names_layer <- c("mean","min","max","q25","q50","q75","range","inter_quartile_range")
  # names_layer <- c("mean","min","max","q25","q50","q75","range","inter_quartile_range","rr","ee","ww")
  names(to_write) <- names_layer
  
  writeRaster(stack(to_write),file = paste0("P:/workspace/jan/S1/S1_data/stms_20m/",stack_names[i],".tif"),
              names(to_write), bylayer=FALSE, format='GTiff')
    
}



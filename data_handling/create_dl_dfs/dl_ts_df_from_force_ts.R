
setwd("C:/Users/geo_jahe/R_Projects/Jan/R_Funs/")
source("data_handling/create_dl_dfs/utils_dl_ts_from_force_ts.R")
source("data_handling/create_dl_dfs/dl_ts_df_from_force_ts.R")

#update.packages("tidyr")
#library(tidyr)
## #1
## load timesync

# file names and paths
tsync_p <- "p:/timesync/bb/"
#out_csv_file <- "p:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_post3_revisited.csv"


## read inters
df_inters <- read.csv2(file.path("P:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_wind.csv"))
df_inters[df_inters$change_process == "Fire_salvage",] <- "Fire"
table(df_inters$change_process)



## PreProcess
# _______________________________________________________________________________ #

## load LS 
path_LandSat_ts <- "P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_dl_2.csv"
L_ts            <- read.csv2(path_LandSat_ts)
length(intersect(df_inters$plotid, L_ts$id))

## filter fot tiles with all incides 
indicies <- unique(L_ts$index)
tiles_to_keep <- L_ts %>% group_by(tile) %>% summarise(n_index = length(unique(index))) %>% filter(n_index == max(n_index)) %>% ungroup()
L_ts <- L_ts[L_ts$tile %in% tiles_to_keep$tile, ]

## only keep samples that are in tsync ref and Landsat
ids_to_keep <- intersect(L_ts$id, df_inters$plotid)

ids_to_exclude <-  c(3712,3711, 3707, 3706, 3674, 3663, 3661, 3609, 3528, 3527, 3506, 3142,719, 3144, 1882, 3038, 1166, 1626, 1819, 2121, 1826, 2059, 2192, 75,424, 1031, 1136, 928, 966, 539, 123, 329, 712, 2110)
wind_exclude <- c(84, 85, 87, 88, 89, 91, 93, 94, 96, 97, 98, 99,100, 102, 103, 104, 106, 107, 108, 109, 110, 111, 112, 113, 114, 119, 120, 122, 126, 127, 130,  131, 134,135, 136, 139, 142, 143, 144, 150, 155, 156, 158, 164, 168, 169, 172, 177, 178, 179, 182, 183, 184, 185, 188, 189, 190, 192, 193, 194, 195, 197, 199, 201, 204, 206, 207, 208, 209, 210, 211, 213, 221, 222, 223, 224, 225, 228, 229, 231, 233, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 260, 261, 262, 264, 267, 268, 269, 270, 271, 272, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 314, 315, 316, 317, 318, 319, 320, 321, 322, 325, 326, 329, 330, 331, 332, 333, 334, 335, 336, 341, 342, 343, 344, 345, 347, 348, 349, 351, 356, 357, 358, 359, 361, 363) + 6000
wind_exclude_2 <- c(0, 2, 3, 4, 6, 7, 9, 11, 19, 20, 21, 24, 27, 30, 33, 34, 35, 43, 46, 47, 49) + 7000
ids_to_exclude <- c(ids_to_exclude, wind_exclude, wind_exclude_2)
rm(wind_exclude, wind_exclude_2)

ids_to_keep <- ids_to_keep[!ids_to_keep %in% ids_to_exclude]
L_ts <- L_ts[L_ts$id %in% ids_to_keep, ]
df_inters <- df_inters[df_inters$plotid %in% ids_to_keep,]

# _______________________________________________________________________________ #

#### #1 Apply sliced time window selection ####
table(df_inters$change_process)
classes_to_include <- c("Fire","Harvest","Other","Wind", "Growth","Stable", "Wind")
number_of_samples  <- 250
# path_LandSat_ts <- "P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_2_with_outliers_till_2022_post1.csv"
padding_days    <- 10
years_prior_stable <- 3


index_list <- c("NBR", "NDV", "TCW", "TCG", "TCB")
index_list <- c("BLU","GRN","RED","NIR","SW1","SW2","NDV","NBR")



df_for_dl <-bm_select_ref_and_extract_dl_df_moving_window(L_ts=L_ts,
                                             df_inters = df_inters,
                                             classes_to_include = classes_to_include,
                                             number_of_samples = number_of_samples, 
                                             padding_days = padding_days, 
                                             years_prior_stable = years_prior_stable, 
                                             ids_to_exclude = ids_to_exclude, 
                                             index_list = index_list)
table(df_for_dl$id)

ids_not_complete <- df_for_dl %>% group_by(id) %>% summarise( n = n()) %>% filter(!n  %in% c(168, 168*2,168*3,168*4,168*5)) %>% select(id)
df_for_dl <- df_for_dl %>% filter(!id %in% ids_not_complete$id)

#write.csv2(df_for_dl, "P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/01_df_for_dl_200_4_classes.csv")
#df_for_dl <- read.csv2("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/01_df_for_dl_200_4_classes.csv")

df_for_dl_wide <- df_for_dl %>% select(!c(date,change_process,diff,instance, sensor)) %>% pivot_wider(names_from = c(time_seq),values_from = value) %>%
  arrange(.,id,index) %>% as.data.frame()

y_safe <-  df_for_dl_wide$changes_rep[seq(1,nrow(df_for_dl_wide),length(index_list))]
x_safe <-  df_for_dl_wide %>% select(c(seq(ncol(df_for_dl_wide)- (2 * padding_days ), ncol(df_for_dl_wide))))
## 799 samples
## anforderungen -- [samples, features, timeseries per feature]
table(y_safe)

y_safe[y_safe == "Fire"] <- 1
y_safe[y_safe == "Harvest"] <- 2
y_safe[y_safe == "Other"] <- 3
y_safe[y_safe == "Wind"] <- 5
y_safe[y_safe == "Stable"] <- 6
y_safe[y_safe == "Growth"] <- 7
y_safe <- as.integer(y_safe)

write.csv2(y_safe, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/05_df_y_",padding_days,"_250smps.csv"),row.names = FALSE)
write.table(x_safe, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/05_df_x_",padding_days,"_250smps.csv"), row.names = FALSE, col.names = TRUE, dec = ".", sep = ";")


# _______________________________________________________________________________ #
#### #2 Apply all time series selection ####

df_for_dl <- bm_extract_dl_df_all_ts(L_ts[1:200000,], df_inters,classes_to_include = c("Harvest","Other","Fire","Growth","Wind","Decline","Hydrology"), 
                              ids_to_exclude = ids_to_exclude, index_list = c("NDV","EVI")
)
table(df_for_dl$change_process)

df_for_dl_wide <- df_for_dl %>% select(!c(change_process,date,diff,instance, sensor)) %>% pivot_wider(names_from = c(time_sequence),values_from = c(value)) %>%
  arrange(.,id,index) %>% as.data.frame()

# df_for_dl_wide_change_process <- df_for_dl %>% select(!c(date,diff,instance, sensor)) %>% pivot_wider(names_from = c(time_sequence),values_from = c(change_process)) %>%
#   arrange(.,id,index) %>% replace_na(is.na(.),0) %>% as.data.frame()

#write.table(df_for_dl_wide, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/03_df_x_50000_ts.csv"), row.names = FALSE, col.names = TRUE, dec = ".", sep = ";")
write.table(df_for_dl, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/03_df_x_200000_ts_long.csv"), row.names = FALSE, col.names = TRUE, dec = ".", sep = ";")



# _______________________________________________________________________________ #
#### #3 Apply selection for unet ####
table(df_inters$change_process)

classes_to_include      <- c("Fire","Harvest","Insect","Wind","Growth")
number_of_samples_each  <- 200
index_list_to_use       <- c("BLU","GRN","RED","NIR","SW1","SW2","NDV","EVI","NBR")


## find minimum length of time series 
head(L_ts)
n_ts_sample <- L_ts %>% filter(index == index_list_to_use[1]) %>% group_by(id) %>% summarise(n_ = n())
hist(n_ts_sample$n_, breaks = 40)
n_ts_sample[order(n_ts_sample$n_),]

## set to 240 
n_ts_sample <- n_ts_sample[n_ts_sample$n_ > 240,]
L_ts <- L_ts[L_ts$id %in% n_ts_sample$id, ]
df_inters <- df_inters[df_inters$plotid %in% n_ts_sample$id,]

## safe data sets 
#write.csv2(L_ts, "P:/workspace/jan/fire_detection/dl/Landsat_ts_filtered.csv")
#write.csv2(df_inters, "P:/workspace/jan/fire_detection/dl/Refs_filtered.csv")

L_ts <- read.csv2(L_ts, "P:/workspace/jan/fire_detection/dl/Landsat_ts_filtered.csv")
df_inters <- read.csv2( "P:/workspace/jan/fire_detection/dl/Refs_filtered.csv")

## select ids from refs 
table(df_inters_select$change_process)
df_inters_select  <- df_inters %>%
  filter(change_process %in% classes_to_include) %>%
  group_by(change_process)  %>% 
  do(sample_n(.,number_of_samples_each))

L_ts_selected <- L_ts_s[L_ts_s$id %in% df_inters_select$plotid,]

## Function for preparation of data sets 
prepared_unet <- bm_extract_dl_df_UNET(L_ts_selected, df_inters = df_inters_select, ts_length = 240, classes_to_include = classes_to_include,
                                       index_list = index_list_to_use)

## 
length(unique(prepared_unet$id))
length(unique(df_inters_select$plotid))
summary(prepared_unet)
df_for_dl_wide <- prepared_unet %>% select(!c(x,y,change_process.x, change_process.y, tile,
                                              diff.y,diff.x,instance.y,instance.x, sensor, date)) %>% pivot_wider(names_from = time_sequence,values_from = value) %>%
  arrange(.,id,index) %>% as.data.frame()

y_safe <-  prepared_unet %>% filter(index %in% index_list_to_use[1]) %>% select(id, date, change_process.y)
x_safe <- df_for_dl_wide %>% mutate(index = factor(index, levels = index_list_to_use)) %>% arrange(id, index)
x_safe <- x_safe[!x_safe$index %in% c("EVI"),]
#
colnames(y_safe) <-  c("id", "date", "change_process")
write.csv2(y_safe, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/04_unet_df_y_200smps.csv"),row.names = FALSE)
write.table(x_safe, paste0("P:/workspace/jan/fire_detection/dl/prepocessed_ref_tables/04_unet_df_x_200smps.csv"), row.names = FALSE, col.names = TRUE, dec = ".", sep = ";")



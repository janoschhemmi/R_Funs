
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

rm(fires_interpretations, bb_interpretations,fires_post1interpretations,fires_post2interpretations, 
   bb_comment, fires_comments,fires_post1_comments, fires_post2_comments)

bm_tsync_prepare(df_inters, df_comments, out_csv_file, overwrite = T)

## df_inters 
df_inters


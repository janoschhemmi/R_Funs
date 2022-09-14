setwd("C:/Users/geo_jahe/R_Projects/Jan/R_Funs/data_handling/TSync_related/")
source("bm_tsync_prepare.R")


#--- prepare timesync -------------------------------------------

# file names and paths
tsync_p <- "p:/timesync/bb/"
out_csv_file <- "p:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_wind.csv"

# read data
bb_comment <- read_csv(file.path(tsync_p, "tsync_plots_bb_jan_interpretations_comment.csv"), show_col_types = FALSE)
fires_comments <- read_csv(file.path(tsync_p, "tsync_fires_interpretations_comment.csv"), show_col_types = FALSE)
fires_post1_comments <- read_csv(file.path(tsync_p, "tsync_fires_post_training1_interpretations_comment.csv"), show_col_types = FALSE)
fires_post2_comments <- read_csv(file.path(tsync_p, "tsync_fires_post_training2_interpretations_comment.csv"), show_col_types = FALSE)
wind_1_comments <- read_csv(file.path(tsync_p, "tsync_wind_1_interpretations_comment.csv"), show_col_types = FALSE)
wind_2_comments <- read_csv(file.path(tsync_p, "tsync_wind_2_interpretations_comment.csv"), show_col_types = FALSE)

## exclude fire
fire_exclude  <- c(3712,3711, 3707, 3706, 3674, 3663, 3661, 3609, 3528, 3527, 3506)
fires_comments <- fires_comments[!fires_comments$plotid %in% fire_exclude,]

## wind 1 exclude 
wind_exclude <- c(84, 85, 87, 88, 89, 91, 93, 94, 96, 97, 98, 99,100, 102, 103, 104, 106, 107, 108, 109, 110, 111, 112, 113, 114, 119, 120, 122, 126, 127, 130,  131, 134,135, 136, 139, 142, 143, 144, 150, 155, 156, 158, 164, 168, 169, 172, 177, 178, 179, 182, 183, 184, 185, 188, 189, 190, 192, 193, 194, 195, 197, 199, 201, 204, 206, 207, 208, 209, 210, 211, 213, 221, 222, 223, 224, 225, 228, 229, 231, 233, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 260, 261, 262, 264, 267, 268, 269, 270, 271, 272, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 314, 315, 316, 317, 318, 319, 320, 321, 322, 325, 326, 329, 330, 331, 332, 333, 334, 335, 336, 341, 342, 343, 344, 345, 347, 348, 349, 351, 356, 357, 358, 359, 361, 363)
wind_1_comments <- wind_1_comments[!wind_1_comments$plotid %in% wind_exclude,]
wind_1_comments <- wind_1_comments$plotid + 6000

## wind 2 exclude 
wind_exclude_2 <- c(0, 2, 3, 4, 6, 7, 9, 11, 19, 20, 21, 24, 27, 30, 33, 34, 35, 43, 46, 47, 49) + 7000
wind_2_comments <- wind_2_comments[!wind_2_comments$plotid %in% wind_exclude_2,]

df_comments <- rbind(bb_comment, fires_comments, fires_post1_comments,fires_post2_comments, wind_1_comments, wind_2_comments)


bb_interpretations <- read_csv(file.path(tsync_p, "tsync_plots_bb_jan_interpretations.csv"), show_col_types = FALSE)
fires_interpretations <- read_csv(file.path(tsync_p, "tsync_fires_interpretations.csv"), show_col_types = FALSE)
fires_interpretations <- fires_interpretations[!fires_interpretations$plotid %in% fire_exclude,]

fires_post1interpretations <- read_csv(file.path(tsync_p, "tsync_fires_post_training1_interpretations.csv"), show_col_types = FALSE)
fires_post2interpretations <- read_csv(file.path(tsync_p, "tsync_fires_post_training2_interpretations.csv"), show_col_types = FALSE)

wind_1_interpretations <- read_csv(file.path(tsync_p, "tsync_wind_1_interpretations.csv"), show_col_types = FALSE)
wind_1_interpretations <- wind_1_interpretations[!wind_1_interpretations$plotid %in% wind_exclude,]
wind_1_interpretations$plotid <- wind_1_interpretations$plotid + 6000

wind_2_interpretations <- read_csv(file.path(tsync_p, "tsync_wind_2_interpretations.csv"), show_col_types = FALSE)
wind_2_interpretations <- wind_2_interpretations[!wind_2_interpretations$plotid %in% wind_exclude_2,]

df_inters <- rbind(fires_interpretations, fires_post1interpretations,fires_post2interpretations, bb_interpretations, wind_1_interpretations, wind_2_interpretations)
df_inters[df_inters$plotid == "3067",]

rm(fires_interpretations, bb_interpretations,fires_post1interpretations,fires_post2interpretations, 
   bb_comment, fires_comments,fires_post1_comments, fires_post2_comments)

bm_tsync_prepare(df_inters, df_comments, out_csv_file, overwrite = T)

tsy <- read.csv2(out_csv_file)
tsy %>% group_by(change_process) %>% tally()
na.omit(fires_comments)

#--- combine timesync and bfast ----------------------------------

# file names and paths
version_tag <- "landsat-nbr-h40-o1-bp3-expansion_revisited"
fn_refs <- "P:/workspace/jan/fire_detection/disturbance_ref/bb_timesync_reference_with_post3_revisited.csv"
outpath <- "p:/workspace/jan/fire_detection/break_detection/break_detection_tables/"


# construct file names
fn_metrics <- file.path(outpath, version_tag, "bfast_metrics.csv")
fn_out <- file.path(outpath, version_tag, "bfast_metrics_with_tsync_revisited.csv")



exclude_plots <- c(3142, 3144, 1882, 3038, 1166, 1626, 1819, 2121, 1826, 2059, 2192, 75,424, 1031, 1136, 928, 966, 539, 123, 329, 712, 2110)
bm_tsync_combine_bfast(fn_refs, fn_metrics, fn_out, exclude_plots=exclude_plots,exclude_years = 2021, overwrite = T)



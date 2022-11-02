
### global Settings 
# .libPaths("S:/BrandSat/02_Code/R/library_3")
# rasterOptions()
# dirname(rasterTmpFile()) 
# rasterOptions(tmpdir='S:/BrandSat/000_RasterTemp')
# Sys.setenv("LANGUAGE"="En")
# Sys.setlocale("LC_ALL", "English")
# 
# # install.packages("hms")
# ## lib
# library(bfast)
# library(zoo)
# library(rlang)
# library(stlplus)
# library(lubridate)
# library(devtools)
# library(bfastSpatial)
# library(raster)
# library(svMisc)
# library(snow)
# library(lubridate)
# library(hms)
# library(ggplot2)
# library(doSnow)
# library(foreach)
# library(rgdal)


## load interpretations

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

load_comments <- function(){
  ## load id bb
  bb_comment <- read.table("P:/timesync/bb/tsync_plots_bb_interpretations_comment.csv",
                           sep = ",", dec = ".", header = TRUE)
  fires_comments <- read.table("P:/timesync/bb/tsync_fires_interpretations_comment.csv",
                               sep = ",", dec = ".", header = TRUE)
  
  comments <- rbind(bb_comment, fires_comments)
  return(comments)
}


load_inters <- function(){
  ## load id bb
  bb_interpretations <- read.table("P:/timesync/bb/tsync_plots_bb_jan_interpretations.csv",
                                   sep = ",", dec = ".", header = TRUE)
  ## id fires 
  fires_interpretations <- read.table("P:/timesync/bb/tsync_fires_interpretations.csv",
                                      sep = ",", dec = ".", header = TRUE)
  
  inters <- rbind(fires_interpretations, bb_interpretations)
  return(inters)
}


preprocess_inters <- function(){
## LOAD 
## -----------------------------------------------------------------------------
ids <- load_samples()
ids <- ids[!duplicated(ids$plotid),] ## 3142  3144
inters <- load_inters()
comments <- load_comments()

## Preprosecc Comments 
## -----------------------------------------------------------------------------

for(r in seq(1:nrow(comments))) {
  
  row <- comments[r,]
  print(row)
  
  row$harvest_fire_same_year     <- 1
  row$start_fire     <- NA
  row$start_fire_year     <- NA
  row$end_fire       <- NA
  row$start_harvest  <- NA

  comment_of_smp_split <- strsplit(row$comments, ",")
  if(!is.na(comment_of_smp_split[[1]][1])){
    if (comment_of_smp_split[[1]][1] == "start" ){
      print("is fire")#
      
      start_fire         <- as.Date(as.integer(comment_of_smp_split[[1]][4]),
                                    origin = paste0(comment_of_smp_split[[1]][3],"-01-01"))
      start_fire_year         <- as.integer(comment_of_smp_split[[1]][3])
      end_fire           <- as.Date(as.integer(comment_of_smp_split[[1]][5]),
                                    origin = paste0(comment_of_smp_split[[1]][3],"-01-01"))
      start_harvest      <- as.Date(as.integer(comment_of_smp_split[[1]][8]),
                                    origin = paste0(comment_of_smp_split[[1]][7],"-01-01"))
      
      harvest_fire_same_year     <- 1
      row$harvest_fire_same_year     <- 1
      row$start_fire          <- as.character(start_fire)
      row$start_fire_year     <- as.character(start_fire_year)
      row$end_fire            <- as.character(end_fire) 
      row$start_harvest       <- as.character(start_harvest) 
      
      print(row)
      
      # dates_return <- c(start_fire, end_fire, start_harvest)
      # print(dates_return)
    }}
  
  
  ## combine
  if  ( r == 1){
    out <- as.data.frame(row)
  } else {
    out <- rbind(out, as.data.frame(row))
    
  }
}

# bring together with interpretations 

inters$change_process_brandsat <- "None"
print(inters[inters$change_process == "Hydrology",])
inters[inters$change_process == "Purple",]$change_process_brandsat     <- "Fire"
inters[inters$change_process == "Harvest",]$change_process_brandsat    <- "Harvest"
inters[inters$change_process == "Wind",]$change_process_brandsat       <- "Wind"
inters[inters$change_process == "Other",]$change_process_brandsat      <- "Other"
inters[inters$change_process == "Hydrology",]$change_process_brandsat   <- "Other"
inters[inters$change_process == "Fire",]$change_process_brandsat       <- "Fire"
unique(inters$change_process_brandsat)
inters$disturbance <-0

inters[inters$change_process_brandsat != "None",]$disturbance <- 1

## set date 
inters$date_of_interpretation <- as.Date(as.integer(inters$image_julday),
                                              origin = paste0(inters$image_year,"-01-01"))

## is change process both no forest no tree? 
inters$no_forest_no_tree_change_process <- 0

for ( i in 1:nrow(inters)){
  
  la <- inters[i,]$landuse
  lac <- inters[i,]$landcover
  
  la_2 <- inters[i+1,]$landuse
  lac_2 <- inters[i+1,]$landcover
  
  if((((la == "Non-forest" && la_2 == "Non-forest") ) && ((lac == "Non-tree" && lac_2 == "Non-tree") ))) {
    
    if(inters[i,]$disturbance == 1){
    inters[i,]$no_forest_no_tree_change_process <- 1
  }}
  print(i)
}

## combine inters and comments
inters
out$start_fire_year <- as.integer(out$start_fire_year)
out      <- out[,-which(names(out) %in% c("project_id","tsa"))]
inters <- left_join(inters, out , by=c("plotid", "image_year" = "start_fire_year"), fill = TRUE)
# unique(inters_t$comments)
## --------------------------------------------------------------------------
## introduce fire harvest 
## fire and harvest can either occure in the same year or in consequtive years 

## fire_harvest in one year after, noted as two breaks in timesync 
## first need distance to following harvest,, of all 
inters$harvest_one_year_after_fire <- 0
inters$harvest_two_year_after_fire <- 0
inters$harvest_three_year_after_fire <- 0
inters$days_harvest_after_fire <- NA

for ( i in 1:nrow(inters)){
  
  print(i)
  plotid <- inters[i,]$plotid
  change_process_brandsat <- inters[i,]$change_process_brandsat
  
  # get date from first 
  date_1 <- make_date(inters[i,]$image_year) +  inters[i,]$image_julday - 1
  
  plotid_2 <- inters[i+1,]$plotid
  change_process_brandsat_2 <- inters[i+1,]$change_process_brandsat
  
  if (i < nrow(inters)){
    date_2 <- make_date(inters[i+1,]$image_year) +  inters[i+1,]$image_julday - 1
  } else {
    date_2   <- date_1
    plotid_2 <- 666
    
  }
  
  if ( i < nrow(inters)){
  if(plotid == plotid_2){
   
  if((change_process_brandsat == "Fire" && change_process_brandsat_2 == "Harvest") )  {
    
    print(paste(date_1, date_2))
    
    inters[i,]$days_harvest_after_fire <- as.integer(date_2 - date_1)
    
    if(as.integer(date_2 - date_1) <= 365){
      print(paste(i, "harvest within one year.. "))
      inters[i,]$harvest_one_year_after_fire <- 1
      inters[i,]$days_harvest_after_fire <- as.integer(date_2 - date_1)
    }
    if((as.integer(date_2 - date_1) <= (365*2) && (as.integer(date_2 - date_1) >= 365))) {
      print("harvest within two years.. ")
      inters[i,]$harvest_two_year_after_fire <- 1
      inters[i,]$days_harvest_after_fire <- as.integer(date_2 - date_1)
    }
    if((as.integer(date_2 - date_1) <= (365*3) && (as.integer(date_2 - date_1) >= (365*2)))){
      print("harvest within three year.. ")
      inters[i,]$harvest_three_year_after_fire <- 1
      print(as.integer(date_2 - date_1))
      inters[i,]$days_harvest_after_fire <- as.integer(date_2 - date_1)
    }
    ## save days 
   
    }}}
  # print(i)
}
sum(inters$harvest_one_year_after_fire)
sum(inters$harvest_two_year_after_fire)
sum(inters$harvest_three_year_after_fire)

## ---------------------------------------------------------------------------
## again for other from comments 
inters_t <- inters

inters$days_harvest_after_fire <-   as.integer(difftime(as.Date(inters[]$start_harvest),  as.Date(inters[]$start_fire), units = "days"))

## but harvest was sometime notated in comments.. better check for that 
inters[i,]$comments
for ( i in 1:nrow(inters)){
  print(i)
  if ((inters[i,]$change_process_brandsat == "Fire") &&
      (!is.na(inters[i,]$start_fire))){
    
    if(inters[i,]$days_harvest_after_fire <= 365){
      print("harvest within one year.. ")
      inters[i,]$harvest_one_year_after_fire <- 1
    }
    if(inters[i,]$days_harvest_after_fire <= (365*2) &&
       (inters[i,]$days_harvest_after_fire >= 365)){
      print("harvest within two year.. ")
      inters[i,]$harvest_two_year_after_fire <- 1
    }
    
    inters[i,]$harvest_one_year_after_fire <- 1
   
  }
}
sum(inters$harvest_one_year_after_fire) 
inters[inters$change_process_brandsat == "Fire",] 

## ----------------------------------------------------------------------------
## load preprocessed fires 
preprocessed_fires <- read.table("P:/workspace/jan/fire_detection/disturbance_ref/fire_ref_per_sample.csv", sep = ";", dec =  ".", header = TRUE)

## bind interpretations with fire
inters
preprocessed_fires$change_process <- "Fire"

inters_2 <- left_join(inters, preprocessed_fires, by = c("plotid" = "UID", "change_process", "image_year" = "year"))
inters_2 <- inters_2[!duplicated(inters_2),]

## introduce disturbance id 
inters_2$tsync_id <- seq(1:nrow(inters))
inters_3 <- inters_2 %>% 
  filter(disturbance == 1) %>%
  mutate(dis_id = seq(1:nrow(.)))

## re join
inters_3_sub <- inters_3[,c("tsync_id", "dis_id")]  
inters_2     <- left_join(inters_2, inters_3_sub, by = c("tsync_id"))

write.table(inters_3, "P:/workspace/jan/fire_detection/disturbance_ref/preprocessed_disturbance_ref.csv", sep = ";", dec = ".", col.names = TRUE, row.names = FALSE)
write.table(inters_2, "P:/workspace/jan/fire_detection/disturbance_ref/preprocessed_ref_all_smp.csv", sep = ";", dec = ".", col.names = TRUE, row.names = FALSE)

## NOW find a way to evaluate detected breaks and see if the references are detected 
}


## ----------------------------------------------------------------------------
## ----------------------------------------------------------------------------
## analysis how many harvests do we have .. how long after fire? 
# '''
# sub_fire <- inters[inters$harvest_one_year_after_fire == 1, ] %>%
#   group_by(how_many_days_harvest_after_fire) %>%
#   tally() %>% as.data.frame()
# 
# ploti <- ggplot(sub_fire, aes(x = how_many_days_harvest_after_fire, y = n )) + geom_bar(stat="identity", width = 1.4) + theme_light() +
#    xlim(0, 200) + ylim(0,16.5) + theme(axis.text=element_text(size=18),
#                                        axis.title=element_text(size=18,face="bold"))
# ggsave(ploti, path = "P:/workspace/jan/fire_detection/Accuracy_assesement/", filename = "how_many_days_harvest_after_fire.png", width = 6, height = 6, device = "png", dpi = 100)
# 
# ## how many fires have sequental harvests?
# sub_fire <- inters[inters$change_process_brandsat == "Fire", ] %>%
#   group_by(harvest_one_year_after_fire, harvest_two_year_after_fire, harvest_three_year_after_fire) %>%
#   tally() %>% as.data.frame()
# 
# ##
# years_after <- as.data.frame(matrix(ncol = 2, nrow = 4))
# colnames(years_after) <- c("years", "n")
# years_after$n     <- c(959, 121, 55, 1)
# years_after$years <-c("No harvest", "one year","two years", "three years")
# 
# '''






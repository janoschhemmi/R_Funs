
## load reference 20 S2 band 
S2_ref <- raster("A:/00_Level1_2/Level2/Kira_20m/X0068_Y0045/2019-2019_253-255_HL_TSA_SEN2L_SW2_TSS.tif")

## load S1 ref 
S1_ref <- raster("X:/eodc/hemmerling/s1/ascending/X0050_Y0035/2014-2019_001-365_HL_TSA_VVVHP_BVV_TSS_20181229_S1AIA.tif")

## reproject S1 Image
S1_ref <- projectRaster(S1_ref, S2_ref)

## Crop S2 image to S1 (for resampling) 
S2_ref_crop <- crop(S2_ref, S1_ref)
plot(S2_ref_crop)
writeRaster(S2_ref_crop, "A:/00_Level1_2/test_treuenbrietzen_crop.tif", overwrite = TRUE)

# resample S1 into S2 
S1_resample <- resample(S1_ref, S2_ref_crop, method='bilinear')
writeRaster(S1_resample, "A:/00_Level1_2/test_treuenbrietzen_S1_resample.tif", overwrite = TRUE)


## list of S1 to resample 
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

## load all file names in folder 
path <- "P:/workspace/jan/S1/S1_data/2018_asc/"
path_store_as <- create_s1_path(path)
path_des <- "P:/workspace/jan/S1/S1_data/2018_dec/"
path_store_des <- create_s1_path(path_des)
out_path <- "P:/workspace/jan/S1/S1_data/2018_dec_resample/"

## loop over list ASC  
for(h in 1:nrow(path_store_des[,])) {
  print(h)
  
  ## load orig S1 
  print((paste0("P:/workspace/jan/S1/S1_data/2018_dec/",path_store_des$file[h])))
  S1_orig <- raster(paste0("P:/workspace/jan/S1/S1_data/2018_dec/",path_store_des$file[h]))
 
  ## reproject   
  S1_orig_rep <- projectRaster(S1_orig, S2_ref)

  ## resample
  S1_rep_resample <- resample(S1_orig_rep, S2_ref_crop, method='bilinear')
  writeRaster(S1_rep_resample, paste0(out_path, substring(path_store_des$file[h],2,nchar(path_store_des$file[h]) - 4),"_resample.tif" ), overwrite = TRUE)
  
}

## ------------------------------------------------------------------------- ##

## For every image calculate VV / VH 

## list of resampled S1, paired by date 
create_s1_path_ratio <- function(path_20m){
  all_path <- as.data.frame(list.files(path_20m, pattern = ".tif"))
  colnames(all_path) <- "file"
  dd      <- as.data.frame(cbind(all_path$file,  str_split_fixed(all_path$file, pattern  = "_", 9)))
  dd$year  <- substring(dd$V9, first = 1, last = 4)
  dd$month <- substring(dd$V9, first = 5, last = 6)
  dd$day   <- substring(dd$V9, first = 7, last = 8)
  dd$date  <- paste0(dd$year,"-",dd$month,"-",dd$day)
  dd$date_as_date <- as.Date(dd$date)
  dd$orbit   <- substring(dd$V10, first = 3, last = 3)
  path_store <- dd[,c(1,7,8,11,12,13,14,15,16)]
  colnames(path_store) <- c("file","Pol","other","year","month","day","date","date_as_date", "orbit") 
  
  ## look for unique dates 
  unique_dates <- unique(path_store$date_as_date)
  dplyr::arrange(path_store,date_as_date)

  double_df <- as.data.frame(matrix(ncol=4,nrow=0))
  ## check for each date if one VV and VH is present
  for (d in seq(1:length(unique_dates))) {
    print(d)
    date <- unique_dates[d]
    print(date[[1]])
    
    sub <- path_store[path_store$date_as_date == date[[1]],]
    print(sub)
    if (nrow(sub) == 2){
      print("yeah")
      
      files <- sub$file
      pols <- sub$Pol
      
      merged <- append(files, pols)
      double_df <- rbind(double_df, merged)
      print(double_df)
      
      }
  }  
  colnames(double_df) <- c("file_1","file_2","VH","VV")
  return(double_df)
}

## function for ratio
calc_ratio <- function(file_df, path, out_path){
  
  for (o in seq(1:nrow(file_df))){
    print(paste0("calc: ", o, " ... "))
    raster_1 <- raster(paste0(path,file_df$file_1[o]))
    raster_2 <- raster(paste0(path,file_df$file_2[o]))
    
    ## VV / VH 
    raster_3 <- raster_2 / raster_1
    file_name  <- substring(file_df$file_1[o], first = 0, last = (nchar(file_df$file_1[o]) - 4))
    print(paste0("write..", file_name))
    writeRaster(raster_3, paste0(out_path,file_name,"_ratio.tif"))
  
    }
}


## load all file names in folder 
path_20m_asc      <- "P:/workspace/jan/S1/S1_data/2018_asc_resample/"
path_20m_asc_ratio      <- "P:/workspace/jan/S1/S1_data/2018_asc_resample_ratio/"
path_store_20m_as <- create_s1_path_ratio(path_20m_asc)
calc_ratio(path_store_20m_as, path_20m_asc,path_20m_asc_ratio)

path_20m_dec      <- "P:/workspace/jan/S1/S1_data/2018_dec_resample/"
path_20m_dec_ratio      <- "P:/workspace/jan/S1/S1_data/2018_dec_resample_ratio/"
path_store_20m_dec <- create_s1_path_ratio(path_20m_dec)
calc_ratio(path_store_20m_dec, path_20m_dec,path_20m_dec_ratio)


#' Compile extracted FORCE time series samples
#'
#' @title Compile extracted FORCE time series samples
#' @description Compiles FORCE time series samples from individual files into a single file.
#' @param inpath character. Path to stacked time series created by `bm_detect_and_extract_tiles()`
#' @param out_csv_file outpath character. Path to bfast metrics created by `bm_detect_and_extract_tiles()`
#' @param overwrite boolean. Overwrite output file if it exists, defaulting to `FALSE`.
#' @return Nothing
#' @author Dirk Pflugmacher
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @md

#out_csv_file <- "p:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_dl_test.csv"

bm_compile_force_extract <- function(inpath, out_csv_file, overwrite=F) {
  
  
  if (file.exists(out_csv_file) & !overwrite) {
    message(paste(out_csv_file, "exists. Use overwrite keyword."))   
    return(invisible(NULL)) 
    } 

  options(readr.show_progress=F)
  
  fns <- list.files(inpath, "^X.*csv$", full.names=T)
  #fns <- fns[grepl("X0068_Y0045",fns)]
  
  for (i in 1:length(fns)) {
    
    #i <- 1
    message(basename(fns[i]))
    
    ds <- readr::read_csv2(fns[i], col_names = TRUE )
    #ds <- read_csv2(fns[i], col_names = TRUE,)
    #ds <- read.table(fns[i], sep = ";", col.names = TRUE, row.names = FALSE, dec = ".")
    
    #ds <- readr::read_csv2(fns[i], show_col_types = FALSE, na = c("", "NA"))
    
    ds <- tidyr::pivot_longer(ds, -c("id","x","y"), values_to = "value" , names_to = "date_sensor")
    
    ds <- ds[!is.na(ds$value),]  %>% 
      tidyr::separate(.data$date_sensor, c("date", "sensor"), sep="_")
    
    
    ds$date <- gsub("X", "", ds$date)
    ds$date <- as.Date(ds$date, format="%Y%m%d")
    
    ds$tile <- substr(basename(fns[i]), 1, 11)
    ds$index <- substr(basename(fns[i][1]), 13, 15)
    
    append <- ifelse(i == 1, F, T)
    
    readr::write_csv2(ds, out_csv_file, append = append)
  }
  
}


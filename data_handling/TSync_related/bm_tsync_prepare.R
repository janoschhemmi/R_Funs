#' Prepare outputs from TimeSync for further analysis with bfast
#'
#' @title Prepare outputs from TimeSync for further analysis with bfast
#' @description This script prepares the outputs from TimeSync for further analysis with bfast. 
#' The change processes are recoded and specific comment fields are extracted to add harvest
#' following fire in the same year 
#' @param df_inters data.frame. TimeSync interpretations table.
#' @param df_comments data.frame. TimeSync comments table.
#' @param out_csv_file character. Output filename
#' @param overwrite boolean. Overwrite output file if it exists, defaulting to `FALSE`.
#' @return Nothing. Writes `out_csv_file` to disk.
#' @family timesync
#' @seealso [bm_tsync_combine_bfast()] for combining timesync and bfast data, [bm_detect_and_extract_samples()] for creating breakpoint metrics.
#' @author Dirk Pflugmacher
#' @export
#' @importFrom magrittr %>%
#' @md


bm_tsync_prepare <- function(df_inters, df_comments, out_csv_file, overwrite=F) {
  

  harvested_fire_csv_file <- sub(".csv", "_harvested-fires.csv", out_csv_file)
  
  if (file.exists(out_csv_file) & !overwrite) {
    return(invisible(NULL)) } 
  else {
    message(paste(out_csv_file, "exists. Use overwrite keyword."))    
  }
  
  #--- recode interpretations --------------------------------------
  
  # remove first vertex
  df_inters <- df_inters[!is.na(df_inters$change_process), ]
  
  j <- which(duplicated(df_inters[,c("plotid", "image_year")]))
  if (length(j) > 0) {
    message(sprintf("Double timesync plotids: %s", paste(unique(df_inters$plotid[j]), collapse=", ")))
    df_inters <- df_inters[-j,]
  }
  
  # TODO: decline currently coded as None
  df_inters$change_process_brandsat <- "None"
  
  df_inters$change_process_brandsat[df_inters$change_process == "Harvest"]    <- "Harvest"
  df_inters$change_process_brandsat[df_inters$change_process == "Wind"]       <- "Wind"
  df_inters$change_process_brandsat[df_inters$change_process == "Other"]      <- "Insect"
  df_inters$change_process_brandsat[df_inters$change_process == "Hydrology"]  <- "Hydrology"
  df_inters$change_process_brandsat[df_inters$change_process == "Fire"]       <- "Fire"
  df_inters$change_process_brandsat[df_inters$change_process == "Fire_salvage"]  <- "Fire_salvage"
  df_inters$change_process_brandsat[df_inters$change_process == "Harvest_salvage"]  <- "None"
  df_inters$change_process_brandsat[df_inters$change_process == "Growth"]     <- "Growth"
  df_inters$change_process_brandsat[df_inters$change_process == "Stable"]     <- "Stable"
  
  # table(df_inters$change_process)
  table(df_inters$change_process, df_inters$change_process_brandsat)
  
  
  df_inters <- df_inters %>% 
    dplyr::select(-c("tsa", "change_process")) %>% 
    dplyr::rename(change_process="change_process_brandsat") %>%
    dplyr::arrange(.data$plotid) %>% 
    dplyr::mutate(uid=1:dplyr::n())
  
  
  #--- extract harvest following fire from comments ------------------------------
  
  df_comments$comments[is.na(df_comments$comments)] <- ""
  
  i <- which(startsWith(df_comments$comments, "start"))
  
  df_harvested_fires <- df_comments[i,] %>% 
    tidyr::separate(.data$comments, c("tmp1", "tmp2", "fire_year", "fire_doy1", "fire_doy2", 
                                      "tmp3", "harvest_year", "harvest_doy"), 
                    sep=",", extra="drop") %>%
    dplyr::mutate(image_year=as.numeric(.data$fire_year), fire_julday=as.numeric(.data$fire_doy1), 
                  harvest_year=as.numeric(.data$harvest_year), harvest_doy=as.numeric(.data$harvest_doy)) %>% 
    dplyr::select(c("project_id", "plotid", "image_year", "fire_julday", "harvest_year", "harvest_doy")) %>%
    dplyr::arrange(.data$plotid) %>% 
    dplyr::left_join(df_inters, by=c("project_id", "plotid", "image_year"))
  
  # check for mismatch
  j <- which(is.na(df_harvested_fires$change_process))
  if (length(j) > 0) {
    message(sprintf("Missing: %s", length(j)))
    message(sprintf("Fix plots: %s", paste(df_harvested_fires$plotid[j], collapse=",")))
    
    print(df_inters[df_inters$plotid %in% df_harvested_fires$plotid[j],])
    print(df_harvested_fires[df_harvested_fires$plotid %in% df_harvested_fires$plotid[j],])
    df_harvested_fires <- df_harvested_fires[-j,]
  }
  
  # check for mismatch
  j <- which(df_harvested_fires$change_process != "Fire")
  if (length(j) > 0) {
    message(sprintf("Not fire: %s", length(j)))
    message(sprintf("Fix plots: %s", paste(df_harvested_fires$plotid[j], collapse=",")))
    
    print(df_inters[df_inters$plotid %in% df_harvested_fires$plotid[j],])
    print(df_harvested_fires[df_harvested_fires$plotid %in% df_harvested_fires$plotid[j],])
    # print(df_comments[df_comments$plotid %in% df_harvested_fires$plotid[j],])
    df_harvested_fires <- df_harvested_fires[-j,]
  }
  
  # save harvested fires in a separate file
  df_harvested_fires %>% 
    dplyr::select(-"image_julday") %>% 
    dplyr::rename(image_julday="fire_julday") %>%
    readr::write_csv2(harvested_fire_csv_file)
  
  df_harvested_fires_f <- df_harvested_fires %>% 
    dplyr::select(-c("harvest_year", "harvest_doy", "image_julday")) %>%
    dplyr::rename(image_julday="fire_julday") %>%
    dplyr::mutate(change_process="Fire_salvage")
  
  df_harvested_fires_h <- df_harvested_fires %>% 
    dplyr::select(-c("image_year", "fire_julday", "image_julday")) %>%
    dplyr::rename(image_year="harvest_year", image_julday="harvest_doy") %>%
    dplyr::mutate(change_process="Harvest_salvage")
  
  df_harvested_fires <- dplyr::bind_rows(df_harvested_fires_f, df_harvested_fires_h)
  
  
  #--- combine comments and interpretations --------------------------------------
  
  # TODO: no_forest_no_tree - Jan had something else here 
  # incorporating previous rows (not working)
  df_combined <- dplyr::filter(df_inters, !.data$uid %in% df_harvested_fires$uid) %>%
    dplyr::bind_rows(df_harvested_fires) %>% 
    dplyr::select(-"uid") %>%
    dplyr::mutate(change_date = as.Date(as.integer(.data$image_julday)-1,
                                 origin = paste0(.data$image_year,"-01-01")),
           disturbance = ifelse(.data$change_process != "None", 1, 0),
           no_forest_no_tree = ifelse(.data$landuse=="Non-forest" & 
                                      .data$landcover=="Non-tree", 1, 0))
  
  readr::write_csv2(df_combined, out_csv_file)
  message("finished.. !")
  
  
}

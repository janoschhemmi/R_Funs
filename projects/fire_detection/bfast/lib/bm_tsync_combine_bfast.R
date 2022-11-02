#' Combine timesync output with bfast breakpoints and metrics
#'
#' @title Combine timesync output with bfast breakpoints and metrics
#' @description This script combines tsync and bfast breakpoints. 
#' @details For each bfast breakpoint, the tsync change closest to the breakpoint is added
#' along with the following information.
#' 
#' `tsync_datediff`:  difference in days: tsync minus bfast
#' 
#' `tsync_date`:      date of the closest timesync observation
#' 
#' `tsync_change`: change type of the closest timesync observation
#' 
#' `tsync_landcover`: land cover of the closest timesync observation
#' 
#' `tsync_landuse`: land use of the closest timesync observation
#' 
#' If no timesync change was observed then `tsync_date=1900-01-01`,
#' `tsync_change="None"`, `tsync_landcover`=`"None"` and `tsync_landuse="None"`.
#' Also `tsync_datediff` is then large accordingly.
#' 
#' For training and validation, use `tsync_date_diff` and `tsync_landuse` 
#' (along with `tsync_change`) to create reference labels.
#' 
#' IMPORTANT: The output also includes tsync change observations
#' for which no breakpoint was detected by bfast. If tsync observed 
#' multiple changes in a year, then these multiple changes show up.
#' Note, that the bfast metrics include `NA`'s for these rows. You
#' can identify them via `n_breaks=0`.
#' @param ref_file character. Filename to timsync data. Output from `bm_tsync_prepare()`
#' @param metrics_file character. Filename to bfast metrics. Output from `bm_detect_and_extract_samples()`
#' @param out_csv_file character. Output filename.
#' @param exclude_plots numeric. Vector of plotid's to exclude.
#' @param overwrite boolean. Overwrite output file if it exists, defaulting to `FALSE`.
#' @return Nothing. Writes `out_csv_file` to disk.
#' @family timesync
#' @seealso [bm_tsync_prepare()] for preparing timesync data, [bm_detect_and_extract_samples()] for creating breakpoint metrics.
#' @author Dirk Pflugmacher
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @md


bm_tsync_combine_bfast <- function(ref_file, metrics_file, out_csv_file, exclude_plots=NULL, exclude_years=NULL, overwrite=F) {
  
  
  if (file.exists(out_csv_file) & !overwrite) {
    message(paste(out_csv_file, "exists. Use overwrite keyword."))   
    return(invisible(NULL))
    } 

  if (!file.exists(ref_file)) {
    message(paste("Reference file not found:", ref_file))    
    return(invisible(NULL))
  } 

  if (!file.exists(metrics_file)) {
    message(paste("Metrics file not found:", metrics_file))    
    return(invisible(NULL))
  } 
  
  # read reference data
  refs <- readr::read_csv2(ref_file, show_col_types = FALSE)
  
  tsync_incomplete <- unique(refs$plotid[apply(refs[,c("plotid", "landuse", "landcover", "change_process", "change_date")],
                                               1, function(x) any(is.na(x)))])
  
  # read metrics file
  dfm <- readr::read_csv2(metrics_file, show_col_types = FALSE)

  if(!is.null(exclude_plots)) {
    refs <- refs[!refs$plotid %in% exclude_plots,]
    dfm  <- dfm[!dfm$id %in% exclude_plots,]
  }

  if(!is.null(exclude_years)) {
    refs <- refs[!refs$image_year %in% exclude_years,]
    dfm  <- dfm[!dfm$breakyear %in% exclude_years,]
  }
  
  message("bm_tsync_combine_bfast()")
  if (length(tsync_incomplete > 0)) message(sprintf("TimeSync incomplete: %s", paste(tsync_incomplete, collapse=", ")))
  message(sprintf("TimeSync (change): %s", length(unique(refs$plotid))))
  message(sprintf("Bfast plots      : %s", length(unique(dfm$id))))
  
  print(refs[refs$plotid %in% unique(tsync_incomplete),])
  
  # check NA's in metrics file
  i <- apply(dfm, 1, function(x) any(is.na(x)))
  message(sprintf("Bfast NA's (#)      : %s", sum(i)))
  message(sprintf("Bfast NA's (ids)    : %s", paste(which(i), collapse=", ")))
  dfm_na <- dfm[i,]
  
  # add date column to metrics file
  dfm$break_date <- as.Date(paste0(dfm$breakyear, "-01-01")) + dfm$doy - 1
  
  
  # Check for plotids missing in TimeSync
  missing_timesync_plotids <- unique(dfm$id[!dfm$id %in% refs$plotid])
  if (length(missing_timesync_plotids)) {
    message(sprintf("TimeSync missing plotids: %s", paste(missing_timesync_plotids, collapse=", ")))
  }
  
  # add disturbance number (remove undisturbed)
  disturbed <- dplyr::filter(refs, .data$disturbance==1) %>%
    dplyr::group_by(.data$plotid) %>% 
    dplyr::mutate(dist_id= 1:dplyr::n()) %>% 
    dplyr::relocate(.data$dist_id) %>% 
    dplyr::ungroup()
  
  # create reference data for undisturbed plots
  undisturbed <- dplyr::filter(refs, .data$disturbance==0) %>%
    dplyr::filter(!.data$plotid %in% unique(disturbed$plotid)) %>%
    dplyr::distinct(.data$plotid, .keep_all=T) %>%
    dplyr::mutate(change_date=as.Date("1900-01-01"),
                  change_process="None",
                  landcover="None",
                  landuse="None",
                  image_year = 1900,
                  image_julday = 1,
                  no_forest_no_tree = 0,
                  dist_id=0)
  
  # combine disturbed and undisturbed
  refs <- dplyr::bind_rows(disturbed, undisturbed) %>% 
          dplyr::arrange(.data$plotid)

  message("TimeSync number of disturbances:")
  print(table(refs$dist_id))
  
  
  # create disturbance by vertex matrices
  refs_d <- tidyr::pivot_wider(disturbed[,c("plotid", "dist_id", "change_date")],
                        values_from = .data$change_date, names_from=.data$dist_id, values_fill=as.Date("1900-01-01"))
  refs_c <- tidyr::pivot_wider(disturbed[,c("plotid", "dist_id", "change_process")],
                        values_from = .data$change_process, names_from=.data$dist_id, values_fill="None")
  refs_l <- tidyr::pivot_wider(disturbed[,c("plotid", "dist_id", "landcover")],
                        values_from = .data$landcover, names_from=.data$dist_id, values_fill="None")
  refs_u <- tidyr::pivot_wider(disturbed[,c("plotid", "dist_id", "landuse")],
                        values_from = .data$landuse, names_from=.data$dist_id, values_fill="None")
  
  # make new column names
  ts_date_name <- paste0("refx_date_", names(refs_d)[-1])
  ts_change_name <- paste0("refx_change_", names(refs_c)[-1])
  ts_landcov_name <- paste0("refx_landcover_", names(refs_l)[-1])
  ts_landuse_name <- paste0("refx_landuse_", names(refs_l)[-1])
  ts_datediff_name <- paste0("refx_datediff_", names(refs_l)[-1])
  
  # rename columns
  names(refs_d)[-1] <- ts_date_name
  names(refs_c)[-1] <- ts_change_name
  names(refs_l)[-1] <- ts_landcov_name
  names(refs_u)[-1] <- ts_landuse_name
  
  # combine
  refs_ts <- merge(refs_d, refs_c)
  refs_ts <- merge(refs_ts, refs_l)
  refs_ts <- merge(refs_ts, refs_u)
  rm(refs_d, refs_c, refs_l, refs_u)
  

  
  # merge tsync and breakpoint dates
  df <- merge(dfm[, c("id", "id_break", "break_date")], refs_ts, by.y="plotid", by.x="id", all.y=T)
  message(sprintf("Bfast undetected : %s", sum(is.na(df$id_break))))
  
  # plots with missing breaks
  df_omitted <- df[is.na(df$break_date),]
  
  
  # remove omitted (put back-in later)
  df <- df[!is.na(df$break_date),]
  
  # duplicate break_dates as many times as there are potential disturbances per year
  df_break_dates <- df[,rep(which(names(df) == "break_date"), each=length(ts_date_name))]
  
  
  # calculate differences
  df_datediff <- df[, ts_date_name] - df_break_dates
  df_datediff <- as.data.frame(lapply(df_datediff, as.numeric))
  names(df_datediff) <- ts_datediff_name
  
  
  # mask/flag disturbance closest to breakpoint 
  datemin <- apply(df_datediff, 1, function(x) x[order(abs(x))][1])
  
  df_datemin <- data.frame(x=datemin)
  df_datemin <- df_datemin[,rep(1, each=length(ts_date_name))]
  
  df_datemask <- df_datemin != df_datediff
  
  df_change  <- df[, ts_change_name]
  df_landcov <- df[, ts_landcov_name]
  df_date    <- df[, ts_date_name]
  df_landuse  <- df[, ts_landuse_name]
  
  
  df_change[df_datemask] <- NA
  df_landcov[df_datemask] <- NA
  df_date[df_datemask] <- NA
  df_landuse[df_datemask] <- NA
  
  df$tsync1_date <- as.Date(apply(df_date, 1, function(x) x[!is.na(x)][1]))
  df$tsync1_datediff <- datemin
  df$tsync1_change <- apply(df_change, 1, function(x) x[!is.na(x)][1])
  df$tsync1_landcover <- apply(df_landcov, 1, function(x) x[!is.na(x)][1])
  df$tsync1_landuse <- apply(df_landuse, 1, function(x) x[!is.na(x)][1])
  
  
  # mask/flag disturbance second closest to breakpoint 
  
  datemin <- apply(df_datediff, 1, function(x) x[order(abs(x))][2])
  
  df_datemin <- data.frame(x=datemin)
  df_datemin <- df_datemin[,rep(1, each=length(ts_date_name))]
  
  df_datemask <- df_datemin != df_datediff
  
  df_change  <- df[, ts_change_name]
  df_landcov <- df[, ts_landcov_name]
  df_date    <- df[, ts_date_name]
  df_landuse  <- df[, ts_landuse_name]
  
  df_change[df_datemask] <- NA
  df_landcov[df_datemask] <- NA
  df_date[df_datemask] <- NA
  df_landuse[df_datemask] <- NA
  
  df$tsync2_date <- as.Date(apply(df_date, 1, function(x) x[!is.na(x)][1]))
  df$tsync2_datediff <- datemin
  df$tsync2_change <- apply(df_change, 1, function(x) x[!is.na(x)][1])
  df$tsync2_landcover <- apply(df_landcov, 1, function(x) x[!is.na(x)][1])
  df$tsync2_landuse <- apply(df_landuse, 1, function(x) x[!is.na(x)][1])
  
  # remove unwanted columns
  df <- df[, -grep("refx", names(df))]
  
  # created rows for bfast omitted
  refs_omitted <- refs[refs$plotid %in% df_omitted$id, c("plotid", "dist_id", 
                                                         "change_process", 
                                                         "change_date",
                                                         "landcover", "landuse")] %>%
    dplyr::rename(id=.data$plotid,
                  id_break=.data$dist_id,
                  tsync1_date=.data$change_date,
                  tsync1_change=.data$change_process,
                  tsync1_landcover=.data$landcover,
                  tsync1_landuse=.data$landuse) %>%
    dplyr::mutate(tsync2_date=as.Date("1990-01-01"),
                  tsync2_change="None",
                  tsync2_landcover="None",
                  tsync2_landuse="None")
  
  
  # merge tsync and breakpoint dates
  result <- merge(dplyr::select(df, -.data$break_date), dfm, by=c("id", "id_break"), all.x=T)
  
  result <- dplyr::bind_rows(result, refs_omitted) %>% 
    dplyr::mutate(n_breaks= tidyr::replace_na(.data$n_breaks, 0)) %>%
    dplyr::arrange(.data$id, .data$id_break)
  
  readr::write_csv2(result, out_csv_file)
  
  message("Bye.")
}

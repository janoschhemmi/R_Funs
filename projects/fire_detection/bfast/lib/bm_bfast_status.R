#' Breakpoint detection status
#'
#' @title Breakpoint detection status
#' @description Determines the percentage of subtiles processed by `bm_stack_and_retile_force_tss()`.
#' @param tss_path character. Path to stacked time series created by `bm_detect_and_extract_tiles()`
#' @param metrics_path character. Path to bfast metrics created by `bm_detect_and_extract_tiles()`
#' @return Nothing
#' @author Dirk Pflugmacher
#' @export
#' @md

bm_bfast_status <- function(tss_path, metrics_path) {
  ifns <- list.files(tss_path, "^X.*tif$", recursive = T, full.names = T)
  ofns <- list.files(metrics_path, "^X.*tif$", recursive = T, full.names = T)
  pct <- 100 * length(ofns) / length(ifns)
  message(paste0("Processed: ", round(pct, 1), "% (", length(ofns), "/", length(ifns), ")" ))
}


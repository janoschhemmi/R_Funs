#' Create bfast metric names
#'
#' @title Create bfast metric names
#' @description Helper function for the developer to manage the bfast metric names in one place. This is the one.
#' @param breaks numeric. Number of breaks.
#' @param indices character. Vector of indices.
#' @return character. Vector of bfast metric names.
#' @author Jan Hemmerling
#' @author Dirk Pflugmacher
#' @export
#' 

bm_metric_names <- function(breaks=3, indices=c("NBR","NDV","TCB","TCG","TCW","TCD")) {
  
  names_fitted <- c("number_detected_break",
                    "breakyear",
                    "doy",
                    "pre_val_fit",
                    "post_val_fit",
                    "magnitude_fit",
                    "slope_pre",
                    "slope_post",
                    "magnitude_segment_pre",
                    "magnitude_segment_post")
  
  names_metrics <- c("pre_val",
                     "post_val",
                     "magnitude",
                     "mean_pre_val",
                     "mean_post_val",
                     "mean_magnitude",
                     "q05_breakyear",
                     "q25_breakyear",
                     "q50_breakyear",
                     "q75_breakyear",
                     "q95_breakyear",
                     "iqr_breakyear",
                     "q05_pre_breakyear",
                     "q25_pre_breakyear",
                     "q50_pre_breakyear",
                     "q75_pre_breakyear",
                     "q95_pre_breakyear",
                     "iqr_pre_breakyear",
                     "q05_post_breakyear",
                     "q25_post_breakyear",
                     "q50_post_breakyear",
                     "q75_post_breakyear",
                     "q95_post_breakyear",
                     "iqr_post_breakyear",
                     "pre_breakyear_single_value",
                     "breakyear_single_value",
                     "post_breakyear_single_value")
  
  tmp <- rep(indices, each=length(names_metrics))
  names_all <- c(names_fitted, paste(tmp, names_metrics, sep="_"))
  
  tmp <- rep(paste0("break_", 1:breaks), each=length(names_all))
  names_all <- c("n_breaks", paste(tmp, names_all, sep="_"))
  
  return(names_all)
}
bm_metric_names_alt <- function(breaks=3, indices=c("NBR","NDV","TCB","TCG","TCW","TCD")) {
  
  names_fitted <- c("breakid_fit", "breakyear_fit","doy_fit", "preval_fit","postval_fit","mag_fit", "preslope_fit","postslope_fit",
                    "presegmag_fit","postsegmag_fit")
  
  names_metrics <- c("preval", "postval", "mag", "prevalavg", "postvalavg", "magavg", 
                     "brkyr05", "brkyr25", "brkyr50", "brkyr75", "brkyr95", "brkyriqr",
                     "prebrkyr05", "prebrkyr25", "prebrkyr50", "prebrkyr75", "prebrkyr95", "prebrkyriqr",
                     "postbrkyr05", "postbrkyr25", "postbrkyr50", "postbrkyr75", "postbrkyr95", "postbrkyriqr",
                     "obsbrkyr", "obsprebrkyr", "obspostbrkyr")
  
  tmp <- expand.grid(names_metrics, tolower(indices))
  names_all <- c(names_fitted, paste(tmp$Var1, tmp$Var2, sep="_"))
  
  tmp <- rep(paste0("bp", 1:breaks), each=length(names_all))
  names_all <- c("nbreaks_fit", paste(tmp, names_all, sep="_"))
  
  return(names_all)
}
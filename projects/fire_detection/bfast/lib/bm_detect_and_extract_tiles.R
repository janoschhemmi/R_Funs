#' Breakpoint detection and extraction for raster tiles
#'
#' @title Breakpoint detection and extraction for raster tiles
#' @description Detects breakpoints using bfast/strucchange and extracts related metrics useful as 
#' predictors for classification.
#' @param in_files Vector of file names. Input are raster files holding time series of spectral values. 
#' Time series of different indices are stacked. First layer must be a mask denoting pixels to 
#' process (1) and skip (0). Layer names must contain the date and index code using the following 
#' convention: `YYYYMMDD_xxx_IND` where `IND` is the index code and `xxx` are irrelevant characters, 
#' e.g. `19850209_LND05_NBR`.
#' @param outpath character. Directory name for the output
#' @param fit_index character. Index used for fitting bfast/strucchange
#' @param h numeric. Minimal segment size either given as fraction relative to the sample size or 
#' as an integer giving the minimal number of observations in each segment. Defaults to `40`.
#' @param breaks numeric. Maximum number of breakpoint, defaulting to `3`.
#' @param order_harmonic numeric. Order of the harmonic term, defaulting to `1`.
#' @param expand boolean. Expand time series values at beginning and end by one year, defaulting to `TRUE`.
#' @param overwrite boolean. Overwrite output file if it exists, defaulting to `FALSE`
#' @param debug_row numeric. Limit processing to this row. Debugging purposes.
#' @param cores numeric. Number of parallel worker processes.
#' @return Nothing. Writes raster file to outpath
#' @seealso [bm_detect_and_extract_samples()] for sample data
#' and [bm_detect_and_extract_breaks()] for a single vector (pixel).
#' @author Dirk Pflugmacher
#' @author Jan Hemmerling
#' @export
#' @md
#' 

bm_detect_and_extract_tiles <- function(in_files, outpath, fit_index="nbr",
                                        h=40, breaks=3, order_harmonic=1, 
                                        expand=T, overwrite=F, debug_row=NA, 
                                        cores=1) {
  
  if (cores==0) return(invisible())
  
  if (!dir.exists(outpath)) dir.create(outpath)
  
  nodata_value <- -9999
  
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    `%doit%` <- foreach::`%dopar%`
  } else {
    `%doit%` <- foreach::`%do%`
  }
  
  foreach::foreach(in_file=in_files, 
           #        .packages=c('bfast', 'zoo', 'lubridate', 'terra', 'strucchangeRcpp'),
                   .export=c("bm_detect_and_extract_breaks", "bm_metric_names", 
                             "bm_expand_one_year", "bm_fix_date")) %doit% {
                     

                     outdir <- file.path(outpath, basename(dirname(in_file)))

                     if (!dir.exists(outdir)) dir.create(outdir)
                     
                     out_file <- file.path(outdir, basename(in_file))
                     
                     if (file.exists(out_file) & !overwrite) return(NULL)

                     tmp_file <- file.path(outdir, paste0("_tmp_", basename(in_file)))
                     
                     # read raster file
                     inRaster <- terra::rast(in_file)
                     
                     # get names of layers
                     layer_names  <- names(inRaster)[-1]
                     
                     # create vector of spectral index labels matching to specval
                     index    <- try(sapply(strsplit(layer_names, "_"), function(x) (x[3])))
                     # index <- rep(index_list, each = length(dates))
                     if ( (class(index)[1]=="try-error") ) {
                       print(paste0("No index names in layers: ", basename(in_file)))
                       return(NULL)
                     }
                     
                     # Indices stacked in input TS-stack
                     index_list <-  unique(index) # c("NBR","NDV","TCB","TCG","TCW","TCD")
                     
                     if (!tolower(fit_index) %in% tolower(index_list)) {
                       print(sprintf("No %s in layers: %s", toupper(fit_index), basename(in_file)))
                       return(NULL)
                     }

                     
                     # get dates of stack; requires coherence of stacked raster
                     dates    <- try(as.Date(substr(layer_names, 1, 8), format="%Y%m%d"))
                     if ( (class(dates)[1]=="try-error") ) {
                       print(paste0("Dates error in ", basename(in_file)))
                       return(NULL)
                     }
                     
                     # if we have feb 29 th replace / Feb 29th can cause zoo error 
                     # also fix duplicate dates
                     for (indx in index_list) {
                       dates[index==indx] <- bm_fix_date(dates[index==indx])
                     }
                     
                     # create metric names
                     names_all <- bm_metric_names(breaks=3, indices=index_list)
                     nLayers <- length(names_all)
                     
                     # create empty raster
                     outRaster <- terra::init(terra::subset(inRaster, 1:nLayers), nodata_value)
                     names(outRaster) <- names_all
  
                     # start empty raster brick
                     ws <- terra::writeStart(outRaster, tmp_file, overwrite=T,  datatype="INT2S")
                     
                     
                     if (is.na(debug_row)) {
                       x <- 1
                       y <- nrow(inRaster)
                     } else {
                       x <- debug_row
                       y <- debug_row
                     }
                     
                     # loop through rows
                     for (i in x:y) {
                       
                       n_rcols <- terra::ncol(inRaster)
                       inputRow <- terra::values(inRaster, row=i, nrows=1)
                       outRow <- matrix(data=nodata_value, nrow=n_rcols, ncol=nLayers)
                       
                       if (any(inputRow[,1] != 0)) {

                         # loop through columns
                         for (j in 1:n_rcols) {
                           if (is.na(inputRow[j,1])==F & inputRow[j,1]!=0) {

                             outRow[j,] = bm_detect_and_extract_breaks(as.numeric(inputRow[j,-1]),
                                                                       dates=dates,
                                                                       index=index,
                                                                       fit_index=fit_index,
                                                                       h=h,
                                                                       breaks=breaks,
                                                                       order_harmonic=order_harmonic,
                                                                       expand=expand)

                           }
                         }
                       }
                       
                       # write row
                       terra::writeValues(x=outRaster, v=outRow, start=i, nrows=1)
                     }
                     
                     log <- terra::writeStop(outRaster)
                     
                     file.rename(tmp_file, out_file)
                     file.rename(paste0(tmp_file, ".aux.xml"), paste0(out_file, ".aux.xml"))
                     }
  
  if (cores > 1) parallel::stopCluster(cl)
}


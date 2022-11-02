#' Stack and retile FORCE time series raster
#'
#' @title Stack and retile FORCE time series raster
#' @description Stacks time series raster from FORCE into a single file and chunks them into smaller tiles. These tiles 
#' then serve as input to `bm_detect_and_extract_tile()``
#' @param in_path character. Path to FORCE folder containing TSS raster.
#' @param out_path character. Path to hold the output raster.
#' @param size numeric. Vector specifying the number of subtiles in x and y direction. Defaults to `c(4, 4)`.
#' @param indices character. Name codes of spectral indices to stack. `c("NBR","NDV","TCB","TCG","TCW","TCD")`
#' @param mask_file character. File name of the mask to include in the output stack. Pixels with mask values `1` are 
#' processed and pixels with mask values `0` are skipped by bfasts.
#' @param cores numeric. Number of parallel worker processes.
#' @param include numeric. List of FORCE tile id's to include.
#' @return Nothing. Writes to raster files to `out_path`.
#' @seealso [bm_detect_and_extract_tiles()] for creating breakpoint metrics.
#' @author Dirk Pflugmacher
#' @author Jan Hemmerling
#' @export
#' @md
#' 
bm_stack_retile_force_tss <- function(in_path, out_path, size=c(4,4), 
                                indices=c("NBR","NDV","TCB","TCG","TCW","TCD"),
                                mask_file=NULL, cores=1, include=NULL) {
  
  if (cores==0) return(invisible(NULL))

  if (is.null(mask_file)) {
    message(paste("Mask file not specified."))
    return(invisible(NULL))
  }
  
  if (!file.exists(mask_file)) {
    message(paste("Mask file not found."))
    return(invisible(NULL))
  }

  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    `%doit%` <- foreach::`%dopar%`
  } else {
    `%doit%` <- foreach::`%do%`
  }
  
  if (!dir.exists(out_path)) dir.create(out_path)
  tile_codes <- grep("^X", basename(list.dirs(in_path)), value=T)
  
  if (!is.null(include)) {
    tile_codes <- tile_codes[tile_codes %in% include]
  }
  
  inpath_list <- file.path(in_path, tile_codes)
  
  
  foreach::foreach(inpath=inpath_list, .packages=c('terra')) %doit% {
    
    out_basename <- file.path(out_path, basename(inpath), paste0(basename(inpath), "_", paste(indices, collapse="-"), "_.tif"))
    
    if (dir.exists(dirname(out_basename))) {
      message(sprintf("%s output exists.", basename(inpath)))
      return(NULL)
    } else {
      message(sprintf("%s is retiling", basename(inpath)))
      dir.create(dirname(out_basename))
    }
    
    rast_list <- list.files(inpath, "_TSS.tif$")
    
    if (length(rast_list) == 0) {
      message(paste("No files found in", inpath))
      return(NULL)
    }
    
    # put layers in the order of the indices list
    i <- as.numeric(sapply(indices, function(x) grep(x, rast_list), simplify = T))
    if (any(is.na(i))) {
      message(sprintf("Missing %s in tile %s", paste(indices[is.na(i)], collapse=", "), inpath))
      return(NULL)
    }
    rast_list <- rast_list[i]
    
    # stack indices and add index to layername
    ras_stack <- terra::rast(file.path(inpath, rast_list))
    layer_names <- paste(names(ras_stack), rep(indices, each = terra::nlyr(ras_stack) / length(indices)), sep="_")
    
    names(ras_stack) <- layer_names
    
    # get mask
    mask_ras <- terra::rast(mask_file)
    mask_ras <- terra::crop(mask_ras, ras_stack)
    terra::ext(mask_ras) <- terra::ext(ras_stack)
    
    ras_stack <- c(mask_ras, ras_stack)
    names(ras_stack) <- c("mask", layer_names)
    
    x <- terra::rast(ncols=4, nrows=4)
    terra::ext(x) <- terra::ext(ras_stack)
    
    ff <- terra::makeTiles(ras_stack, x, out_basename, datatype = "INT2S")
    
  }
  
  if (cores > 1) parallel::stopCluster(cl)
  
}
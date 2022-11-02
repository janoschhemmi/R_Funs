require(tidyr)

p.timesync <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/timesync/'

f.strata_samples <- file.path(p.timesync, 'strata/bb_fdist_p27_19862019_disturbed_year_mmu6n4_tsync-samples.csv')
f.strata_weights <- file.path(p.timesync, 'strata/bb_fdist_p27_19862019_disturbed_year_mmu6n4.vat.csv')

read.training_samples <- function(fname, process=NULL, asFactors=T) {
  
  ds <- read.csv(fname, stringsAsFactors = F)
  row.names(ds) <- ds$id
  
  if (is.null(process)) process <- c("Harvest", "Wind", "Decline", "Fire", "Hydrology", "Debris", "Other", "Purple")
  
  ds$class_disturbed <- ifelse(ds$process %in% process, 1, 0)
  ds$class_cleared <- ifelse(ds$class_disturbed == 1 & ds$landcover == "Non-tree", 1, 0)

  ds$class_abrupt <- ifelse(ds$duration %in% c(1), 10, 0) # code abrupt disturbance 10
  ds$class_distype <- ds$class_disturbed + ds$class_cleared # 0:undisturbed 1: partial 2:cleared
  ds$class_distype5 <- ds$class_distype + ds$class_abrupt
  ds$class_distype5[ds$class_distype5==10] <- 0  
  ds$class_distype5 <- ifelse(ds$class_distype5 == 11, 3, ds$class_distype5)
  ds$class_distype5 <- ifelse(ds$class_distype5 == 12, 4, ds$class_distype5) # 0:undisturbed 1: partial 2:cleared, 3: abrupt partial, 4:abrupt cleared
  
  # sampling weights 
  # ctab <- data.frame(country=c('austria', 'czech','germany','poland','slovakia', 'switzerland'),
  #                    land_area = c(39600, 26000, 114190, 90000, 20006, 12540),
  #                    forest_prop = c(0.472, 0.34, 0.32, 0.288, 0.488, 0.318), stringsAsFactors = F)
  # ctab$forest_area <- ctab$land_area * ctab$forest_prop
  # ctab$stratum_weight <- ctab$forest_area/sum(ctab$forest_area)
  # 
  # ds$sp <- aa_selection_probability(ds$country, ctab[,c('country', 'stratum_weight')]) *nrow(ds)
  return(ds)
}

read_landtrendr_vertices <- function(fname) {
  
  ds <- read.csv(fname, stringsAsFactors = F)
  
  ds <- na.omit(ds[, !names(ds) %in% c('.geo', 'system.index')])
  i <- which(names(ds) %in% c('plotid'))
  ds.ftv = gather(ds[, c(i, grep('ftv', names(ds)))], vertex, ftv, -plotid)
  ds.ftv$vertex <- as.numeric(gsub('ftv', '', ds.ftv$vertex))
  ds.doy = gather(ds[, c(i, grep('doy', names(ds)))], vertex, doy, -plotid)
  ds.doy$vertex <- as.numeric(gsub('doy', '', ds.ftv$vertex))
  df <- merge(ds.doy, ds.ftv)
  df <- df[df$doy!=0,]
  return(df)
}


disturbanceFactors <- function(ds) {
  
  
  ds$disturbed <- factor(ds$class_disturbed, levels=0:1, labels=c('undisturbed', 'disturbed'))
  ds$cleared <- factor(ds$class_cleared, levels=0:1, labels=c('partial', 'cleared'))
  ds$distype <- factor(ds$class_distype, levels=0:2, labels=c('undisturbed', 'partial', 'cleared'))
  ds$distype5 <- factor(ds$class_distype5, levels=c(0,1,2, 3, 4), 
                        labels=c('undisturbed', 'part-long', 'clear-long', 'part-abrupt', 'clear-abrupt'))

  # lc.lut <- data.frame(id=1:13, class=c('artificial', 'cropland seasonal', 'cropland perennial', 
  #                                       'DBF', 'ENF', 'MIX', 
  #                                       'shrubland', 'grassland', 'bare land', 'water', 'wetland inland',
  #                                       'wetland coastal', 'snow ice'))
  # ds$lc1985 <- factor(ds$lc1985, levels=lc.lut$id, labels=lc.lut$class)
  # ds$lc2015 <- factor(ds$lc2015, levels=lc.lut$id, labels=lc.lut$class)
  return(ds)
}



export_training <- function(dataset, outbase, shp_file=NULL, country=NULL) {
  
  
  if (!is.null(country)) dataset <- dataset[dataset$country %in% country, ]
  
  tmp <- dataset[dataset$discount > -4, !names(dataset) %in% c('country', 'year', 'process', 'disturbance', 'standreplacing',
                                                               'interpreter', 'discount', 'plotid', 'id', 'lc1985', 'lc2015')]
  
  names(tmp) <- sub('spectral', 'sp', names(tmp))
  names(tmp) <- sub('duration', 'dt', names(tmp))
  names(tmp) <- sub('delta', 'ds', names(tmp))
  # tmp$class <- tmp$class + 1

  write.csv(tmp, paste0(outbase, '.csv'), row.names=F)
  
  if (!is.null(shp_file)) {
    
    require(rgdal)
    require(sp)
    shp <- readOGR(shp_file)
    if (!is.null(country)) shp <- shp[shp$country %in% country,]
    shp.out <- merge(tmp, shp@data, by='plotid', all.x=T, all.y=F)
    shp.out <- SpatialPointsDataFrame(shp.out[,c('laea_x', 'laea_y')], shp.out, proj4string=shp@proj4string)
    writeOGR(shp.out, dirname(outbase), basename(outbase), driver="ESRI Shapefile", overwrite_layer=T)
  }
  
  
}

compile_landtrendr_features_from_vertices <- function(fname, scale=T) {
  
  
  ltr_expand <- function(x) {
    x <- x[order(x$doy),]
    a <- approx(x$doy, x$ftv, n=length(min(x$doy):max(x$doy)))
    dur <- diff(x$doy)
    dur <- c(0, rep(dur, dur))
    return(data.frame(year=a$x, sp=a$y, dt=dur, ds=c(0,diff(a$y)) ))
  }
  
  ds <- read_landtrendr_vertices(fname)
  
  if (scale) {
    slut <- data.frame(i = toupper(c('b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'tcb', 'tcg',
                                     'tcw', 'tca', 'nbr', 'ndvi', 'ndmi')),
                       s = c(1, 1, 1, -1, 1, 1, 1, -1, -1, -1, -1, -1, -1))
    
    band <- strsplit(gsub('.csv', '', basename(fname)), '_')[[1]][3]
    s <- slut$s[slut$i==band]
    
    if (length(s)==0) {
      message('error in scalar')
      return(invisible(NULL))
    } else {
      ds$ftv <- ds$ftv * s
    }
    
  }
  
  band <- tolower(strsplit(gsub('.csv', '', basename(fname)), '_')[[1]][3])
  
  result <- ds %>% nest_by(plotid) %>% summarise(across(everything(), .fns=ltr_expand), .groups="drop")
  result <- cbind(plotid=result$plotid, result$data)
  
  
  j <- which(names(result) %in% c('sp', 'dt', 'ds'))
  names(result)[j] <- paste(names(result)[j], band, sep='_')
  message(paste('Compiled', fname))
  return(result)
}


compile_landtrendr_features_batch <- function(path, out_csv_file, overwrite=F) {
  
  if (file.exists(out_csv_file) & overwrite==F) {
    message(paste("File exists:", out_csv_file))
    return(invisible(NULL))
  }
  
  fn <- list.files(path, glob2rx('*vertices*.csv'), full.names=T)
  
  regions <- substr(basename(fn), 1, 2)
  result <- NULL
  for (r in unique(regions)) {
    message(paste('### ', r, ' #################################'))
    fnsub <-fn[regions == r]
    result.sub <- NULL
    for (i in 1:length(fnsub)) {
      tmp.sub <- compile_landtrendr_features_from_vertices(fnsub[i], scale=T)
      tmp.sub$region <- r
      if (i==1) result.sub <- tmp.sub else result.sub <- merge(result.sub, tmp.sub)
    }
    result <- rbind(result, result.sub)
  }
  
  j = grep('ds_', names(result))
  result$discount = apply(result[,j], 1, function(x) sum(x < 0))
  
  write.csv(result, out_csv_file, row.names = F)
}


compile_tsync_training <- function(fn_tsync_interpretations, fn_landtrendr_features, out_file) {
  
  tsv_expand <- function(x) {
    x <- x[order(x$image_year),]
    y <- data.frame(year=min(x$image_year):max(x$image_year))
    y <- merge(y, x, by.y='image_year', by.x='year', all=T)
    dur <- diff(x$image_year)
    y$process <- c(NA, rep(x$change_process[-1], dur))
    y$landcover <- c(x$landcover[1], rep(x$landcover[-1], dur))
    y$landuse <- c(x$landuse[1], rep(x$landuse[-1], dur))
    
    dur <- diff(x$image_year)
    y$duration <- c(0, rep(dur, dur))
    
    isNontree <- ifelse(y$landcover == 'Non-tree', 0, 1)
    y <- y[cumsum(isNontree) > 0,]
    
    return(y[,c('year', 'process', 'duration', 'landuse', 'landcover')]) #'disturbed', 'cleared'
  }
  
  # landtrendr features
  ltr_features <- read.csv(fn_landtrendr_features, stringsAsFactors = F)
  
  # input timesync interpretations
  tsync_data <- read.csv(fn_tsync_interpretations, stringsAsFactors = F)
  
  # determine number of time a plot was interpreted as forest
  func <- function(x) data.frame(forestcount=sum(x$landuse=='Forest'),
                                 nonforestcount=sum(x$landuse=='Non-forest'))
  tsync_forest <- tsync_data %>% nest_by(plotid) %>% summarise(across(everything(), .fns=func), .groups="drop")
  tsync_forest <- cbind(plotid=tsync_forest$plotid, tsync_forest$data)
  
  # merge with interpretation table to delete all plots that were never forest
  tsync_forest <- merge(tsync_forest, tsync_data)
  tsync_forest <- tsync_forest[tsync_forest$forestcount != 0,]
  
  # convert interpretation table from row-per-vertex format to row-per-year format
  #result <- ddply(tsync_forest, c("project_id", "plotid"), function(x) tsv_expand(x), .parallel = F)
  result <- tsync_forest %>% nest_by(plotid) %>% summarise(across(everything(), .fns=tsv_expand), .groups="drop")
  result <- cbind(plotid=result$plotid, result$data)
  result <- result[!(result$process=="Stable" & result$landuse=="Non-forest"),]
  
  # merge
  result <- merge(result, ltr_features, by=c('plotid', 'year'))
  
  result <- result[!is.na(result$process),]
  
  names(result) <- sub('spectral_', 'sp_', names(result))
  names(result) <- sub('duration_', 'dt_', names(result))
  names(result) <- sub('delta_', 'ds_', names(result))
  
  result$id <- result$plotid * 10000 + result$year
  
  write.csv(result, out_file, row.names = F)
  
}





startOfDisturbance <- function(ds, varNames=NULL, disturbanceClass=NULL, position=F){
  
  if (!all(varNames %in% names(ds))) {
    message(paste('varNames not found:', varNames[!(varNames %in% names(ds))]))
    return()
  }
  
  identify_start <- function(df) {
    
    # identify first occurrances in a binary series
    
    df <- df[order(df$year),]
    
    for (varName in varNames) {
      # add 0 to allow disturbances in the first year
      
      x <- ifelse(df[, varName]==disturbanceClass, 1, 0)
      z <- rle(as.character(x))$lengths
      
      x <- c(0, x)
      
      start <- which(diff(x) == 1)
      
      x <- x[-1]

      if (length(start)>0) x[-start] <- 0    
      
      # mask out duration everywhere except at starts
      dur <- rep(z, z) * x
      
      if (position) {
        df[, paste0(varName, '_sod')] <- cumsum(x) * x
      } else {
        df[, paste0(varName, '_sod')] <- x
      }
      
      df[, paste0(varName, '_dur')] <- dur
    }
    
    return(df)
  }
  
  result <- ds %>% group_by(plotid) %>% do(identify_start(.))
  
  return(result)
}


collapse_to_first_disturbance <-  function(df, className , append=NULL, static=NULL) {
  
  require(plyr)
  
  # requires df with predicted and observed class
  
  tsv_collapse <- function(x, pos=1, class=NULL, append=NULL) {
    
    x <- x[order(x$year),]
    names(x)[names(x)=='predicted_probability'] <- 'probs'
    dnames <- c('probs', 'year', append)
    
    inds <- which(x$predicted == class)
    if (length(inds)>0) {
      firstp <- x[min(inds), dnames]
      
    } else {
      firstp <- data.frame(matrix(ncol = length(dnames), nrow = 1))
      names(firstp) <- dnames
      firstp[is.na(firstp)] <- 0
      for (v in static) {
        firstp[,v] <- x[1,v]
      }
    }

    firsto <- x[which(x$observed == class), ]
    if (nrow(firsto)>0) {
      firsto <- firsto[which.min(abs(firsto$year-firstp$year)), dnames]
    } else {
      firsto <- data.frame(matrix(ncol = length(dnames), nrow = 1))
      names(firsto) <- dnames
      firsto[is.na(firsto)] <- 0
      for (v in static) {
        firsto[,v] <- x[1,v]
      }
    }
    
    names(firstp) <- paste0(names(firstp), '_pred')
    names(firsto) <- paste0(names(firsto), '_obs')
    
    return(cbind(firsto, firstp))
  }
  
  result <- ddply(df, c("plotid"), function(x) tsv_collapse(x, class=className), .parallel = F)  

  result$year_pred[result$year_pred == 0] <- 1970
  result$year_obs[result$year_obs == 0] <- 1970
  
  result$disturbed_pred <- ifelse(result$year_pred > 1970, 1, 0)
  result$disturbed_obs <- ifelse(result$year_obs > 1970, 1, 0)
  
  return(result)
}

aa_first_disturbance <- function(result, pngBasename) {
  
  # sampling weights 
  ctab <- data.frame(country=c('austria', 'czech','germany','poland','slovakia', 'switzerland'),
                     land_area = c(39600, 26000, 114190, 90000, 20006, 12540),
                     forest_prop = c(0.472, 0.34, 0.32, 0.288, 0.488, 0.318), stringsAsFactors = F)
  ctab$forest_area <- ctab$land_area * ctab$forest_prop
  ctab$stratum_weight <- ctab$forest_area/sum(ctab$forest_area)
  
  result$sp_prop <- aa_selection_probability(result$country, ctab[,c('country', 'stratum_weight')]) 
  result$sp_tot <- result$sp_prop *nrow(result)
  
  cm <- aa_confusion_matrix(result$disturbed_obs, result$disturbed_pred, prob=result$sp_tot)
  #colnames(cm) <- c('undisturbed', 'disturbed')
  #rownames(cm) <- c('undisturbed', 'disturbed')
  
  aa <- aa_card(cm, confusion_matrix = T)
  write.csv(aa$stats, file.path(fig.path, paste0(pngBasename, '_accuracy.csv')))
  write.csv(aa_confusion_matrix_bind(aa, accuracy.multiplier = 100), file.path(fig.path, paste0(pngBasename, '_accuracy_cm.csv')))
  
  
  result$diff <- abs(result$year_pred - result$year_obs)
  result$pred_year_fuzzy <- ifelse(result$diff < 2, result$year_obs, result$year_pred)
  aa.fuzzy <- aa_card(result[,c('year_obs', 'pred_year_fuzzy')], confusion_matrix = F)
  write.csv(aa.fuzzy$stats, file.path(fig.path, paste0(pngBasename, '_accuracy_fuzzy1year.csv')))
  write.csv(aa_confusion_matrix_bind(aa.fuzzy), file.path(fig.path, paste0(pngBasename, '_accuracy_fuzzy1year_cm.csv')))
  
  aa.stack <- aa.fuzzy$stats[-1,c('class', 'ua', 'ua_se')]
  names(aa.stack) <- c('year', 'accuracy', 'se')
  aa.stack$year <- as.numeric(as.character(aa.stack$year))
  aa.stack$type <- "User's accuracy"
  aa.stack2 <- aa.fuzzy$stats[-1,c('class', 'pa', 'pa_se')]
  names(aa.stack2) <- c('year', 'accuracy', 'se')
  aa.stack2$year <- as.numeric(as.character(aa.stack2$year))
  aa.stack2$type <- "Producer's accuracy"
  aa.stack <- rbind(aa.stack, aa.stack2)
  aa.stack$low <- aa.stack$accuracy - aa.stack$se
  aa.stack$high <- aa.stack$accuracy + aa.stack$se
  
  p1 <- ggplot(aa.stack, aes(year, accuracy, group=type)) +
    geom_line(aes(colour=type)) + ylab('Accuracy') + xlab('Year') +
    geom_ribbon(aes(ymin=low, ymax=high, fill=type), alpha=0.2) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank(), legend.position="bottom") 
  
  ggsave(file.path(fig.path, paste0(pngBasename, '_accuracy_fuzzy1year.png')), plot=p1, device='png', 
         width=90, height=90, dpi=600, units='mm', scale=1.2)
  
  
  result.dist <- result[result$year_pred != 1970 & result$year_obs != 1970,]
  
  
  # add frequency colum
  xt <- as.data.frame(table(result.dist$year_pred, result.dist$year_obs), stringsAsFactors=F)
  names(xt) <- c('year_pred', 'year_obs', 'freq')
  xt$year_pred <- as.numeric(xt$year_pred)
  xt$year_obs <- as.numeric(xt$year_obs)
  result.dist <- merge(result.dist, xt, all.x=T)
  result.dist$Samples <- ifelse(result.dist$freq > 15, 15, result.dist$freq)
  
  p2 <- ggplot(result.dist) +
    geom_point(aes(year_pred, year_obs, col=Samples), shape = 16, size = 2) +
    theme_bw()  + geom_abline(intercept=0, slope=1) +
    ylab('Observed year') + xlab('Predicted year') +
    scale_colour_gradient(low = "lightblue", high = "black") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position="bottom")
  
  ggsave(file.path(fig.path, paste0(pngBasename, '_scatterplot.png')), plot=p2, device='png', 
         width=90, height=95, dpi=600, units='mm', scale=1.2)
  
  return(result.dist)
  
}

# aa_rfm <- function(modelFile=NULL, probThreshold=NULL, fig.path=NULL, rfm.path=NULL, years=NULL) {
#   
#   rfm <- readRDS(modelFile)
#   ds.val <- readRDS(paste0(modelFile, '.csv.rds'))
#   
#   bn <- grep(".rds", "", basename(modelFile))
#   
#   pngBasename <- paste0('classerror_', rfm$classes[2], '_', bn)
#   
#   # unadjusted
#   result.uad <- aa_commission_omission_plot(ds.val$observed, ds.val$predicted_probability, 
#                                             class=rfm$classes[2], increment=0.01,
#                                             png_file=file.path(fig.path, paste0(pngBasename, '_uadj.png')))
#   # adjusted plot
#   result.adj <- aa_commission_omission_plot(ds.val$observed, ds.val$predicted_probability, strata_codes=, 
#                                             class=rfm$classes[2], increment=0.01, 
#                                             png_file=file.path(fig.path, paste0(pngBasename, '.png')))
#   
#   # area-adjusted
#   ds.val$predicted <- ifelse(ds.val$predicted_probability > probThreshold, rfm$classes[2], rfm$classes[1])
#   cm <- aa_confusion_matrix(ds.val$observed, ds.val$predicted, prob=ds.val$sp) 
#   aa <- aa_card(round(cm), confusion_matrix=T)
#   aa_probs_annual(ds.val, file.path(fig.path, paste0(pngBasename, '_annual.png')), years=years)
# }

aa_annual <- function(df, stratum_code=NULL, stratum_count=NULL, png_file, years=NULL) {
  
  require(pixelr)
  
  ## annual accuracy
  aa.annual <- data.frame(year=NULL, accuracy=NULL, se=NULL, type=NULL, n=NULL)
  area.annual <- data.frame(year=NULL, proportion=NULL, se=NULL)
  for (year in unique(df$year)) {
    tmp <- df[df$year==year,]
    
    aa <-  aa_stratified(tmp$stratum, tmp$observed, tmp$predicted, h=stratum_code, N_h=stratum_count)
    
    aa.annual <- rbind(aa.annual, data.frame(year=year, 
                                             accuracy=c(aa$stats$ua[1], aa$stats$pa[1]),
                                             se=c(aa$stats$ua_se[1], aa$stats$pa_se[1]),
                                             type = c("User's accuracy", "Producer's accuracy"),
                                             n=nrow(tmp)))
    
    area.annual <- rbind(area.annual, data.frame(year=year,
                                                 proportion=aa$area$proportion[1],
                                                 se=aa$area$proportion_se[1]))
  }
  aa.annual$low <- aa.annual$accuracy - aa.annual$se
  aa.annual$high <- aa.annual$accuracy + aa.annual$se
  
  area.annual$low <- area.annual$proportion - area.annual$se
  area.annual$high <- area.annual$proportion + area.annual$se
  
  if (!is.null(years)) aa.annual <- aa.annual[aa.annual$year %in% years,]
  if (!is.null(years)) area.annual <- area.annual[area.annual$year %in% years,]
  
  p1 <- ggplot(aa.annual, aes(year, accuracy, group=type)) +
    geom_line(aes(colour=type)) + ylim(0.2,1.2) + ylab('Accuracy') + xlab('Year') +
    geom_ribbon(aes(ymin=low, ymax=high, fill=type), alpha=0.2) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank(), legend.position="bottom") 
  ggsave(png_file, plot=p1, device='png', width=90, height=90, dpi=600, units='mm', scale=1.2)
  
  p2 <- ggplot(area.annual, aes(year, proportion)) +
    geom_line() + ylab('Area proportion') + xlab('Year') +
    geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank(), legend.position="bottom") 
  ggsave(paste0(png_file, '_area.png'), plot=p2, device='png', width=90, height=90, dpi=600, units='mm', scale=1.2)
  
  write.csv(aa.annual, paste0(png_file, '.csv'), row.names=F)
  write.csv(area.annual, paste0(png_file, '_area.csv'), row.names=F)
  
}


# aa_with_prob <- function(rfmodel, prob, class='disturbed') {
#   require(pixelr)
#   probs <- predict(rfmodel, type='prob')
#   classes <- unique(as.character(rfmodel$y))
#   df <- data.frame(observed=as.character(rfmodel$y),
#                    predicted=ifelse(probs[, class] > prob, class, classes[classes!=class]),
#                    stringsAsFactors=F)
#   if (is.factor(df$observed)) df$observed <- droplevels(df$observed)
#   if (is.factor(df$predicted)) df$predicted <- droplevels(df$predicted)
#   
#   aa <- aa_card(cbind(df$observed, df$predicted), confusion_matrix=F)
#   return(aa)
# }

aa_commission_omission_plot <- function(observed, predicted_probability, strata=NULL, strata_codes=NULL, strata_count=NULL,
                                        increment=0.05, png_file=NULL, sub=NULL, class='disturbed') {
  
  require(tidyr)
  require(ggplot2)
  require(pixelr)

  classes <- unique(as.character(observed))
  
  result <- data.frame(accuracy=NULL, se=NULL, type=NULL, prob=NULL)
  for (i in seq(0, 0.9, increment)) {

    predicted <- ifelse(predicted_probability > i, class, classes[classes!=class])
    
    if (is.null(strata)) {
      aa <- aa_card(cbind(observed, predicted), confusion_matrix=F)
    } else {
      aa <- aa_stratified(strata, observed, predicted, h=strata_codes, N_h=strata_count)
    }
    
    result <- rbind(result, data.frame(accuracy = c(aa$stats$ua[aa$stats$class==class], aa$stats$pa[aa$stats$class==class]),
                                       se = c(aa$stats$ua_se[aa$stats$class==class], aa$stats$pa_se[aa$stats$class==class]),
                                       type = c("User's accuracy", "Producer's accuracy"),
                                       prob=i))
  }
  
  temp.ua <- result[result$type=="User's accuracy",]
  temp.pa <- result[result$type=="Producer's accuracy",]
  temp <- merge(temp.ua, temp.pa, by='prob')
  balanced <- temp[which.min(abs(temp$accuracy.x-temp$accuracy.y)),]
  
  thresh <- balanced$prob
  tua <- balanced$accuracy.x
  tpa <- balanced$accuracy.y
  result$low <- result$accuracy - (result$se)
  result$high <- result$accuracy + ( result$se )

  title <- paste0('P: ', thresh, ' | A: ', round(tua,3))
  if (!is.null(sub)) title <- paste0(sub, ' | ', title)
  p <- ggplot(result, aes(x=prob, y=accuracy, grouop=type)) + 
    geom_line(aes(colour=type)) + xlab('Probability') + ylab('Accuracy') +
    ggtitle(title) + 
  #  theme(legend.title=element_blank()) +
  #  theme(legend.position="bottom") +
    geom_ribbon(aes(ymin=low, ymax=high, fill=type), alpha=0.2) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
          #  panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.title=element_blank(), legend.position="bottom") 
  
  if (!is.null(png_file)) {
    ggsave(png_file, plot=p, device='png', width=90, height=70, dpi=600, units='mm', scale=1.4)
    write.csv(result, paste0(png_file, '.csv'), row.names=F)
  } else {
    print(p)
  }
  
  return(invisible(list(accuracy=result, threshold=thresh, ua=tua, pa=tpa, ggplot=p)))
}

aa_validationData <- function(ds.val, model=NULL, out.path=NULL) {
  
  ds.val$observed <- model$y
  ds.val$predicted_probability <- predict(model, type='prob')[, model$classes[2]]
  
  # add stratum info
  strata_samples <- read.csv(f.strata_samples)
  ds.val <- merge(ds.val, strata_samples, by='plotid')
  
  if (!is.null(out.path)) {

    out_rds_file   <- file.path(out.path, paste0(substitute(model), ".csv.rds"))
    saveRDS(ds.val, out_rds_file)
    
    out_csv_file   <- file.path(out.path, paste0(substitute(model), ".csv"))
    write.csv(ds.val, out_csv_file, row.names=F)
    message(out_csv_file)
  }

  return(invisible(ds.val))
}


plot_multiple_ggplots <- function(mylist, png_file=NULL, ncol=3) {
  
  if (is.null(png_file)) return
  
  require(grid, warn.conflicts = F)
  require(gridExtra, warn.conflicts = F, quietly=T)
  
  figs = list()
  for (i in names(mylist)) figs[[i]] <- mylist[[i]]$ggplot
  g <- grid.arrange(grobs=figs, ncol=ncol)
  ggsave(png_file, plot=g, device='png', width=120, height=80, dpi=1200, units='mm', scale=2.2)
  detach("package:gridExtra", unload=TRUE)
}

plot_ts <- function(data, plotid, country) {
  require(ggplot2)
  require(tidyr)
  tmp <- data[data$plotid==plotid & data$country==country,]
  
  if (nrow(tmp)==0) {
    message('No plotid')
    return(invisible(NA))
  }
  
  #  tmp <- tmp[,c('year', 'spectral_b1', 'spectral_b2', 'spectral_b3', 'spectral_b4', 'spectral_b5', 
  #                 'spectral_b7', 'spectral_tca', 'spectral_tcg', 'spectral_tcw')]
  tmp <- tmp[,c('year', 'spectral_tcb', 'spectral_tcg', 'spectral_tcw')]
  tmp = gather(tmp, band, reflectance, -year)
  tmp$reflectance <- tmp$reflectance/10000
  p <- ggplot(tmp, aes(x=year, y=reflectance, colour=band)) + 
    geom_line() + ggtitle(plotid)
  print(p)
  return(invisible(data[data$plotid==plotid & data$country==country,]))
}

load_gee_data <- function(version=c('landtrendr_verts10', 'landtrendr_verts12')) {
  j <- c('TCW', 'NBR', 'NDVI', 'NDMI')
  tsrc <<- list()
  vrts6 <<- list()
  vrts10 <<- list()
  vrts12 <<- list()
  
  p.source <- file.path(p.timesync, 'landtrendr_source')
  p.vrts6 <- file.path(p.timesync, 'landtrendr_verts06')
  p.vrts10 <- file.path(p.timesync, 'landtrendr_verts10')
  p.vrts12 <- file.path(p.timesync, 'landtrendr_verts12_despike50')
  
  for (i in j) {
    tsrc[[i]] <<- read.csv(file.path(p.source, paste0('bb_source_', i ,'.csv')))
    vrts6[[i]] <<- read.csv(file.path(p.vrts6, paste0('bb_vertices6_', i ,'.csv')))
    vrts10[[i]] <<- read.csv(file.path(p.vrts10, paste0('bb_vertices10_', i ,'.csv')))
    vrts12[[i]] <<- read.csv(file.path(p.vrts12, paste0('bb_vertices12_', i ,'.csv')))
    #vrts10[[paste0(i,'rr')]] <<- read.csv(file.path(p.vrts10, paste0('bb_reduceRegions_vertices10_', i ,'.csv')))
    #vrts12[[paste0(i,'rr')]] <<- read.csv(file.path(p.vrts12, paste0('bb_reduceRegions_vertices12_', i ,'.csv')))
  }
}

plot_ltr <- function(pid, index='TCW', rr='', dsVal=NULL, ...) {
  
  this_ts <- tsrc[[index]]

  
  if (index %in% c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7')) {
    multiplier <- 1/10000
  } else {
    multiplier <- -1/10000
  }
  
  y <- this_ts[this_ts$plotid==pid, paste0('X', 1985:2019)] * multiplier
  x <- 1985:2019
  
  plot(x, y, xlab='Year', ylab=index, pch=19, col=rgb(0, 0, 1, 0.3), las=1, ...)
  
  v <- vrts6[[index]]
  if (!is.null(v)) {
    fvx <- v[v$plotid==pid, paste0('doy', 0:6)]
    fvy <- v[v$plotid==pid, paste0('ftv', 0:6)]
    fvy <- fvy[fvx!=0] * multiplier
    fvx <- fvx[fvx!=0]
    lines(fvx, fvy, col="grey", lwd=2)
  }
  
  v <- vrts10[[paste0(index, rr)]]
  if (!is.null(v)) {
    fvx <- v[v$plotid==pid, paste0('doy', 0:10)]
    fvy <- v[v$plotid==pid, paste0('ftv', 0:10)]
    fvy <- fvy[fvx!=0] * multiplier
    fvx <- fvx[fvx!=0] 
    lines(fvx, fvy, col=4, lwd=2)
  }
  
  v <- vrts12[[paste0(index, rr)]]
  if (!is.null(v)) {
    fvx <- v[v$plotid==pid, paste0('doy', 0:12)]
    fvy <- v[v$plotid==pid, paste0('ftv', 0:12)]
    fvy <- fvy[fvx!=0] * multiplier
    fvx <- fvx[fvx!=0] 
    lines(fvx, fvy, col=2, lwd=2)
  }
  
  if (!is.null(dsVal)) {
    tmp  <- dsVal[dsVal$plotid==pid, ]
    prd <- tmp$year[tmp$predicted=="disturbed"]
    obs <- tmp$year[tmp$observed=="disturbed"]
    abline(v=obs, lwd=5, col=rgb(0,0,0,0.2))
    abline(v=prd)
    legend("bottomright", legend=c("obs", "pred"), lty=1, lwd=c(5,1), col=c(rgb(0,0,0,0.2), rgb(0,0,0)), bty='n')
  }

  legend("topleft", legend=pid, bty='n')
  legend("topright", legend=index, bty='n')
  legend("bottomleft", legend=c("v10", "v12"), lty=1, col=c(4,2), bty='n')
  grid()
}

rf_importance <- function(modelFile, outpath, norm=F, name=NULL) {
  
  require(pixelr)
  require(randomForest)
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  
  if (is.null(name)) {
    name <- substitute(rfmodel)
  }
  
  fname <- modelFile
  
  rfmodel <- readRDS(modelFile)
  
  rfim <- data.frame(importance(rfmodel))
  
  rfim$metric <- substr(row.names(rfim), 1, 2)
  rfim$band <- substr(row.names(rfim), 4, 10)
  
  
  # Metric importance ---------------------------------------------
  
  p1 <- ggplot(rfim, aes(x=metric, y=MeanDecreaseGini)) + 
    geom_boxplot() + xlab('') + ylab('MeanDecreaseGini') +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  ggsave(sub('.rds', '_importance_metrics.png', fname), plot=p1, device='png', 
         width=90, height=90, dpi=1200, units='mm', scale=1)
  
  # Band importance ------------------------------------------------
  
  p2 <- ggplot(rfim, aes(x=band, y=MeanDecreaseGini)) + 
    geom_boxplot() + xlab('') + ylab('MeanDecreaseGini') +
    theme_bw() +
    coord_flip() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  ggsave(sub('.rds', '_importance_bands.png', fname), plot=p2, device='png', 
         width=90, height=90, dpi=1200, units='mm', scale=1)
  
}
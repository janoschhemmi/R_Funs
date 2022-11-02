require(RSQLite)
require(DBI)


# 1 populates the timesync sqlite database with plot spectra extracted from GEE
# 2 adds the plot list to the timesync database (the same plot list that was used 
#   to extract spectra in GEE)
# 3 the input sqlite database can be an empty or filled copy
# 4 all content of the input sqlite database will be deleted or overwritting
# 5 searches for image chips with blank plots (e.g. because of SLC-off scanlines) 
#   and moves them to a different directory, e.g. /tc to /tc_blank. You'll get a 
#   report that X spectra are missing, because those k plots were over NA pixel.
#   The corresponding k chips will be moved (so that you can delete them yourself.)

# 2018-11-09 created by D. Pflugmacher

# SART OF PARAMTER SECTION 

# path to the output root dir supplied to the GEE scripts
path <- 'p:/timesync/bb/'

# timesync database WARNING: content will be deleted
sqlite_file <- file.path(path, 'tsync_brandsat_bb_fire_post1.dat')

# plot list
plot_file <- file.path(path, 'tsync_fires_post_training1.csv')

# project id, the same id that was used in plots.csv
project_id <- 8001

# short name of project 
project_code <- 'BerlinBrandenburg'

# path to the image chips: no need to change 
# if path is also the output directory of your GEE extraction script
# then everything should be in place
path.b743chips <- file.path(path, 'tschips', paste0('prj_', project_id), 'b743')
path.tcchips <- file.path(path, 'tschips', paste0('prj_', project_id), 'tc')

path.spectra <- file.path(path, 'spectra', paste0('prj_', project_id))

# response design
landcover=c("Tree","Non-tree")
change_process= c("Stable", "Growth", "Harvest", "Fire","Decline","Wind", "Other")
landuse=c("Forest", "Non-forest")

# END OF PARAMTER SECTION 

### functions #################################

move_blank_chips <- function(chip.path) {
  
  chip.dirty <- paste0(chip.path, '_blank')
  
  chips <- list.files(chip.path, '.png', recursive = T)
  dc <- data.frame(name=basename(chips), stringsAsFactors = F)
  dc$name <- gsub('[.]png', '', dc$name)
  dc$plotid <- as.numeric(sapply(strsplit(dc$name, '_'), FUN=function(x) x[[2]]))
  dc$image_year <- as.numeric(sapply(strsplit(dc$name, '_'), FUN=function(x) x[[3]]))
  dc$image_julday <- as.numeric(sapply(strsplit(dc$name, '_'), FUN=function(x) x[[4]]))
  
  result <- merge(df.spectra, dc, by=c('plotid', 'image_year', 'image_julday'), all=T)
  message(paste(nrow(result[is.na(result$name),]), 'missing chips in', chip.path))
  m <- result[is.na(result$sensor),]
  message(paste(nrow(m), 'missing spectra'))
  
  if (nrow(m)>0) {
    if (!file.exists(chip.dirty)) dir.create(chip.dirty)
    for (i in m$name) {
      in_file <- file.path(chip.path, paste(strsplit(i, '_')[[1]][1:2], collapse='_'), paste0(i, '.png'))
      out_file <- file.path(chip.dirty, paste0(i, '.png'))
      file.rename(in_file, out_file)
    }
  }
}

read_spectra <- function(path_to_spectra) {
  
  ds <- list.files(path_to_spectra, pattern='^(plot).*spectra[.]csv', full.names=T)
  df <- data.frame()
  for (i in ds) {
    if (file.size(i) > 0) {
      tmp <- read.table(i, stringsAsFactors = F, header=F, sep=',', dec='.')
      names(tmp) <- c('sensor', 'project_id', 'plotid', 'tsa', 'image_year', 'image_julday', 'b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'cloud')
      df <- rbind(df, tmp)  
    }
  }
  
  df$cloud <- ifelse(df$cloud > 0, 1, 0)
  df$tcb <-  0.2043*df$b1 + 0.4158*df$b2 + 0.5524*df$b3 + 0.5741*df$b4 + 0.3124*df$b5 + 0.2303*df$b7
  df$tcg <- -0.1603*df$b1 - 0.2819*df$b2 - 0.4934*df$b3 + 0.7940*df$b4 - 0.0002*df$b5 - 0.1446*df$b7
  df$tcw <-  0.0315*df$b1 + 0.2021*df$b2 + 0.3102*df$b3 + 0.1594*df$b4 - 0.6806*df$b5 - 0.6109*df$b7
  df$cloud_cover <- df$clou
  df$spectral_scaler <- 10000
  df$priority <- 1
  df$tsa <- 999999
  df$target_day <- 200
  df$packet <- 0
  
  df$tcb <- round(df$tcb)
  df$tcg <- round(df$tcg)
  df$tcw <- round(df$tcw)
  df$key <- paste0(df$sensor, df$image_year, df$image_julday)

  outNames <- c('project_id','tsa','plotid','sensor','image_year','image_julday',
                'b1','b2','b3','b4','b5','b7','tcb','tcg','tcw','cloud','cloud_cover',
                'spectral_scaler','priority','target_day', 'packet')
  
  df <- df[,outNames]

  invisible(df)
}

insert_spectra <- function(sqlite_file, spectra, target_day=200) {
  
  con = dbConnect(drv=SQLite(), dbname=sqlite_file)
  rs <- dbSendStatement(con, "DELETE FROM region_spectrals;")
  dbClearResult(rs)
  dbWriteTable(con, spectra, name="region_spectrals", append=T, row.names=F)
  rs <- dbSendStatement(con, "VACUUM;")
  dbClearResult(rs)
  dbDisconnect(con)
  
  message(paste('Inserted:', nrow(spectra), 'spectra.'))
  
}


insert_plots <- function(sqlite_file, plot_file, project_code='project') {
  
  ds <- read.csv(plot_file)
  ds <- ds[,1:4]
  names(ds) <- c('project_id','plotid', 'x', 'y')
  ds$tsa <- 999999
  
  
  con = dbConnect(drv=SQLite(), dbname=sqlite_file)
  
  rs <- dbSendStatement(con, "DELETE FROM plots;")
  dbClearResult(rs)
  
  dbWriteTable(con, ds, name="plots", append=T, row.names=F)

  rs <- dbSendStatement(con, "DELETE FROM projects;")
  dbClearResult(rs)
  dbWriteTable(con, data.frame(project_id=ds$project_id[1],
                               project_code=project_code,
                               project_name=project_code,
                               plot_size=1, target_day=215), 
               name="projects", append=T, row.names=F)
  
  rs <- dbSendStatement(con, "VACUUM;")
  dbClearResult(rs)
  dbDisconnect(con)

  message(paste('Inserted:', nrow(ds), 'plots.'))
}

update_response_design <- function(sqlite_file, response_id=1,
                                   project_id=1,
                                   landcover=c("Dense Forest","Open Forest","Cropland",
                                               "Open Grassland","Wooded Grassland","Open Water",
                                               "Vegetated Wetland","Settlements","Otherland"),
                                   change_process= c("Stable","Greening","Fire","Harvest","Flooding","Conversion"), 
                                   landuse=c("Forest","Agriculture","Other")) {
  
  con = dbConnect(drv=SQLite(), dbname=sqlite_file)

  rs <- dbSendStatement(con, "DELETE FROM response_design;")
  dbClearResult(rs)
  
  dbWriteTable(con, data.frame(response_id=response_id,
                               project_id=project_id,
                               landcover= paste(landcover, collapse=';'),
                               change_process= paste(change_process, collapse=';'), 
                               landuse= paste(landuse, collapse=';')), 
               name="response_design", append=T, row.names=F)
  
  dbDisconnect(con)

}


clear_tables <- function(sqlite_file, tables=c('image_preference', 'plot_comments',
                                               'region_spectrals', 'plots', 'vertex')) {
  
  con = dbConnect(drv=SQLite(), dbname=sqlite_file)
  
  for (tab in tables) {
    rs <- dbSendStatement(con, paste0("DELETE FROM ", tab,";"))
    dbClearResult(rs)
  }

  rs <- dbSendStatement(con, "VACUUM;")
  dbClearResult(rs)
  dbDisconnect(con)

  message(paste0('Cleared: ', paste(tables, collapse=', '), '.'))
}



### read spectra #####
df.spectra <- read_spectra(path.spectra)

### move chips with NA center pixels #####

# move_blank_chips(path.tcchips)
# move_blank_chips(path.b743chips)

### clear data tables - fresh start #####

message(paste('Database:', sqlite_file))

clear_tables(sqlite_file)

insert_spectra(sqlite_file, df.spectra)

insert_plots(sqlite_file, plot_file, project_code=project_code)

update_response_design(sqlite_file, response_id=1, project_id=project_id,
                       landcover=landcover, change_process=change_process, landuse=landuse)


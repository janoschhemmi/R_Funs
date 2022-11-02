library(rgdal)
library(raster)



#d <- as.Date('2018-07-07')- as.Date('2018-01-01') + 1

# get the info from the refefence image
sp <- readOGR('q:/Spring/workspace/brandsat/timesync/plots_center.shp')
r <- stack('d:/GDrive/LC08_L1TP_193023_20180707_20180717_01_T1_ltr.tif')

x <- extract(r, sp, na.rm=T, df=T)
rdf <- cbind(sp@data, x)
rdf <- na.omit(rdf)
rdf <- rdf[rdf$B4!=0,]
names(rdf)[7:12] <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')


p <- 'q:\\Spring\\workspace\\brandsat\\timesync\\spectra\\prj_6000\\'
bn <- c('B2', 'B3', 'B4', 'B5', 'B6')
testplots <- c(24,26,34,36,40,41,42,43)
result <- NULL

for (plotid in testplots) {

  fname <- file.path(p, paste0('plot_', plotid, '_spectra.csv'))
  if (file.exists(fname)) {
    df <- read.csv(fname, header=F)
  names(df) <- c('sensor', 'project_id', 'plotid', 'tsa', 'image_year', 'image_julday', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'cloud')
  ds <- df[df$image_year==2018 & df$tsa==193023 & df$image_julday==188,]
  
  if (nrow(ds)!=0) {
    tmp = data.frame(bands=bn,
                     plotid=plotid,
                     image=as.numeric(rdf[rdf$plotid==plotid,bn]),
                     timesync=as.numeric(ds[, bn]))
    tmp$difference <- tmp$image - tmp$timesync
    
    result <- rbind(result, tmp)
    message(plotid)
  } else {
    message(paste(plotid, 'skipped.'))
  }
  } else {
    message(paste(plotid, 'skipped.'))
  }
}
result[result$difference!=0,]

result6000 <- result


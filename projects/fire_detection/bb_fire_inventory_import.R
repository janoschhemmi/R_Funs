library(tidyverse)
library(readxl)
library(sf)


root <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/forst/'

#---- Compile Feuer 2012 - 2019 -------------------------------------

wabra <- NULL
for (i in 2012:2019) {
  fn <- file.path(root, 'xlsx', paste0('Wabra_Bbg_', i,'.xlsx'))
  xl <- as.data.frame(read_excel(fn, skip=6))
  if (i > 2012) {
    if (length(setdiff(names(xl), names(wabra)) > 0)) print(i)
  } 
  
  if ("uaa" %in% names(xl)) print(i)
  xl$fireid <- i * 10000 + 1:nrow(xl)
  xl$year <- i
  wabra <- rbind(wabra, xl)
}

#wabra <- wabra[wabra$Fl > 0.1, ]

# wabra <- wabra[!is.na(wabra$Abt),]

wabra$UAb_letter <- ""
for (i in 1:nrow(wabra)) {
 wabra$UAb_letter[i] <- letters[wabra$UAb[i]]
}

# 
wabra$adresse_abt <- paste(wabra$Obf, wabra$Rv, wabra$WAG, wabra$Abt, sep='|')
wabra$adresse_tf <- paste(wabra$Obf, wabra$Rv, wabra$WAG, wabra$Abt, wabra$uaa, wabra$TF, sep='|')
 
write.csv(wabra, file.path(root, "Wabra_Bbg_2012-2019.csv"), row.names=F)

#---- Compile Feuer 1985 - 1990 -------------------------------------

wabra <- NULL
for (i in 1985:1990) {
  fn <- file.path(root, 'xlsx', paste0('Wabra_Bbg_', i,'.xlsx'))
  xl <- as.data.frame(read_excel(fn))
  xl <- xl[,!names(xl) %in% c("NUMMER", "BODENSTREU")]
  if (i > 1985) {
    if (length(setdiff(names(xl), names(wabra)) > 0)) print(i)
  } 
  xl$fireid <- i * 10000 + 1:nrow(xl)
  xl$year <- i
  wabra <- rbind(wabra, xl)
}

write.csv(wabra, file.path(root, "Wabra_Bbg_1985-1990.csv"), row.names=F)


#---- Compile Feuer 1991 - 2005 -------------------------------------

wabra <- read.csv(text="LAND,STFB,OBF,REV,ABT,UAB,TFL,DATUM,WARNSTUFE,URSACHE,URSACHB,STANDORT,ALTER,SCHLUSGR,FLAECHE,FLAECHEW,NRALT,ADRESSE,fireid,year,X_COORD,Y_COORD,URSACHBUND")
for (i in 1991:2005) {
  fn <- file.path(root, 'xlsx', paste0('Wabra_Bbg_', i,'.xlsx'))
  xl <- as.data.frame(read_excel(fn))
  d <- setdiff(names(wabra), names(xl))
  for (j in d) {
    xl[j] <- NA
  }
    
  xl <- xl[,!names(xl) %in% c("NUMMER", "BODENSTREU", "STD", "MIN")]
  if (i > 1993) {
    d <- setdiff(names(xl), names(wabra))
    if (length(d > 0)) print(c(d,i))
  } 
  xl$fireid <- i * 10000 + 1:nrow(xl)
  xl$year <- i
  wabra <- rbind(wabra, xl)
}

write.csv(wabra, file.path(root, "Wabra_Bbg_1991-2005.csv"), row.names=F)


#---- Compile Feuer 2006 - 2011 -------------------------------------

wabra <- read.csv(text="Obf,Rv,WAG,Abt,UAb,TF,Alt,Verursacher,UrsNr,Ursache,DatumE,Fl,FlW,WS")
for (i in 2006:2011) {
  fn <- file.path(root, 'xlsx', paste0('Wabra_Bbg_', i,'.xlsx'))
  xl <- as.data.frame(read_excel(fn, skip=6))
  d <- setdiff(names(wabra), names(xl))
  for (j in d) {
    xl[j] <- NA
  }
  
  xl <- xl[,!names(xl) %in% c("NUMMER", "BODENSTREU", "STD", "MIN")]
  if (i > 2006) {
    d <- setdiff(names(xl), names(wabra))
    if (length(d > 0)) print(c(d,i))
  } 
  xl$fireid <- i * 10000 + 1:nrow(xl)
  xl$year <- i
  wabra <- rbind(wabra, xl)
}

write.csv(wabra, file.path(root, "Wabra_Bbg_2006-2011.csv"), row.names=F)





# #---- FGK Land  -------------------------------------------------
# 
# shp <- '/Volumes/Icesat/brandsat/bb_forst/fgk_fl.shp'
# fgk <-read_sf(shp)
# fgk <- fgk[!is.na(fgk$hh_obf),-1]
# fgk$hh_obf <- as.numeric(fgk$hh_obf)
# fgk$hh_rev <- as.numeric(fgk$hh_rev)
# fgk <- fgk[, !(names(fgk) %in% c("shape_Leng", "shape_Area"))]
# 
# fgk$adresse_abt <- paste(fgk$hh_obf, fgk$hh_rev, fgk$wag, fgk$abt, sep='|')
# fgk$adresse_tf <- paste(fgk$hh_obf, fgk$hh_rev, fgk$wag, fgk$abt, fgk$uaa, fgk$tf, sep='|')
# 
# # abteilung
# ds.abt <- merge(fgk[,!names(fgk) %in% c('adresse_tf', 'uaa', 'wag', 'abt', 'tf')], wabra, by="adresse_abt")
# write_sf(ds.abt, file.path(root, "Wabra_Bbg_2012-2019_abteilung.gpkg"))
# 
# # teilfläche
# ds.tf <- merge(fgk[,!names(fgk) %in% c('adresse_abt', 'uaa', 'wag', 'abt', 'tf')], wabra, by="adresse_tf")
# write_sf(ds.tf, file.path(root, "Wabra_Bbg_2012-2019_teilflaeche.gpkg"))
# 
# 
# 
# #---- FUEK  -----------------------------------------------------
# 
# # missing <- xl[!(xl$fireid %in% unique(ds$fireid)),]
# 
# shp <- '/Volumes/Icesat/brandsat/bb_forst/fuek_abt.shp'
# fgk <-read_sf(shp)
# fgk$hh_obf <- as.numeric(fgk$hh_obf)
# fgk$hh_rev <- as.numeric(fgk$hh_rev)
# fgk$wag <- as.numeric(fgk$wag)
# fgk <- fgk[, !(names(fgk) %in% c("gdb_geomat"))]
# 
# ds.abt2 <- merge(fgk[,!names(fgk) %in% c('adresse_tf', 'uaa', 'wag', 'abt', 'tf')], wabra, by.x="adresse", by.y="adresse_abt")
# write_sf(ds.abt2, file.path(root, "Wabra_Bbg_2012-2019_abteilung_fuek.gpkg"))



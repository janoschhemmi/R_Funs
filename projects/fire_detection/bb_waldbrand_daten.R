library(tidyverse)
library(readxl)
library(sf)


root <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/forst/'

#---- Compile Feuer greater than 0.1 ha -------------------------------------

wabra <- NULL
for (i in 2012:2019) {
  fn <- file.path(root, 'xlsx', paste0('Wabra_Bbg_', i,'.xlsx'))
  xl <- read_excel(fn, skip=6)
  xl$fireid <- i * 10000 + 1:nrow(xl)
  xl$year <- i
  wabra <- rbind(wabra, xl)
}

wabra <- wabra[wabra$Fl > 0.1, ]

wabra <- wabra[!is.na(wabra$Abt),]

wabra$uaa <- ""
for (i in 1:nrow(wabra)) {
  wabra$uaa[i] <- letters[wabra$UAb[i]]
}

wabra$adresse_abt <- paste(wabra$Obf, wabra$Rv, wabra$WAG, wabra$Abt, sep='|')
wabra$adresse_tf <- paste(wabra$Obf, wabra$Rv, wabra$WAG, wabra$Abt, wabra$uaa, wabra$TF, sep='|')

write.csv(wabra, file.path(root, "Wabra_Bbg_2012-2019_gt0-1ha.csv"), row.names=F)


#---- FGK Land  -------------------------------------------------

shp <- '/Volumes/Icesat/brandsat/bb_forst/fgk_fl.shp'
fgk <-read_sf(shp)
fgk <- fgk[!is.na(fgk$hh_obf),-1]
fgk$hh_obf <- as.numeric(fgk$hh_obf)
fgk$hh_rev <- as.numeric(fgk$hh_rev)
fgk <- fgk[, !(names(fgk) %in% c("shape_Leng", "shape_Area"))]

fgk$adresse_abt <- paste(fgk$hh_obf, fgk$hh_rev, fgk$wag, fgk$abt, sep='|')
fgk$adresse_tf <- paste(fgk$hh_obf, fgk$hh_rev, fgk$wag, fgk$abt, fgk$uaa, fgk$tf, sep='|')

# abteilung
ds.abt <- merge(fgk[,!names(fgk) %in% c('adresse_tf', 'uaa', 'wag', 'abt', 'tf')], wabra, by="adresse_abt")
write_sf(ds.abt, file.path(root, "Wabra_Bbg_2012-2019_abteilung.gpkg"))

# teilfläche
ds.tf <- merge(fgk[,!names(fgk) %in% c('adresse_abt', 'uaa', 'wag', 'abt', 'tf')], wabra, by="adresse_tf")
write_sf(ds.tf, file.path(root, "Wabra_Bbg_2012-2019_teilflaeche.gpkg"))


excluded <- wabra[wabra$FEUERID %in% ds.abt$FEUERID, ]



#---- FUEK  -----------------------------------------------------

# missing <- xl[!(xl$fireid %in% unique(ds$fireid)),]

shp <- '/Volumes/Icesat/brandsat/bb_forst/fuek_abt.shp'
fgk <-read_sf(shp)
fgk$hh_obf <- as.numeric(fgk$hh_obf)
fgk$hh_rev <- as.numeric(fgk$hh_rev)
fgk$wag <- as.numeric(fgk$wag)
fgk <- fgk[, !(names(fgk) %in% c("gdb_geomat"))]

ds.abt2 <- merge(fgk[,!names(fgk) %in% c('adresse_tf', 'uaa', 'wag', 'abt', 'tf')], wabra, by.x="adresse", by.y="adresse_abt")
write_sf(ds.abt2, file.path(root, "Wabra_Bbg_2012-2019_abteilung_fuek.gpkg"))



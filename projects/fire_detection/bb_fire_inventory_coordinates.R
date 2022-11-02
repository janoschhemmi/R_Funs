library(readxl)
library(dplyr)
library(sf)

root <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/data/burned_area/brandenburg/'

dsc <- read_xlsx(file.path(root, 'Wabra_neu/Datenausgang_Pflugmacher_2020.xlsx'))
ds12 <- read.csv(file.path(root, 'Wabra_merged/Wabra_Bbg_2012-2019.csv'))
ds06 <- read.csv(file.path(root, 'Wabra_merged/Wabra_Bbg_2006-2011.csv'))

#write.csv(ds, file.path(root, 'wabra_koordinaten.csv'))

names(ds06) <- tolower(names(ds06))
names(ds12) <- tolower(names(ds12))
names(dsc) <- tolower(names(dsc))

dsc$datume <- substr(dsc$datume, 1, 19)
dsc <- unique(select(dsc, -uab, -tf))

ds12c <- merge(ds12, dsc, by=c('obf', 'rv', 'wag', 'abt', 'datume'))
ds06c <- merge(ds06, dsc, by=c('obf', 'rv', 'wag', 'abt', 'datume'))

ds <- rbind(ds06c, select(ds12c, -uab_letter, -adresse_abt, -adresse_tf))
ds.utm <- ds[ds$x_koordinate < 1000000,]
ds.gk <- ds[ds$x_koordinate > 1000000,]

ds.gk2 <- ds.gk
ds.gk2$x_koordinate <- ds.gk2$x_koordinate - 3000000

#epsg=28403
#epsg=32633

sf.utm = st_as_sf(ds.utm, coords = c("x_koordinate", "y_koordinate"), 
                  crs = 25833)

#st_write(sf.utm2, dsn=file.path(root, "Wabra_Bbg_2006-2019_koordinate_utm4.gpkg"), layer="Wabra_Bbg_2006-2019_koordinate_utm")

sf.gk = st_as_sf(ds.gk2, coords = c("x_koordinate", "y_koordinate"), 
                  crs = 25833)
#sf.gkutm <- st_transform(sf.gk, crs=32633)

sf.all <- rbind(sf.utm, sf.gk)

st_write(sf.all, file.path(root, "Wabra_Bbg_2006-2019_koordinates_epsg25833.shp"))

st_write(sf.all[sf.all$flw > 0.1,], file.path(root, "Wabra_Bbg_2006-2019_koordinates_epsg25833_gt0_1ha.shp"))

#sf.gk <- st_read(file.path(root, "Wabra_Bbg_2006-2019_koordinate_gk.shp"))

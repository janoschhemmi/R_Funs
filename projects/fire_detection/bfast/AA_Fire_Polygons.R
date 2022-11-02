## 
library(terra)

## load wabra ref

fo_1 <- read.csv("P:/workspace/fire_digitize/Wabra_merged/Wabra_Bbg_1985-1990.csv")
fo_1 <- fo_1 %>% dplyr::select(year,FLAECHE, fireid)
fo_2 <- read.csv("P:/workspace/fire_digitize/Wabra_merged/Wabra_Bbg_1991-2005.csv")
fo_2 <- fo_2 %>% dplyr::select(year,FLAECHE, fireid)
fo_3 <- read.csv("P:/workspace/fire_digitize/Wabra_merged/Wabra_Bbg_2006-2011.csv")
fo_3 <- fo_3 %>% dplyr::select(year,FlW, fireid)
fo_4 <- read.csv("P:/workspace/fire_digitize/Wabra_merged/Wabra_Bbg_2012-2019.csv")
fo_4 <- fo_4 %>% dplyr::select(year, FlW, fireid )
colnames(fo_1) <- c("year_lfe","area_lfe", "fireid")
colnames(fo_2) <- c("year_lfe","area_lfe", "fireid")
colnames(fo_3) <- c("year_lfe","area_lfe", "fireid")
colnames(fo_4) <- c("year_lfe","area_lfe", "fireid")
fo <- rbind(fo_1,fo_2,fo_3,fo_4)
# fo <- fo[fo$area > 2,]
# fo <- rbind(fo,c(2001,2))


##load fire polygons 
poly <- vect("P:/workspace/jan/shapes/Fire_Polygons_Brandsat/fire_digitized_Brandsat.shp")
poly <- poly[poly$lfe_ref == 1]
## load classification fire
fires <- rast("P:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/landsat-nbr-h40-o1-bp3-expansion/model_basic_mtry_10_redo_3_greatest_disturbance_mosaic/model_basic_mtry_10_redo_3_greatest_disturbance_mosaic_fire.tif")

## load forest mask 
mask <- rast("P:/workspace/jan/masks/RSE_Full_Brandenburg_Forest_mmu12_2_larger_extent_resample.tif")

## 

### counter persent of masked pixel within polygon (pixel that are crossed by polygon)
ext_mask <-  terra::extract(mask, poly, touches = TRUE)
colnames(ext_mask) <- c("id","value")
ext_mask <- ext_mask %>% group_by(id) %>% summarise(n = n(), ones = sum(value == 1), zeros = sum(value == 0)) %>%
  mutate(perc = zeros * 100 / n)
ext_mask$area <- ext_mask$n * 0.09


## how many polygons are affected?
plot_affected_mask <- ggplot(ext_mask, aes(x = area , y = perc)) + geom_point(shape = 4, size = 1.4) +
  theme_bw() +
  xlab("area of fire ref polygon [ha]") +
  ylab("%") +
  ggtitle ("percent pixel within polygon affected by forest mask")
ggsave(plot_affected_mask, filename = "plot_01_percent.png", path = "P:/workspace/jan/fire_detection/plots/plots_aa_polygons/", height = 7, width = 6)

## out how many polygons < 50 percent forest mask??
nrow(ext_mask[ext_mask$perc < 50,])


## extract fires from buffered vector 
buffered <- terra::buffer(poly, width = 50)
# writeVector(buffered,"P:/workspace/jan/shapes/Fire_Polygons_Brandsat/fire_digitized_Brandsat_buffered.shp")

ext_fires <-  terra::extract(fires, buffered, touches = TRUE)
ext_fires_1 <- ext_fires[,1:5]
ext_fires_2 <- ext_fires[,c(1,6,7,8,9)]
colnames(ext_fires_2) <- colnames(ext_fires_1)
ext_fires <- rbind(ext_fires_1, ext_fires_2)
ggplot(ext_fires, aes(x = greatest_prob, greatest_mag)) + geom_point()
ext_fires <- ext_fires %>% group_by(ID, greatest_year) %>% tally() %>% pivot_wider(., id_cols = ID,names_from = greatest_year, values_from = n, names_sort = TRUE)

## ids to polygons 
buffered$id <- seq(1:118)
df <- as.data.frame(buffered)

## merge with ref data
df <- left_join(df, fo, by = "fireid")
df$Area_polyg <- df$Area_polyg * 0.0001 

##
df <- left_join(df, ext_fires, by = c("id"="ID"))
df$year_lfe <- as.character(df$year_lfe)

is.integer(df$year_lfe)
## select column per year
dd <- df %>% 
  group_by(rn = row_number()) %>%
  do(data.frame(., value= .[[.$year_lfe]]))

## filter polygons 
poly



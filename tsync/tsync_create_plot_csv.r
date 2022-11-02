library(sf)
library(dplyr)
library(readr)

in_file <- "p:/timesync/bb/gis/tsync_fires_post_training1.gpkg"
out_file <- "p:/timesync/bb/tsync_fires_post_training1.csv"

shp <- read_sf(in_file)

mutate(shp, project_id=8001) %>%
  st_drop_geometry() %>%
  select("project_id", "plotid", "x", "y") %>%
  write_csv(out_file)

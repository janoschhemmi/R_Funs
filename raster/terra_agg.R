
## terra aggregate 

mask <- rast("P:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/model_01_basic_sieve/greatest_disturbance_model_01_basic_01_all_fire_to_3_masked.tif")

agg <- terra::aggregate(mask, fact = 1000, fun = "mean")

writeRaster(agg, "P:/workspace/jan/Zwischenbericht/agg_fires_1000.tif")




## ddann nochmal nur mit der maske 
mask <- rast("P:/workspace/jan/fire_detection/break_detection/classified_breaks_cube/model_01_basic_sieve/greatest_disturbance_model_01_basic_01_all_fire_to_1_sieved.tif")
agg_2 <- terra::aggregate(mask, fact = 100, fun = "mean")
writeRaster(agg_2, "P:/workspace/jan/Zwischenbericht/agg_fires_100_mask.tif")

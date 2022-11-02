

mask <- rast("P:/workspace/brbrg_forest/forestmask_2018_HL_ML_MLP_mmuh100n4.tif")
ts <- rast("P:/workspace/treespecies/maps/classification_BrandSat/RF_Predict_05_Full_model_all_Forest_no_mask_Brandenburg.tif")
croped <- crop(mask, ts)
croped <- croped - 1
plot(croped)
masked <- terra::mask(ts, croped, maskvalues = 1)
??terra::mask
plot(ts)
plot(croped)

croped[100:1000, 100:1000]
terra::writeRaster(masked, "P:/workspace/treespecies/maps/classification_BrandSat/RF_Predict_05_Full_model_all_Forest_S2_1ha_Brandenburg.tif", overwrite = T)

ext(mask)[1]

ext(ts)

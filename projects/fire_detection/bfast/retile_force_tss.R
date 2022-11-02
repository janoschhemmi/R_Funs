source("bfast/lib_bfast_utils.R")



#--- Landsat ------------------------------
tss_path <- "h:/Brandsat/01_Data_Level3/202203_LANDSAT_S2" # /X0068_Y0044"
out_path <- "h:/Brandsat/01_Data_Level3/202203_LANDSAT_S2_retile"
mask_file <- paste0("P:/workspace/jan/masks/RSE_Full_Brandenburg_Forest_mmu12_2_larger_extent.tif")


tileids <- c("X0065_Y0040", "X0065_Y0041", "X0066_Y0040", "X0066_Y0041", "X0066_Y0042", 
             "X0067_Y0040", "X0067_Y0041", "X0067_Y0042", "X0067_Y0043", "X0067_Y0044", 
             "X0067_Y0045", "X0068_Y0040", "X0068_Y0041", "X0068_Y0042", "X0068_Y0043", 
             "X0068_Y0044", "X0068_Y0045", "X0069_Y0040", "X0069_Y0041", "X0069_Y0042", 
             "X0069_Y0043", "X0069_Y0044", "X0069_Y0045", "X0069_Y0046", "X0069_Y0047", 
             "X0070_Y0039", "X0070_Y0040", "X0070_Y0041", "X0070_Y0042", "X0070_Y0043", 
             "X0070_Y0044", "X0070_Y0045", "X0070_Y0046", "X0070_Y0047", "X0071_Y0039",
             "X0071_Y0040", "X0071_Y0041", "X0071_Y0042", "X0071_Y0043", "X0071_Y0044", 
             "X0071_Y0045", "X0071_Y0046", "X0071_Y0047", "X0072_Y0040", "X0072_Y0042", 
             "X0072_Y0043", "X0072_Y0044", "X0072_Y0045", "X0072_Y0046", "X0073_Y0046",
             "X0073_Y0045", "X0073_Y0044")


bm_stack_retile_force_tss(tss_path, out_path, mask_file=mask_file, include=tileids, cores=1)



#--- Landsat +Sentinel-2 -------------------

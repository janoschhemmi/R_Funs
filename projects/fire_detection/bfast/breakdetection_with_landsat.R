source("bfast/lib_bfast.r")
require(tictoc)
#options(warn=2)
break


#--- compile force data extracted at samples ------
inpath <- "p:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_dl/"
fn_samples_force <- "p:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_dl_3.csv"

if (!file.exists(fn_samples_force)) bm_compile_force_extract(inpath, fn_samples_force)



#--- detect and extract breakpoints for samples ----
fn_samples_force <- "p:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_Sentinel2_ts_with_outliers_till_2022_post2.csv"
out_basename     <- "p:/workspace/jan/fire_detection/break_detection/break_detection_tables/landsat-S2-"
exclude_plots <- c(3142, 3144, 1882)


# read sample file
df_force <- read_csv2(fn_samples_force)
if (!is.null(exclude_plots)) df_force <- df_force[!df_force$id %in% exclude_plots,]

tic()

bm_detect_and_extract_samples(df_force, out_basename, fit_index="nbr",
                              h=40, breaks=3, order_harmonic=1, 
                              indices=c("NBR","NDV","TCB","TCG","TCW","TCD"),
                              expand=T, cores=12,overwrite = T )
toc()


#--- detect and extract breakpoints in cube -------

inpath <- "l:/Brandsat/01_Data_Level3/202202_LANDSAT_retile"  #/X0068_Y0045
outpath <- "p:/workspace/jan/fire_detection/break_detection/break_detection_cube/landsat-nbr-h40-o1-bp3-expansion"
bm_bfast_status(inpath, outpath)

if (!dir.exists(outpath)) dir.create(outpath)

in_files <- list.files(inpath, "^X.*tif$", recursive = T, full.names = T)

bm_detect_and_extract_tiles(in_files, outpath, fit_index="nbr",
                           h=40, breaks=3, order_harmonic=1, expand=T,
                           cores=30)




#--- testing -------------------

# library(profvis)
#a <- profvis(detect_and_extract_tile(in_file, outpath="d:/work/tmp", debug_row=248, expand=F))
#htmlwidgets::saveWidget(a, "profile.html")

if (F) {
  in_file <- "d:/work/tmp/X0065_Y0041/X0065_Y0041_stack_croped_8.tif"
  in_file <- "l:/Brandsat/01_Data_Level3/202202_LANDSAT_retile/X0065_Y0041/X0065_Y0041_NBR-NDV-TCB-TCG-TCW-TCD_8.tif"
  outpath <- "p:/workspace/jan/fire_detection/break_detection/break_detection_cube/landsat-nbr-h40-o1-bp3-expansion_test"
  
  # detect_and_extract_breaks_c <- compiler::cmpfun(detect_and_extract_breaks)
  
  tic()
  bm_detect_and_extract_tiles(in_file, outpath=outpath, debug_row=164, expand=T, overwrite=T) #  , debug_row=164
  toc()
}

if (F) {
  outras <- rast("d:/work/tmp/out/X0065_Y0041/X0065_Y0041_stack_croped_8.tif")
  testras <- rast("d:/work/tmp/X0065_Y0041_stack_croped_8_testrow248_v5.tif")
  
  not_comparable <- any(values(outras) != values(testras), na.rm=T)
  if (not_comparable) {
    message("Test failed.")
  } else {
    message("Test successful.")
  }
  
  # mo <- values(outras)
  # mi <- values(testras)
  # i <- apply(mo != mi, 1, function(x) any(x, na.rm=T))
  # di <- mi[i,] 
  # do <- mo[i,]
  # 
  # j <- 4
  # a <- di != do
  # di[j, a[j,]]
  # do[j, a[j,]]
  
}


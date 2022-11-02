require(readr)
require(tidyr)
require(dplyr)
require(randomForest)
require(terra)

if (packageVersion("terra") < "1.5.21") {
  message("Update package terra to version > 1.5.21")
  message("Not all functions will work properly with your verion!")
  stop()
}


source("bfast/lib/bm_tsync_combine_bfast.R")
source("bfast/lib/bm_tsync_prepare.R")
source("bfast/lib/bm_classify_bfast_output.R")
#source("bfast/lib/bm_classify_bfast_output_file.R")
source("bfast/lib/bm_stack_retile_force_tss.R")


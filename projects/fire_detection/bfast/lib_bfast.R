require(bfast)
require(zoo)
require(lubridate)
require(terra)
require(strucchangeRcpp)
require(readr)
require(tidyr)
require(dplyr)
require(foreach)

if (packageVersion("terra") < "1.5.21") {
  message("Update package terra to version > 1.5.21")
  message("Not all functions will work properly with your verion!")
  stop()
}



source("bfast/lib/bm_bfast_status.R")
source("bfast/lib/file_delete.R")
source("bfast/lib/bm_expand_one_year.R")
source("bfast/lib/bm_metric_names.R")
source("bfast/lib/bm_compile_force_extract.R")
source("bfast/lib/bm_detect_and_extract_breaks.R")
source("bfast/lib/bm_detect_and_extract_samples.R")
source("bfast/lib/bm_detect_and_extract_tiles.R")
source("bfast/lib/bm_fix_date.R")



### global Settings 
.libPaths("S:/BrandSat/02_Code/R/library_6")


library(bfast)
library(zoo)
library(rlang)
library(stlplus)
library(lubridate)
library(devtools)
library(svMisc)
library(snow)
library(lubridate)
library(hms)
library(rgdal)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyr)
library(Rcpp)
library(dplyr)
library(randomForest)
library(data.table)
library(randomForest)
library(flextable)
library(terra)
library(readr)
library(purrr)

### Configure git with Rstudio ############################################

## set your user name and email:
usethis::use_git_config(user.name = "janoschhemmi", user.email = "jan.hemmerling@t-online.de")

## create a personal access token for authentication:
usethis::create_github_token() 
## in case usethis version < 2.0.0: usethis::browse_github_token() (or even better: update usethis!)

## set personal access token:
credentials::set_github_pat("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")

## or store it manually in '.Renviron':
usethis::edit_r_environ()
## store your personal access token in the file that opens in your editor with:
## GITHUB_PAT=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
## and make sure '.Renviron' ends with a newline

# ----------------------------------------------------------------------------

#### 4. Restart R! ###########################################################

# ----------------------------------------------------------------------------

#### 5. Verify settings ######################################################

usethis::git_sitrep()

## Your username and email should be stated correctly in the output. 
## Also, the report shoud cotain something like:
## 'Personal access token: '<found in env var>''

## If you are still having troubles, read the output carefully.
## It might be that the PAT is still not updated in your `.Renviron` file.
## Call `usethis::edit_r_environ()` to update that file manually.

# ----------------------------------------------------------------------------

## THAT'S IT!
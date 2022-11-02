require(dplyr)
source('lib_bbdis.r')

### paths ####
root.mac <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/timesync/'
root.gsx <- 'x:/OneDrive/projects/europe_disturbance/timesync'

if (dir.exists(root.mac)) root <- root.mac
if (dir.exists(root.gsx)) root <- root.gsx

lt_version <- 'landtrendr_verts12_despike50'

path <- file.path(root, lt_version)


#--- filenames ------------------------------- ---------------------------------------
fn_landtrendr_features <- file.path(root, lt_version, 'tsync_landtrendr_features_bb.csv')
fn_tsync_interpretations <- file.path(root, 'tsync_plots_bb_interpretations.csv')
fn_tsync_training <- file.path(path, paste0('tsync_training_bb.csv'))

#--- Compile landtrendr features from vertices ---------------------------------------

compile_landtrendr_features_batch(path, fn_landtrendr_features)


#--- Compile timesync training samples  ---------------------------------------

compile_tsync_training(fn_tsync_interpretations, fn_landtrendr_features, fn_tsync_training)

#--- Classify  ---------------------------------------
source('landtrendr_classify.r')

source('landtrendr_evaluate.r')

# 604
# 785
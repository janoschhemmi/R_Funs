library(randomForest)
library(pixelr)
library(dplyr)
source('lib_bbdis.r')


### paths ####
root.mac <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/timesync/'
root.gsx <- 'x:/OneDrive/projects/europe_disturbance/timesync'
root.fra <- 'D:/OneDrive/projects/europe_disturbance/timesync'

if (dir.exists(root.mac)) root <- root.mac
if (dir.exists(root.gsx)) root <- root.gsx
if (dir.exists(root.fra)) root <- root.fra

lt_version <- 'landtrendr_verts12_despike50'
v <- paste0(1986, 2019)
path <- file.path(root, lt_version)

fig.path <- file.path(path, paste0('figs_', v))
rfm.path <- file.path(path, paste0('models_', v))
if (!dir.exists(rfm.path)) dir.create(rfm.path)
if (!dir.exists(fig.path)) dir.create(fig.path)

#### import sample ####

ds <- read.training_samples(file.path(path, paste0('tsync_training_bb.csv')),
                            process=c("Harvest", "Wind", "Fire", "Hydrology", "Debris", "Other", "Purple", "Decline")) #"Decline", 

#### rf: bb: undisturbed=0, disturbed=1 ####

ds.fac <- disturbanceFactors(ds)
#i.predictors <- multi.grep(c('ds_', 'dt_', 'sp_'), names(ds.fac))
i.predictors <- multi.grep(c('b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'nbr', 'ndmi', 'tcb', 'tcg', 'tcw', 'tca', 'ndvi'), names(ds.fac))
i.response <- which(names(ds.fac) == 'disturbed')
ds.train <- ds.fac[, c(i.response, i.predictors)]

set.seed(42)
rfm_disturbed <- randomForest(disturbed ~ ., data=ds.train, ntree=500)

#rfm <- prandomForest(disturbed ~ ., data=ds.train, gtree=5, k=20, importance=T)

model_file <- file.path(rfm.path, 'rfm_disturbed.rds')

saveRDS(rfm_disturbed, file=model_file)
ds.val <- aa_validationData(ds.fac, model=rfm_disturbed, out.path=rfm.path) #[, -i.predictors]
rf_importance(model_file)
# rfm_disturbed <- readRDS(file.path(rfm.path, 'rfm_disturbed.rds'))


#rm(ds.train, i.predictors, i.response)


rm(list=ls())


library(randomForest)
library(pixelr)
library(dplyr)
library(ggplot2)
source('lib_bbdis.r')


### paths ####
root.mac <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/timesync/'
root.q <- "q:/Geomultisens/workspace/disturbance_ceurope/classification/"

if (dir.exists(root.mac)) root <- root.mac
if (dir.exists(root.q)) root <- root.q


lt_version <- 'landtrendr_verts12_despike50'
v <- paste0(1986, 2019)
path <- file.path(root, lt_version)

fig.path <- file.path(path, paste0('figs_', v))
rfm.path <- file.path(path, paste0('models_', v))


#### import data ####

strata_samples <- read.csv(f.strata_samples)
strata_weights <- read.csv(f.strata_weights)

rfm <- readRDS(file.path(rfm.path, 'rfm_disturbed.rds'))
ds.val <- readRDS(file.path(rfm.path, 'rfm_disturbed.csv.rds'))

pngBasename <- file.path(fig.path, 'rfm_disturbed.rds')


# commission omission plots of entire dataset
result.adj <- aa_commission_omission_plot(ds.val$observed, ds.val$predicted_probability,
                                          strata=ds.val$stratum,
                                          strata_codes=strata_weights$stratum, strata_count=strata_weights$stratum_count,
                                          class=rfm$classes[2], increment=0.05, 
                                          png_file=paste0(pngBasename, '.png'))

ds.val$predicted <- ifelse(ds.val$predicted_probability > 0.3, 1, 0)
ds.val$predicted <- factor(ds.val$predicted, levels=0:1, labels=c('undisturbed', 'disturbed'))

#aa <-  aa_stratified(ds.val$stratum, ds.val$observed, ds.val$predicted, h=strata_weights$stratum, N_h=strata_weights$stratum_count)


if (F) {
  
  # annual stats
  aa_annual(ds.val, stratum_code=strata_weights$stratum, stratum_count=strata_weights$stratum_count, 
            png_file=paste0(pngBasename, '_annual.png'), years=1986:2019)

  # get starts of disturbance
  
  ds.sods <- startOfDisturbance(ds.val, varNames=c('observed', 'predicted'), disturbanceClass = 'disturbed', position=F)
  ds.sods$observed <- factor(ds.sods$observed_sod, levels=0:1, labels=c('undisturbed', 'disturbed'))
  ds.sods$predicted <- factor(ds.sods$predicted_sod, levels=0:1, labels=c('undisturbed', 'disturbed'))
  #table(predicted=ds.sods$predicted,observed=ds.sods$observed)
  
  aa_annual(ds.sods, stratum_code=strata_weights$stratum, stratum_count=strata_weights$stratum_count, 
            png_file=paste0(pngBasename, '_annual-sods.png'), years=1987:2019)
  
}



#---- Evaluate Landtrendr at Timesync plots ----------------
load_gee_data()

pdf(file.path(fig.path, 'landtrendr_fits_tcw.pdf'), paper="a4r", width=10)
par(mfrow=c(3,3), mar=c(4,4,0.5,0.5))
for (plt in 1:2200) plot_ltr(plt, dsVal=ds.val, index='TCW', ylim=c(-0.4, 0.1))
dev.off()

pdf(file.path(fig.path, 'landtrendr_fits_nbr.pdf'), paper="a4r", width=10)
par(mfrow=c(3,3), mar=c(4,4,0.5,0.5))
for (plt in 1:2200) plot_ltr(plt, dsVal=ds.val, index='NBR', ylim=c(0, 0.2))
dev.off()



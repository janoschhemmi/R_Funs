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


lt_version <- 'landtrendr_verts12'
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

prob_threshold <- 0.3

# commission omission plots of entire dataset
result.adj <- aa_commission_omission_plot(ds.val$observed, ds.val$predicted_probability,
                                          strata=ds.val$stratum,
                                          strata_codes=strata_weights$stratum, strata_count=strata_weights$stratum_count,
                                          class=rfm$classes[2], increment=0.05, 
                                          png_file=paste0(pngBasename, '.png'))

ds.val$predicted <- ifelse(ds.val$predicted_probability > prob_threshold, 1, 0)
ds.val$predicted <- factor(ds.val$predicted, levels=0:1, labels=c('undisturbed', 'disturbed'))

#aa <-  aa_stratified(ds.val$stratum, ds.val$observed, ds.val$predicted, h=strata_weights$stratum, N_h=strata_weights$stratum_count)


# annual stats
aa_annual(ds.val, stratum_code=strata_weights$stratum, stratum_count=strata_weights$stratum_count, 
          png_file=paste0(pngBasename, '_annual.png'), years=1986:2019)




# get starts of disturbance
ds.sods <- startOfDisturbance(ds.val, varNames=c('observed', 'predicted'), disturbanceClass = 'disturbed', position=F)
ds.sods$observed <- factor(ds.sods$observed_sod, levels=0:1, labels=c('undisturbed', 'disturbed'))
ds.sods$predicted <- factor(ds.sods$predicted_sod, levels=0:1, labels=c('undisturbed', 'disturbed'))
table(predicted=ds.sods$predicted,observed=ds.sods$observed)

aa_annual(ds.sods, stratum_code=strata_weights$stratum, stratum_count=strata_weights$stratum_count, 
          png_file=paste0(pngBasename, '_annual-sods.png'), years=1987:2019)

table(ds.sods$predicted_dur)
table(ds.sods$observed, ds.sods$observed_dur)

table(ds.sods$predicted, ds.sods$predicted_dur)


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

plt <- 882
plot_ltr(plt, dsVal=ds.val, index='TCW', ylim=c(-0.4, 0.1))
plot_ltr(plt, dsVal=ds.val, index='NBR', ylim=c(-0.1, 0.2))
plot_ltr(plt, dsVal=ds.val, index='B5')
plot_ltr(plt, index='NDVI', ylim=c(0, 0.2))
plot_ltr(plt, index='NDMI', ylim=c(-0.1, 0.2))


#---- Debugging ----------------

tmp  <- ds[ds$plotid==plt, c(1:8, grep("tcw", names(ds)))]
tmp
points(tmp$year, tmp$sp_tcw/10000)

tmp2 <- dts.features[dts.features$plotid==plt, c(1:3, grep("tcw", names(dts.features)))]
tmp2
points(tmp2$year, tmp2$spectral_tcw/10000)

tmp4= read_landtrendr_vertices('/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/timesync/landtrendr_verts12/bb_vertices12_TCW.csv')
tmp4 <- tmp4[tmp4$plotid==plt,]
tmp4
points(tmp4$doy, tmp4$ftv/(-10000))


# print plots with long durations
q <- ds.sods$plotid[ds.sods$observed_dur>1]
q

tmp <- ds.sods[ds.sods$year==1998,]
tmp2 <- tmp[tmp$observed!=tmp$predicted,]
ds.val[ds.val$plotid==tmp2$plotid[44],c('plotid', 'year','disturbed', 'predicted_probability', 'predicted')]
table(predicted=tmp$predicted, observed=tmp$observed)

tmp3 <- unique(ds.val$plotid[ds.val$process=='Decline'])
ds.val[ds.val$plotid==tmp3[47],c('plotid', 'year','disturbed', 'predicted_probability', 'predicted')]

table(duration=ds.sods$duration, observed_dur=ds.sods$observed_dur)

# check for decline plots
q <- unique(ds.sods$plotid[ds.sods$process=='Decline'])
q

# check for multiple segment declines
q <- unique(ds.sods$plotid[ds.sods$duration!=ds.sods$observed_dur & ds.sods$observed_dur>1])
q

# check for long declines
q <- unique(ds.sods$plotid[ds.sods$duration>30 & ds.sods$process=='Decline'])
q

# ---
tmp4 <- unique(ds.val$plotid[ds.val$process=='Other'], )
ds.val[ds.val$plotid==tmp4[1],c('plotid', 'year','disturbed', 'predicted_probability', 'predicted')]
tmp4

# 1 - 200
# 900





#ds.first <- collapse_to_first_disturbance(ds.val, className='disturbed')
#ds.first <- merge(ds.first, strata_samples, by='plotid')

#aa.first <-  aa_stratified(ds.first$stratum, ds.first$year_obs, ds.first$year_pred, h=strata_weights$stratum, N_h=strata_weights$stratum_count)

#plot(aa.first$area$class[-1], aa.first$area$proportion[-1])



#------------------------

ignore <- c(grep('sp_', names(ds.sods)), grep('dt_', names(ds.sods)), grep('ds_', names(ds.sods)))

df <- ds.sods[,-ignore]

year <- 2003
h <- strata_weights$stratum
N_h <- strata_weights$stratum_count




## annual accuracy
aa.annual <- data.frame(year=NULL, accuracy=NULL, se=NULL, type=NULL, n=NULL)
area.annual <- data.frame(year=NULL, proportion=NULL, se=NULL)
for (year in unique(df$year)) {
  tmp <- df[df$year==year,]

  tmp[tmp$observed=="disturbed" & tmp$predicted=="undisturbed",]
  
  aa_confusion_matrix(tmp$observed, tmp$predicted)
  
  aa <-  aa_stratified(tmp$stratum, tmp$observed, tmp$predicted, h=h, N_h=N_h)
  
  aa.annual <- rbind(aa.annual, data.frame(year=year, 
                                           accuracy=c(aa$stats$ua[1], aa$stats$pa[1]),
                                           se=c(aa$stats$ua_se[1], aa$stats$pa_se[1]),
                                           type = c("User's accuracy", "Producer's accuracy"),
                                           n=nrow(tmp)))
  
  area.annual <- rbind(area.annual, data.frame(year=year,
                                               proportion=aa$area$proportion[1],
                                               se=aa$area$proportion_se[1]))
}
aa.annual$low <- aa.annual$accuracy - aa.annual$se
aa.annual$high <- aa.annual$accuracy + aa.annual$se

area.annual$low <- area.annual$proportion - area.annual$se
area.annual$high <- area.annual$proportion + area.annual$se


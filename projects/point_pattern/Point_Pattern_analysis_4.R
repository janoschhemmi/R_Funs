##Point pattern analysis 
.libPaths("S:/BrandSat/02_Code/R/library_4")

#install.packages("geosphere")
library(geosphere)
library(dplyr)
library(sf)
library(ggplot2)

## load points ----
xy <- read.table("S:/PointPatternAnalysis/lnglat.csv", sep = ";", dec = ".", header = TRUE)

## load gemeinden shape
#study_area <- read_sf("P:/workspace/jan/shapes/Borders/Studyarea_PointPattern_4326.shp")
study_area <- read_sf("P:/workspace/jan/shapes/point_pattern/studyarea_redo_4326.shp")


## load points in study area
# xy <- readOGR("P:/workspace/jan/shapes/point_pattern/Points_studyarea.shp")

my_sf <- st_as_sf(xy, coords = c('lon', 'lat'))
my_sf <- st_set_crs(my_sf, 4326)

plot(study_area$geometry)
plot(my_sf,add = TRUE)

## filter to study area 
my_sf$row_id <- seq(1:nrow(my_sf))
all   <- my_sf
my_sf <- st_filter(my_sf, study_area, .predicate = st_intersects)

all$in_study_area <- 0
all[row_id %in% my_sf$row_id, ]$in_study_area <- 1 

xy <- cbind(my_sf$id,my_sf$St.B,sf::st_coordinates(my_sf), my_sf$row_id) %>% as.data.frame()
zz <- xy[xy$V2==1,]
### remove same locations
## 685 for both
## redo:: 668 for both
oo <- xy[,] %>% distinct(X,Y,.keep_all = FALSE)

## 598 for 0
## 582 for 0 redo
oo_0 <- xy[xy$V2 == 0,] %>% distinct(X,Y,.keep_all = TRUE)

## 134 for 1
## 133 for 1 reddo 
oo_1 <- xy[xy$V2 == 1,] %>% distinct(X,Y,.keep_all = TRUE)

all$excluded_duplicated  <- 1
all[row_id %in% oo_0$V5 | row_id %in% oo_1$V5 , ]$excluded_duplicated <- 0 
all %>% group_by(St.B, excluded) %>% tally()

write.csv(all, "P:/workspace/jan/shapes/point_pattern/points_exluded.csv")
### list for julian 
## --> Patienten ID, Adresse, Einbezogen in Analyse (Ja/Nein), St?B (Ja/Nein)


## Aggregate ---- 
## how many DIstance of 0?
min_dis <- apply(dis[,3:ncol(dis)],1,min)
ss <- (dis[min_dis==0,])

length(min_dis[min_dis == 0])

## Split GRoups ----
# xy_0 <- xy[xy$St.B == 0 ,]
# xy_1 <- xy[xy$St.B == 1 ,]
# 
# dis_matrix_0 <- geosphere::distm(x = oo_0[,1:2],fun = distGeo)
# dis_matrix_1 <- geosphere::distm(x = oo_1[,1:2],fun = distGeo)
# 
# dd_0 <- dis_matrix_0[upper.tri(dis_matrix_0) | lower.tri(dis_matrix_0)]
# dd_1 <- dis_matrix_1[upper.tri(dis_matrix_1) | lower.tri(dis_matrix_1)]
# 
# dd_0 <- matrix(dd_0, ncol = 598, nrow = 597)
# dis_0 <- cbind(xy_0$id, t(dd_0)) %>% as.data.frame()
# dd_1 <- matrix(dd_1, ncol = 134, nrow = 133)
# dis_1 <- cbind(xy_1$id, t(dd_1)) %>% as.data.frame()
# 
# ## Aggregate ---- 
# ## how many DIstance of 0?
# min_dis_0 <- apply(dis_0[,2:ncol(dis_0)],1,min)
# length(min_dis_0[min_dis_0 == 0]) ## 306
# min_dis_1 <- apply(dis_1[,2:ncol(dis_1)],1,min)
# length(min_dis_1[min_dis_1 == 0]) ## 53
# 
# mean(min_dis_0) ## 338.4809
# mean(min_dis_1) ## 787.8206
# 
# ## min 10 
# f <- function(rw) {
#   O <- order(rw)[1:10]
#   #rbind(O,rw[O])
#   mean <- mean(rw[O])
# }
# 
# ## durchschnittliche Distanz zu allen anderen Punkten
# 
# result_0 <- mean(as.data.frame(dis_0)) # 1096.398
# result_1 <- mean(t(apply(dis_1,1,f))) # 1992

## SpatSTat PAckage 
# install.packages("spatstat")
library(spatstat)

##
s  <- st_read("P:/workspace/jan/shapes/point_pattern/studyarea_redo_utm.shp")
w  <- as.owin(s)
studyarea.km <- rescale(w, 1000)

s    <- st_read("P:/workspace/jan/shapes/point_pattern/Points_studyarea_utm.shp") 
s_0  <- st_read("P:/workspace/jan/shapes/point_pattern/Points_studyarea_utm_0_redo.shp") 
s_1  <- st_read("P:/workspace/jan/shapes/point_pattern/Points_studyarea_utm_1_redo.shp") 

## filter points to unique
s_0 <- s_0[s_0$id %in% oo_0$V1,]
s_1 <- s_1[s_1$id %in% oo_1$V1,]

## 0 
points_0  <- as.ppp(s_0)
points_0 <- rescale(points_0, 1000)
Window(points_0) <- studyarea.km
plot(points_0, main=NULL, cols=rgb(0,0,0,.2), pch=20)

## 1 
points_1  <- as.ppp(s_1)
points_1 <- rescale(points_1, 1000)
Window(points_1) <- studyarea.km
plot(points_1, main=NULL, cols=rgb(0,0,0,.2), pch=20)

hist(pairdist(points_0), breaks = 40)
hist(pairdist(points_1), breaks = 40)
quantile(pairdist(points_0))
quantile(pairdist(points_1))

## test -------------------------------------------------------------------------------
## skewness test 
#install.packages("e1071")
library(e1071)                    # load e1071 
skewness(pairdist(points_1))  

## bind
p_0 <- pairdist(points_0)

hist(p_0)
select_all_but_diag <- function(x) matrix(x[lower.tri(x, diag = F) | upper.tri(x, diag = F)], nrow = nrow(x) - 1, ncol = ncol(x))
p_0_tri <- p_0[upper.tri(p_0)]
hist(p_0_tri)
p_0 <- select_all_but_diag(p_0)
p_0[p_0 == 0] <- NA
sd(p_0, na.rm = T)
mean(p_0)


hist(p_1)

p_1 <- pairdist(points_1)
p_1_tri <- p_1[upper.tri(p_1)]
p_1 <- select_all_but_diag(p_1)
p_1[p_1 == 0] <- NA
sd(p_1_tri, na.rm = T)

tt <- read.csv2("P:/workspace/jan/fire_detection/Landsat_ts/extracted_Landsat_ts_2_with_outliers_till_2022_post1/")
tt

## rowmeans 
mean(p_0, na.rm = TRUE)
mean(p_1, na.rm = TRUE)
mean(p_0_tri, na.rm = TRUE)
mean(p_1_tri, na.rm = TRUE)

dd <- as.data.frame(rbind(cbind(apply(p_0,1,mean, na.rm=TRUE)
,"p_0"), cbind(apply(p_1,1,mean, na.rm=TRUE)
,"p_1")))

## mean of means 
mean(as.numeric(dd[dd$V2 == "p_0",]$V1))



q_0 <- apply(p_0,1,quantile, na.rm=TRUE, probs = 0.50)
q_1 <- (apply(p_1,1,quantile, na.rm=TRUE, probs = 0.50))
hist(q_0, breaks = 20)
hist(q_1, breaks = 20)


dd <- as.data.frame(rbind(cbind(as.vector(q_0) ,rep("q_0", length(q_0))),
                            cbind(as.vector(q_1) ,rep("q_1", length(q_1) ) ) ))
#  

dd$V1 <- as.numeric(dd$V1)
summary(dd)
wilcox.test(V1 ~ V2,data = dd,  paired = FALSE, na.action = na.omit) 
t.test(V1 ~ V2,data = dd,  paired = FALSE, na.action = na.omit) 

??wilcox.test

## test for differences
var.test(pairdist(points_0), pairdist(points_1))
agep <- t.test(pairdist(points_0), pairdist(points_1), var.equal = FALSE)
 

### not significant --> try to exclude cluster
# 
# points_0_xy <- s_0 %>%
#   st_coordinates()
#  
# points_1_xy <- s_1 %>%
#   st_coordinates()
# 
# ## dbscan
# # install.packages("dbscan")
# install.packages("fpc")
# library("dbscan")
# library("fpc")
# dbscan::kNNdistplot(matrix(s_0$lat, s_0$lon, nrow = 741,ncol = 2), k =  10)
# #db <- fpc::dbscan(matrix(s_0$lat, s_0$lon, nrow = 741,ncol = 2), eps = 1000, MinPts = 10)
# points_0_dbscan <- dbscan::dbscan(points_0_xy, eps = 500, minPts = 30)
# plot(points_0_xy, col = points_0_dbscan$cluster)
# 
# dbscan::kNNdistplot(matrix(s_1$lat, s_1$lon, nrow = 163,ncol = 2), k =  5)
# points_1_dbscan <- dbscan::dbscan(points_1_xy, eps = 500, minPts = 10)
# points_1_dbscan$cluster[points_1_dbscan$cluster == 0] <- 3
# plot(points_1_xy, col = points_1_dbscan$cluster)
# 
# ## exclude cluster
# points_1_sub <- subset(points_1,points_1_dbscan$cluster < 1 )
# points_0_sub <- subset(points_0,points_0_dbscan$cluster < 1 )
# 
# ## pairwise of sub points 
# p_0_sub <- pairdist(points_0_sub)
# p_1_sub <- pairdist(points_1_sub)
# 
# ## exclude 0s
# p_0_sub[p_0_sub == 0] <- NA
# p_1_sub[p_1_sub == 0] <- NA
# 
# ## into one df 
# hist(as.vector(p_0_sub))
# hist(as.vector(p_1_sub))
# mean(na.omit(as.vector(p_0_sub)))
# mean(na.omit(as.vector(p_1_sub)))
# 
#      
# dd <- as.data.frame(rbind(cbind(as.vector(p_0_sub) ,rep("p_0", length(p_0_sub))),
#                            cbind(as.vector(p_1_sub) ,rep("p_1", length(p_1_sub) ) ) ))
# 
# ## test 
# dd$V1 <- as.numeric(dd$V1)
# wilcox.test(V1 ~ V2,data = dd,  paired = FALSE, na.action = na.omit) 

## not significant -- take median of pairwise distance
p_0 <- pairdist(points_0)
p_1 <- pairdist(points_1)



par(mar=c(2,2,2,2), mfrow=c(1,2), oma=c(0,0,2,0))
hist(pairdist(points_0), 
     freq =F, 
     #breaks=nrow(pairdist(points_0)), 
     breaks=50, 
     xlim = c(0,60),
     xlab = "[km]",
     main='stationär')
hist(pairdist(points_1), 
     freq =F, 
     breaks=50, 
     xlim = c(0,60),
     xlab = '[km]',
     main='stationsäquivalent')
mtext("Histogram paarweiser Distanzen", outer = TRUE, cex = 1.5)

## FFUNCTION
??Fest
plot(Fest(points_0,  correction=c("rs")), main='independant')
plot(Fest(points_1,  correction=c("rs")), main='independant')

plot(Gest(points_0), main='independant')

## GFUNCTION
par(mar=c(2,2,2,2), mfrow=c(1,2), oma=c(0,0,2,0))
plot(Gest(points_0), main='stationär', xlim = c(0,2),ylim = c(0,1))
plot(Gest(points_1), main='stationsäquivalent', xlim = c(0,3), ylim = c(0,1))
mtext("G-Function", outer = TRUE, cex = 1.5)

g_0 <- Gest(points_0)
g_1 <- Gest(points_1)

plot((g_0$km - g_1$km) ~ g_0$r, main='stationär', xlim = c(0,2))


##
# k_0 <- Kest(points_0)
# k_1 <- Kest(points_1)
plot(k_0, main='clustered')
plot(K_1, main='clustered')

par(mar=c(2,2,2,2), mfrow=c(1,3), oma=c(0,0,2,0))
plot(k_0, main='stationär',xlim = c(0,12.5), ylim = c(0,950))
plot(k_1, main='stationsäquivalent', xlim = c(0,12.5), ylim = c(0,950))
plot((k_0$iso - k_1$iso) ~ k_0$r, type = "o" , las=1, main = "dif stationär stationsäquivalent" )#, ylim = c(-0.01, 0.02))

mtext("K-Function", outer = TRUE, cex = 1.5)

## q test 
points_0$marks <- "0"
points_1$marks <- "1"
ppp.merge(points_0, points_1)

q_0 <- quadrat.test(points_0, nx = 4, ny = 4)
plot(q_0)
q_0$p.value

q_1 <- quadrat.test(points_1, nx = 4, ny = 4)
plot(q_1)
q_1$p.value

## pairdist 
quantile(nndist(points_0))
quantile(nndist(points_1))


## set scale 
cuts=c(  0.0 , 0.5  ,1.0  ,1.5  ,2.0  ,2.5  ,3.0,  3.5,  4.0,  4.5,  5.0,  5.5  ,6.0  ,
         6.5,  7.0,  7.5  ,8.0,  8.5,  9.0,  9.5, 10.0, 10.5, 11.0,
         11.5 ,12.0 ,12.5, 13.0, 13.5, 14.0 ,14.5 ,15.0) #set breaks
pal <- colorRampPalette(c("midnightblue","bisque2"))

## layout 
par(mar=c(2,2,2,2), mfrow=c(1,2), oma=c(0,0,2,0), xpd = TRUE)

emp_0 <- distmap(points_0, clip = TRUE,eps = c(0.1,0.1))
plot(emp_0, main = "Empty space distances", col = pal(31),xlab = "",ylab = "")
plot(cells, add = TRUE)
emp_0  <- as.data.frame(emp_0)
mean(emp_0$value)

emp_1 <- distmap(points_1,clip = TRUE,eps = c(0.1,0.1))

plot(emp_1, main = "Empty space distances", col = pal(31))
plot(cells, add = TRUE)
# plot(studyarea.km, add = TRUE,border="gray40", lwd=3.5)
emp_1  <- as.data.frame(emp_1)
#ggplot(emp_1,aes(x,y))+geom_raster(aes(fill=value))
mean(emp_1$value)

##
# ggplot
emp_0$group <- "stationär"
emp_1$group <- "stationsäquivalent"
emp <- rbind(emp_0 , emp_1)
hist(emp_0$value)
hist(emp_1$value)

emp_0$brks <- cut(emp_0$value, 
                   breaks = (c( 1, 2, 3, 4, 5, 6, 7,8,9,10,11,12,13,14,15)), 
                   labels = (c( "1", "2", "3", "4", "5", "6", "7","8","9",
                              "10","11","12","13","14","15")),
                   include.lowest = TRUE)


distanz_plot <- ggplot(data=emp, aes(x,y)) + 

  geom_raster(aes(fill = value)) +
  coord_equal() +
  facet_wrap(~ group) +

  scale_fill_gradient(
    low="red", high="green") +
  #theme_void() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.947, 0.8),
        legend.key = element_blank(),
        legend.margin= margin(0, 0, 0, 0, "cm"),
        plot.background= element_blank(),
        panel.background = element_rect(fill='white', colour='grey40'),
        strip.background = element_rect(fill='grey90', colour='grey40'),
        strip.text = element_text(colour = 'black', face = "bold", size = 9),
        strip.text.y = element_text(angle = 0, hjust = -1)
      
        ) + scale_fill_viridis_c(option = "magma", name = "Distanz [km]")
  #scale_fill_gradient2(low = "blue", mid = "tomato4",high = "dodgerblue4",name = "Distanz [km]",
  #                     limits=c(0, 20))
  #geom_polygon(data = studyarea.km)
  #scale_fill_continuous(name = "Distanz [km]" )
distanz_plot  
ggsave(distanz_plot, filename = "Anhang_6_empty_Distance.png",
       path= "P:/workspace/jan/shapes/point_pattern/",
       dpi = 300, width = 12, height = 8)

## test for distanz 
# 1  homoscedasticity--are the variances homogenous?
var.test(emp_0$value, emp_1$value)
agep <- t.test(emp_0$value, emp_1$value, var.equal = FALSE)

??t.test
agep$p.value
#
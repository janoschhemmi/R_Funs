## create df from force feature response 


# set lib path 
.libPaths("S:/BrandSat/02_Code/R/library")

library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(reshape2)
library(formattable)
library(maptools)
library(sp)
library(rgdal)
library(raster)
library(sf)
library(rgeos)
library(randomForest)
library(caret)
library(matrixStats)
library(fortify)
library(mapac)
library(e1071)
library(gt)

cat(seq(20:165)+19)
data.path <- 'A:/04_Sampling/2021-02-05_S2_new_interpolation/Kernel_6/'
setwd(data.path)

# Force Features
pre <- ""
features <- read.table(paste0(pre,"features.csv"), sep = " "   ,dec = "." )
int      <- read.table(paste0(pre,"response.csv"), sep = " "   , dec = ".")
coo      <- read.table(paste0(pre,"coordinates.csv"), sep = " ", dec = ".")

unique(int) ## int bereinigt split get loaded + poplar
int_tt <- read_int_tt(int, return = "int_redifined")



Band_names <- c( #'Band01_BLU_TSI',
                 'Band02_GRN_TSI',
                 'Band03_RED_TSI',
                 'Band04_RE1_TSI',
                 'Band05_RE2_TSI',
                 'Band06_RE3_TSI',
                 'Band07_NIR_TSI',
                 'Band08_SW1_TSI',
                 'Band09_SW2_TSI'#,
                 #'Band10_NDV_TSI'
              ) # BLUE GREEN RED RE1 RE2 RE3 NIR SWIR1 SWIR2 NDVI 
#,
#  grn red re1 re2 re3 nir sw1 sw2 

header_list <- list()
lo = 1
for (bands in Band_names){
  print(lo)
  print(bands)
  for (i in seq (1:146)) {
    #print(i)
    year <- ifelse(i <= 73, "2018", 
                   ifelse( i <= 146, "2019",
                            "2020_delete"))
    # DOY
    DOY <- ifelse(i <= 73, i, 
                   i - 73)
    DOY <- DOY * 5
    DOY <- DOY - 1
    col_name <- (paste(bands, i,year, DOY, sep="_"))
    #print(col_name)
    header_list <- append(header_list, col_name)
  }
  lo = lo + 1  
} # create header list bands 
colnames(coo) <- c("coo_x", "coo_y")

colnames(features) <- header_list

colnames(features) <- header_list
df <- as.data.frame(cbind(coo, int_tt,features))
df_2 <- df
df_2 <- na.omit(df_2)
head(df_2)
list_list <- append(c("coo_x", "coo_y","int_split","TreeSpecies","int","int_redifined","int_testing","int_paper"), header_list)
colnames(df_2) <- list_list

unique(df_2$int_paper)
## write 
df_2[,1:10]
write.table(df_2, "df_features_kernel6.csv", sep = ";", col.names = TRUE, row.names = FALSE)

df_na <- df_2
df_na[df_na == -9999] <- NA
colSums(is.na(df_na))
df_na[is.na(df_na$Band03_RED_TSI_13_2018_64),1:2]

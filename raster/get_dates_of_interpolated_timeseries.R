
library(greenbrown)
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


install.packages("XML")
library(XML)

"//141.20.140.91//NAS_Rodinia/"
data <- xmlParse("C:/Uni/Masterarbeit/02_Force_Data/2016-2019_001-365_LEVEL4_TSA_LNDLG_SW1_TSI.tif.aux.xml")
xml_data <- xmlToList(data)

list = list()
for (i in seq(75:294)){
  i <- i + 74
  print(i)
  tt <- xml_data[i]
  date_info <- unlist(tt)[3]
  date_df <- data.frame(as.list(date_info))
  sub <- substring(date_df[1,1],1,10)
  list <- append(list , sub)
}


df_dates <- as.data.frame(list)
write.table(df_dates, "C:/Uni/Masterarbeit/03_Processing/03_model/df_dates_of_interpolatet.csv", sep =";", dec = ".")



library(readxl)
library(tidyverse)

folder<-"data/"

fileB<-"ETCICM_1621_NorthSea_biota_20200817.txt"
fileS<-"ETCICM_1621_NorthSea_sediment_20200817.txt"
fileW<-"ETCICM_1621_NorthSea_water_20200817.txt"
fileStn<-"ETCICM_1621_Stations_20200817.txt"

dfStn <- read.table(paste0(folder,fileStn),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")
dfS <- read.table(paste0(folder,fileS),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")
dfW <- read.table(paste0(folder,fileW),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")
dfB <- read.table(paste0(folder,fileB),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")

save(dfStn,file="data/ICES_CHASE_NS_Stns.Rda")
save(dfS,file="data/ICES_CHASE_NS_Sediment.Rda")
save(dfW,file="data/ICES_CHASE_NS_Water.Rda")
save(dfB,file="data/ICES_CHASE_NS_Biota.Rda")

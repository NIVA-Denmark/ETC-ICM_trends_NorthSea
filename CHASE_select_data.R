library(tidyverse)
library(lubridate)
library(sf)
library(hrbrthemes)
library(rworldmap)  # getMap()
library(raster)  # intersect()
library(rgdal)
library(graticule)
library(scales)
library(patchwork)



load("data/Stn_select.Rda")

  
load("data/ICES_CHASE_NS_Sediment.Rda") # dfS
load("data/ICES_CHASE_NS_Water.Rda")    # dfW
load("data/ICES_CHASE_NS_Biota.Rda")    # dfB

dfS <- dfS %>%
  mutate(SD_StationName=ifelse(SD_StationName=="",STATN,SD_StationName)) %>%
  left_join(df_stn_select,by=c("SD_StationName"="Station_Name")) %>%
  filter(!is.na(GRIDCODE))

dfW <- dfW %>%
  mutate(SD_StationName=ifelse(SD_StationName=="",STATN,SD_StationName)) %>%
  left_join(df_stn_select,by=c("SD_StationName"="Station_Name"))  %>%
  filter(!is.na(GRIDCODE))

dfB <- dfB %>%
  mutate(SD_StationName=ifelse(SD_StationName=="",STATN,SD_StationName)) %>%
  left_join(df_stn_select,by=c("SD_StationName"="Station_Name")) %>%
  filter(!is.na(GRIDCODE))

save(dfS,dfW,dfB,file="working_select_data.Rda")

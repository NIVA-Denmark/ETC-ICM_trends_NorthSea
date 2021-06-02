library(lubridate)
library(hrbrthemes)
library(patchwork)
library(scales)
library(sf)
library(rworldmap)  # getMap()
library(raster)  # intersect()
library(rgdal)
library(graticule)
library(tidyverse)

load("data/ICES_CHASE_NS_Stns.Rda")     # dfStn

# ----- world map for map backgroud --------------------------------------

world <- getMap(resolution = "high")

# define the geographical projections
prjLAEA <- CRS("+init=epsg:3035")
prjWGS84 <- CRS("+init=epsg:4326")

# define a rectangle used to crop the world map and assign projection
clip_map_WGS84 <- as(extent(-5, 15, 50, 65), "SpatialPolygons")
proj4string(clip_map_WGS84) <- prjWGS84

# crop the world map to the rectangle
world_clip_WGS84 <- raster::intersect(world, clip_map_WGS84)

# project the world map to TM36 and fortify for plotting in ggplot
world_clip_LAEA <- spTransform(world_clip_WGS84,prjLAEA)
world_clip_LAEA_f <- fortify(world_clip_LAEA) 

# ----- Select North Sea stations  -----------------------------------------------------------------

shape <- st_read(dsn="gis",layer="grid_NS_noUK")
shape <- st_set_crs(shape, 3035)

#plot(shape)

sf_stn <-  dfStn %>% 
  st_as_sf(coords=c("Station_Longitude", "Station_Latitude"))

sf_stn_geo = st_set_crs(sf_stn, 4326)

sf_stn_proj <- st_transform(sf_stn_geo, crs=3035)

df_stn_proj <- sf_stn_proj %>%
  mutate(x = unlist(map(sf_stn_proj$geometry,1)),
         y = unlist(map(sf_stn_proj$geometry,2)))
df_stn_proj$geometry <- NULL

sf_stn_select <- st_intersection(sf_stn_proj,shape)

df_stn_select <- sf_stn_select %>%
  mutate(x = unlist(map(sf_stn_select$geometry,1)),
         y = unlist(map(sf_stn_select$geometry,2)))
df_stn_select$geometry <- NULL

df_stn_select <- df_stn_select %>%
  dplyr::select(Station_Code,Station_Name,GRIDCODE,x,y,Shape_Area)

save(df_stn_select,file="data/Stn_select.Rda")

dfGridArea <- shape
dfGridArea$geometry <- NULL

save(dfGridArea,file="data/GridArea_km2.Rda")


# ----- Plot the North Sea stations  -----------------------------------------------------------------


p<-ggplot() +
  geom_sf(data=shape,fill = "#EEEEFF", colour = "#AAAAFF", alpha=0.6) +
  geom_polygon(data = world_clip_LAEA_f,
               aes(x = long, y = lat, group = group),
               fill = "#FFFFFF", colour = NA, alpha=1) +  #"black"
  geom_polygon(data = world_clip_LAEA_f,
               aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "#AAAAAA", alpha=0.4) +  #"black"
  geom_point(data=df_stn_select,aes(x=x,y=y),size=0.2) +
  ylab("") + xlab("LAEA [EPSG:3035]") +
  #coord_sf(expand=FALSE, crs=st_crs(3035), datum=st_crs(3035)) +
  coord_sf(xlim=c(3600000,4500000),ylim=c(3100000,4300000), expand=FALSE, crs=st_crs(3035) ) +
  theme_ipsum() +
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1,1),
        legend.background=element_rect(fill="#FFFFFF",
                                       size=0.5,
                                       linetype="solid",
                                       colour ="#AAAAAA"))
p

ggsave("png/station_map.png",p,dpi=100,units="cm",width=12,height=15)


library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(patchwork)
library(scales)




#load("data/ICES_CHASE_NS_Stns.Rda")     # dfStn
#load("data/ICES_CHASE_NS_Sediment.Rda") # dfS
#load("data/ICES_CHASE_NS_Water.Rda")    # dfW
#load("data/ICES_CHASE_NS_Biota.Rda")    # dfB

load("working_select_data.Rda")

dfS2 <- dfS %>%
  mutate(STATN=ifelse(STATN=="",paste0(Longitude,"_",Latitude),STATN)) %>%
  group_by(STATN,MYEAR,DATE) %>%
  summarise(Lat=mean(Latitude,na.rm=T),Lon=mean(Longitude,na.rm=T)) %>%
  mutate(Type="Sediment")
dfB2 <- dfB %>%
  mutate(STATN=ifelse(STATN=="",paste0(Longitude,"_",Latitude),STATN)) %>%
  group_by(STATN,MYEAR,DATE) %>%
  summarise(Lat=mean(Latitude,na.rm=T),Lon=mean(Longitude,na.rm=T)) %>%
  mutate(Type="Biota")
dfW2 <- dfW %>%
  mutate(STATN=ifelse(STATN=="",paste0(Longitude,"_",Latitude),STATN)) %>%
  group_by(STATN,MYEAR,DATE) %>%
  summarise(Lat=mean(Latitude,na.rm=T),Lon=mean(Longitude,na.rm=T)) %>%
  mutate(Type="Water")

dfX <- bind_rows(dfS2,dfB2,dfW2)

df1 <- dfX %>%
  distinct(MYEAR,STATN,Type,DATE) %>%
  mutate(Colour=ifelse(MYEAR %in% c(1970,1980,1990,2000,2010,2020),1,0)) %>%
  mutate(CountOf="Samples")

df2 <- dfX %>%
  distinct(MYEAR,STATN,Type) %>%
  mutate(Colour=ifelse(MYEAR %in% c(1970,1980,1990,2000,2010,2020),1,0))  %>%
  mutate(CountOf="Stations")


df <- bind_rows(df1,df2)


mypal <- c("#AAAAAA","#666666")

p <-ggplot(df, aes(x=MYEAR,fill=factor(Colour))) +
  geom_histogram(stat="count",width=0.5,alpha=0.5) +
  facet_grid(Type~CountOf,scales="free_y") +
  theme_ipsum() +
  theme(panel.spacing = unit(1, "lines"),
        strip.text.y = element_text(angle=0,size=9)) +
  scale_y_continuous(breaks= pretty_breaks(n=3)) +
  ylab("Count") +
  xlab("Year") +
  scale_fill_manual(guide=NULL,values=mypal)
p


ggsave("png/station_count.png",p,dpi=100,units="cm",width=20,height=12)

dfstnplot <- dfX %>%
  distinct(Lat,Lon)
dfstnplot$ID <- 1:nrow(dfstnplot)

# write.table(dfstnplot,file="gis/NS_stations.txt",sep=",",col.names=T,row.names=F)

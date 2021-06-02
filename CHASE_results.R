library(tidyverse)
library(lubridate)
library(patchwork)
library(hrbrthemes)
library(scales)

load("results/CHASE_QE.Rda") # dfCHASE_QE
load("data/GridArea_km2.Rda")


pal_class<-c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000")
ClassNames <- c("High","Good","Mod","Poor","Bad")

CHASEcat<-function(CS){
  if(!is.numeric(CS)) return(NA)
  cat <- ifelse(CS<0.5,1,2)
  cat <- ifelse(CS>1,3,cat)
  cat <- ifelse(CS>5,4,cat)
  cat <- ifelse(CS>10,5,cat)
  return(cat)
}

dfCHASE_QE <- dfCHASE_QE %>%
  filter(GRIDCODE!="")

dfCHASE <- dfCHASE_QE %>%
  group_by(GRIDCODE,MYEAR) %>%
  arrange(desc(CSum)) %>%
  slice(1)

dfCHASE <- dfCHASE %>%
  left_join(dfGridArea,by="GRIDCODE") %>%
  mutate(Cat=CHASEcat(CSum))

dfCHASE_area <- dfCHASE %>%
  group_by(MYEAR,Cat) %>%
  summarise(Area_km2=sum(Shape_Area,na.rm=T)) %>%
  mutate(pct=Area_km2/sum(Area_km2),
         Area_000km2 = 0.001*Area_km2) %>%
  mutate(Class=ClassNames[Cat])

dfCHASE_area$Cat <- factor(dfCHASE_area$Cat)
dfCHASE_area$Class <- factor(dfCHASE_area$Class,levels=ClassNames)

p1 <- ggplot(dfCHASE_area, aes(x=MYEAR, y=Area_000km2, fill=Class)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=pal_class) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("Area ['000 km2]")

sum(dfGridArea$Shape_Area) # 382489.4 km2

p1

dfCHASE_NS <- dfCHASE %>%
  group_by(MYEAR) %>%
  mutate(logCS=log10(CSum)) %>%
  summarise(logCS=sum(logCS*Shape_Area,na.rm=T)/sum(Shape_Area,na.rm=T),
            CSum=sum(CSum*Shape_Area,na.rm=T)/sum(Shape_Area,na.rm=T)) %>%
  mutate(logCSum=log10(CSum))



alpha_bands<-0.4
ER0<- -0.5
ER05<-log10(0.5)
ER10<-log10(1)
ER15<-log10(5)
ER20<-log10(10)
ERmax <- 3

pal_class<-c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000")
ClassNames <- c("High","Good","Mod","Poor","Bad")
alpha_bands <- 0.5

p2 <-ggplot(dfCHASE_NS, aes(x=MYEAR, y=logCSum)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_ribbon(aes(ymin=ER0,ymax=ER05,x=MYEAR),fill="#007eff",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER05,ymax=ER10,x=MYEAR),fill="#00d600",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER10,ymax=ER15,x=MYEAR),fill="#ffff00",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER15,ymax=ER20,x=MYEAR),fill="#ff8c2b",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER20,ymax=ERmax,x=MYEAR),fill="#ff0000",alpha=alpha_bands)+
  geom_smooth(method='lm',se=FALSE, color='turquoise4') +
  geom_point(colour="#000000") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CS)")

p2

plots_aligned <- align_patches(p1, p2)

p1 <- plots_aligned[[1]]
p2 <- plots_aligned[[2]]

ggsave("png/CHASE_area.png",p1,dpi=100,units="cm",width=25,height=12)
ggsave("png/CHASE_timeseries.png",p2,dpi=100,units="cm",width=25,height=12)




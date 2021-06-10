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

mgn_cm <- margin(0.0,0.2,0,0.3,"cm")
alpha_bands <- 0.5

p1 <- ggplot(dfCHASE_area, aes(x=MYEAR, y=Area_000km2)) + 
  geom_bar(stat = "identity",fill="#BBBBBB") +
  theme_ipsum() +
  scale_y_continuous(breaks = seq(0,400,200)) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.2,0.2,0.3,0.2,"cm")) +
  coord_cartesian(xlim=c(1978.5,2020),ylim=c(0,401),expand=F) +
  ylab("'000 km2")

p1

p2 <- ggplot(dfCHASE_area, aes(x=MYEAR, y=Area_000km2, fill=Class)) + 
  geom_bar(position="fill", stat="identity",alpha=alpha_bands) +
  scale_fill_manual(values=pal_class,guide=NULL) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme_ipsum() +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0,0.2,0.3,0.2,"cm"),
        ) +
  coord_cartesian(xlim=c(1978.5,2020),expand=F) +
  ylab("Area fraction")

p2


dfCHASE_NS <- dfCHASE %>%
  group_by(MYEAR) %>%
  mutate(logCS=log10(CSum)) %>%
  summarise(logCS=sum(logCS*Shape_Area,na.rm=T)/sum(Shape_Area,na.rm=T),
            CSum=sum(CSum*Shape_Area,na.rm=T)/sum(Shape_Area,na.rm=T)) %>%
  mutate(logCSum=log10(CSum)) %>%
  mutate(bandYr=MYEAR)
yrfrom<-min(dfCHASE_NS$bandYr)
yrto<-max(dfCHASE_NS$bandYr)

 dfCHASE_NS <- dfCHASE_NS %>%
   mutate(bandYr=ifelse(bandYr==yrfrom,yrfrom-1,ifelse(bandYr==yrto,yrto+1,bandYr)))


ER0<- -0.5
ER05<-log10(0.5)
ER10<-log10(1)
ER15<-log10(5)
ER20<-log10(10)
ERmax <- 3

pal_class<-c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000")
ClassNames <- c("High","Good","Mod","Poor","Bad")


dfBands <- data.frame(Class=c("Bad","Poor","Mod","Good","High"),
                      ymin=c(log10(10),log10(5),log10(1),log10(0.5),-0.5),
                      ymax=c(3,log10(10),log10(5),log10(1),log10(0.5)))
dfBandYears <- data.frame(Year=c(1978.5,2020))
dfBands <- merge(dfBands,dfBandYears,all=T)
dfBands$Class <- factor(dfBands$Class,levels=ClassNames)


mod<-summary(lm(logCSum~MYEAR,dfCHASE_NS))
mod_coeff <- mod$coefficients
reglabel <- paste0("y = ",round(mod_coeff[1,1],digits=2)," ",round(mod_coeff[2,1],digits=4),"x")

reglabel <- paste0(reglabel,"\np = ",round(mod_coeff[2,4],2))

p3 <-ggplot() +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_ribbon(data=dfBands,aes(ymin=ymin,ymax=ymax,x=Year,fill=Class),alpha=alpha_bands)+
  geom_smooth(data=dfCHASE_NS, aes(x=MYEAR, y=logCSum),
              method='lm',se=FALSE, color='#000000', linetype=2,size=1) +
  geom_point(data=dfCHASE_NS, aes(x=MYEAR, y=logCSum),colour="#000000") +
  scale_fill_manual(values=pal_class) +
  theme_ipsum() +
  coord_cartesian(xlim=c(1978.5,2020),expand=F) +
  theme(axis.text.x=element_text(angle=0,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0,0.2,0,0.2,"cm"),
        legend.position="bottom",legend.justification="right",
        legend.key.size = unit(0.5, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0)) +
  xlab("Year") + ylab("log10(CS)")  +
  annotate("text", label=reglabel,x=2019,y=1.7,size=4,hjust=1)

p3


layout <- '
A
B
B
B
C
C
C
'

p <- wrap_plots(A=p1, B=p2, C=p3, design = layout) 
p

pngfile <- "png/CHASE.png"
ggsave(pngfile,p,dpi=100,units="cm",width=25,height=15)


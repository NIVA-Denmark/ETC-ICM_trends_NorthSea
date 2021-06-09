rm(list=ls())
library("tidyverse")

# #bEMODNET<-FALSE
# bExcludeHG<-FALSE
# #bExcludeHG<-TRUE
# bExcludePBDE<-FALSE
# #bExcludePBDE<-TRUE
# #bExcludeMetals<-TRUE
bExcludeHG<-F
bExcludePBDE<-F
bExcludeMetals<-F
bOutput<-T


cat("Starting assessment.R\n")
#--------------------------------------------------------------------------------------------------------------------------------

resfolder <- "results/"

load("working_select_data.Rda")

df.biota.1 <- dfB
df.sediment <- dfS
df.water <- dfW

#df.conversion<-read.table("data/species_avg_lipid_drywt_OSPAR.txt", quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
#df.params<-read.table("data/PARAM.txt", quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.species<-read.table("data/species_type.txt", quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)


#------------------ process thresholds  -----------------------------------------
#df.thrsh <- read.table(paste0(thrshfolder,"thresholds.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.thrsh <- read.table("data/thresholds_v6.txt", quote="'",sep="\t", header=TRUE,fileEncoding="UTF-16",stringsAsFactors=FALSE)
df.groups <- read.table("data/groups_v4.txt", quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", allowEscapes = FALSE,stringsAsFactors=FALSE)
df.groups <- df.groups  %>%
  filter(!GROUP %in% c("WHOTEQ-PCB","BFSUM","PAH10","PCB6","PCB7"))

df.group.result <- read.table("data/Result_group_v4.txt", quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", allowEscapes = FALSE,stringsAsFactors=FALSE)
df.unit.factor <- read.table("data/unit_factor_v2.txt", quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", allowEscapes = FALSE,stringsAsFactors=FALSE)



df.groups <- select(df.groups,PARAM,GROUP,Multiplier,Required)

excl_list<-""
if(bExcludeHG==TRUE){
  cat("Excluding HG from thresholds\n")
  df.thrsh <- filter(df.thrsh,PARAM!="HG") # remove HG
  if (nchar(excl_list)<1){excl_list<-"_excl"}
  excl_list = paste0(excl_list,"_HG")
}
if(bExcludePBDE==TRUE){
  cat("Excluding PBDEs from thresholds\n")
  #df.thrsh <- filter(df.thrsh,!ID %in% c(3,21,131,172)) # remove BDEs
  df.thrsh <- filter(df.thrsh,PARAM!="PBDE6") # remove BDEs
  if (nchar(excl_list)<1){excl_list<-"_excl"}
  excl_list = paste0(excl_list,"_PBDEs")
}
if(bExcludeMetals==TRUE){
  cat("Excluding Metals from thresholds\n")
  df.thrsh <- filter(df.thrsh,!PARAM %in% c("CD","PB","HG","CR","ZN","CU","NI","AS")) # remove HG
  if (nchar(excl_list)<1){excl_list<-"_excl"}
  excl_list = paste0(excl_list,"_Metals")
}


df.thrsh <- filter(df.thrsh,PARAM!="")
df.thrsh <- filter(df.thrsh,is.na(Exclude))
df.thrsh.1 <- filter(df.thrsh,GROUP==1)
df.thrsh.2 <- filter(df.thrsh,is.na(GROUP))
df.thrsh.1 <-left_join(df.thrsh.1,df.groups,by=c("PARAM"="GROUP"))
df.thrsh.1$GROUP <- df.thrsh.1$PARAM
df.thrsh.1$PARAM <- df.thrsh.1$PARAM.y
df.thrsh.1$PARAM.y <- NULL
df.thrsh.2$GROUP <- df.thrsh.2$PARAM
df.thrsh.2$Multiplier <- 1
df.thrsh.2$Required <- NA
df.thrsh <- rbind(df.thrsh.1,df.thrsh.2)
rm(list=c("df.thrsh.1","df.thrsh.2"))

df.thrsh.sed<-filter(df.thrsh,Category=="Sediment")
df.thrsh.wat<-filter(df.thrsh,Category=="Water")


# where threshold is specified for both shellfish and fish, then make a copy foreach of them
df.thrsh.1 <- filter(df.thrsh,Biota.Type!="Both")
df.thrsh.2 <- filter(df.thrsh,Biota.Type=="Both")
df.thrsh.2$Biota.Type <- "Fish"
df.thrsh <- rbind(df.thrsh.1,df.thrsh.2)
df.thrsh.2$Biota.Type <- "Shellfish"
df.thrsh <- rbind(df.thrsh,df.thrsh.2)
df.thrsh <- arrange(df.thrsh, Category, ID)

rm(list=c("df.thrsh.1","df.thrsh.2"))

#------------------ process biota -----------------------------------------
# LNMEA – Length mean
# DRYWT% - Dryweight percent
# EXLIP% - Extractable lipid percent
# FATWT% - Fat weight percent
# LIPIDWT%
# priority 1. FATWT%, 2. LIPIDWT%, 3. EXLIP%

# Cleaning
#incorrect unit used for some data - I don't know what the correct one is, so delete these lines
df.biota.1 <- filter(df.biota.1,MUNIT!="umol/min/mg protein")
#df.biota2 <- filter(df.biota,(PARAM=="EXLIP%" && Value==0))

# replace "ug Sn/kg" with "ug/kg"
df.biota.1$MUNIT <- ifelse(df.biota.1$MUNIT=="ug Sn/kg","ug/kg",df.biota.1$MUNIT)
df.biota.1$MATRX<-ifelse(df.biota.1$MATRX=="MU&EP","MU",df.biota.1$MATRX)
df.biota.1$MATRX<-ifelse(df.biota.1$MATRX=="MU&FA","MU",df.biota.1$MATRX)
df.biota.1$Species<-ifelse(df.biota.1$Species=="Clupea harengus membras","Clupea harengus",df.biota.1$Species)
df.biota.1$Value<-df.biota.1$Value*ifelse(df.biota.1$PARAM %in% c("DRYWT%","EXLIP%","FATWT%","LIPIDWT%") & df.biota.1$Value > 1000,0.01,1)


df.biota.1$Species<- trimws(df.biota.1$Species)
df.biota.1 <- left_join(df.biota.1,df.species,by=c("Species"="Species"))
df.biota.1 <- filter(df.biota.1,Type %in% c("Fish","Shellfish"))

df.biota.1$Value <- df.biota.1$Value*ifelse(df.biota.1$QFLAG=="<",0.5,1)
df.biota.1 <- select(df.biota.1,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,STATN,
                               Latitude,Longitude,NOINP,UNCRT,tblSpotID,
                               tblUploadID,tblAnalysisID,QFLAG,
                               LMQNT))

#----------------Biota - Calculate normalization values: wet weight and lipid weight ---------------------

df.biota.norm <- df.biota.1 %>%
  filter(PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%")) %>%
  spread(PARAM,Value)

if((length(names(df.biota.norm)[names(df.biota.norm)=="FATWT%"]))<1){
  df.biota.norm[,'FATWT'] <- NA
}else{
  names(df.biota.norm)[names(df.biota.norm)=="FATWT%"]<-"FATWT"
}

if((length(names(df.biota.norm)[names(df.biota.norm)=="EXLIP%"]))<1){
  df.biota.norm[,'EXLIP'] <- NA
}else{
  names(df.biota.norm)[names(df.biota.norm)=="EXLIP%"]<-"EXLIP"
}

if((length(names(df.biota.norm)[names(df.biota.norm)=="LIPIDWT%"]))<1){
  df.biota.norm[,'LIPIDWT'] <- NA
}else{
  names(df.biota.norm)[names(df.biota.norm)=="LIPIDWT%"]<-"LIPIDWT"
}

names(df.biota.norm)[names(df.biota.norm)=="DRYWT%"]<-"DRYWT"

df.biota.norm$FATWT <- ifelse(is.na(df.biota.norm$FATWT),
                              ifelse(is.na(df.biota.norm$LIPIDWT),
                                     df.biota.norm$EXLIP,
                                     df.biota.norm$LIPIDWT),
                              df.biota.norm$FATWT)

df.biota.norm <- df.biota.norm %>%
  select(-c(EXLIP,LIPIDWT))

# remove erroneous values
df.biota.norm$OK <- 1
df.biota.norm$OK <- ifelse(df.biota.norm$DRYWT > 40 & df.biota.norm$Type == "Shellfish",0,df.biota.norm$OK)
df.biota.norm$OK <- ifelse(df.biota.norm$DRYWT < 3 & !is.na(df.biota.norm$DRYWT),0,df.biota.norm$OK)
df.biota.norm$OK <- ifelse(df.biota.norm$DRYWT > 30 & df.biota.norm$Type == "Fish" & df.biota.norm$MATRX == "MU",0,df.biota.norm$OK)


df.biota.norm <- df.biota.norm %>%
  filter(OK == 1)


df.biota.norm.species <- df.biota.norm %>%
  group_by(Type,Species,MATRX,MUNIT) %>%
  summarize(DRYWT_SPECIES=mean(DRYWT,na.rm=TRUE),FATWT_SPECIES=mean(FATWT,na.rm=TRUE)) %>%
  ungroup()

df.biota.norm.type <- df.biota.norm %>%
  group_by(Type,MATRX,MUNIT) %>%
  summarize(DRYWT_TYPE=mean(DRYWT,na.rm=TRUE),FATWT_TYPE=mean(FATWT,na.rm=TRUE)) %>%
  ungroup()

df.biota.norm.species <- df.biota.norm.species %>%
  left_join(df.biota.norm.type,by=c("Type"="Type","MATRX"="MATRX","MUNIT"="MUNIT"))

df.biota.norm.species$DRYWT_SPECIES<-ifelse(is.nan(df.biota.norm.species$DRYWT_SPECIES),
                                            df.biota.norm.species$DRYWT_TYPE,
                                            df.biota.norm.species$DRYWT_SPECIES)
df.biota.norm.species$FATWT_SPECIES<-ifelse(is.nan(df.biota.norm.species$FATWT_SPECIES),
                                            df.biota.norm.species$FATWT_TYPE,
                                            df.biota.norm.species$FATWT_SPECIES)
df.biota.norm.species<-select(df.biota.norm.species,-c(DRYWT_TYPE,FATWT_TYPE))


df.biota.norm <- df.biota.norm %>%
  group_by(tblBioID,tblSampleID,Type,Species,MATRX,MUNIT) %>%
  summarize(DRYWT=mean(DRYWT,na.rm=TRUE),FATWT=mean(FATWT,na.rm=TRUE)) %>%
  ungroup()

df.biota.norm <- df.biota.norm %>%
  left_join(df.biota.norm.species,by=c("Type"="Type","Species"="Species","MATRX"="MATRX","MUNIT"="MUNIT"))

df.biota.norm$DRYWT<-ifelse(is.nan(df.biota.norm$DRYWT),
                            ifelse(is.nan(df.biota.norm$DRYWT_SPECIES),NA,df.biota.norm$DRYWT_SPECIES),
                            df.biota.norm$DRYWT)
df.biota.norm$FATWT<-ifelse(is.nan(df.biota.norm$FATWT),
                            ifelse(is.nan(df.biota.norm$FATWT_SPECIES),NA,df.biota.norm$FATWT_SPECIES),
                            df.biota.norm$FATWT)
df.biota.norm<-select(df.biota.norm,-c(DRYWT_SPECIES,FATWT_SPECIES))



# Add the normalization values back to the main biota data
df.biota.1 <- filter(df.biota.1,!PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%"))
df.biota.1 <-left_join(df.biota.1, df.biota.norm)



df.biota<-df.biota.1

df.biota<-df.biota %>% filter(!is.na(Value))

df.biota$Species<- trimws(df.biota$Species)



# ------------------------------------------------------------------------------------------
# ------------------------ bioeffects ------------------------------------------------------
# ------------------------------------------------------------------------------------------

df.thrsh.be <- filter(df.thrsh,Category=="BioEffects") #Threshold.Species,
df.thrsh <- select(df.thrsh,PARAM,GROUP,Category,Biota.Type,Threshold.Unit,Threshold.BASIS,Threshold.Tissue,Threshold.Species,Threshold.Value,Multiplier,Required)

rm(list=c("df.biota.DRYWT","df.biota.FATWT","df.biota.EXLIP","df.biota.LIPIDWT"))

df.bioeffects<-filter(df.biota,PARAM %in% c("VDSI","MNC","LMD"))
df.biota<-filter(df.biota,!PARAM %in% c("VDSI","MNC","LMD"))


# VDSI for Littorina, Buccinum and Nassarius 
#  Littorina littorea
#  Buccinum undatum
#  Nassarius reticulatus
# VDSI for Neptunea and Nucella
#  Neptunea antiqua
#  Nucella lapillus

df.bioeffects.1<-inner_join(df.bioeffects,
                            select(df.thrsh.be,Substance.name,PARAM,Threshold.Species,Threshold.Unit,Threshold.Value),
                            by=c("PARAM"="PARAM","Species"="Threshold.Species"))
df.bioeffects.2<-inner_join(df.bioeffects,
                            select(df.thrsh.be,Substance.name,PARAM,Biota.Type,Threshold.Unit,Threshold.Value),
                            by=c("PARAM"="PARAM","Type"="Biota.Type"))

df.bioeffects<-rbind(df.bioeffects.1,df.bioeffects.2)
#rm(list=c("df.bioeffects.1","df.bioeffects.2"))

df.bioeffects$RESPONSE<-ifelse(df.bioeffects$PARAM=="LMD",-1,1)

df.bioeffects<-df.bioeffects %>%
  group_by(GRIDCODE,MYEAR,DATE,Substance.name,PARAM,
           Threshold.Unit,Threshold.Value,RESPONSE) %>%
  summarise(Value=mean(Value,na.rm=TRUE),n=n()) %>%
  ungroup()

#df.bioeffects.1<-filter(df.bioeffects.1,GRIDCODE=="100kmE40N37")
# We now have station/date averages (and sums for ggroup variables) 

# ----------------------- Bioeffects - mean or median ------------------------------------------ 
# Take the average (+median within GRIDCODE)
df.bioeffects<-df.bioeffects %>%
  group_by(GRIDCODE,Substance.name,PARAM,Threshold.Unit,Threshold.Value,RESPONSE) %>%
  summarise(Value=mean(Value,na.rm=TRUE),n=n()) %>%
  #summarise(Value=median(Value,na.rm=TRUE),n=n()) %>%
  ungroup()



df.bioeffects$CR<-ifelse(df.bioeffects$RESPONSE<1,df.bioeffects$Threshold.Value/df.bioeffects$Value,df.bioeffects$Value/df.bioeffects$Threshold.Value)

df.chase.bioeffects<-df.bioeffects %>%
  group_by(GRIDCODE) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n(),Max=max(CR,na.rm=T)) %>%
  ungroup()

df.chase.bioeffects$CSum<-df.chase.bioeffects$SumCR/df.chase.bioeffects$n

df.chase.bioeffects.worst <- df.chase.bioeffects %>%
  left_join(select(df.bioeffects,GRIDCODE,Max=CR,PARAM)) %>%
  group_by(GRIDCODE) %>%
  arrange(GRIDCODE,desc(Max)) %>%
  slice(1) %>% ungroup() %>%
  #filter(Max>=1) %>%
  left_join(select(df.group.result,PARAM,GroupResults)) %>%
  filter(CSum>=1, GRIDCODE!="")
df.chase.bioeffects.count <- df.chase.bioeffects.worst %>%
  group_by(GroupResults) %>%
  summarise(n=n())

  
# ------------------------------------------------------------------------------------------
# ------------------------ biota -----------------------------------------------------------
# ------------------------------------------------------------------------------------------

cat(paste0(nrow(df.biota)," rows\n"))

# Drop measurements in eggs
df.biota <- filter(df.biota,MATRX!="EG")


# match Biota with threshold values
df.biota<-inner_join(df.biota,
                     filter(df.thrsh,Category %in% c("Biota")),
                     by=c("PARAM"="PARAM","Type"="Biota.Type"))

# remove duplicate OSPAR metal thresholds for mussels / oysters
df.biota$DROP <- ifelse(df.biota$Threshold.Species=="",NA,TRUE)
df.biota$DROP <- ifelse(df.biota$Species %in% c("Ostrea edulis","Crassostrea gigas") & df.biota$Threshold.Species=="Oysters",
                          NA,df.biota$DROP)
df.biota$DROP <- ifelse((!df.biota$Species %in% c("Ostrea edulis","Crassostrea gigas")) & df.biota$Threshold.Species!="Oysters",
                          NA,df.biota$DROP)

df.biota <- df.biota %>% filter(is.na(DROP)) %>% select(-DROP)

cat(paste0(nrow(df.biota)," rows\n"))

df.out.biota<-df.biota

df.biota$match<-ifelse(df.biota$MATRX=="LI",
                       ifelse(df.biota$Threshold.Tissue=="MU",0,1),
                       ifelse(df.biota$MATRX=="MU",
                              ifelse(df.biota$Threshold.Tissue=="LI",0,1),
                              1))
cat(paste0(nrow(df.biota)," rows\n"))

df.biota<-df.biota %>%
  filter(match==1) %>%
  select(-c(match))

speciesavg<-df.biota %>%
  group_by(Type,Species,MATRX) %>%
  summarise(DRYWT_SPECIES=mean(DRYWT,na.rm=TRUE),FATWT_SPECIES=mean(FATWT,na.rm=TRUE)) 
typeavg<-df.biota %>%
  group_by(Type,MATRX) %>%
  summarise(DRYWT_TYPE=mean(DRYWT,na.rm=TRUE),FATWT_TYPE=mean(FATWT,na.rm=TRUE)) 

speciesavg<-speciesavg %>% 
  left_join(typeavg)

speciesavg$DRYWT_SPECIES<-ifelse(is.nan(speciesavg$DRYWT_SPECIES),speciesavg$DRYWT_TYPE,speciesavg$DRYWT_SPECIES)
speciesavg$FATWT_SPECIES<-ifelse(is.nan(speciesavg$FATWT_SPECIES),speciesavg$FATWT_TYPE,speciesavg$FATWT_SPECIES)
speciesavg<-select(speciesavg,-c(DRYWT_TYPE,FATWT_TYPE))

# now redo it without matrix to get overall average for shellfish / fish
typeavg2<-df.biota %>%
  group_by(Type) %>%
  summarise(DRYWT_TYPE=mean(DRYWT,na.rm=TRUE),FATWT_TYPE=mean(FATWT,na.rm=TRUE)) 

speciesavg<-speciesavg %>% 
  left_join(typeavg2)
  
speciesavg$DRYWT_SPECIES<-ifelse(is.nan(speciesavg$DRYWT_SPECIES),speciesavg$DRYWT_TYPE,speciesavg$DRYWT_SPECIES)
speciesavg$FATWT_SPECIES<-ifelse(is.nan(speciesavg$FATWT_SPECIES),speciesavg$FATWT_TYPE,speciesavg$FATWT_SPECIES)
speciesavg<-select(speciesavg,-c(DRYWT_TYPE,FATWT_TYPE))





df.biota<-left_join(df.biota,speciesavg)
df.biota$DRYWT<-ifelse(is.na(df.biota$DRYWT),df.biota$DRYWT_SPECIES,df.biota$DRYWT)
df.biota$FATWT<-ifelse(is.na(df.biota$FATWT),df.biota$FATWT_SPECIES,df.biota$FATWT)
df.biota<-select(df.biota,-c(DRYWT_SPECIES,FATWT_SPECIES))

cat(paste0(nrow(df.biota)," rows\n"))


# 05032019 - discovered that EMODNET uses "Dry" "Wet" instead of "D" "W"
df.biota$BASIS <- substr(df.biota$BASIS,1,1)


df.biota$factor.basis<-ifelse(df.biota$BASIS==df.biota$Threshold.BASIS,1,NA)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="W" & df.biota$Threshold.BASIS=="D",100/df.biota$DRYWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="W" & df.biota$Threshold.BASIS=="L",100/df.biota$FATWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="D" & df.biota$Threshold.BASIS=="W",df.biota$DRYWT/100,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="D" & df.biota$Threshold.BASIS=="L",df.biota$DRYWT/df.biota$FATWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="L" & df.biota$Threshold.BASIS=="D",df.biota$FATWT/df.biota$DRYWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="L" & df.biota$Threshold.BASIS=="W",df.biota$FATWT/100,df.biota$factor.basis)

df.biota<-arrange(df.biota,GRIDCODE,MYEAR,DATE,Category,Species,PARAM,GROUP,
                  tblBioID,tblSampleID,tblParamID,
                  Threshold.BASIS,Threshold.Unit,Threshold.Tissue,
                  Value,Multiplier,Threshold.Value,Required)

#find the factor for unit conversion  
df.biota <- left_join(df.biota,df.unit.factor,by=c("MUNIT","Threshold.Unit"))

#factor for the tissue conversion
df.biota$factor.Tissue<-ifelse(
  df.biota$Type=="Fish" & df.biota$MATRX=="LI" & df.biota$Threshold.Tissue %in% c("WB","MU"),
  0.1,1.0)
df.biota$factor.Tissue<-ifelse(
  df.biota$Type=="Fish" & df.biota$MATRX %in% c("WB","MU")  & df.biota$Threshold.Tissue=="LI",
  10,df.biota$factor.Tissue)
df.biota$factor.Tissue<-ifelse(
  df.biota$factor.Tissue==0.1 & df.biota$PARAM=="PFOS",
  0.0559,df.biota$factor.Tissue)

df.biota <- df.biota %>%
  mutate(factor.basis=ifelse(is.nan(factor.basis),1,factor.basis)) %>%
  mutate(Factor.Unit=ifelse(is.na(Factor.Unit),1,Factor.Unit)) 
  
df.biota$Value<-df.biota$Value*df.biota$factor.basis*df.biota$Factor.Unit *df.biota$factor.Tissue
df.biota$BASIS<-df.biota$Threshold.BASIS
df.biota$Unit<-df.biota$Threshold.Unit

df.biota<-df.biota %>%
  select(GRIDCODE,MYEAR,DATE,Category,Type,Species,PARAM,GROUP,
         BASIS=Threshold.BASIS,UNIT=Threshold.Unit,Tissue=Threshold.Tissue,
         Value,Multiplier,Threshold=Threshold.Value,Required,
         tblBioID,tblSampleID,tblParamID)

# Take mean of samples
df.biota<-df.biota %>%
  group_by(GRIDCODE,MYEAR,DATE,Category,Type,Species,PARAM,GROUP,
           BASIS,UNIT,Tissue,Multiplier,Threshold,Required) %>%
  summarise(Value=mean(Value,na.rm=TRUE)) %>% 
  ungroup()

df.biota$Value<-df.biota$Value*df.biota$Multiplier


df.biota<-df.biota %>%
  group_by(GRIDCODE,MYEAR,DATE,Category,Type,Species,GROUP,
           BASIS,UNIT,Tissue,Threshold) %>%
  summarise(Value=sum(Value,na.rm=TRUE),nreq=sum(Required,na.rm=TRUE),n=n()) %>%
  ungroup()


# We now have station/date averages (and sums for group variables) 

# Take the average  within GRIDCODE
df.biota<-df.biota %>%
  group_by(GRIDCODE,MYEAR,Category,Type,GROUP,BASIS,UNIT,Tissue,Threshold) %>%
  summarise(Value=mean(Value,na.rm=T)) %>%
  ungroup()


df.biota <- df.biota %>% filter(!is.na(Value))
df.biota$CR<-df.biota$Value/df.biota$Threshold

# If there is more than one CR value per substance (e.g. if there is a value for fish and shellfish) then take only the worst

df.chase.biota<-df.biota %>%
  group_by(GRIDCODE,MYEAR,Category,GROUP) %>%
  summarise(CR=max(CR,na.rm=TRUE)) %>% ungroup()


# added this line to exclude the values not used from df.biota
df.biota <- df.chase.biota %>% 
  ungroup() %>%
  left_join(df.biota)


df.chase.biota<-df.chase.biota %>%
  filter(!is.na(CR)) %>%
  group_by(GRIDCODE,MYEAR) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n(),Max=max(CR,na.rm=T)) %>%
  ungroup()

df.chase.biota$CSum<-df.chase.biota$SumCR/sqrt(df.chase.biota$n)

df.chase.biota.worst <- df.chase.biota %>%
  left_join(select(df.biota,GRIDCODE,MYEAR,Max=CR,GROUP)) %>%
  group_by(GRIDCODE,MYEAR) %>%
  arrange(GRIDCODE,MYEAR,desc(Max)) %>%
  slice(1) %>% ungroup() %>%
  # filter(Max>=1) %>%
  left_join(select(df.group.result,GROUP=PARAM,GroupResults)) %>%
  filter(CSum>1, GRIDCODE!="")

df.chase.biota.count <- df.chase.biota.worst %>%
  group_by(GroupResults) %>%
  summarise(n=n())


# ------------------------------------------------------------------------------------------
# ------------------------ sediment ------------------------------------------------------
# ------------------------------------------------------------------------------------------
df.sed <- df.sediment
df.sed$MUNIT <- ifelse(df.sed$MUNIT=="ug Sn/kg","ug/kg",df.sed$MUNIT)
df.sed$Value <- df.sed$Value*ifelse(df.sed$QFLAG=="<",0.5,1)
df.sed <- select(df.sed,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,
                           Latitude,Longitude,UNCRT,tblSpotID,
                           tblUploadID,tblAnalysisID,QFLAG,
                           LMQNT))
df.sed <- filter(df.sed,DEPHU<0.01)
df.sed$PARAM<-ifelse(df.sed$PARAM=="Cr","CR",df.sed$PARAM)


# find normalization parameters (C-org, Al)
df.sed.norm <- df.sed %>%
  filter(PARAM %in% c("AL","DRYWT%","Li","CORG")) %>%
  group_by(tblSampleID,PARAM,MATRX,BASIS,MUNIT) %>%
  summarize(Value=mean(Value,na.rm=TRUE)) %>%
  ungroup()

unit<-c("g/kg","mg/kg","ug/kg","ug/g","%")
factor<-c(0.001,1e-6,1e-9,1e-6,1)
factors<-data.frame(unit,factor,stringsAsFactors = FALSE)

df.sed.DRYWT <- df.sed.norm %>%
  filter(PARAM == "DRYWT%") %>%
  select(tblSampleID,MATRX,BASIS,Value,MUNIT) %>%
  left_join(factors,by=c("MUNIT"="unit")) %>%
  mutate(DRYWT=Value*factor,MUNIT="%") %>%
  select(-c(factor,MUNIT,Value))

df.sed.AL <- df.sed.norm %>%
  filter(PARAM == "AL") %>%
  select(tblSampleID,MATRX,BASIS,Value,MUNIT) %>%
  left_join(factors,by=c("MUNIT"="unit"))  %>%
  mutate(AL=Value*factor,MUNIT="%") %>%
  select(-c(factor,MUNIT,Value))

df.sed.CORG <- df.sed.norm %>%
  filter(PARAM == "CORG") %>%
  select(tblSampleID,MATRX,BASIS,Value,MUNIT) %>%
  left_join(factors,by=c("MUNIT"="unit")) %>%
  mutate(CORG=Value*factor,MUNIT="%") %>%
  select(-c(factor,MUNIT,Value))

df.sed.norm<-full_join(df.sed.DRYWT,df.sed.AL,by=c("tblSampleID","MATRX","BASIS"))
df.sed.norm<-full_join(df.sed.norm,df.sed.CORG,by=c("tblSampleID","MATRX","BASIS"))

rm(list=c("df.sed.AL","df.sed.CORG","df.sed.DRYWT"))
df.sed <- df.sed %>%
  filter(!PARAM %in% c("AL","DRYWT%","Li","CORG")) 

# Add normalization parameters back to data
df.sed <- left_join(df.sed,df.sed.norm,by=c("tblSampleID","MATRX","BASIS"))
df.sed$Dataset<-"DOME"

# Correction of Sediment CORG, AL
df.sed$CORG<-df.sed$CORG*ifelse(df.sed$CORG < 0.1,100,1)
df.sed$AL<-df.sed$AL*ifelse(df.sed$AL<0.1,100,1)
df.sed$AL<-ifelse(df.sed$AL<0.1,NA,df.sed$AL)

# calculate global average for CORG, DRYWT, AL
select<-df.sed %>% distinct(tblSampleID,GRIDCODE,DATE,CORG) %>% filter(CORG>0)
select$lnCORG<-log(select$CORG)
corgavg<-exp(mean(log(select$CORG),na.rm=T))
#ggplot(select,aes(lnCORG)) + theme_grey(base_size = 5)+geom_histogram(binwidth=0.1)

select<-df.sed %>% distinct(tblSampleID,GRIDCODE,DATE,DRYWT) %>% filter(DRYWT<90,DRYWT>1)
drywtavg<-exp(mean(log(select$DRYWT),na.rm=T))
#ggplot(select,aes(DRYWT)) + theme_grey(base_size = 5)+geom_histogram(binwidth=1)

select<-df.sed %>% distinct(tblSampleID,GRIDCODE,DATE,AL) %>% filter(AL<20,AL>0.1)
alavg<-mean(select$AL,na.rm=T)
#ggplot(select,aes(AL)) + theme_grey(base_size = 5)+geom_histogram(binwidth=0.1)


# use the relationship between ln(WETWT) and ln(CORG): ln(WETWT)=3.68+0.323*ln(CORG)
df.sed$DRYWT<-ifelse(is.na(df.sed$DRYWT),
                     ifelse(df.sed$CORG<15,100-39.6*(df.sed$CORG^0.323),NA),
                     df.sed$DRYWT)

df.sed$CORG<-ifelse(is.na(df.sed$CORG),
                    ifelse(df.sed$DRYWT>5,1.127e-05*((100-df.sed$DRYWT)^3.096),NA),
                    df.sed$CORG)

# If still missing, then use global averages
df.sed$AL<-ifelse(is.na(df.sed$AL),alavg,df.sed$AL)
df.sed$CORG<-ifelse(is.na(df.sed$CORG),corgavg,df.sed$CORG)
df.sed$DRYWT<-ifelse(is.na(df.sed$DRYWT),drywtavg,df.sed$DRYWT)

df.sed$NORM<-0

# Add threshold values
df.thrsh.sed <- df.thrsh.sed %>%
  select(PARAM,GROUP,Threshold.Unit,Threshold.BASIS,REF.PARAM,REF.VALUE,REF.UNIT,Threshold.Value,Threshold.Unit,Required)
df.thrsh.sed$REF.PARAM<-ifelse(df.thrsh.sed$REF.PARAM=="","CORG",df.thrsh.sed$REF.PARAM)
df.thrsh.sed$REF.VALUE<-ifelse(is.na(df.thrsh.sed$REF.VALUE),
                               ifelse(df.thrsh.sed$REF.PARAM=="CORG",2.5,
                                      ifelse(df.thrsh.sed$REF.PARAM=="AL",5,
                                             df.thrsh.sed$REF.VALUE)),
                               df.thrsh.sed$REF.VALUE)
df.thrsh.sed$REF.UNIT<-"%"

df.sed<-left_join(df.sed,df.thrsh.sed,by=c("PARAM"="PARAM"))

df.sed$factor<-ifelse(df.sed$REF.PARAM=="AL" & !is.na(df.sed$AL) & df.sed$AL > 0.5,df.sed$AL/df.sed$REF.VALUE,1)
df.sed$factor<-ifelse(df.sed$REF.PARAM=="CORG" & !is.na(df.sed$CORG),df.sed$CORG/df.sed$REF.VALUE,df.sed$factor)

df.sed <- left_join(df.sed,df.unit.factor,by=c("MUNIT","Threshold.Unit"))

BASIS<-c("D","D","W","W")
Threshold.BASIS<-c("D","W","D","W")
Power<-c(0,1,-1,0)

df.basis<-data.frame(BASIS,Threshold.BASIS,Power,stringsAsFactors=F)

df.sed <- left_join(df.sed,df.basis,by=c("BASIS"="BASIS","Threshold.BASIS"="Threshold.BASIS"))

df.sed$Factor.Basis<-(df.sed$DRYWT/100)^df.sed$Power
df.sed$Power<-NULL

df.sed$Value<-df.sed$Value*df.sed$factor*df.sed$Factor.Unit*df.sed$Factor.Basis

df.sed$BASIS<-df.sed$Threshold.BASIS
df.sed$Unit<-df.sed$Threshold.Unit

df.out.sed<-df.sed

# We need to take the average value per Grid before comparing with threshold value!

df.sed<-df.sed %>%
  filter(!is.na(Threshold.Value),!is.na(Value)) %>%
  select(GRIDCODE,STATN,MYEAR,DATE,PARAM,GROUP,Unit,Value,Threshold=Threshold.Value,Required) %>%
  arrange(GRIDCODE,STATN,MYEAR,DATE,PARAM,GROUP,Unit,Threshold)

#Average by STATN,DATE
df.sed<-df.sed %>%
  group_by(GRIDCODE,STATN,MYEAR,DATE,PARAM,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=mean(Value),na.rm=TRUE) %>%
  ungroup()

# Average by GRIDCODE, PARAM
df.sed<-df.sed %>%
  group_by(GRIDCODE,MYEAR,PARAM,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=mean(Value),na.rm=TRUE) %>%
  #summarise(Value=median(Value,na.rm=T)) %>%
  ungroup()

# combine group variables
df.sed<-df.sed %>%
  group_by(GRIDCODE,MYEAR,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=sum(Value),n=n(),nreq=sum(Required)) %>%
  ungroup()

df.sed <- df.sed %>% filter(!is.na(Value))
df.sed$CR<-df.sed$Value/df.sed$Threshold


df.chase.sed<-df.sed %>%
  filter(!is.na(CR)) %>%
  group_by(GRIDCODE,MYEAR) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n(),Max=max(CR,na.rm=T)) %>%
  ungroup()

df.chase.sed$CSum<-df.chase.sed$SumCR/sqrt(df.chase.sed$n)

df.chase.sed.worst <- df.chase.sed %>%
  left_join(select(df.sed,GRIDCODE,MYEAR,Max=CR,GROUP)) %>%
  group_by(GRIDCODE,MYEAR) %>%
  arrange(GRIDCODE,MYEAR,desc(Max)) %>%
  slice(1) %>% ungroup() %>%
  #filter(Max>=1) %>%
  left_join(select(df.group.result,GROUP=PARAM,GroupResults))  %>%
  filter(CSum>=1, GRIDCODE!="")
df.chase.sed.count <- df.chase.sed.worst %>%
  group_by(GroupResults) %>%
  summarise(n=n())


# ------------------------------------------------------------------------------------------
# ------------------------ water ------------------------------------------------------
# ------------------------------------------------------------------------------------------


df.wat.1 <- df.water
df.wat.1$MUNIT <- ifelse(df.wat.1$MUNIT=="ug Sn/kg","ug/kg",df.wat.1$MUNIT)
df.wat.1$Value <- df.wat.1$Value*ifelse(df.wat.1$QFLAG=="<",0.5,1)
df.wat.1 <- df.wat.1 %>%
  select(GRIDCODE,MYEAR,STATN,DATE,PARAM,Value,MUNIT)
df.wat.1$Dataset<-"DOME"

df.wat<-df.wat.1

df.out.wat<-df.wat

# Average for statn / date
df.wat<-df.wat %>% group_by(GRIDCODE,MYEAR,DATE,STATN,PARAM,MUNIT) %>%
  summarise(Value=mean(Value,na.rm=T))


df.wat<-df.wat %>% group_by(GRIDCODE,MYEAR,PARAM,MUNIT) %>%
  summarise(Value=mean(Value,na.rm=T))
  #summarise(Value=median(Value,na.rm=T))


df.wat <- df.wat %>%
  left_join(select(df.thrsh.wat,GROUP,PARAM,Threshold.Unit,Threshold.Value,Required),by=c("PARAM")) %>%
  filter(!is.na(Threshold.Value))

df.wat<-df.wat %>%
  left_join(df.unit.factor,by=c("MUNIT","Threshold.Unit"))

df.wat$Value<-df.wat$Value*df.wat$Factor.Unit


df.wat <- df.wat %>%
  select(GRIDCODE,MYEAR,GROUP,PARAM,Unit=Threshold.Unit,Value,Threshold=Threshold.Value,Required)

# combine group variables
df.wat<-df.wat %>%
  group_by(GRIDCODE,MYEAR,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=sum(Value),n=n(),nreq=sum(Required)) %>%
  ungroup()

df.wat <- df.wat %>% filter(!is.na(Value))
df.wat <- df.wat %>% mutate(Value=ifelse(Value<0,-1*Value,Value))

df.wat$CR<-df.wat$Value/df.wat$Threshold

df.chase.wat<-df.wat %>%
  group_by(GRIDCODE,MYEAR) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n(),Max=max(CR,na.rm=T)) %>%
  ungroup()

df.chase.wat$CSum<-df.chase.wat$SumCR/sqrt(df.chase.wat$n)

df.chase.wat.worst <- df.chase.wat %>%
  left_join(select(df.wat,GRIDCODE,Max=CR,GROUP)) %>%
  group_by(GRIDCODE,MYEAR) %>%
  arrange(GRIDCODE,MYEAR,desc(Max)) %>%
  slice(1) %>% ungroup() %>%
  # filter(Max>=1) %>%
  left_join(select(df.group.result,GROUP=PARAM,GroupResults)) %>%
  filter(CSum>=1, GRIDCODE!="")
df.chase.wat.count <- df.chase.wat.worst %>%
  group_by(GroupResults) %>%
  summarise(n=n())

# ------------------------------------------------------------------------------------------
# ------------------------ SUMMARY ------------------------------------------------------
# ------------------------------------------------------------------------------------------

df.chase.sed$Category<-"Sediment"
df.chase.biota$Category<-"Biota"
#df.chase.bioeffects$Category<-"Bio.Effects"
df.chase.wat$Category<-"Water"

dfCHASE_QE<-rbind(df.chase.biota,df.chase.sed,df.chase.wat)


save(dfCHASE_QE,file="results/CHASE_QE.Rda")

#####  ---------------------------- STOP HERE ------------------------------------

df.CHASE.QE <- df.CHASE.QE %>%
  filter(GRIDCODE!="")

df.CHASE <- df.CHASE.QE %>%
  group_by(GRIDCODE,MYEAR) %>%
  arrange(desc(CSum)) %>%
  slice(1)


df.CHASE <- df.CHASE %>%
  left_join(select(df.CHASE.QE,GRIDCODE,CSum,Category),by=c("GRIDCODE"="GRIDCODE","CHASE"="CSum")) %>%
  mutate(Worst=Category) %>%
  select(-c(Category))

df.CHASE_TRNSP<-df.CHASE.QE %>%
  select(GRIDCODE,Category,CSum) %>%
  ungroup() %>%
  spread(Category,CSum) %>%
  left_join(df.CHASE,by=c("GRIDCODE"="GRIDCODE"))

df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>0.5,2,1)
df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>1,3,df.CHASE_TRNSP$CHASE_CAT)
df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>5,4,df.CHASE_TRNSP$CHASE_CAT)
df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>10,5,df.CHASE_TRNSP$CHASE_CAT)

df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>0.5,2,1)
df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>1,3,df.CHASE_TRNSP$BIOEFF_CAT)
df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>5,4,df.CHASE_TRNSP$BIOEFF_CAT)
df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>10,5,df.CHASE_TRNSP$BIOEFF_CAT)

df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>0.5,2,1)
df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>1,3,df.CHASE_TRNSP$BIOTA_CAT)
df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>5,4,df.CHASE_TRNSP$BIOTA_CAT)
df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>10,5,df.CHASE_TRNSP$BIOTA_CAT)

df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>0.5,2,1)
df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>1,3,df.CHASE_TRNSP$SED_CAT)
df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>5,4,df.CHASE_TRNSP$SED_CAT)
df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>10,5,df.CHASE_TRNSP$SED_CAT)

df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>0.5,2,1)
df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>1,3,df.CHASE_TRNSP$WAT_CAT)
df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>5,4,df.CHASE_TRNSP$WAT_CAT)
df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>10,5,df.CHASE_TRNSP$WAT_CAT)

df.region<-read.table(paste0("./","grid_region.txt"), quote="",sep=",", header=TRUE, stringsAsFactors=FALSE)
df.region<-df.region %>% select(GRIDCODE,REGION)
df.CHASE_TRNSP2<-left_join(df.CHASE_TRNSP,df.region,by=c("GRIDCODE"="GRIDCODE"))


# summary of counts
df.chase.sed.count$Category<-"Sediment"
df.chase.biota.count$Category<-"Biota"
df.chase.bioeffects.count$Category<-"Bio.Effects"
df.chase.wat.count$Category<-"Water"
df.CHASE.count<-rbind(df.chase.bioeffects.count,df.chase.biota.count,df.chase.sed.count,df.chase.wat.count)

df.CHASE.count<-df.CHASE.count %>% select(Category,GroupResults,n) %>%  arrange(Category,desc(n))

# aggregate of worst
df.chase.bioeffects.worst$Category<-"Bio.Effects"
names(df.chase.bioeffects.worst)[names(df.chase.bioeffects.worst)=="PARAM"]<-"GROUP"
df.chase.sed.worst$Category<-"Sediment"
df.chase.biota.worst$Category<-"Biota"
df.chase.wat.worst$Category<-"Water"
#df.CHASE.worst<-rbind(df.chase.bioeffects.worst,df.chase.biota.worst,df.chase.sed.worst,df.chase.wat.worst)
df.CHASE.worst<-bind_rows(df.chase.bioeffects.worst,df.chase.biota.worst,df.chase.sed.worst,df.chase.wat.worst)

# if two substances have the same worst value, take only the first one (done within matrix)
df.CHASE.worst <- df.CHASE.worst %>%
  group_by(Category,GRIDCODE) %>%
  arrange(Category,GRIDCODE,desc(CSum)) %>%
  slice(1) %>%
  ungroup

df.CHASE.worst <- df.CHASE_TRNSP %>% filter(CHASE > 1) %>% select(GRIDCODE,Category=Worst) %>%
  left_join(df.CHASE.worst,by=c("GRIDCODE"="GRIDCODE","Category"="Category"))

df.CHASE.worst.count.1 <-  df.CHASE.worst %>%
  group_by(Category,GroupResults) %>%
  summarise(n=n()) %>%
  filter(!is.na(GroupResults)) %>% 
  arrange(Category,desc(n))

df.CHASE.worst.count <-  df.CHASE.worst %>%
  group_by(GroupResults) %>%
  summarise(n=n()) %>%
  filter(!is.na(GroupResults)) %>% 
  arrange(desc(n)) %>%
  mutate(Category="CHASE")

df.CHASE.count <-rbind(df.CHASE.count,df.CHASE.worst.count)
#there is one grid cell where none of the individual CR exceed 1.0 but the final result is > 1.0
# In this case the "worst" substance is not included
if(bOutput==TRUE){
  write.table(df.CHASE_TRNSP,file=paste0(resfolder,"CHASE_results",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
  write.table(df.CHASE.count,file=paste0(resfolder,"CHASE_worst",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
}

# -----------------------------------------------------------------------------------------------------------------------

df<-df.CHASE_TRNSP %>% left_join(select(df.region,GRIDCODE=GRIDCODE,Region=REGION))

df1<-df %>% filter(!is.na(Bio.Effects)) %>% group_by(Region) %>% summarise(n=n()) %>% mutate(Category="Bio.Effects")
df2<-df %>% filter(!is.na(Biota)) %>% group_by(Region) %>% summarise(n=n()) %>% mutate(Category="Biota")
df3<-df %>% filter(!is.na(Sediment)) %>% group_by(Region) %>% summarise(n=n()) %>% mutate(Category="Sediment")
df4<-df %>% filter(!is.na(Water)) %>% group_by(Region) %>% summarise(n=n()) %>% mutate(Category="Water")

df<-bind_rows(df1,df2,df3,df4)
df$Region<-ifelse(df$Region=="North-east Atlantic Ocean","NE Atlantic",df$Region)
df$Region<-ifelse(df$Region=="Mediterranean Sea","Mediterranean",df$Region)
df$Category <- factor(df$Category,levels=c("Water","Sediment","Biota","Bio.Effects"))
df$Region <- factor(df$Region,levels=c("Baltic Sea","Black Sea","Mediterranean","NE Atlantic"))

Region<-c("Baltic Sea","Black Sea","Mediterranean","NE Atlantic")
df.Region<-data.frame(Region)
df.Region$Region <- factor(df.Region$Region,levels=c("Baltic Sea","Black Sea","Mediterranean","NE Atlantic"))
df.Region$X<-1
Category<-c("Water","Sediment","Biota","Bio.Effects")
df.Category<-data.frame(Category)
df.Category$Category <- factor(df.Category$Category,levels=c("Water","Sediment","Biota","Bio.Effects"))
df.Category$X<-1
df<-left_join(df.Region,df.Category) %>% select(-X) %>% left_join(df)


library(ggplot2)
bar_cols <- c("#C7E0E9","#B4CB4C","#E3B73B","#990033")
p <- ggplot(df,aes(x=Region,fill=Category,y=n)) + theme_classic(base_size = 9) + ylab("Count of assessment units") +
  scale_x_discrete(drop=FALSE) + xlab("Region") +
  theme(legend.key.size=unit(0.4,"cm"), axis.title.x=element_blank(), axis.title.y = element_text(size=7)) +
  geom_bar(stat="identity", position = position_dodge(width=0.8),width=0.7) + scale_fill_manual(values=bar_cols)
p
if(bOutput==TRUE){
  #resfolder <- "../gis/results_20180403/"
  ph<-1.04*1.35*4.93/3.56
  pw<-1.04*6.2
  ggsave(filename=paste0(resfolder,"plot_GridCount",excl_list,".png"),plot=p, width = pw, height = ph,dpi=600)
  #ggsave(filename=paste0(resfolder,"plot_GridCount",excl_list,".png"),plot=p, width = 1.04*6.2, height = 1.04*1.35,dpi=600)
}

# ----------------- Region totals ---------------------------------------------------------------------


areafile<-"../gis/calc_real_area/Grid_sea_area.csv"
dfarea<-read.table(areafile, quote="",sep=",", header=TRUE, fileEncoding="UTF-8", stringsAsFactors=FALSE) %>%
  select(GRIDCODE,SEA_KM2)



df.CHASE.QE.2<-df.CHASE_TRNSP %>% select(GRIDCODE,CSum=CHASE)
df.CHASE.QE.2$Category<-"Integrated"
df.CHASE.QE.2<-bind_rows(select(df.CHASE.QE,GRIDCODE,CSum,Category),df.CHASE.QE.2)

df.CHASE.Regions <- df.CHASE.QE.2 %>% left_join(df.region,by=c("GRIDCODE"="GRIDCODE"))
df.CHASE.Regions$Class <-ifelse(df.CHASE.Regions$CSum <= 0.5, "H",
                          ifelse(df.CHASE.Regions$CSum <= 1, "G",
                                 ifelse(df.CHASE.Regions$CSum <= 5, "M",
                                        ifelse(df.CHASE.Regions$CSum <= 10, "P","B"))))
df.CHASE.Regions$Class <- factor(df.CHASE.Regions$Class, levels=c("H","G","M","P","B","Total"))
df.CHASE.Regions$km_2 <- ifelse(substr(df.CHASE.Regions$GRIDCODE,1,5)=="100km",10000,400)
df.CHASE.Regions$CO <- ifelse(substr(df.CHASE.Regions$GRIDCODE,1,5)=="100km","O","C")
df.CHASE.Regions$PA<- ifelse(df.CHASE.Regions$CSum<=1,"NPA","PA")

df.CHASE.Regions <- df.CHASE.Regions %>%
  left_join(dfarea,by=c("GRIDCODE"="GRIDCODE")) %>%
  mutate(km_2=SEA_KM2)
   

# Coverage 
df.CHASE.Regions.Coverage <- df.CHASE.Regions %>% 
  filter(Category=="Integrated") %>%
  group_by(REGION,CO) %>%
  summarise(n_cover=n(),km_2_cover=sum(km_2,na.rm=T))

df.Region.Coverage <- df.region %>%
  left_join(dfarea,by="GRIDCODE") %>%
  mutate(km_2=SEA_KM2,
         CO=ifelse(substr(GRIDCODE,1,5)=="100km","O","C")) %>%
  group_by(REGION,CO) %>%
  summarise(n_total=n(),km_2_total=sum(km_2,na.rm=T))

df.CHASE.Regions.Coverage <- df.CHASE.Regions.Coverage %>%
  left_join(df.Region.Coverage,by=c("REGION","CO")) %>%
  select(REGION,CO,km_2_cover,km_2_total,n_cover,n_total)

# df.CHASE.Regions.PApct <- df.region %>% 
#   mutate(km_2=ifelse(substr(GRIDCODE,1,5)=="100km",100,20)) %>%
#   group_by(REGION) %>%
#   summarise(n_total=n(),km_2_total=sum(km_2,na.rm=T))  # NOT NEEDED - we don't compare area of PA with unassessed area

df.CHASE.Regions.PApct <- df.CHASE.Regions %>% 
  filter(Category=="Integrated") %>%
  group_by(REGION) %>%
  summarise(n=n(),km_2=sum(km_2,na.rm=T))

df.CHASE.Regions.PApct <- df.CHASE.Regions %>% 
  filter(PA=="PA",Category=="Integrated") %>%
  group_by(REGION) %>%
  summarise(n_PA=n(),km_2_PA=sum(km_2,na.rm=T)) %>%
  left_join(df.CHASE.Regions.PApct,by="REGION") %>%
  mutate(area_pct_PA=sprintf("%.1f %%", 100*km_2_PA/km_2))


df.CHASE.PApct.OVERALL <- df.CHASE.Regions %>% 
  filter(Category=="Integrated") %>%
  group_by(PA) %>%
  summarise(n=n(),km_2=sum(km_2,na.rm=T))

if(bOutput==TRUE){
  write.table(df.CHASE.Regions.PApct,file=paste0(resfolder,"CHASE_PA_percent_km2",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
  write.table(df.CHASE.Regions.Coverage,file=paste0(resfolder,"CHASE_coverage_km2",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
}

# -------- Counts of classifications ------------------------------------------
# ALL regions
df.CHASE.Regions.1 <- df.CHASE.Regions %>% group_by(Category,Class) %>%
  summarise(n=n(),km_2=sum(km_2,na.rm=T))
df.CHASE.Regions.1$REGION<-"Total"
# BY region
df.CHASE.Regions.2 <- df.CHASE.Regions %>% group_by(REGION,Category,Class) %>%
  summarise(n=n())

df.CHASE.Regions.3 <- bind_rows(df.CHASE.Regions.1,df.CHASE.Regions.2)
df.CHASE.Regions.4 <- df.CHASE.Regions.3 %>% group_by(Category,REGION) %>%
  summarise(n=sum(n))
df.CHASE.Regions.4$Class<-"Total"
df.CHASE.Regions.4$Class <- factor(df.CHASE.Regions.4$Class, levels=c("H","G","M","P","B","Total"))
df.CHASE.Regions.Sum<-bind_rows(df.CHASE.Regions.3,df.CHASE.Regions.4)
df.CHASE.Regions.Sum<-df.CHASE.Regions.Sum %>% arrange(Category,REGION,Class)
df.CHASE.Regions.Sum$Category <- factor(df.CHASE.Regions.Sum$Category,levels=c("Water","Sediment","Biota","Bio.Effects","Integrated"))
df.CHASE.Regions.Sum <- df.CHASE.Regions.Sum %>%
  select(Category,Class,n,REGION)

df.CHASE.Regions.Sum.Trnsp<-df.CHASE.Regions.Sum %>% spread(REGION,n)
if(bOutput==TRUE){
  write.table(df.CHASE.Regions.Sum.Trnsp,file=paste0(resfolder,"CHASE_results_REGIONS",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=';', na="")
}

# aggregate of worst
df.chase.bioeffects.worst$Category<-"Bio.Effects"
names(df.chase.bioeffects.worst)[names(df.chase.bioeffects.worst)=="PARAM"]<-"GROUP"
df.chase.sed.worst$Category<-"Sediment"
df.chase.biota.worst$Category<-"Biota"
df.chase.wat.worst$Category<-"Water"
#df.CHASE.worst<-rbind(df.chase.bioeffects.worst,df.chase.biota.worst,df.chase.sed.worst,df.chase.wat.worst)
df.CHASE.worst<-bind_rows(df.chase.bioeffects.worst,df.chase.biota.worst,df.chase.sed.worst,df.chase.wat.worst)


# if two substances have the same worst value, take only the first one (done within matrix)
df.CHASE.worst <- df.CHASE.worst %>%
  group_by(Category,GRIDCODE) %>%
  arrange(Category,GRIDCODE,desc(CSum)) %>%
  slice(1) %>%
  ungroup

# Black Sea metals

df.CHASE.BlackSea <- df.CHASE %>%
  left_join(select(df.CHASE.worst,GRIDCODE,Worst=Category,Metal=GROUP)) %>%
  filter(!is.na(Worst))
df.CHASE.BlackSea <- df.CHASE.BlackSea %>% left_join(df.region,by=c("GRIDCODE"="GRIDCODE")) %>%
  filter(REGION=="Black Sea") %>% group_by(Metal) %>% summarise(n=n())
# ------------------------------------------------------------------------------------------------------

#Prepare input for offline CHASE calculations
df.input.sed<-df.sed
df.input.biota<-df.biota
df.input.bioeffects<-df.bioeffects
df.input.water<-df.wat
df.input.sed$Category<-"Sediment"
df.input.water$Category<-"Water"
df.input.bioeffects$Category<-"Bio.Effects"
df.input.biota<-df.input.biota %>% rename(Unit=UNIT)
df.input.bioeffects<-df.input.bioeffects %>% select(-Substance.name) %>% rename(GROUP=PARAM,Unit=Threshold.Unit,Value=Threshold.Value) 
df.input.ALL<-bind_rows(df.input.water,df.input.biota,df.input.bioeffects,df.input.sed)
df.input.ALL$Category<-factor(df.input.ALL$Category,levels=c("Water","Sediment","Biota","Bio.Effects"))
df.input.ALL <- df.input.ALL %>% 
  left_join(select(df.group.result,GROUP=PARAM,SubstGroup=GroupResults)) %>% 
  filter(GRIDCODE!="")%>%
  select(GRIDCODE,Category,SubstGroup,GROUP,Type,Tissue,Unit,BASIS,Threshold,Value,CR,RESPONSE) %>% ungroup()

df.input.ALL <- df.input.ALL %>% select(-RESPONSE)
if(bOutput==TRUE){
  write.table(df.input.ALL,file=paste0(resfolder,"CHASE_input",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
}

df.groupsum <- df.input.ALL %>% group_by(GRIDCODE,Category,SubstGroup) %>% summarise(sumCR=sum(CR,na.rm=T))
#df.groupsum$SubstGroup <- factor(df.groupsum$SubstGroup,levels=c("Metals","PAHs","PCBs","Organohalogens","Organotins","Other bio-effects","Alkylphenols","Phthalates","MAHs"))
  
df.group.worst <- df.groupsum %>% arrange(Category,GRIDCODE,desc(sumCR)) %>%
  slice(1) %>%
  ungroup()

df.group.worst.QE <- df.CHASE.QE %>% 
  filter(CSum>1) %>%
  select(GRIDCODE,Category) %>%
  left_join(df.group.worst)

df.group.worst.QE.count <- df.group.worst.QE %>%
  group_by(Category,SubstGroup) %>%
  summarise(n=n()) %>%
  spread(Category,n)

df.group.worst <- df.CHASE %>% 
  filter(CHASE>1) %>%
  select(GRIDCODE,Category=Worst) %>%
  left_join(df.group.worst) %>%
  left_join(select(df.region,GRIDCODE=GRIDCODE,REGION))

df.group.worst$REGION <- factor(df.group.worst$REGION,levels=c("Baltic Sea","Black Sea","Mediterranean Sea","North-east Atlantic Ocean"))

df.group.worst.region.count <- df.group.worst %>%
  group_by(REGION,SubstGroup) %>%
  summarise(n=n()) %>%
  spread(REGION,n) %>%
  arrange(desc(`North-east Atlantic Ocean`))

if(bOutput==TRUE){
  write.table(df.group.worst.region.count,file=paste0(resfolder,"CHASE_worst_REGIONS",excl_list,".csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
}


# ------ Plot bar-charts for df.CHASE.Regions.Sum -----------------------------------------------

df.CHASE.Regions.Plot<-df.CHASE.Regions.Sum %>%
  filter(Class!="Total",REGION!="Total") %>%
  rename(Region=REGION)

df.CHASE.Regions.Plot$Region<-ifelse(df.CHASE.Regions.Plot$Region=="North-east Atlantic Ocean","NE Atlantic",df.CHASE.Regions.Plot$Region)
df.CHASE.Regions.Plot$Region<-ifelse(df.CHASE.Regions.Plot$Region=="Mediterranean Sea","Mediterranean",df.CHASE.Regions.Plot$Region)

#df.CHASE.Regions.Plot$REGION <- factor(df.CHASE.Regions.Plot$REGION,levels=c("Baltic Sea","Black Sea","Mediterranean Sea","North-east Atlantic Ocean","Total"))
df.CHASE.Regions.Plot$Region <- factor(df.CHASE.Regions.Plot$Region,levels=c("Baltic Sea","Black Sea","Mediterranean","NE Atlantic","Total"))

df.CHASE.Regions.Plot.Sum <- df.CHASE.Regions.Plot %>% group_by(Category,Region) %>% summarise(sum=sum(n,na.rm=T))
df.CHASE.Regions.Plot <- df.CHASE.Regions.Plot %>% left_join(df.CHASE.Regions.Plot.Sum)
df.CHASE.Regions.Plot <- df.CHASE.Regions.Plot %>% mutate(f=n/sum)

x1<-df.CHASE.Regions.Plot %>% ungroup() %>% distinct(Region) %>% mutate(X=1)
x2<-df.CHASE.Regions.Plot %>% ungroup() %>%distinct(Class) %>% mutate(X=1)
x3<-df.CHASE.Regions.Plot %>% ungroup() %>%distinct(Category) %>% mutate(X=1)
x <- x1 %>% left_join(x2) %>% left_join(x3) %>% select(-X)
df.CHASE.Regions.Plot <- x %>% left_join(df.CHASE.Regions.Plot)

library(ggplot2)
library(ggthemes)
#bar_cols <- c("#33AA00","#99FF66","#FFD5CC","#FF8066","#FF2B00")
bar_cols <- c("#FFF5E6","#FFE0B3","#FFB84D","#F29100","#B36B00") #Brown scale

bar_labs <- c("High","Good","Moderate","Poor","Bad")

theme_set(theme_minimal(base_size = 15))
theme = theme_update(legend.position="bottom", legend.title=element_blank(), panel.grid.major.x=element_blank())

levels(df.CHASE.Regions.Plot$Region) <- c("Baltic Sea","Black Sea","Mediterranean\nSea","North-East\nAtlantic Ocean","Total")

g <- ggplot(df.CHASE.Regions.Plot, aes(Category,n)) + scale_fill_manual(values=bar_cols,labels=bar_labs)
g <- g + geom_col(width = 0.5, aes(fill=Class), position=position_dodge()) + facet_grid(Region~., scales="free", margins = FALSE) +
  ylab("Count of assessment units") + xlab("Category")
g
if(bOutput==TRUE){
  ggsave(filename=paste0(resfolder,"plot_class_count",excl_list,".png"),plot=g, width = 10, height = 12,dpi=600)
}

#write.table(df.thrsh,file=paste0(resfolder,"Thresholds.csv"), row.names=FALSE,quote=FALSE,sep=';', na="")


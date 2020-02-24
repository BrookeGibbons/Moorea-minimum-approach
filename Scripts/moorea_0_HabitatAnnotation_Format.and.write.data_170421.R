# Set directories----
rm(list=ls())
study<-"mad.schools"

# testing

# Add you work dir here-
work.dir=("C:/GitHub/Moorea-minimum-approach")

habitat.data=paste(work.dir,"Data/Habitat",sep="/")
tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
plots=paste(work.dir,"Plots",sep="/")

# Libraries required
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load and format habitat annotation data from TransectMeasure----
setwd(habitat.data)
dir()

hab<-read.delim("170421_inside_outside_habitat.TXT",header=T,skip=4,stringsAsFactors=FALSE)%>%
  setNames(tolower(names(.)))%>%
  separate(filename, c("OpCode", "B"),sep="_")%>%
  select(-c(x,x.1,frame,time..mins.,date,location,site..,transect..,latitude,longitude,rugosity,depth,collector,fishing.status,spare,spare.1,code,B,morphology,type,radius..))%>%
  group_by(Period = rep(1:324, each = 200))%>%
  mutate(OpCode_Period=paste(OpCode,Period,sep="_"))%>%
  glimpse()

64800/200 # Number of transects

# Create %fov----
hab$row <- 1:nrow(hab)

fov<-hab%>%
  select(-c(broad,relief))%>%
  filter(!fieldofview=="")%>%
  filter(!is.na(fieldofview))%>%
  mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  mutate(count=1)%>%
  # select(-c(image.row,image.col,Period,OpCode))%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  ungroup()%>%
  select(-c(image.row,image.col,row,OpCode,Period))%>%
  group_by(OpCode_Period)%>%
  summarise_each(funs(sum))%>%
  group_by(OpCode_Period)%>%
  mutate_each(funs(./2))%>%
  mutate_each(funs(replace(.,is.na(.),0)))%>%
  separate(OpCode_Period, c("OpCode", "Period"),sep="_")%>%
  group_by(OpCode) %>%
  mutate(Transect = seq_along(OpCode))%>%
  mutate(Sample=paste(OpCode,Transect,sep="."))%>%
  ungroup()%>%
  select(-c(OpCode,Period,Transect))%>%
  glimpse()


# Create relief----
relief<-hab%>%
  ungroup()%>%
  filter(!broad%in%c("Unknown","Open Water"))%>%
  filter(!relief%in%c("Unknown",""))%>%
  select(-c(broad,fieldofview,image.row,image.col))%>%
  mutate(relief.rank=ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  select(-c(relief))%>%
  mutate(relief.rank=as.numeric(relief.rank))%>%
  group_by(OpCode_Period)%>%
  summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  separate(OpCode_Period, c("OpCode", "Period"),sep="_")%>%
  group_by(OpCode) %>%
  mutate(Transect = seq_along(OpCode))%>%
  mutate(Sample=paste(OpCode,Transect,sep="."))%>%
  ungroup()%>%
  select(-c(OpCode,Period,Transect))%>%
  glimpse()

# CREATE catami_broad------
broad<-hab%>%
  select(-c(fieldofview,relief))%>%
  mutate(broad=ifelse(broad=="Octocoral/Black","Octocoral.Black",ifelse(broad=="Stony corals","Stony.corals",ifelse(broad=="Open Water","Open.Water",broad))))%>% #correct bad names
  filter(!broad=="")%>%
  filter(!is.na(broad))%>%
  filter(!broad=="Unknown")%>%
  filter(!broad=="Open.Water")%>%
  mutate(broad=paste("broad",broad,sep = "."))%>%
  mutate(count=1)%>%
  group_by(OpCode_Period)%>%
  spread(key=broad,value=count,fill=0)%>%
  select(-c(image.row,image.col,row,OpCode,Period))%>%
  group_by(OpCode_Period)%>%
  # mutate_each(funs(replace(.,is.na(.),0)))%>%
  summarise_each(funs(sum))%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(OpCode_Period)%>%
  mutate_each(funs(./Total.Sum), matches("broad"))%>%
  select(-Total.Sum)%>%
  mutate(broad.Reef=broad.Consolidated+broad.Macroalgae+broad.Stony.corals)%>%
  separate(OpCode_Period, c("OpCode", "Period"),sep="_")%>%
  group_by(OpCode) %>%
  mutate(Transect = seq_along(OpCode))%>%
  mutate(Sample=paste(OpCode,Transect,sep="."))%>%
  ungroup()%>%
  select(-c(OpCode,Period,Transect))%>%
  glimpse()


# Write final habitat data----
# join starting with relief - as this is most liekly to have the most samples with habitat data
setwd(tidy.data)
dir()

habitat<-relief%>%
  left_join(fov,by="Sample")%>%
  left_join(broad,by="Sample")%>%
  rename(hard.corals=broad.Stony.corals,rock=broad.Consolidated,macroalgae=broad.Macroalgae,sand=broad.Unconsolidated,reef=broad.Reef)%>%
  glimpse()

write.csv(habitat,file=paste(study,"_habitat.output.csv",sep = "_"), row.names=FALSE)

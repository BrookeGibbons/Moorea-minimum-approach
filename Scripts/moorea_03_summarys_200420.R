
###### Format MaxN, Length and 3D point that results from Checked data outputed from EventMeasure ######

### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois et al. 2015. Length selectivity of commercial fish traps assessed from in situ comparisons with stereo-videos: is there evidence of sampling bias? Fisheries Research"
# Please cite it if you like it


### objective is to 

# 1. Import checked data
# 2. Make mass estimates from Length
# 3. Make species richness and total
# 4. Write long and wide data sets for further analysis


# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
# Reading and formatting data
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

# Plotting
library(ggplot2)

# Googlesheets
#library(googlesheets)
library(googlesheets4)  


# Set directories----
rm(list=ls())
study<-"mad.ellen.brooke"

# Add you working directory here ----
work.dir <- ("~/Git Projects/current/2020-Moorea-minimum-approach") # Use this directory name from now on (MAC)
work.dir <- ("Y:/2020-Moorea-minimum-approach") # Work laptop

em.export=paste(work.dir,"Data/EM export",sep="/")
em.check=paste(work.dir,"Data/EM to check",sep="/")
tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
summaries=paste(work.dir,"Data/Summaries",sep="/") # For Ellen's Outside
# summaries=paste(work.dir,"Data/Summaries/Combined",sep="/") # For Brooke's Inside/Outside #TJL - that is confusing

plots=paste(work.dir,"Plots",sep="/")
life.history=("~/Google Drive/Projects/Project_Moorea Fish behaviour")


# Read in the data----
setwd(tidy.data)
dir()
data<-read.csv("2017-05-04_mad.ellen.brooke_combined.factors.habitat.csv")



# Summarise the long data----
setwd(summaries)
dir()

dat<-data%>%
  filter(!is.na(Length))%>%
  filter(Length<300)%>%
  filter(Length>80)%>%
  filter(Status=="Fished")

head(dat,2)


# Summaries for Ellen----

family.summary.mad<-dat%>%
  group_by(Region,Reef.Lagoon,Family,Site) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=Family,value=Count, fill = 0)
head(family.summary.mad,10)
write.csv(family.summary.mad,file=paste(study,"family.summary.mad.csv",sep = "_"), row.names=FALSE)

trophic.summary.mad<-dat%>%
  group_by(Region,Reef.Lagoon,diet,Site) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=diet,value=Count, fill = 0)
head(trophic.summary.mad,10)
write.csv(trophic.summary.mad,file=paste(study,"trophic.summary.mad.csv",sep = "_"), row.names=FALSE)

TargetLoc.summary.mad<-dat%>%
  group_by(Region,Reef.Lagoon,TargetLoc,Site,Location,Status) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=TargetLoc,value=Count, fill = 0)
head(TargetLoc.summary.mad,10)
write.csv(TargetLoc.summary.mad,file=paste(study,"TargetLoc.summary.mad.csv",sep = "_"), row.names=FALSE)

## Separate Genus_species for Genus as Factor summary
dat2<-data%>%
  filter(!is.na(Length))%>%
  filter(Length<300)%>%
  separate(Genus_species, c("Genus", "species"),sep=" ")

head(dat2, 2)

genus.summary.mad<-dat2%>%
  group_by(Region,Reef.Lagoon,Genus,Site) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=Genus,value=Count, fill = 0)
head(genus.summary.mad,10)
write.csv(genus.summary.mad,file=paste(study,"genus.summary.mad.csv",sep = "_"), row.names=FALSE)

genus.species.summary.mad<-dat%>%
  group_by(Region,Reef.Lagoon,Genus_species,Site) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=Genus_species,value=Count, fill = 0)
head(genus.species.summary.mad,10)
write.csv(genus.species.summary.mad,file=paste(study,"genus.species.summary.mad.csv",sep = "_"), row.names=FALSE)


# Summaries for Brooke----

status.family.summary.mad.<-dat%>%
  filter(!Location%in%c("Motu Ahi","Taotaha"))%>%
  group_by(Location,Status,Reef.Lagoon,Family,Site) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=Family,value=Count, fill = 0)
head(status.family.summary.mad.,10)
write.csv(status.family.summary.mad.,file=paste(study,"status.family.summary.mad..csv",sep = "_"), row.names=FALSE)

head(dat,2)

status.trophic.summary.mad.<-dat%>%
  filter(!Location%in%c("Motu Ahi","Taotaha"))%>%
  group_by(Location,Status,Reef.Lagoon,diet) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=diet,value=Count, fill = 0)
head(status.trophic.summary.mad.,3)
write.csv(status.trophic.summary.mad.,file=paste(study,"status.trophic.summary.mad..csv",sep = "_"), row.names=FALSE)

head(dat,2)

library(magrittr)
library(dplyr)
dta %>% 
  summarise_each(funs(sum(is.na(.)) < n()/2)) %>% 
  unlist() %>%
  extract(dta,.)



status.species.summary.mad.<-dat%>%
  filter(!Location%in%c("Motu Ahi","Taotaha","Nuarei","Maatea"))%>% 
  group_by(Location,Status,Reef.Lagoon,Genus_species) %>%
  summarise(Count=length(final.mad))%>%
  spread(key=Genus_species,value=Count)    #, fill = 0
  


# 
#   group_by(Location,Status,Reef.Lagoon) %>%
#   summarise(Min=length(final.mad))%>%
#   spread(key=Genus_species,value=Count, fill = 0)%>%
#   ungroup()
#   # select(-c(Location,Status,Reef.Lagoon))%>%
#   # Filter(function(x) mean(1) < 0.5, .)
#   # select(which(colSums(. < 5)))

head(status.species.summary.mad.,10)
write.csv(status.species.summary.mad.,file=paste(study,"status.species.summary.mad..csv",sep = "_"), row.names=FALSE)

new<-status.species.summary.mad. %>%
  Filter(function(x) sum(x, na.rm=TRUE))


new<-status.species.summary.mad. %>%
  replace(is.na(.), 0) %>%
  group_by(Location,Status,Reef.Lagoon)%>%
  summarise(Minimum=sum(.))

head(new,3)

new<-status.species.summary.mad. %>% 
  summarise_each(funs(min(>5))) %>% 
  unlist() %>%
  extract(status.species.summary.mad.,.)




###### Format MaxN, Length and 3D point that results from Checked data outputed from EventMeasure ######

### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois et al. 2015. Length selectivity of commercial fish traps assessed from in situ comparisons with stereo-videos: is there evidence of sampling bias? Fisheries Research"
# Please cite it if you like it


### objective is to 

# 1. Import checked data
# 2. Write long  data sets for further analysis

# Libraries required
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(googlesheets)


# Set directories----
rm(list=ls())
study<-"mad.schools"

# Add you work dir here-
work.dir=("C:/GitHub/Moorea-minimum-approach")

em.export=paste(work.dir,"Data/EM export",sep="/")
em.check=paste(work.dir,"Data/EM to check",sep="/")
tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
summaries=paste(work.dir,"Data/Summaries",sep="/")
plots=paste(work.dir,"Plots",sep="/")


# Read in the data----
# Change dates for the new data
setwd(tidy.data)
dir()

length.factors<-read.csv("mad.schools.length.factors.csv") #TJL-Need to add this in
length<-read.csv("mad.schools.length.csv")%>%
  mutate(sample=paste(OpCode,Period,sep="."))

unique(length$OpCode)
unique(length.factors$OpCode)
unique(length$Comment.1)

# Import from Life_history----
gs_ls()
Life_history <- gs_title("Moorea Species List_170406")#register a sheet
master<-Life_history%>%
  gs_read_csv(ws = "Sheet1")%>%
  mutate(Max=as.numeric(Max))%>%
  mutate(Max_length=Max*10)%>%
  mutate(Min_length=0)%>%
  dplyr::rename(diet=`Diet 7cl2`)%>%
  select(Genus_species,Family,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length)%>%glimpse()

names(master)
unique(master$CommLoc)
unique(master$CommReg)
unique(master$CommReg)
unique(master$diet)


# Write LONG data from length ----
setwd(tidy.data)
length.expanded<-length%>%
  select(OpCode,Period,Family,Genus_species,Length,Number,Range,Incoming,final.mad,School)%>% #Next line makes a complete set of the following factors - filling in blanks with NA - the nesting() only fills in variable where it already exists for species
  tidyr::complete(nesting(OpCode,Period),nesting(Family,Genus_species))%>% # Brooke has fixed this
  ungroup()%>%
  mutate(Sample=paste(OpCode,Period,sep="."))%>%
  mutate(Number=ifelse(is.na(Number),0,Number))%>%
  left_join(master,by=c("Family","Genus_species"))%>%
  glimpse()

unique(length.expanded$OpCode)
unique(length.expanded$Period)

# factors from lab sheet
name<-"length.expanded.factors"
length.expanded.factors<-length.factors%>%
  inner_join(length.expanded, by="OpCode") #using inner_join here as we don't want zero/NA if there is no length data in a sample
  head(length.expanded.factors)
  
unique(length.expanded.factors$Region)
unique(length.expanded.factors$OpCode)

write.csv(length.expanded.factors, file=paste(study,name,".csv",sep = "."), row.names=FALSE)

# Bring in the enviromental data----
setwd(tidy.data)
dir()

dov.fine.habitat<-read.csv("mad.schools__habitat.output.csv")%>%
  glimpse()

unique(dov.fine.habitat$Sample)

# Bring in period time ----
setwd(em.export)
dir()
period.times<-read.delim("period.lengths.TXT", header=T,skip=4,stringsAsFactors = FALSE)%>%
  setNames(tolower(names(.)))%>%
  select(period,period.length..mins.,opcode)%>%
  dplyr::rename(PeriodLength=period.length..mins.)%>%
  mutate(Sample=paste(opcode,period,sep="."))%>%
  glimpse()

unique(period.times$opcode)
unique(period.times$period)
unique(period.times$Sample)


## Bring in Fish Feeding and Poaching levels
feeding <- gs_title("170503_Moorea MPA")%>%
  gs_read_csv(ws = "Sheet2")%>%
  select(Location, Size, Feeding, DayPoaching, NightPoaching)

# Write final data----
setwd(tidy.data)
dir()

head(length.expanded.factors)
head(dov.fine.habitat)

data<-length.expanded.factors%>%
  inner_join(dov.fine.habitat, by="Sample")%>%
  left_join(period.times, by="Sample")%>%
  left_join(feeding, by="Location")%>%
  glimpse()

# Data corrections - that should be investigated and fixed where possible----
final.data<-data%>%
  select(final.mad,Depth,Location,Status,Site,Region,Reef.Lagoon,Genus_species,Family,Length,Number,Sample,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef, PeriodLength, Feeding, Size, DayPoaching,NightPoaching,School)%>%
  mutate(Region=as.character(Region))%>%
  mutate(Region=ifelse(Region=="East","Northeast",ifelse(Region=="North","Northeast",Region)))%>%
  mutate(Region=as.factor(Region))%>%
  filter(!is.na(Genus_species))%>%# there is 1
  #filter(!is.na(diet))%>%# there are 2
  filter(!is.na(TargetLoc))#%>%)# there are 2
  # filter(!is.na(Max_length))%>%# there is 1
  # filter(!is.na(Resilience)) %>%#there are quite a few
  # filter(!is.na(Commercial))%>%#there are quite a few

tests<-data%>%
  #filter(is.na(Family))%>%
  filter(is.na(diet))%>%
  filter(is.na(TargetLoc))%>%
  filter(Number>0)
  #filter(is.na(Genus_species)) # None BG
  #filter(is.na(Max_length))


write.csv(final.data, file=paste(Sys.Date(),study,"combined.factors.habitat.csv",sep = "_"), row.names=FALSE)


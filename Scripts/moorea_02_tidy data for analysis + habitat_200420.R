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

# Clear memory----
rm(list=ls()) #clear memory

# Study name
study<-"mad.schools"

# Add you working directory here ----
work.dir <- ("~/Git Projects/current/2020-Moorea-minimum-approach") # Use this directory name from now on (MAC)
work.dir <- ("Y:/2020-Moorea-minimum-approach") # Work laptop

# Subdirectories ----
data.dir=paste(work.dir,"Data",sep="/")
tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
em.export <- paste(work.dir, "Data/EM export", sep="/")
plots=paste(work.dir,"Plots",sep="/")

# Read in the data ----
## Change dates for the new data
setwd(tidy.data)
dir()

length.factors<-read.csv(paste(study,"length.factors.csv",sep=".")) #TJL-Need to add this in

length<-read.csv("mad.schools.length.csv")%>%
  mutate(sample=paste(OpCode,Period,sep="."))%>%
  glimpse()

unique(length$OpCode)
unique(length.factors$OpCode)
unique(length$Comment.1)

# Import from Life_history----
url <- ("https://docs.google.com/spreadsheets/d/1ud-Bk7GAVVB90ptH_1DizLhEByRwyJYwacvWpernU3s/edit#gid=956213975")

master <- googlesheets4::read_sheet(url)%>%
  mutate(Max=as.numeric(Max))%>%
  mutate(Max_length=Max*10)%>%
  mutate(Min_length=0)%>%
  dplyr::rename(diet=`Diet 7cl2`)%>%
  dplyr::select(Genus_species,Family,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length)%>%
  mutate(TargetLoc=as.character(TargetLoc))%>%
  mutate(Commercial=as.character(Commercial))%>%
  glimpse()

names(master)
unique(master$CommLoc)
unique(master$CommReg)
unique(master$CommReg)
unique(master$diet)

# For offline ----
setwd(data.dir)
dir()

master <- read.delim("Moorea Species List_170406.txt")%>%
  mutate(Max=as.numeric(Max))%>%
  mutate(Max_length=Max*10)%>%
  mutate(Min_length=0)%>%
  dplyr::rename(diet=Diet.7cl2)%>%
  dplyr::select(Genus_species,Family,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length)%>%
  mutate(TargetLoc=as.character(TargetLoc))%>%
  mutate(Commercial=as.character(Commercial))%>%
  glimpse()

# Write LONG data from length ----
setwd(tidy.data)

length.expanded<-length%>%
  dplyr::select(OpCode,Period,Family,Genus_species,Length,Number,Range,Incoming,final.mad,School)%>% #Next line makes a complete set of the following factors - filling in blanks with NA - the nesting() only fills in variable where it already exists for species
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

write.csv(length.expanded.factors, file=paste(study,name,"csv",sep = "."), row.names=FALSE)

# Bring in the enviromental data----
setwd(tidy.data)
dir()

dov.fine.habitat <- read.csv("mad.schools__habitat.output.csv")%>%
  glimpse()

unique(dov.fine.habitat$Sample)

# Bring in period time ----
setwd(em.export)
dir()

period.times <- read.delim("period.lengths.TXT", header=T,skip=4,stringsAsFactors = FALSE)%>%
  setNames(tolower(names(.)))%>%
  select(period,period.length..mins.,opcode)%>%
  dplyr::rename(PeriodLength=period.length..mins.)%>%
  mutate(Sample=paste(opcode,period,sep="."))%>%
  glimpse()

unique(period.times$opcode)
unique(period.times$period)
unique(period.times$Sample)

## Bring in Fish Feeding and Poaching levels
# url <- ("")
# 
# feeding <- googlesheets4::read_sheet(url)%>%
#   dplyr::select(Location, Size, Feeding, DayPoaching, NightPoaching)%>%
#   glimpse()

# feeding <- gs_title("170503_Moorea MPA")%>%
#   gs_read_csv(ws = "Sheet2")%>%
#   select(Location, Size, Feeding, DayPoaching, NightPoaching)

# Write final data----
setwd(tidy.data)
dir()

head(length.expanded.factors)
head(dov.fine.habitat)

data <- length.expanded.factors%>%
  inner_join(dov.fine.habitat, by="Sample")%>%
  left_join(period.times, by="Sample")%>%
  #left_join(feeding, by="Location")%>%
  glimpse()

# Data corrections - that should be investigated and fixed where possible----
final.data <- data%>%
  dplyr::select(final.mad,Depth,Location,Status,Site,Region,Reef.Lagoon,Genus_species,Family,Length,Number,Sample,diet,TargetLoc,Commercial,Resilience,Max_length,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef, PeriodLength, School)%>% #Feeding, Size, DayPoaching,NightPoaching,CommLoc,CommReg,Ciguatera,
  mutate(Region=as.character(Region))%>%
  mutate(Region=ifelse(Region=="East","Northeast",ifelse(Region=="North","Northeast",Region)))%>%
  mutate(Region=as.factor(Region))%>%
  filter(!is.na(Genus_species))%>%# there is 1
  #filter(!is.na(diet))%>%# there are 2
  filter(!is.na(TargetLoc))#%>%)# there are 2
  # filter(!is.na(Max_length))%>%# there is 1
  # filter(!is.na(Resilience)) %>%#there are quite a few
  # filter(!is.na(Commercial))%>%#there are quite a few

tests <- data%>%
  #filter(is.na(Family))%>%
  filter(is.na(diet))%>%
  filter(is.na(TargetLoc))%>%
  filter(Number>0)
  #filter(is.na(Genus_species)) # None BG
  #filter(is.na(Max_length))

write.csv(final.data, file=paste(Sys.Date(),study,"combined.factors.habitat.csv",sep = "_"), row.names=FALSE)

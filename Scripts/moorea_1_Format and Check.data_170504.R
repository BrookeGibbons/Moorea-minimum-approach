# Libraries required
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(googlesheets)
library(stringr)
library(googlesheets4)

# Set directories----
rm(list=ls()) #clear memory
study<-"mad.schools"

# Functions----
gsr <- function(Source, Search, Replace) { 
  if (length(Search) != length(Replace))     stop("Search and Replace Must Have Equal Number of Items\n") 
  Changed <- as.character(Source) 
  for (i in 1:length(Search)) 
  { 
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
    Changed <- replace(Changed, Changed == Search[i], Replace[i])   } 
  cat("\n")    
  Changed 
}



# Add you work dir here-
work.dir=("C:/GitHub/Moorea-minimum-approach")
# work.dir=("Y:/Moorea-minimum-approach")

em.export=paste(work.dir,"Data/EM export",sep="/")
em.check=paste(work.dir,"Data/EM to check",sep="/")
tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
plots=paste(work.dir,"Plots",sep="/")

# Read in data files----
setwd(em.export)
dir()

# Bring in Ellen outside sites-
length.outside.ellen<-read.delim("outside sites_Ellen.TXT",skip = 4, header=T, stringsAsFactors = FALSE,strip.white = TRUE,na.strings = c("", " "))%>%
  mutate(Period=as.numeric(Period))%>%
  mutate(MAD=as.numeric(MAD))%>%
  mutate(Length=Length..mm.)%>%
  mutate(Range=Range..mm.)%>%
  mutate(Genus = ifelse(is.na(Genus), Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
  #dplyr::rename(TapeReader=Tape.Reader)%>%
  mutate(Comment=paste(Comment,Comment.1,sep=""))%>%
  mutate(Comment=str_replace_all(.$Comment,"NA",""))

names(length.outside.ellen)

# Check length
head(length.outside.ellen,2)
unique(length.outside.ellen$Incoming)
unique(length.outside.ellen$MAD)
unique(length.outside.ellen$Period)

unique(length.outside.ellen$Comment)
unique(length.outside.ellen$Comment.1)

# Bring in Brooke outside sites-
dir()
length.outside.brooke<-read.delim("outside sites_Brooke.TXT",skip = 4, header=T, stringsAsFactors = FALSE,strip.white = TRUE,na.strings = c("", " "))%>%
  mutate(Period=as.numeric(Period))%>%
  mutate(MAD=as.numeric(MAD))%>%
  mutate(Length=Length..mm.)%>%
  mutate(Range=Range..mm.)%>%
  mutate(Genus = ifelse(is.na(Genus), Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
  #dplyr::rename(TapeReader=Tape.Reader)%>%
  mutate(Comment=paste(Comment,Comment.1,sep=""))%>%
  mutate(Comment=str_replace_all(.$Comment,"NA",""))

# Check length
head(length.outside.brooke,2)
str(length.outside.brooke)
unique(length.outside.brooke$Incoming)
unique(length.outside.brooke$MAD)
unique(length.outside.brooke$OpCode)

unique(length.outside.brooke$Comment)
unique(length.outside.brooke$Comment.1)

# Bring in Brooke inside sites-
setwd(em.export)
dir()
length.inside.brooke<-read.delim("inside sites.TXT",skip = 4, header=T, stringsAsFactors = FALSE,strip.white = TRUE,na.strings = c("", " "))%>%
  mutate(Period=as.numeric(Period))%>%
  mutate(MAD=as.numeric(MAD))%>%
  mutate(Length=Length..mm.)%>%
  mutate(Range=Range..mm.)%>%
  mutate(Genus = ifelse(is.na(Genus), Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
  mutate(Comment=paste(Comment,Comment.1,sep=""))%>%
  mutate(Comment=str_replace_all(.$Comment,"NA",""))

# Check length
head(length.inside.brooke,2)
str(length.inside.brooke)
unique(length.inside.brooke$Incoming)
unique(length.inside.brooke$MAD)
unique(length.inside.brooke$Period)
unique(length.inside.brooke$Comment)
unique(length.inside.brooke$Comment.1)
names(length.inside.brooke)

test<-length.inside.brooke%>%filter(Comment=="Y")

# Combine length files
length<-bind_rows(length.outside.ellen,length.inside.brooke,length.outside.brooke)%>%
  mutate(School=str_replace_all(.$Comment,c("school"="School","multispecies "="","sames species "="","same species"=""," Multispecies"="","Same Species"="","multispecies"="","Mulitspecies"="","Multipspecies"="","1"="","2"="","3"="","4"="","5"="","6"="","7"="","8"="","9"="","[^[:alnum:]]"="","School"="School.")))

#unique(length$OpCode)
unique(length$Comment)
unique(length$School)%>%sort()
str(length)

# Make Factors to merge back in after summarises -----
# gs_ls()
# sheet <- gs_title("MEG_Labsheets")#register a sheet
# 
# factors<-sheet%>%
#   gs_read_csv(ws = "2016 stereo-DOV OpCode track")%>%
#   select(CampaignID,Sample,Depth,Location,Status,Site,Observer,Region,Reef.Lagoon)%>%
#   distinct(CampaignID,Sample,Depth,Location,Status,Site,Observer,Region,Reef.Lagoon)%>%
#   rename(OpCode=Sample)%>%
#   glimpse()

# CSV version for offline - BG 09/08/2019



# Use  Observer to indicate if  Length were possible to collect - giving us our true zeros 
length.factors<-factors%>%
  filter(!is.na(Observer))%>%
  filter(!Observer %in% c("NA","N/A"))
  
# BASIC Checks----
setwd(em.check)
head(length)

# Check if we have 3d points (Number) in addition to length----
three.d.points<-length%>%
  filter(is.na(Length))%>%
  filter(!is.na(Number))%>%
  glimpse()#if there are any records here we have 3d points - yes we do


# Check if we have schools associated with single length measures----
schools<-filter(length,Number>1)%>%
  glimpse() # nope


#finding sync points that are not fish----
# normally Lengths without a Number
sync.points<-filter(length,is.na(Number)) # none

#Standardise for RANGE and Error for Length----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundnance calculated from it, will be restricted by range
setwd(em.check)
summary(length$Range)
out.of.range<-filter(length,Range>10000);head(out.of.range)
length <- filter(length,Range < 10000)

write.csv(out.of.range,file=paste(study,"out.of.range.csv",sep = "_"), row.names=FALSE)

# Check on the BIG fish length data----
fish.greater.than.1.meter<-filter(length,Length>1000)%>%
  glimpse()#All sharks, 

write.csv(fish.greater.than.1.meter,file=paste( study,"fish.greater.than.1.meter.csv",sep = "_"), row.names=FALSE)

# Plot to visualise length data----
setwd(em.check)
dir()
gg.check.length<-ggplot(data=length, aes(as.numeric(Length))) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) 
gg.check.length
ggsave(gg.check.length,file=paste( study,"gg.check.length.png",sep = "_"),width = 8, height = 8,units = "in")
dir()

# Plot to visualise range data----
gg.check.range<-ggplot(data=length, aes(as.numeric(Range))) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="green", 
                 alpha = .2) 
gg.check.range
ggsave(gg.check.range,file=paste( study,"gg.check.range.png",sep = "_"),width = 8, height = 8,units = "in")

# Plot to visualise length/range data----
gg.check.range.vs.length<-ggplot(data=length, aes(as.numeric(Length),as.numeric(Range))) + 
  geom_point()+
  geom_smooth()
gg.check.range.vs.length
ggsave(gg.check.range.vs.length,file=paste( study,"gg.check.range.vs.length.png",sep = "_"),width = 8, height = 8,units = "in")

# SERIOUS data checking to compare taxa and min/max lengths----
# Moorea life history ----
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


# # Update names of species that may have changed----
# Check for taxa.not.match----
setwd(em.check)
dir()

x<-"length.taxa.not.match.life.history" #quick look at taxa in length that don't match
head(length)
length.taxa.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  select(Genus_species)%>%
  distinct(Genus_species)
  
head(length.taxa.not.match,39)
write.csv(length.taxa.not.match,file=paste( study,x,".csv",sep = "_"), row.names=FALSE)


x<-"length.taxa.by.opcode.not.match.life.history" #a more useful list of taxa in lenght that do not match
length.taxa.and.opcode.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  distinct(Genus_species,OpCode)%>% 
  select(Genus_species,OpCode)
head(length.taxa.and.opcode.not.match)
write.csv(length.taxa.and.opcode.not.match,file=paste( study,x,".csv",sep = "_"), row.names=FALSE)


### SERIOUS Check for Min Max Length compared to Master list----
setwd(em.check)
x<-"out.of.bounds.length.vs.life.history"
# Before running the length check we must NOT have any non-matching taxa - so first remove these from
keep<-select(master,Genus_species)

length10<-length%>%
  semi_join(keep,by="Genus_species")%>%
  filter(!is.na(Length))

# Make a vector of names to compare against
Genus_species.Vec<- sort(unique(length10$Genus_species)) #Need to order by name

# Make a dummy list for checking
wrong.length=vector('list',length=length(Genus_species.Vec))
names(wrong.length)=Genus_species.Vec
Matching.Species.Table=filter(master,Genus_species%in%Genus_species.Vec)
Matching.Species.Table=Matching.Species.Table[order(Matching.Species.Table$Genus_species),]
head(Matching.Species.Table)
Min=Matching.Species.Table$Min_length #Vector of Min lengths
Max=Matching.Species.Table$Max_length #Vector of Max lengths
names(Min)=names(Max)=Matching.Species.Table$Genus_species #Add names to the Min and Max - very important vectors are in order


# Run the loop to check the length data---
test=NA
for(i in 1:length(Genus_species.Vec))  
{
  
  Data=subset(length10,Genus_species==Genus_species.Vec[i])
  Data=subset(Data,!is.na(Length))
  test=which(Data$Length  <Min[i])
  test=c(test,which(Data$Length  >Max[i]))
  test=Data[test,]
  wrong.length[[i]]=test
}
wrong.length1<-do.call(rbind,wrong.length)

# Merge with Matching.Species.Table
wrong.length.taxa<-wrong.length1%>%
  inner_join(Matching.Species.Table,by="Genus_species")%>%
  select(OpCode, Genus_species,Length,Min_length,Max_length)%>%
  mutate(Difference=Length-Max_length)%>%
  arrange(desc(Difference))%>%
  glimpse()

write.csv(wrong.length.taxa,file=paste( study,x,".csv",sep = "_"), row.names=FALSE)

# WRITE FINAL checked data----
setwd(tidy.data)
dir()

x<-"length.factors"
write.csv(length.factors, file=paste( study,x,"csv",sep = "."), row.names=FALSE)

x<-"length"
length.final.with.mad<-
  master%>%
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%#this will drop any speices not in the Master list - that came from the Life_history sheet
  mutate(final.mad=Range-(MAD*1000))%>%
  mutate(final.mad=ifelse(is.na(final.mad),Range,final.mad))%>%
  glimpse()

setwd(tidy.data)
write.csv(length.final.with.mad, file=paste(study,x,"csv",sep = "."), row.names=FALSE)
unique(length.final.with.mad$OpCode)

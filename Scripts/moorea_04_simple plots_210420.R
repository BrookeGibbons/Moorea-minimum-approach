# Reading and formatting data
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

# Plotting
library(ggplot2)

# Clear memory----
rm(list=ls()) #clear memory

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

# Study name
study<-"mad.schools"

# Add you working directory here ----
work.dir <- ("~/Git Projects/current/2020-Moorea-minimum-approach") # Use this directory name from now on (MAC)
work.dir <- ("Y:/2020-Moorea-minimum-approach") # Work laptop

tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
plots=paste(work.dir,"Plots",sep="/")

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

# Bring in length data ----
setwd(tidy.data)
dir()

raw.data <- read.csv("2020-04-20_mad.schools_combined.factors.habitat.csv")%>%
  filter(Reef.Lagoon == "Lagoon")%>% # I am only looking at Lagoon sites
  filter(Location %in% c("Pihaena", "Tiahura", "Tetaiuo"))%>% # only in these three reserves
  glimpse()

# Make factors and sample list ----
samples <- raw.data%>%
  distinct(Sample)

factors <- raw.data%>%
  distinct(Sample,Depth,Location,Status,Site,Reef.Lagoon,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,PeriodLength)

# Make TA and SR for gams ----
ta.sr <- raw.data%>%
  group_by(Genus_species,Sample) %>%
  dplyr::summarise(Abundance = sum(Number))%>%
  spread(Genus_species, Abundance, fill = 0)%>%
  mutate(Total.Abundance = rowSums(.[,2:(ncol(.))],na.rm = TRUE ))#Add in Totals

Presence.Absence <- ta.sr[,2:(ncol(ta.sr))-1]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
total.abundance.species.richness <- ta.sr%>%
  mutate(Species.Richness = rowSums(Presence.Absence,na.rm = TRUE))%>%
  inner_join(factors, by="Sample")%>%
  dplyr::select(Depth,Location,Status,Site,Sample,PeriodLength,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,Total.Abundance,Species.Richness)%>%
  gather(key=Metric, value = Response, (match("reef",names(.))+1):ncol(.))

## Make size class data ----
size.class <- raw.data%>%
  dplyr::rename(Response = Number)%>%
  filter(!is.na(Length))%>% 
  mutate(Indicator = ifelse(Length<=(Max_length/3),"small","large"))%>%
  dplyr::select(-c(Depth,Location,Status,Site,Region,Reef.Lagoon,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,PeriodLength,diet,TargetLoc,Commercial,Max_length,final.mad,School))%>%
  left_join(samples,.,by="Sample")%>%
  tidyr::complete(Sample,tidyr::nesting(Family,Genus_species),Indicator)%>%
  left_join(factors,., by = "Sample")%>%
  replace_na(list(Response = 0))%>%
  filter(Reef.Lagoon == "Lagoon")%>% # I am only looking at Lagoon sites
  filter(Location%in%c("Pihaena","Tiahura","Tetaiuo"))%>% # only in these three reserves
  left_join(master, by = c("Family", "Genus_species"))%>%
  mutate(Metric=Indicator)%>%
  dplyr::select(-c(Resilience.x,Resilience.y,diet,CommLoc,CommReg,Ciguatera,Commercial,Max_length))%>%
  glimpse()

names(size.class)
unique(size.class$Sample)

# Abundance by size and TargetLoc ----
size.class.target <- size.class%>%
  filter(!is.na(TargetLoc))%>%
  dplyr::group_by(Sample,Indicator,TargetLoc)%>%
  dplyr::summarise(Response=sum(Response))%>%
  ungroup()%>%
  inner_join(factors, by="Sample")%>%
  mutate(Metric=paste("Abundance.TargetLoc",TargetLoc,Indicator,sep="."))%>%
  dplyr::select(-c(TargetLoc,Indicator,Reef.Lagoon))%>%
  glimpse()

# Combine data frames ----
abundance.dat <- bind_rows(total.abundance.species.richness,
                                           size.class,
                                           size.class.target)

# Plots ----
# Total abundance ----
ta.plot<-ggplot(filter(abundance.dat, Metric%in%c("Total.Abundance")), aes(x = Status,y=Response, fill = Status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  #annotation_custom(fish.img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  xlab("")+
  # ylab("Average abundance per XXXX \n(+/- SE)")+
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
  # Theme1+
  ggtitle("Total Abundance by Status")
ta.plot

ta.location.plot<-ggplot(filter(abundance.dat, Metric%in%c("Total.Abundance")), aes(x = Location,y=Response, fill = Status, group=Status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black",position = "dodge") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, position = position_dodge(width = 0.9)) +

  xlab("")+
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
  ggtitle("Total Abundance by Status/Location")

ta.location.plot

# Species richness
sr.plot<-ggplot(filter(abundance.dat, Metric%in%c("Species.Richness")), aes(x = Status,y=Response, fill = Status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  #annotation_custom(fish.img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  xlab("")+
  # ylab("Average abundance per XXXX \n(+/- SE)")+
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
  # Theme1+
  ggtitle("Species Richness by Status")
sr.plot

sr.location.plot<-ggplot(filter(abundance.dat, Metric%in%c("Species.Richness")), aes(x = Location,y=Response, fill = Status, group=Status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black",position = "dodge") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, position = position_dodge(width = 0.9)) +
  
  xlab("")+
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
  ggtitle("Species Richness by Status/Location")

sr.location.plot

# Size class
sc.plot<-ggplot(filter(abundance.dat, Metric%in%c("small","large")), aes(x = Metric,y=Response, fill = Status, group=Status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black",position = "dodge") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, position = position_dodge(width = 0.9)) +
  
  xlab("")+
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
  ggtitle("Size class by status")

sc.plot

sc.plot<-ggplot(filter(abundance.dat, Metric%in%c("small","large")), aes(x = Metric,y=Response, fill = Status, group=Status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black",position = "dodge") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, position = position_dodge(width = 0.9)) +
  
  xlab("")+
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
  ggtitle("Size class by status")+
  facet_wrap( ~ Location, ncol=2)

sc.plot





# librarys----
library(tidyr)
library(dplyr)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(googlesheets4)
library(RCurl) #needed to download data from GitHub

rm(list=ls())

# install package----
#devtools::install_github("beckyfisher/FSSgam_package", force =TRUE) #run once
library(FSSgam)

# Study name
study<-"mad.schools"

# Add you working directory here ----
work.dir <- ("~/Git Projects/current/2020-Moorea-minimum-approach") # Use this directory name from now on (MAC)
work.dir <- ("Y:/2020-Moorea-minimum-approach") # Work laptop

tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
summaries=paste(work.dir,"Data/Summaries",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
plots=paste(work.dir,"Plots",sep="/")
model.out=paste(work.dir,"ModelOut",sep="/")

# Moorea life history ----
# url <- ("https://docs.google.com/spreadsheets/d/1ud-Bk7GAVVB90ptH_1DizLhEByRwyJYwacvWpernU3s/edit#gid=956213975")
# 
# master <- googlesheets4::read_sheet(url)%>%
#   mutate(Max=as.numeric(Max))%>%
#   mutate(Max_length=Max*10)%>%
#   mutate(Min_length=0)%>%
#   dplyr::rename(diet=`Diet 7cl2`)%>%
#   dplyr::select(Genus_species,Family,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length)%>%
#   mutate(TargetLoc=as.character(TargetLoc))%>%
#   mutate(Commercial=as.character(Commercial))%>%
#   glimpse()

# When offline ----
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

# Number and size of schools ----
schools.summary <- raw.data%>%
  dplyr::filter(!is.na(School))%>%
  dplyr::group_by(School,Sample) %>%
  dplyr::summarise(Abundance = sum(Number))%>%
  spread(School,Abundance, fill = 0)%>%
  mutate(School.Total=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) # Add in Totals

Presence.Absence <- schools.summary[,2:(ncol(schools.summary))-1]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}

number.of.schools<-schools.summary%>%
  mutate(Number.of.schools = rowSums(Presence.Absence,na.rm = TRUE))%>%
  inner_join(factors, by="Sample")%>%
  dplyr::select(Depth,Location,Status,Site,Sample,PeriodLength,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,School.Total,Number.of.schools)%>%
  gather(key=Metric, value = Response, (match("reef",names(.))+1):ncol(.))

# Combine datasets together ----
combined.abundance.dataframes <- bind_rows(total.abundance.species.richness,
                                         size.class.target,
                                         number.of.schools)

unique(combined.abundance.dataframes$Site)


# Mad data----
mad.data<-raw.data%>%
  filter(final.mad>0)%>% # NEED TO CREATE SCHOOLS BEFORE FILTERING THESE OUT
  filter(!is.na(Length))%>%
  filter(Length>80)%>%
  #filter(Length<300)%>%
  mutate(Indicator="Mad")%>%
  mutate(TargetLoc=as.factor(TargetLoc))%>%
  dplyr::rename(response=final.mad)%>%
  select(-c(Reef.Lagoon))%>%
  glimpse()

mad.schools<-mad.data%>%
  filter(grepl("School",School)) # filter to only those in schools

mad.school.size<-mad.schools%>%
  group_by(Sample,School)%>%
  filter(!School%in%c(""))%>%
  dplyr::summarise(school.size=sum(Number))

mad.individuals<-mad.data%>%
  filter(!grepl("School",School)) # filter to only individuals

mad.individuals<-mad.individuals%>%
  mutate(School=paste("School",1:nrow(mad.individuals), sep=".")) # make a unique id for each individual

mad<-bind_rows(mad.schools, mad.individuals)

# BG 04/09/19
# trying to figure out if we have schools of mixed target level

test.schools<-mad%>%
  mutate(TargetLoc=as.numeric(TargetLoc))%>%
  group_by(Sample,School)%>%
  summarise(number=length(unique(TargetLoc)),average=mean(TargetLoc))

schools.with.mutliple.targetlocs<-test.schools%>%
  filter(number>1)%>%
  distinct(Sample,School)

# Need to ask Tim about this
# Out of 3894 schools 32 had two levels and 1 had all three
# Ideas
# 1st idea - remove the 33 mixed groups
# Then I only have three groups

# 2nd idea - rename 1 and 2 as both targeted and 0 as non-target
# will then have less groups to remove and only 2 gams yay

# 3rd idea - have none, low, high and mixed

mad.final<-mad%>%
  anti_join(schools.with.mutliple.targetlocs)%>%
  mutate(Metric=ifelse(TargetLoc%in%c(0),"non-target",ifelse(TargetLoc%in%c(1),"mod-target","high-target")))
# Only removes 266 fish (5179-4913)


# mad.targetloc0<-mad.final%>%
#   filter(TargetLoc%in%c(0))%>%
#   mutate(Metric="non-target")
# 
# mad.targetloc1<-mad.final%>%
#   filter(TargetLoc%in%c(1))%>%
#   mutate(Metric="mod-target")
# 
# mad.targetloc2<-mad.final%>%
#   filter(TargetLoc%in%c(2))%>%
#   mutate(Metric="high-target")

#combined.mad.data<-bind_rows(mad.targetloc0,mad.targetloc1,mad.targetloc2)

# Need to make a row for each school
# with min, mean and max length
# and school size
mad.sum<-mad.final%>%
  group_by(Sample,School,Metric)%>% # need to keep target loc in
  dplyr::summarise(response=min(response),min.length=min(Length),mean.length=mean(Length),max.length=max(Length))%>%
  ungroup()%>%
  left_join(mad.school.size)%>%
  replace_na(list(school.size=1))%>%
  left_join(factors)%>%
  #mutate(Metric=as.factor(Metric),School=as.factor(School))%>%
  as.data.frame()%>%
  glimpse()

# Set predictor variables ----
pred.vars=c("Depth","PeriodLength","sd.relief","rock","hard.corals","sand","reef","macroalgae") 

# Removed 
# reef - correlated with sand
# macroalgae - too few
# Depth didnt used to be in but maybe keep it??? BG

dat<-combined.abundance.dataframes

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars]),2)

# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# Review of individual predictors - we have to make sure they have an even distribution---
# If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3
# Decided that X4mm, X2mm, X1mm and X500um needed a sqrt transformation
# Decided Depth, x63um, InPreds and BioTurb were not informative variables. 

dat<-combined.abundance.dataframes%>%
  dplyr::rename(response=Response)

glimpse(dat)

dat<-as.data.frame(dat)

## ABUNDANCE ----
# Re-set the predictors for modeling----
pred.vars=c("rock","sd.relief","hard.corals","sand") 
pred.vars=c("reef","sd.relief","mean.relief") 

# Set name of models ----
name<-"abundance.output"

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Metric==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     

# Run the full subset model selection----
setwd(model.out) # Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat
factor.vars=c("Status")# Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals,k=3,bs='cr')+ s(Site,Location,bs="re"),
             family=tw(),  offset=PeriodLength, data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               #linear.vars="Distance",
                               k=3,
                               null.terms="s(Site,Location,bs='re')")
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance ----
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,8), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)



### Targetloc as factor ----
dat<-size.class%>%
  dplyr::rename(response=Response)

glimpse(dat)

dat<-as.data.frame(dat)

# Set name of models ----
name<-"abundance.target"

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Metric==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     

# Run the full subset model selection----
setwd(model.out) # Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat
factor.vars=c("Status","TargetLoc")# Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals,k=3,bs='cr')+ s(Site,Location,bs="re"),
             family=tw(),  offset=PeriodLength, data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               #linear.vars="Distance",
                               k=3,
                               null.terms="s(Site,Location,bs='re')")
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance ----
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,8), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)
# A simple function for full subsets multiple regression in ecology with R
# R. Fisher, S.K. Wilson, S.M. Sin, A.C. Lee & T.J. Langlois

rm(list=ls())
study<-"mad.schools"
name<-"mad.schools"

# Source functions----
library(RCurl)
require(mgcv)
require(MuMIn)
require(doParallel)
require(plyr)
library(dplyr)
library(tidyr)
library(googlesheets)
library(gamm4)
library(MuMIn)
library(doSNOW)

function_full_subsets_gam <- getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/function_full_subsets_gam_v1.11.R?token=AOSO6tZYAozKTAZ1Kt-aqlQIsiKuxONjks5ZZCtiwA%3D%3D", ssl.verifypeer = FALSE)
eval(parse(text = function_full_subsets_gam))

function_check_correlations <- getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/function_check_correlations_v1.00.R?token=AOSO6uxF2ON3UFyXj10uqm_N_94ZSEM3ks5ZZCyCwA%3D%3D", ssl.verifypeer = FALSE)
eval(parse(text = function_check_correlations))

# load example data
dat <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study1_dataset.csv?token=AcAXe95Jzzp5XRZ1SJZBNm2d8fxXaJUOks5ZaBGbwA%3D%3D"))

# Bring in my data ----
work.dir=("C:/GitHub/Moorea-minimum-approach") # Windows
#work.dir=("~/Git Projects/Moorea-minimum-approach") # Mac

em.export=paste(work.dir,"Data/EM export",sep="/")
em.check=paste(work.dir,"Data/EM to check",sep="/")
tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
summaries=paste(work.dir,"Data/Summaries",sep="/")
plots=paste(work.dir,"Plots",sep="/")
model.out=paste(work.dir,"ModelOut",sep="/")

# Moorea life history ----
master <- gs_title("Moorea Species List_170406")%>%
  gs_read_csv(ws = "Sheet1")%>%
  mutate(Max=as.numeric(Max))%>%
  mutate(Max_length=Max*10)%>%
  mutate(Min_length=0)%>%
  dplyr::rename(diet=`Diet 7cl2`)%>%
  select(Genus_species,Family,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length)%>%
  glimpse()

# Bring in length data ----
setwd(tidy.data)
dir()

brooke.dat<-read.csv("2019-02-12_mad.schools_combined.factors.habitat.csv")%>%
  filter(Reef.Lagoon=="Lagoon")%>% # I am only looking at Lagoon sites
  filter(Location%in%c("Pihaena","Tiahura","Tetaiuo"))%>% # only in these three reserves
  glimpse()

# Make factors and sample list ----
dat.sample<-brooke.dat%>%
  distinct(Sample)
dat.factor<-brooke.dat%>%
  distinct(Sample,Depth,Location,Status,Site,Reef.Lagoon,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,PeriodLength)

# Make TA and SR for gams ----
ta.sr<-brooke.dat%>%
  group_by(Genus_species,Sample) %>%
  dplyr::summarise(Abundance = sum(Number))%>%
  spread(Genus_species,Abundance, fill = 0)%>%
  mutate(Total.Abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))#Add in Totals

Presence.Absence <- ta.sr[,2:(ncol(ta.sr))-1]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
dat.1<-ta.sr%>%
  mutate(Species.Richness = rowSums(Presence.Absence,na.rm = TRUE))%>%
  inner_join(dat.factor, by="Sample")%>%
  select(Depth,Location,Status,Site,Sample,PeriodLength,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,Total.Abundance,Species.Richness)%>%
  gather(key=Metric, value = Response, (match("reef",names(.))+1):ncol(.))

## Make size class data ----
dat.size.class<-brooke.dat%>%
  dplyr::rename(Response=Number)%>%
  filter(!is.na(Length))%>%
  mutate(Indicator=ifelse(Length<=(Max_length/3),"small","large"))%>%
  dplyr::select(-c(Depth,Location,Status,Site,Region,Reef.Lagoon,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,PeriodLength,Feeding,DayPoaching,NightPoaching,Size,diet,CommLoc,CommReg,TargetLoc,Commercial,Ciguatera,Resilience,Max_length,final.mad,School))%>%
  left_join(dat.sample,.,by="Sample")%>%
  tidyr::complete(Sample,tidyr::nesting(Family,Genus_species),Indicator)%>%
  left_join(dat.factor,., by = "Sample")%>%
  replace_na(list(Response=0))%>%
  filter(Reef.Lagoon=="Lagoon")%>% # I am only looking at Lagoon sites
  filter(Location%in%c("Pihaena","Tiahura","Tetaiuo"))%>% # only in these three reserves
  left_join(master, by = c("Family", "Genus_species"))

names(dat.size.class)
unique(dat.size.class$Sample)

dat.2<-dat.size.class%>%
  filter(Genus_species%in%(c("Ctenochaetus striatus","Chlorurus sordidus")))%>%
  dplyr::group_by(Sample,Genus_species,Indicator)%>%
  dplyr::summarise(Response=sum(Response))%>%
  ungroup()%>%
  inner_join(dat.factor, by="Sample")%>%
  mutate(Metric=paste("Abundance",Genus_species,Indicator,sep="."))%>%
  select(-c(Genus_species,Indicator,Reef.Lagoon))

# Abundance by size and TargetLoc ----
dat.3<-dat.size.class%>%
  filter(!is.na(TargetLoc))%>%
  group_by(Sample,Indicator,TargetLoc)%>%
  dplyr::summarise(Response=sum(Response))%>%
  ungroup()%>%
  inner_join(dat.factor, by="Sample")%>%
  mutate(Metric=paste("Abundance.TargetLoc",TargetLoc,Indicator,sep="."))%>%
  select(-c(TargetLoc,Indicator,Reef.Lagoon))%>%
  glimpse()

# Number and size of schools ----
schools<-brooke.dat%>%
  group_by(School,Sample) %>%
  dplyr::summarise(Abundance = sum(Number))%>%
  spread(School,Abundance, fill = 0)%>%
  mutate(School.Total=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))#Add in Totals
Presence.Absence <- schools[,2:(ncol(schools))-1]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
dat.4<-schools%>%
  mutate(Number.of.schools = rowSums(Presence.Absence,na.rm = TRUE))%>%
  inner_join(dat.factor, by="Sample")%>%
  select(Depth,Location,Status,Site,Sample,PeriodLength,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,School.Total,Number.of.schools)%>%
  gather(key=Metric, value = Response, (match("reef",names(.))+1):ncol(.))

# Combine datasets together ----
combined<-bind_rows(dat.1,dat.2,dat.3,dat.4)

# Mad data----
dat.mad<-brooke.dat%>%
  filter(final.mad>0)%>%
  filter(!is.na(Length))%>%
  filter(Length>80)%>%
  #filter(Length<300)%>%
  mutate(Indicator="Mad")%>%
  mutate(TargetLoc=as.factor(TargetLoc))%>%
  #mutate(School=ifelse((is.na(School)|School==""),"Individual","School"))%>%
  #mutate(School=as.factor(School))%>%
  dplyr::rename(response=final.mad)%>%
  select(-c(Reef.Lagoon))%>%
  glimpse()


dat.mad.schools<-dat.mad%>%
  filter(grepl("School",School)) # filter to only those in schools

dat.mad.school.size<-dat.mad.schools%>%
  group_by(Sample,School)%>%
  filter(!School%in%c(""))%>%
  dplyr::summarise(school.size=sum(Number))

dat.mad.individuals<-dat.mad%>%
  filter(!grepl("School",School)) # filter to only individuals
dat.mad.individuals<-dat.mad.individuals%>%
  mutate(School=paste("School",1:nrow(dat.mad.individuals),sep=".")) # make a unique id for each individual

dat.mad<-bind_rows(dat.mad.schools,dat.mad.individuals)

dat.mad.targetloc<-dat.mad%>%
  select(Sample,TargetLoc,response,Length,School)%>% 
  group_by(Sample,TargetLoc,School)%>% # need to summarise min mad for these columns, and create length vars
  dplyr::summarise(response=min(response),mean.length=mean(Length),min.length=min(Length),max.length=max(Length))%>%
  ungroup()%>%
  left_join(dat.mad.school.size)%>% # bring in school size for actual schools
  replace_na(list(school.size=1))%>% # make school size 1 for individuals
  mutate(Metric="TargetLoc")%>% # make metric name to feed into gams
  inner_join(dat.factor, by="Sample") # bring in co-vars

dat.mad.species<-dat.mad%>%
  select(Sample,Genus_species,response,Length,School)%>%
  filter(Genus_species%in%c("Ctenochaetus striatus","Chlorurus sordidus"))%>%
  mutate(Metric=Genus_species)%>%
  group_by(Sample,Metric,School)%>% # need to summarise min mad for these columns, and create length vars
  dplyr::summarise(response=min(response),mean.length=mean(Length),min.length=min(Length),max.length=max(Length))%>%
  ungroup()%>%
  left_join(dat.mad.school.size)%>%
  replace_na(list(school.size=1))%>%
  inner_join(dat.factor, by="Sample")

dat.mad.complete<-bind_rows(dat.mad.targetloc,dat.mad.species)

############ Begining of models ----
#### Abundance models -----

# Set the Pred.vars for all models----
dat<-combined%>%
  dplyr::rename(response=Response)
names(dat)

pred.vars=c("mean.relief","sd.relief","hard.corals","rock") 
pred.vars
length(pred.vars)

# Set directory for the model outputs-
setwd(model.out)

unique.vars=unique(as.character(dat$Metric))
unique.vars.use=as.character(c(unique.vars))
unique.vars.use

setwd(model.out)

# Full-sebset models---  
resp.vars=unique.vars.use
factor.vars=c("Status")
use.dat=dat
out.all=list() 
var.imp=list()

str(dat)

name<-"abundance.models"

# for new function
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals, k=3, bs='cr')+s(Site,Location,bs='re'),
             family=tw(), 
             offset=PeriodLength,
             data=use.dat)
  
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            null.terms="s(Site,Location,bs='re')" ,
                            max.models=600,
                            parallel=T)  
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) 
  #Either raw importance score
  #var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.r2.scaled)) 
  #Or importance score weighted by r2...this doesn't exist in Rebecca Fisher's model
  
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


# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(Sys.Date(),name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(Sys.Date(),name,"all.var.imp.csv",sep="_"))
dev.off()


########### MAD models (School as Factor)----
## TargetLoc as factor ----
dat<-dat.mad.targetloc
names(dat)

pred.vars=c("mean.relief","sd.relief","hard.corals","rock","mean.length","min.length","max.length","school.size")

# Set directory for the model outputs-
setwd(model.out)

# Clean up response variables--
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=as.character(c(unique.vars))
unique.vars.use

setwd(model.out)
# Full-sebset models---  
resp.vars=unique.vars.use
factor.vars=c("Status","TargetLoc")
use.dat=dat
out.all=list() 
var.imp=list()
name<-"mad.targetloc.as.fac"

# for new function
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals, k=3, bs='cr')+s(Site,Location,bs='re'),
             family=gaussian(link = "identity"),
             offset=PeriodLength,
             data=use.dat)
  
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            null.terms="s(Site,Location,bs='re')" ,
                            max.models=600,
                            parallel=T)  
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) 
  #Either raw importance score
  #var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.r2.scaled)) 
  #Or importance score weighted by r2...this doesn't exist in Rebecca Fisher's model
  
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


# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(Sys.Date(),name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(Sys.Date(),name,"all.var.imp.csv",sep="_"))
dev.off()

## Species MAD models ----
dat<-dat.mad.species
names(dat)

pred.vars=c("mean.relief","sd.relief","hard.corals","rock","mean.length","min.length","max.length","school.size")

# Set directory for the model outputs-
setwd(model.out)

# Clean up response variables--
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=as.character(c(unique.vars))
unique.vars.use

setwd(model.out)
# Full-sebset models---  
resp.vars=unique.vars.use
factor.vars=c("Status") # ,"School"
use.dat=dat
out.all=list() 
var.imp=list()
name<-"mad.species"

# for new function
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals, k=3, bs='cr')+s(Site,Location,bs='re'),
             family=gaussian(link = "identity"),
             offset=PeriodLength,
             data=use.dat)
  
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            null.terms="s(Site,Location,bs='re')" ,
                            max.models=600,
                            parallel=T)  
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) 
  #Either raw importance score
  #var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.r2.scaled)) 
  #Or importance score weighted by r2...this doesn't exist in Rebecca Fisher's model
  
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


# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(Sys.Date(),name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(Sys.Date(),name,"all.var.imp.csv",sep="_"))
dev.off()

########### MAD models (School as pred var)----
## TargetLoc as factor ----
dat<-dat.mad.targetloc
names(dat)

pred.vars=c("mean.relief","sd.relief","hard.corals","rock","Length","num.o")

# Set directory for the model outputs-
setwd(model.out)

# Clean up response variables--
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=as.character(c(unique.vars))
unique.vars.use

setwd(model.out)
# Full-sebset models---  
resp.vars=unique.vars.use
factor.vars=c("Status","School","TargetLoc")
use.dat=dat
out.all=list() 
var.imp=list()
name<-"mad.targetloc.as.fac"

# for new function
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals, k=3, bs='cr')+s(Site,Location,bs='re'),
             family=gaussian(link = "identity"),
             offset=PeriodLength,
             data=use.dat)
  
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            null.terms="s(Site,Location,bs='re')" ,
                            max.models=600,
                            parallel=T)  
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) 
  #Either raw importance score
  #var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.r2.scaled)) 
  #Or importance score weighted by r2...this doesn't exist in Rebecca Fisher's model
  
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


# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(Sys.Date(),name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(Sys.Date(),name,"all.var.imp.csv",sep="_"))
dev.off()

## Species MAD models ----
dat<-dat.mad.species
names(dat)

pred.vars=c("mean.relief","sd.relief","hard.corals","rock","Length")

# Set directory for the model outputs-
setwd(model.out)

# Clean up response variables--
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=as.character(c(unique.vars))
unique.vars.use

setwd(model.out)
# Full-sebset models---  
resp.vars=unique.vars.use
factor.vars=c("Status","School")
use.dat=dat
out.all=list() 
var.imp=list()
name<-"mad.species"

# for new function
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals, k=3, bs='cr')+s(Site,Location,bs='re'),
             family=gaussian(link = "identity"),
             offset=PeriodLength,
             data=use.dat)
  
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            null.terms="s(Site,Location,bs='re')" ,
                            max.models=600,
                            parallel=T)  
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) 
  #Either raw importance score
  #var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.r2.scaled)) 
  #Or importance score weighted by r2...this doesn't exist in Rebecca Fisher's model
  
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


# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(Sys.Date(),name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(Sys.Date(),name,"all.var.imp.csv",sep="_"))
dev.off()


# Part 2 - custom plot of importance scores----

# Load the importance score dataset produced above
dat.taxa <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_all.var.imp.csv?token=AOSO6ma3MdgxTWJgICEtKgUVUGiZkRW0ks5ZbagowA%3D%3D"))%>%
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()



# Plotting defaults----
library(ggplot2)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# colour ramps-
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"

# Annotations-
dat.taxa.label<-dat.taxa%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="Distance"&resp.var=="BDS","X",ifelse(predictor=="Status"&resp.var=="BDS","X",ifelse(predictor=="sqrt.X500um"&resp.var=="BDS","X",label))))%>%
  mutate(label=ifelse(predictor=="lobster"&resp.var=="BMS","X",label))%>%
  mutate(label=ifelse(predictor=="sqrt.X4mm"&resp.var=="CPN","X",ifelse(predictor=="lobster"&resp.var=="CPN","X",label)))%>%
  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("Distance",
                            "Status",
                            "lobster",
                            "snapper",
                            "fetch",
                            "org",
                            "sqrt.X4mm",
                            "sqrt.X2mm",
                            "sqrt.X1mm",
                            "sqrt.X500um"),
                   labels=c(
                     "Distance",
                     "Status",
                     "Lobster",
                     "Snapper",
                     "Fetch (km)",
                     "Organic content",
                     "Grain size: 4mm",
                     "            2mm",
                     "            1mm",
                     "            500um"
                   ))+
  scale_y_discrete(limits = c("CPN",
                              "BMS",
                              "BDS"),
                   labels=c("P. novizelandiae",
                            "M. striata",
                            "D. subrosea"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.scores


# Part 3 - plots of the most parsimonious models----

### now  make a nice plot of the most interesting models-----
library(gridExtra)
library(grid)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Bring in and format the raw data----
name<-"clams"

dat <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_dataset.csv?token=AOSO6uyYhat9-Era46nbjALQpTydsTskks5ZY3vhwA%3D%3D"))%>%
  rename(response=Abundance)%>%
  #   Transform variables
  mutate(sqrt.X4mm=sqrt(X4mm))%>%
  mutate(sqrt.X2mm=sqrt(X2mm))%>%
  mutate(sqrt.X1mm=sqrt(X1mm))%>%
  mutate(sqrt.X500um=sqrt(X500um))%>%
  mutate(distance=as.numeric(as.character(Distance)))%>%
  na.omit()%>%
  glimpse()

# Manually make the most parsimonious GAM models for each taxa ----
# MODEL Bivalve.Dosina.subrosea 500um + distance x Status ----
dat.bds<-dat%>%filter(Taxa=="BDS")
gamm=gam(response~s(sqrt.X500um,k=3,bs='cr')+s(distance,k=1,bs='cr', by=Status)+ s(Location,Site,bs="re")+ Status, family=tw(),data=dat.bds)

# predict - status from MODEL Bivalve.Dosina.subrosea----
mod<-gamm
testdata <- expand.grid(distance=mean(mod$model$distance),
                        sqrt.X500um=mean(mod$model$sqrt.X500um),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))
head(testdata)
fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bds.status = testdata%>%data.frame(fits)%>%
  group_by(Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bds.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bds.status<-read.csv("predicts.csv")%>%
  glimpse()

# predict - distance.x.status from MODEL Bivalve.Dosina.subrosea----
mod<-gamm
testdata <- expand.grid(distance=seq(min(dat$distance),max(dat$distance),length.out = 20),
                        sqrt.X500um=mean(mod$model$sqrt.X500um),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bds.distance.x.status = testdata%>%data.frame(fits)%>%
  group_by(distance,Status)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bds.distance.x.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bds.distance.x.status<-read.csv("predicts.csv")%>%
  glimpse()

# predict 500um from MODEL Bivalve.Dosina.subrosea----
mod<-gamm
testdata <- expand.grid(sqrt.X500um=seq(min(dat$sqrt.X500um),max(dat$sqrt.X500um),length.out = 20),
                        distance=mean(mod$model$distance),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bds.500um = testdata%>%data.frame(fits)%>%
  group_by(sqrt.X500um)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bds.500um,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bds.500um<-read.csv("predicts.csv")%>%
  glimpse()

# MODEL Bivalve.Myadora.striata  Lobster----
dat.bms<-dat%>%filter(Taxa=="BMS")
head(dat.bms,2)
gamm=gam(response~s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"), family=tw(),data=dat.bms)

# predict - lobster from model for Bivalve.Myadora.striata ----
mod<-gamm
testdata <- expand.grid(lobster=seq(min(dat$lobster),max(dat$lobster),length.out = 20),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bms.lobster = testdata%>%data.frame(fits)%>%
  group_by(lobster)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bms.lobster,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bms.lobster<-read.csv("predicts.csv")%>%
  glimpse()

# MODEL Decapod.P.novazelandiae 4mm + Lobster----
dat.cpn<-dat%>%filter(Taxa=="CPN")
head(dat.cpn,2)
gamm=gam(response~s(sqrt.X4mm,k=3,bs='cr')+s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"), family=tw(),data=dat.cpn)

# predict - sqrt.X4mm from model for Decapod.P.novazelandiae ----
mod<-gamm
testdata <- expand.grid(sqrt.X4mm=seq(min(dat$sqrt.X4mm),max(dat$sqrt.X4mm),length.out = 20),
                        lobster=mean(mod$model$lobster),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
head(fits,2)
predicts.cpn.4mm = testdata%>%data.frame(fits)%>%
  group_by(sqrt.X4mm)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.cpn.4mm,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.cpn.4mm<-read.csv("predicts.csv")%>%
  glimpse()

# predict - lobster from model for Decapod.P.novazelandiae ----
mod<-gamm
testdata <- expand.grid(lobster=seq(min(dat$lobster),max(dat$lobster),length.out = 20),
                        sqrt.X4mm=mean(mod$model$sqrt.X4mm),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.cpn.lobster = testdata%>%data.frame(fits)%>%
  group_by(lobster)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.cpn.lobster,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.cpn.lobster<-read.csv("predicts.csv")%>%
  glimpse()

# PLOTS for Bivalve.Dosina.subrosea 500um + distance x Status ----
ggmod.bds.status<- ggplot(aes(x=Status,y=response,fill=Status,colour=Status), data=predicts.bds.status) +
  ylab(" ")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.bds.status$Status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(a)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "   Dosinia subrosea",vjust = 1, hjust = -.1,size=5,fontface="italic")
ggmod.bds.status

ggmod.bds.distance.x.status<- ggplot(aes(x=distance,y=response,colour=Status), data=dat.bds) +
  ylab(" ")+
  xlab('Distance (m)')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
  # geom_point(alpha=0.75, size=2)+
  geom_line(data=predicts.bds.distance.x.status,show.legend=FALSE)+
  geom_line(data=predicts.bds.distance.x.status,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.bds.distance.x.status,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=5)
ggmod.bds.distance.x.status

ggmod.bds.500um<- ggplot() +
  ylab(" ")+
  xlab('Grain size: 500 um (sqrt)')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_point(data=dat.bds,aes(x=sqrt.X500um,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response),alpha=0.5)+
  geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.bds.500um

# PLOTS Bivalve M.striata lobster ----
ggmod.bms.lobster<- ggplot() +
  ylab("Abundance")+
  xlab(bquote('Density of legal lobster (no./25' *m^-2*')'))+
  scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.bms,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response),alpha=0.5)+
  geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "   Myadora striata",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.bms.lobster

# PLOTS Decapod.P.novazelandiae 4mm + lobster ----
ggmod.cpn.lobster<- ggplot() +
  ylab(" ")+
  xlab(bquote('Density of legal lobster (no./25' *m^-2*')'))+
  scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.cpn,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response),alpha=0.5)+
  geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(e)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "  Pagurus novizelandiae",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.cpn,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.cpn.lobster

ggmod.cpn.4mm<- ggplot() +
  ylab(" ")+
  xlab('Grain size: 4 mm (sqrt)')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  geom_point(data=dat.cpn,aes(x=sqrt.X4mm,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response),alpha=0.5)+
  geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(f)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = " ",vjust = 1, hjust = -.1,size=5,fontface="italic")
ggmod.cpn.4mm

# combined.plot using grid() and gridExtra()------
blank <- grid.rect(gp=gpar(col="white"))

# To see what they will look like use grid.arrange() - make sure Plot window is large enough! - or will error!
grid.arrange(ggmod.bds.status,ggmod.bds.distance.x.status,ggmod.bds.500um,
             ggmod.bms.lobster,blank,blank,
             ggmod.cpn.lobster,ggmod.cpn.4mm,blank,nrow=3,ncol=3)

# Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
combine.plot<-arrangeGrob(ggmod.bds.status,ggmod.bds.distance.x.status,ggmod.bds.500um,
                          ggmod.bms.lobster,blank,blank,
                          ggmod.cpn.lobster,ggmod.cpn.4mm,blank,nrow=3,ncol=3)

ggsave(combine.plot,file="Langlois_gamm.plot.png", width = 30, height = 30,units = "cm")
<<<<<<< HEAD
=======
  
  #### OLD SCRIPT ----
setwd(model.out)
# set predictors---
#pred.vars=c("mean.relief","sd.relief","hard.corals","rock")  #,"Length"
pred.vars=c("Depth","mean.relief","sd.relief","rock","macroalgae","hard.corals","sand","reef")

# Clean up Response vector---
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Metric==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use
write.csv(unique.vars.use,file=paste("unique.vars.use.csv",sep = "_"))

dat<-dat%>%
  ungroup()

str(dat)

write.csv(dat,"dat.csv")
dat<-read.csv("dat.csv")

# Full-subset models---
head(dat,2)
resp.vars=unique.vars.use
use.dat=dat
out.all=list()
var.imp=list()
factor.vars=c("Status")
str(dat)

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals,bs='cr')+s(Location,Site,bs="re"),
             family=tw(),
             #family=gaussian(link = "identity"), #this is much quicker! but change to tw() for actual analysis
             offset=PeriodLength,
             data=use.dat)
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            # linear.vars="Length",
                            null.terms="s(Location,Site,bs='re')",      
                            max.models=600,
                            parallel=T)
  names(out.list)
  
  
  # examine the list of failed models
  out.list$failed.models
  # look at the model selection table
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  # mod.table$cumsum.wi=cumsum(mod.table$wi.AICc) #changed here TJL
  # best.mods.index=c(1,1+which(mod.table$cumsum.wi<0.9))
  # out.i=mod.table[ best.mods.index,]
  out.i=mod.table[which(mod.table$delta.AICc<=4),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.per.mod))
  #write.csv(mod.table,"test_out_modfits_mgcv.csv")
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      #       plot(best.model$gam,all.terms=T,pages=1,residuals=T,pch=16) #change if you change the gam function
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# Generic importance plots-
pdf(file=paste(name,"var_importance_heatmap.pdf",sep="_"),onefile=T)
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,8), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)
dev.off()

# END OF MODEL---
# model.out.TargetLoc.factor.mad----
name<-"model.out.TargetLoc.factor.mad"
setwd(model.out)
dir()

# set predictors---
pred.vars=c("mean.relief","sd.relief","hard.corals","rock","Length")

dat<-dat.mad.targetloc

# Clean up Response vector---
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Metric==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use
write.csv(unique.vars.use,file=paste("unique.vars.use.csv",sep = "_"))

# Full-subset models---
head(dat,2)
resp.vars=unique.vars.use
use.dat=dat
out.all=list()
var.imp=list()
factor.vars=c("Status","TargetLoc","School")
str(dat)

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals,bs='cr')+s(Location,Site,bs="re"), #TJL removed Sample
             # family=tw(),
             family=gaussian(link = "identity"), #this is much quicker! but change to tw() for actual analysis
             offset=PeriodLength,
             data=use.dat)
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            # linear.vars="Length",
                            null.terms="s(Location,Site,bs='re')",  #TJL removed Sample
                            max.models=600,
                            parallel=T)
  names(out.list)
  
  # examine the list of failed models
  out.list$failed.models
  # look at the model selection table
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  # mod.table$cumsum.wi=cumsum(mod.table$wi.AICc) #changed here TJL
  # best.mods.index=c(1,1+which(mod.table$cumsum.wi<0.9))
  # out.i=mod.table[ best.mods.index,]
  out.i=mod.table[which(mod.table$delta.AICc<=4),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.per.mod))
  #write.csv(mod.table,"test_out_modfits_mgcv.csv")
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      #       plot(best.model$gam,all.terms=T,pages=1,residuals=T,pch=16) #change if you change the gam function
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# END OF MODEL

# model.out.Species.mad----
name<-"model.out.species.mad"
setwd(model.out)
dir()

# set predictors---
pred.vars=c("mean.relief","sd.relief","hard.corals","rock","Length")

dat<-dat.mad.species

# Clean up Response vector---
unique.vars=unique(as.character(dat$Metric))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Metric==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use
write.csv(unique.vars.use,file=paste("unique.vars.use.csv",sep = "_"))

# Full-subset models---
head(dat,2)
resp.vars=unique.vars.use
use.dat=dat
out.all=list()
var.imp=list()
factor.vars=c("Status","School")
str(dat)

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(hard.corals,bs='cr')+s(Location,Site,bs="re"), #TJL removed Sample
             # family=tw(),
             family=gaussian(link = "identity"), #this is much quicker! but change to tw() for actual analysis
             offset=PeriodLength,
             data=use.dat)
  out.list=full.subsets.gam(use.dat=use.dat,
                            test.fit=Model1,
                            pred.vars.cont=pred.vars,
                            pred.vars.fact=factor.vars,
                            k=3,
                            # linear.vars="Length",
                            null.terms="s(Location,Site,bs='re')",  #TJL removed Sample
                            max.models=600,
                            parallel=T)
  names(out.list)
  
  # examine the list of failed models
  out.list$failed.models
  # look at the model selection table
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  # mod.table$cumsum.wi=cumsum(mod.table$wi.AICc) #changed here TJL
  # best.mods.index=c(1,1+which(mod.table$cumsum.wi<0.9))
  # out.i=mod.table[ best.mods.index,]
  out.i=mod.table[which(mod.table$delta.AICc<=4),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.per.mod))
  #write.csv(mod.table,"test_out_modfits_mgcv.csv")
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      #       plot(best.model$gam,all.terms=T,pages=1,residuals=T,pch=16) #change if you change the gam function
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# END OF MODEL

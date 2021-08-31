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
library(stringr)

rm(list=ls())

# install package----
#devtools::install_github("beckyfisher/FSSgam_package", force =TRUE) #run once
library(FSSgam)

# Study name
study<-"mad.schools"

# Add you work dir here ----
work.dir <- ("~/Git Projects/current/2020-Moorea-minimum-approach") # Use this directory name from now on
work.dir<-("C:/GitHub/Moorea-minimum-approach") # work desktop
work.dir <- ("Y:/Moorea-minimum-approach") # Work laptop

tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
summaries=paste(work.dir,"Data/Summaries",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
plots=paste(work.dir,"Plots",sep="/")
model.out.mad.as.taxa=paste(work.dir,"ModelOut/MAD as taxa",sep="/")
model.out.mad.as.factor=paste(work.dir,"ModelOut/MAD as factor",sep="/")

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

# Bring in length data ----
setwd(tidy.data)
dir()

raw.data <- read.csv("2020-02-18_mad.schools_combined.factors.habitat.csv")%>%
  filter(Reef.Lagoon == "Lagoon")%>% # I am only looking at Lagoon sites
  filter(Location %in% c("Pihaena", "Tiahura", "Tetaiuo"))%>% # only in these three reserves
  glimpse()

# Make factors and sample list ----
samples <- raw.data%>%
  distinct(Sample)

factors <- raw.data%>%
  distinct(Sample,Depth,Location,Status,Site,Reef.Lagoon,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,PeriodLength)

# Mad data----
mad.data<-raw.data%>%
  dplyr::filter(final.mad>0)%>% # NEED TO CREATE SCHOOLS BEFORE FILTERING THESE OUT
  #filter(!is.na(Length))%>%
  #filter(Length>80)%>%
  #filter(Length<300)%>%
  dplyr::mutate(Indicator="Mad")%>%
  # dplyr::mutate(TargetLoc=str_replace_all(.$TargetLoc,c("1"="targeted","2"="targeted","0"="non-target")))%>% # test
  dplyr::mutate(TargetLoc=as.factor(TargetLoc))%>%
  dplyr::rename(response=final.mad)%>%
  dplyr::select(-c(Reef.Lagoon))%>%
  glimpse()

# Add a median length within a school
# if no lengths in a school give median length for that species in that transect?

mad.with.length <- mad.data[!is.na(mad.data$Length),]

median.schools <- plyr::ddply(mad.with.length, (Genus_species ~ Sample ~ School), summarise, Length.new = median(Length),)

median.all <- plyr::ddply(mad.with.length, (Genus_species ~ Location), summarise, Length.new = median(Length),)

# Subset to all missing lengths
mad.no.length <- mad.data[is.na(mad.data$Length),]

mad.new.lengths <- dplyr::left_join(mad.no.length, median.schools)%>%
  dplyr::mutate(Length=ifelse(is.na(Length), Length.new, Length))%>%
  dplyr::select(!Length.new)

still.no.length <- mad.new.lengths[is.na(mad.new.lengths$Length),]

last.add.lengths<- dplyr::left_join(still.no.length, median.all)%>%
  dplyr::mutate(Length=ifelse(is.na(Length), Length.new, Length))

new.mad.data <- mad.new.lengths %>%
  filter(!is.na(Length))%>%
  bind_rows(., mad.with.length, last.add.lengths)

mad.data <- new.mad.data

mad.schools<-mad.data%>%
  dplyr::filter(grepl("School",School)) # filter to only those in schools

mad.school.size<-mad.schools%>%
  dplyr::group_by(Sample,School)%>%
  dplyr::filter(!School%in%c(""))%>%
  dplyr::summarise(school.size=sum(Number))

mad.individuals<-mad.data%>%
  dplyr::filter(!grepl("School",School)) # filter to only individuals

mad.individuals<-mad.individuals%>%
  dplyr::mutate(School=paste("School",1:nrow(mad.individuals), sep=".")) # make a unique id for each individual

mad<-bind_rows(mad.schools, mad.individuals)

# BG 04/09/19
# trying to figure out if we have schools of mixed target level

test.schools<-mad%>%
  mutate(TargetLoc=as.numeric(TargetLoc))%>%
  #mutate(TargetLoc=ifelse(TargetLoc==1,2,TargetLoc))%>%
  group_by(Sample,School)%>%
  summarise(number=length(unique(TargetLoc)))#,average=mean(TargetLoc))


# 75 schools (594 fish) 6% if 3 levels of target and we remove all mixed fish
# 73 schools () if 2 levels of target and we remove all mixed fish

schools.with.mutliple.targetlocs<-test.schools%>%
  dplyr::filter(number>1)%>%
  distinct(Sample,School) # might need to come back to this

mad.final <- mad%>%
  anti_join(schools.with.mutliple.targetlocs)%>%
  mutate(Metric=ifelse(TargetLoc%in%c(0),"non-target",ifelse(TargetLoc%in%c(1),"mod-target","high-target")))

# Removes 594 fish (8975-8381) (594/8975*100=6%)

# Need to make a row for each school
# with min, mean and max length
# and school size
mad.sum <- mad.final%>%
  filter(Length<300)%>%
  dplyr::group_by(Sample,School,Metric,TargetLoc)%>% # need to keep target loc in
  dplyr::summarise(response=min(response),min.length=min(Length),mean.length=mean(Length),max.length=max(Length))%>%
  ungroup()%>%
  left_join(mad.school.size)%>%
  replace_na(list(school.size=1))%>%
  left_join(factors)%>%
  #mutate(Metric=as.factor(Metric),School=as.factor(School))%>%
  as.data.frame()%>%
  filter(!is.na(mean.length))%>%
  glimpse()

# Set predictor variables ----
pred.vars=c("mean.relief","sd.relief","hard.corals","rock","reef",
            "min.length","mean.length","max.length","school.size") 

# Removed 
# reef - correlated with sand
# macroalgae - too few
# Depth didnt used to be in but maybe keep it??? BG

dat<-mad.sum %>%
  filter(!school.size>200)

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars]),2)

# All of the length variables are highly correlated.
# I think I have to run through this 3 times using each one (min, mean and max)

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

# school size is very shitty
# rock is also pretty shitty
# sqrt hard corals

### MAD as taxa ----
# Re-set the predictors for modeling----
pred.vars=c("mean.relief","sd.relief","hard.corals","reef",
            "mean.length","school.size")

names(dat)

# Set name of models ----
name<-"mad.output"

# Data
dat<-mad.sum%>%
  filter(!school.size>200)

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
setwd(model.out.mad.as.taxa) #Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat

glimpse(dat)

factor.vars=c("Status") # Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each target group----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(reef,k=3,bs='cr')+s(Location,Site,bs="re"),
             family=gaussian(link = "identity"),  
             offset=PeriodLength, 
             data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               k=3,
                               null.terms="s(Location,Site,bs='re')")
  
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

# Model fits and importance---
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

### MAD as factor ----
# Re-set the predictors for modeling----
pred.vars=c("mean.relief","sd.relief","hard.corals","reef",
            "mean.length")

names(dat)

# Set name of models ----
name<-"mad.as.factor.without.school.size"

# Data
dat<-mad.sum%>%mutate(Metric="MAD")%>%
  filter(!school.size>200)

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
setwd(model.out.mad.as.factor) #Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat

glimpse(dat)

factor.vars=c("Status","TargetLoc") # Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each target group----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(reef,k=3,bs='cr')+s(Location,Site,bs="re"),
             family=gaussian(link = "identity"),  
             offset=PeriodLength, 
             data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               k=3,
                               null.terms="s(Location,Site,bs='re')")
  
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

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

### MAD as factor with school size ----
# Re-set the predictors for modeling----
pred.vars=c("mean.relief","sd.relief","hard.corals","reef",
            "mean.length","school.size")

names(dat)

# Set name of models ----
name<-"mad.as.factor.with.school.size"

# Data
dat<-mad.sum%>%mutate(Metric="MAD")%>%
  filter(!school.size>200)

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
setwd(model.out.mad.as.factor) #Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat

glimpse(dat)

factor.vars=c("Status","TargetLoc") # Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each target group----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Metric==resp.vars[i]),]
  
  Model1=gam(response~s(reef,k=3,bs='cr')+s(Location,Site,bs="re"),
             family=gaussian(link = "identity"),  
             offset=PeriodLength, 
             data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               k=3,
                               null.terms="s(Location,Site,bs='re')")
  
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

# Model fits and importance---
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


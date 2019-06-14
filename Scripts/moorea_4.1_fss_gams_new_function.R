# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub

rm(list=ls())

# install package----
devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Bring in and format the data----
name<-"moorea.schools"

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

unique(combined$Site)

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

dat<-combined

names(dat)

# Set predictor variables---
pred.vars=c("Depth","PeriodLength","sd.relief","rock","hard.corals","sand") 

# Removed 
# reef - correlated with sand
# macroalgae - too few
# Depth didnt used to be in but maybe keep it??? BG

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
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3
# Decided that X4mm, X2mm, X1mm and X500um needed a sqrt transformation
#Decided Depth, x63um, InPreds and BioTurb were not informative variables. 

dat<-dat%>%
  dplyr::rename(response=Response)

glimpse(dat)

dat<-as.data.frame(dat)


## ABUNDANCE ----

# # Re-set the predictors for modeling----
pred.vars=c("rock","sd.relief","hard.corals","sand") 

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
setwd(model.out) #Set wd for example outputs - will differ on your computer
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


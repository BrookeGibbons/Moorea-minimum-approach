# librarys----
library(tidyr)
library(dplyr)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) # this can removed?
library(doSNOW)
library(gamm4)
library(googlesheets)
library(googlesheets4)
library(RCurl) # needed to download data from GitHub

rm(list=ls())

# install package----
#devtools::install_github("beckyfisher/FSSgam_package", force =TRUE) #run once
library(FSSgam)

# Study name
study<-"mad.schools"

# Add you work dir here ----
work.dir <- ("~/Git Projects/current/2020-Moorea-minimum-approach") # Use this directory name from now on

tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
summaries=paste(work.dir,"Data/Summaries",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
plots=paste(work.dir,"Plots",sep="/")
model.out=paste(work.dir,"ModelOut",sep="/")

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

# For offline ----
setwd(data.dir)
dir()

master <- read.delim("Moorea Species List_170330.txt")%>%
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
  dplyr::mutate(TargetLoc=as.factor(TargetLoc))%>%
  dplyr::rename(response=final.mad)%>%
  dplyr::select(-c(Reef.Lagoon))%>%
  glimpse()

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
  mutate(TargetLoc=ifelse(TargetLoc==1,2,TargetLoc))%>%
  mutate(Target=ifelse(TargetLoc%in%c(2,1),"targeted","non-target"))%>%
  group_by(Sample,School)%>%
  #summarise(number=length(unique(TargetLoc)),average=mean(TargetLoc))%>%
  summarise(number=length(unique(Target)),average=mean(Target))

schools.with.mutliple.targetlocs<-test.schools%>%
  dplyr::filter(number>1)%>%
  distinct(Sample,School) # might need to come back to this

mad.final <- mad%>%
  anti_join(schools.with.mutliple.targetlocs)%>%
  mutate(Target=ifelse(TargetLoc%in%c(2,1),"targeted","non-target"))%>%
  mutate(Metric=ifelse(TargetLoc%in%c(0),"non-target",ifelse(TargetLoc%in%c(1),"mod-target","high-target")))


unique(mad.final$Metric)
unique(mad.final$Target)

# Removes 594 fish (8975-8381) (594/8975*100=6%)

# Need to make a row for each school
# with min, mean and max length
# and school size
mad.sum <- mad.final%>%
  dplyr::group_by(Sample,School,Metric,TargetLoc,Target)%>% # need to keep target loc in
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
pred.vars=c("sd.relief","reef","mean.releif","mean.length","school.size") 

# Removed 
# reef - correlated with sand
# macroalgae - too few
# Depth didnt used to be in but maybe keep it??? BG

dat<-mad.sum

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

### MAD ----
# Need to change all of this (copied from abundance)----
# Re-set the predictors for modeling----
pred.vars=c("mean.relief","sd.relief","hard.corals","rock","min.length","school.size") # "max.length","mean.length",


pred.vars=c("sd.relief","reef","mean.relief","mean.length","school.size") 

#pred.vars=c("mean.relief","sd.relief","hard.corals","rock","Length")

names(dat)

# Set name of models ----
name<-"mad.output"

# Data
dat<-mad.sum%>%
  mutate(Metric="mad")

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$Target))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Target==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     

# Run the full subset model selection----
setwd(model.out) #Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat

glimpse(dat)

factor.vars=c("Status") # Status as a Factor with two levels # "TargetLoc"
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

#### New Script 2020/02/05 -----

cat.preds="Status"
null.vars=c("Location","Site") # use as random effect and null model # ,"PeriodLength"

# HOW DO I ADD AN OFFSET FOR PERIODLENGth

cont.preds=c("mean.relief","sd.relief","hard.corals","rock","min.length","school.size") # use as continuous predictors.
cor(dat[,cont.preds])

# have a look at the distribution of the continuous predictors
pdf(file="pred_vars.pdf",onefile=T)
for(p in 1:length(cont.preds)){
  par(mfrow=c(2,1))
  hist(dat[,cont.preds[p]],main=cont.preds[p])
  plot(jitter(dat[,cont.preds[p]]))
}
dev.off()

resp.vars.fams=list("response"=gaussian(link = "identity")#,
                    #"mod-target"=gaussian(link = "identity"),
                    #"non-target"=gaussian(link = "identity")
                    )

resp.vars=names(resp.vars.fams)

# take a look at the response variables
pdf(file="resp_vars.pdf",onefile=T)
for(r in 1:length(resp.vars)){
  par(mfrow=c(2,1))
  hist(dat[,resp.vars[r]],main=resp.vars[r])
  plot(jitter(dat[,resp.vars[r]]))
}
dev.off()

dat=mad.sum
use.dat=dat


i=1
out.all=list()
var.imp=list()
fss.all=list()
top.all=list()

pdf(file="mod_fits_functional_biomass.pdf",onefile=T)
for(i in 1:length(resp.vars)){
  use.dat=na.omit(dat[,c(null.vars,cont.preds,cat.preds,resp.vars[i])])
  use.dat$response=use.dat[,resp.vars[i]]
  
  Model1=gam(response~s(hard.corals,k=3,bs='cr')+s(Location,Site,bs="re"),
             family=gaussian(link = "identity"),  
             #offset=PeriodLength, 
             data=use.dat)
  # Model1=gam(response~s(hard.corals,k=3,bs='cr')+s(Location,Site,bs="re"),
  #            family=gaussian(link = "identity"),  
  #            offset=PeriodLength, 
  #            data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               max.predictors=3,   # limit size here because null model already complex
                               test.fit=Model1,
                               k=3,
                               pred.vars.cont=cont.preds,
                               pred.vars.fact=cat.preds,
                               null.terms="s(Location,Site,bs='re')")
  
  # model.set=generate.model.set(use.dat=use.dat,
  #                              test.fit=Model1,
  #                              pred.vars.cont=pred.vars,
  #                              pred.vars.fact=factor.vars,
  #                              k=3,
  #                              null.terms="s(Location,Site,bs='re')")
  
  
  out.list=fit.model.set(model.set)
  #names(out.list)
  # examine the list of failed models
  #out.list$failed.models
  #out.list$success.models
  fss.all=c(fss.all,list(out.list))
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  out.i=mod.table
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
  all.less.2AICc=mod.table[which(mod.table$delta.AICc<2),]
  top.all=c(top.all,list(all.less.2AICc))
  
  # plot the all best models
  par(oma=c(1,1,4,1))
  for(r in 1:nrow(all.less.2AICc)){
    best.model.name=as.character(all.less.2AICc$modname[r])
    best.model=out.list$success.models[[best.model.name]]
    if(best.model.name!="null"){
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=3,text=resp.vars[i],outer=T)}
  }
}
dev.off()

names(out.all)=resp.vars
names(var.imp)=resp.vars
names(top.all)=resp.vars
names(fss.all)=resp.vars

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
top.mod.fits=do.call("rbind",top.all)

require(car)
require(doBy)
require(gplots)
require(RColorBrewer)

pdf(file="var_importance_heatmap_functional_biomass.pdf",height=5,width=7,pointsize=10)
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","orange","red"))(30),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,14), lhei=c(3,10),lwid=c(3,10),
          Rowv=FALSE,Colv=FALSE)
dev.off()

write.csv(all.mod.fits[,-2],"all_model_fits_functional_biomass.csv")
write.csv(top.mod.fits[,-2],"top_model_fits_functional_biomass.csv")
write.csv(model.set$predictor.correlations,"predictor_correlations.csv")



#### pretty plots of best models -----------------------------------------------
zones=levels(dat$ZONE)

pdf("best_top_model_quick_plots.pdf",height=8,width=7,pointsize=12)
par(mfcol=c(4,2),mar=c(4,4,0.5,0.5),oma=c(2,0.5,0.5,0.5),bty="l")
for(r in 1:length(resp.vars)){
  tab.r=out.all[[resp.vars[r]]]
  top.mods.r=tab.r[1,]
  mod.r.m=as.character(top.mods.r[1,"modname"])
  mod.m=fss.all[[resp.vars[r]]]$success.models[[mod.r.m]]
  mod.vars=unique(unlist(strsplit(unlist(strsplit(mod.r.m,split="+",fixed=T)),
                                  split=".by.")))
  # which continuous predictor is the variable included?
  plot.var=as.character(na.omit(mod.vars[match(cont.preds,mod.vars)]))
  # plot that variables, with symbol colours for zone
  plot(dat[,plot.var],dat[,resp.vars[r]],pch=16,
       ylab=resp.vars[r],xlab=plot.var,col=dat$ZONE)
  legend("topleft",legend=paste("(",LETTERS[r],")",sep=""),
         bty="n")
  range.v=range(dat[,plot.var])
  seq.v=seq(range.v[1],range.v[2],length=20)
  newdat.list=list(seq.v,# across the range of the included variable
                   mean(use.dat$depth), # for a median depth
                   mean(use.dat$SQRTSA),# for a median SQRTSA
                   "MANGROVE", # pick the first site, except don't predict on
                   # this by setting terms=c(plot.var,"ZONE")
                   zones)  # for each zone
  names(newdat.list)=c(plot.var,"depth","SQRTSA","site","ZONE")
  pred.vals=predict(mod.m,newdata=expand.grid(newdat.list),
                    type="response",se=T,exclude=c("site","SQRTSA","depth"))
  for(z in 1:length(zones)){
    zone.index=which(expand.grid(newdat.list)$ZONE==zones[z])
    lines(seq.v,pred.vals$fit[zone.index],col=z)
    lines(seq.v,pred.vals$fit[zone.index]+pred.vals$se[zone.index]*1.96,lty=3,col=z)
    lines(seq.v,pred.vals$fit[zone.index]-pred.vals$se[zone.index]*1.96,lty=3,col=z)}
}
legend("bottom",legend= zones,bty="n",ncol=2,col=c(1,2),pch=c(16,16),
       inset=-0.61,xpd=NA,cex=.8)
dev.off()

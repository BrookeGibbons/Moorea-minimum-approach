# librarys ----
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
library(GlobalArchive)

rm(list=ls())

# install package----
#devtools::install_github("beckyfisher/FSSgam_package", force =TRUE) #run once
library(FSSgam)

# Study name
study <- "mad.schools"

# Bring in length data ----
raw.data <- readRDS("Data/Tidy data/length.combined.factors.habitat.RDS")%>%
  ga.clean.names() %>%
  filter(reef.lagoon %in% c("Lagoon"))%>% # I am only looking at Lagoon sites
  filter(location %in% c("Pihaena", "Tiahura", "Tetaiuo")) %>% # only in these three reserves
  glimpse()

# Make factors and sample list ----
samples <- raw.data %>%
  distinct(sample)

factors <- raw.data %>%
  distinct(sample, depth, location, status, site, reef.lagoon, mean.relief, sd.relief, rock, macroalgae, hard.corals, sand,reef, periodlength)

# Make TA and SR for gams ----
ta.sr <- raw.data %>%
  group_by(genus_species, sample) %>%
  dplyr::summarise(abundance = sum(number)) %>%
  spread(genus_species, abundance, fill = 0) %>%
  mutate(total.abundance = rowSums(.[,2:(ncol(.))],na.rm = TRUE )) # Add in Totals

Presence.Absence <- ta.sr[,2:(ncol(ta.sr))-1]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
total.abundance.species.richness <- ta.sr %>%
  mutate(species.richness = rowSums(Presence.Absence, na.rm = TRUE)) %>%
  inner_join(factors, by = "sample") %>%
  dplyr::select(depth, location, status, site, sample, periodlength, mean.relief, sd.relief, rock, macroalgae, hard.corals, sand, reef, total.abundance, species.richness) %>%
  gather(key = metric, value = response, (match("reef", names(.))+1):ncol(.))

## Make size class data ----
# size.class <- raw.data%>%
#   dplyr::rename(Response = Number)%>%
#   filter(!is.na(Length))%>% 
#   mutate(Indicator = ifelse(Length<=(Max_length/3),"small","large"))%>%
#   dplyr::select(-c(Depth,Location,Status,Site,Region,Reef.Lagoon,mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,PeriodLength,diet,TargetLoc,Commercial,Max_length,final.mad,School))%>%
#   left_join(samples,.,by="Sample")%>%
#   tidyr::complete(Sample,tidyr::nesting(Family,Genus_species),Indicator)%>%
#   left_join(factors,., by = "Sample")%>%
#   replace_na(list(Response = 0))%>%
#   filter(Reef.Lagoon == "Lagoon")%>% # I am only looking at Lagoon sites
#   filter(Location%in%c("Pihaena","Tiahura","Tetaiuo"))%>% # only in these three reserves
#   left_join(master, by = c("Family", "Genus_species"))%>%
#   mutate(Metric=Indicator)%>%
#   dplyr::select(-c(Resilience.x,Resilience.y,diet,CommLoc,CommReg,Ciguatera,Commercial,Max_length))%>%
#   glimpse()

size.class <- raw.data %>%
  dplyr::rename(response = number)%>%
  filter(!is.na(length)) %>% 
  mutate(indicator = ifelse(length <= (max_length/3), "small", "large")) %>%
  dplyr::select(-c(depth, location, status, site, region, reef.lagoon, mean.relief,sd.relief,rock,macroalgae,hard.corals,sand,reef,periodlength,diet,commercial,max_length,final.mad,school))%>%
  full_join(samples, ., by = "sample") %>%
  tidyr::complete(sample, tidyr::nesting(family, genus_species), indicator) %>%
  replace_na(list(response = 0)) %>%
  dplyr::group_by(sample, indicator)%>%
  dplyr::summarise(response = sum(response)) %>%
  left_join(factors, ., by = "sample")%>%
  filter(reef.lagoon == "Lagoon") %>% # I am only looking at Lagoon sites
  filter(location %in% c("Pihaena", "Tiahura", "Tetaiuo")) %>% # only in these three reserves
  mutate(metric = indicator) %>%
  glimpse()

names(size.class)
unique(size.class$sample)

# Abundance by size and TargetLoc ----
size.class.target <- raw.data %>%
  dplyr::rename(response = number)%>%
  filter(!is.na(length)) %>% 
  mutate(indicator = ifelse(length <= (max_length/3), "small", "large")) %>%
  dplyr::select(-c(depth,location, status, site, region, reef.lagoon, mean.relief, sd.relief, rock, macroalgae, hard.corals, sand, reef, periodlength, diet, commercial, max_length,final.mad,school))%>%
  full_join(samples, ., by = "sample") %>%
  tidyr::complete(sample, tidyr::nesting(family, genus_species), indicator, targetloc) %>%
  replace_na(list(response = 0)) %>%
  dplyr::group_by(sample, indicator, targetloc)%>%
  dplyr::summarise(response = sum(response)) %>%
  left_join(factors, ., by = "sample")%>%
  filter(reef.lagoon == "Lagoon") %>% # I am only looking at Lagoon sites
  filter(location %in% c("Pihaena", "Tiahura", "Tetaiuo")) %>% # only in these three reserves
  mutate(metric = indicator) %>%
  filter(!is.na(targetloc)) %>%
  ungroup()%>%
  mutate(metric=paste("abundance.targetloc", targetloc, indicator, sep = "."))%>%
  dplyr::select(-c(targetloc, reef.lagoon))%>%
  glimpse()

unique(size.class.target$sample)
unique(size.class.target$metric)
6*96

# Number and size of schools ----
schools.total <- raw.data %>%
  dplyr::filter(!is.na(school)) %>%
  dplyr::group_by(school, sample) %>%
  dplyr::summarise(response = sum(number)) %>%
  dplyr::mutate(metric = "number in school") %>%
  full_join(samples, ., by = "sample") %>%
  replace_na(list(response = 0)) %>%
  left_join(factors, ., by = "sample") %>%
  ungroup()

number.of.schools <- schools.total %>%
  filter(!school %in% c("NA", NA, "")) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(response = n()) %>%
  ungroup() %>%
  full_join(samples, ., by = "sample") %>%
  left_join(factors, ., by = "sample") %>%
  dplyr::mutate(metric = "number of schools") %>%
  replace_na(list(response = 0))
  
# Combine datasets together ----
combined.abundance <- bind_rows(total.abundance.species.richness,
                                         size.class, 
                                         size.class.target,
                                         schools.total,
                                         number.of.schools) %>%
  left_join(factors)


# Set predictor variables ----
pred.vars = c("depth",
            "periodlength",
            "sd.relief",
            "mean.relief",
            "rock",
            "hard.corals",
            "sand",
            "reef",
            "macroalgae") 

# Removed 
# reef - correlated with sand
# macroalgae - too few
# Depth didnt used to be in but maybe keep it??? BG

# NEW DECISIONS 2020
# Mean relief is correlated with reef + sand - keep reef + sand
# Reef is correlated with sand - keep reef
# Keep sd relief, periodlength (offset), remove macroalgae

dat <- combined.abundance%>%glimpse()

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

dat<-combined.abundance %>%
  dplyr::mutate(status = as.factor(status))%>%
  dplyr::mutate(location = as.factor(location))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(sample = as.factor(sample))

## ABUNDANCE ----
# Re-set the predictors for modeling----
# pred.vars=c("rock","sd.relief","hard.corals","sand") 
pred.vars=c("sand","sd.relief","hard.corals","mean.relief") # NEW PREDICTORS

# Set name of models ----
name<-"abundance.output"

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$metric))
unique.vars.use=character()

for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$metric==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use     

# Run the full subset model selection----
# setwd("/ModelOut") # Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat
factor.vars=c("status")# Status as a Factor with two levels #,"TargetLoc" BG removed 03/03/2023
out.all=list() 
var.imp=list()

names(dat)

# dat <- dat %>%
#   select(-c(reef.lagoon, indicator, school, depth, macroalgae, sand, rock))

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  
  use.dat=dat[which(dat$metric==resp.vars[i]),] %>% as.data.frame()
  
  # glimpse(use.dat)
  # names(use.dat)
  
  Model1 = gam(response~s(mean.relief, k = 3, bs = 'cr')+ s(site, location, bs = 're'),
             family = tw(),  offset = periodlength, data = use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               #linear.vars="Distance",
                               k=3,
                               null.terms="s(site, location, bs='re')")
  
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=3),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste("ModelOut/",name,resp.vars[i],m,"mod_fits.png",sep="_"))
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
write.csv(all.mod.fits[,-2],file=paste("ModelOut/", name, "all.mod.fits.csv", sep = "_"))
write.csv(all.var.imp,file=paste("ModelOut/", name, "all.var.imp.csv", sep = "_"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,8), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)

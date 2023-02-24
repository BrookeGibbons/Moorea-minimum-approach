# Libraries required
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

# Clear memory ----
rm(list=ls())

# Study name ----
study <- "mad.schools"

# Read in data using database exports ----
# Lengths ----
lengths.outside.ellen <- read.delim("Database exports/moorea_outside_Lengths.txt") %>%
  dplyr::rename(Mad = Attribute9, Incoming =  Attribute10) %>%
  glimpse() # 10422

unique(lengths.outside.ellen$Mad)
unique(lengths.outside.ellen$Incoming)

lengths.outside.brooke <- read.delim("Database exports/moorea_outside brooke_Lengths.txt") %>%
  dplyr::rename(Mad = Attribute10, Incoming = Attribute9) %>%
  glimpse() # 1224

unique(lengths.outside.brooke$Mad)
unique(lengths.outside.brooke$Incoming)

lengths.inside <- read.delim("Database exports/moorea_inside_Lengths.txt") %>%
  dplyr::rename(Mad = Attribute10, Incoming = Attribute9) %>%
  glimpse() # 7757

unique(lengths.inside$Mad)
unique(lengths.inside$Incoming)

# 3D points ----
threed.outside.ellen <- read.delim("Database exports/moorea_outside_3DPoints.txt") %>%
  dplyr::rename(Mad = Attribute9, Incoming= Attribute10) %>%
  glimpse()  # 8576

unique(threed.outside.ellen$Mad)
unique(threed.outside.ellen$Incoming)

threed.outside.brooke <- read.delim("Database exports/moorea_outside brooke_3DPoints.txt") %>%
  dplyr::rename(Mad = Attribute10, Incoming = Attribute9) %>%
  glimpse() # 402

unique(threed.outside.brooke$Mad)
unique(threed.outside.brooke$Incoming)

threed.inside <- read.delim("Database exports/moorea_inside_3DPoints.txt") %>%
  dplyr::rename(Mad = Attribute10, Incoming = Attribute9) %>%
  glimpse() # 4961

unique(threed.inside$Mad)
unique(threed.inside$Incoming)

# Combine files ----
length <- bind_rows(lengths.outside.ellen,
                    lengths.outside.brooke,
                    lengths.inside,
                    threed.outside.ellen,
                    threed.outside.brooke,
                    threed.inside) %>%
  dplyr::mutate(Period = as.numeric(Period)) %>%
  dplyr::mutate(Mad = as.numeric(Mad)) %>%
  dplyr::mutate(Genus = ifelse(is.na(Genus), Family, Genus)) %>% #fill in any blank Genus names with family
  dplyr::mutate(Genus_species = paste(Genus, Species, sep = ' ')) %>% #paste Genus species together
  dplyr::mutate(School = str_replace_all(.$Comment, c("school" = "School",
                                                      "multispecies " = "",
                                                      "sames species " = "",
                                                      "same species" = "",
                                                      " Multispecies" = "",
                                                      "Same Species" = "", "multispecies" = "",
                                                      "Mulitspecies" = "",
                                                      "Multipspecies" = "",
                                                      "1" = "",
                                                      "2" = "",
                                                      "3" = "",
                                                      "4" = "",
                                                      "5" = "",
                                                      "6" = "",
                                                      "7" = "",
                                                      "8" = "",
                                                      "9" = "",
                                                      "[^[:alnum:]]" = "", "School" = "School."))) %>%
  dplyr::select(OpCode, Time, Period, PeriodTime, Length, Range, Family, Genus_species, Genus, Species, Number, Comment, Mad, Incoming, School) %>%
  glimpse() # 33342

unique(length$Comment)
unique(length$School)%>%sort() # The Y is from a inside reef site (to be fixed later), ID is from a reef site too 

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
# 
# 
# # Use  Observer to indicate if  Length were possible to collect - giving us our true zeros 
# length.factors<-factors%>%
#   filter(!is.na(Observer))%>%
#   filter(!Observer %in% c("NA","N/A"))
  
# BASIC Checks----
# Check if we have schools associated with single length measures----
schools <- filter(length, Number>  1) %>%
  glimpse() # nope

# Standardise for RANGE and Error for Length----
summary(length$Range)

out.of.range <- filter(length, Range > 10000); head(out.of.range)
length <- filter(length, Range < 10000)

# Check on the BIG fish length data----
fish.greater.than.1.meter <- filter(length, Length > 1000) %>%
  glimpse() # A shark 

# Plot to visualise length data----
gg.check.length <- ggplot(data = length, aes(as.numeric(Length))) + 
  geom_histogram(aes(y = ..density..), 
                 col = "red", 
                 fill = "blue", 
                 alpha = .2) 

gg.check.length

# Plot to visualise range data----
gg.check.range <- ggplot(data=length, aes(as.numeric(Range))) + 
  geom_histogram(aes(y = ..density..), 
                 col = "red", 
                 fill = "green", 
                 alpha = .2) 

gg.check.range

# Plot to visualise length/range data----
gg.check.range.vs.length <- ggplot(data = length, aes(as.numeric(Length), as.numeric(Range))) + 
  geom_point() +
  geom_smooth()

gg.check.range.vs.length

# SERIOUS data checking to compare taxa and min/max lengths----
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
master <- read.delim("Data/Moorea Species List_170406.txt") %>%
  mutate(Max = as.numeric(Max)) %>%
  mutate(Max_length = Max * 10) %>%
  mutate(Min_length = 0) %>%
  dplyr::rename(diet = Diet.7cl2) %>%
  dplyr::select(Genus_species, Family, diet, CommLoc, CommReg, TargetLoc, Commercial, Ciguatera, Resilience, Max_length)%>%
  mutate(TargetLoc = as.character(TargetLoc)) %>%
  mutate(Commercial = as.character(Commercial)) %>%
  glimpse()

# Check for taxa.not.match----
length.taxa.not.match <- master%>%
  dplyr::select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  dplyr::select(Genus_species)%>%
  distinct(Genus_species)%>%
  glimpse()

length.taxa.and.opcode.not.match <- master%>%
  dplyr::select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  distinct(Genus_species,OpCode)%>% 
  dplyr::select(Genus_species,OpCode)%>%
  glimpse()

### SERIOUS Check for Min Max Length compared to Master list----
length.bigger <- length %>%
  left_join(master) %>%
  filter(Length > Max_length) %>%
  mutate(difference = Length - Max_length)

# WRITE FINAL checked data----
length.final.with.mad <- master %>%
  select(Genus_species) %>%
  semi_join(length, ., by = "Genus_species") %>% #this will drop any speices not in the Master list - that came from the Life_history sheet
  mutate(final.mad = Range - (Mad * 1000)) %>%
  mutate(final.mad = ifelse(is.na(final.mad), Range, final.mad)) %>%
  glimpse()

write.csv(length.final.with.mad, file = paste("Data/Tidy data", study, "_length.csv", sep = ""), row.names=FALSE)
saveRDS(length.final.with.mad, "Data/Tidy data/length.RDS")
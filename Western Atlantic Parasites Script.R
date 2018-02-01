#Skylar Hopkins, NCEAS 2017

###To Do list###
#Compare/combine Strona and NMH
#Look at which fish spp we're missing parasites for
#Use PaCO to predict parasites for missing fish spp?
#Lit searches to add in newer worms and arthropods

###################################################################################
##############################Libraries###########################################
###################################################################################
#I was hoping that the FishBase R package could download a list of marine fish
#species by state, like you can do on the web interface. But it appears that you can only
#use the R package to pull info given a list of fish spp. 

#install.packages("rfishbase", repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"), type="source")
library(rfishbase) #FishBase

#install.packages("devtools")
#devtools::install_github("rOpenSci/helminthR")
library("helminthR")

library(dplyr)
library(tidyr)
library(readr)

#########################################################################################
#################Create a list of western Atlantic fish spp##################################
#########################################################################################
##Right now, I just import the list of Massachusetts marine fish species that you can
##get on FishBase. There might be another location to get a most complete list? Might also
##make sense to download a separate list from each state from ME -> VA and merge them
FishSpp <- read_csv("~/Documents/Smithsonian Proposal/Western Atlantic Fishes from FishBase Massachusetts.csv")
head(FishSpp)
length(FishSpp$LatinName)

#We need to separate genus and species, so that we can pass them to helminthR
#We get an error for cases with subspecies, because they have three words in the latin
#name, but it doesn't affect outcome for this particular spp list
FishSpp<-separate(data=FishSpp, col=LatinName, into=c("Genus", "species"), sep=" ", remove=F)
FishSpp<-as.data.frame(FishSpp)
#warning() # the annoying "Unknown or uninitialised column" error doesn't actually hurt anything
FishSpp[129,] #ex: subspp example

#View(FishSpp)

#########################################################################################
#########################################################################################
#########################################################################################
#Important note: for now, we're assuming that all parasites ever reported from a host spp
#can infect that host in Mass. So we're including PAL and NEO in Strona dataset,
#and we aren't limiting to US in NHM, because location info often missing
#############################IGNORE LOCATION##########################################
#########################################################################################
#########################################################################################
#########################################################################################

#########################################################################################
####Create a list of western Atlantic parasite spp using Strona dataset###################
#########################################################################################
#Strona et al. (2013) published a list of ~11,800 fish parasites which is available at
#DOI: 10.1890/12-1419.1 and http://esapubs.org/archive/ecol/E094/045/#data
#This probably isn't a complete list, but we can use it to get started
Strona <- read_csv("~/Documents/Smithsonian Proposal/StronaFishParasiteDataset.csv")
names(Strona)

#Strona$H_SP corresponds to our FishSpp$LatinName category, and both should be based on FishBase names
#so reduce dataset to only the host spp we care about
head(Strona$H_SP) 
WAHostsandParasites<-subset(Strona, H_SP %in% FishSpp$LatinName)

#We get duplicates when a parasite was recorded in multiple geographic regions
WAHostsandParasites$UniqueHspPsp<-paste(WAHostsandParasites$H_SP, WAHostsandParasites$P_SP, "")
length(WAHostsandParasites$UniqueHspPsp)
length(unique(WAHostsandParasites$UniqueHspPsp))

#for now, let's just pick the first host-parasite combo and ignore geography
WAHostsandParasites<-WAHostsandParasites[!duplicated(WAHostsandParasites$UniqueHspPsp),]

length(unique(WAHostsandParasites$H_SP)) #only 81 of the fish spp are in the Strona database?
MissingHosts<-FishSpp$LatinName[!(FishSpp$LatinName %in% WAHostsandParasites$H_SP)] #list of missing host spp
MissingHosts #52 missing

#spot checking shows that at least some missing host spp have parasites listed in Londom NHM Helminth database
findHost(genus = "Acanthurus", species = "chirurgus", removeDuplicates = T, speciesOnly = T)

#For some reason, Lobotes surinamensis isn't listed as marine
WAHostsandParasites$H_SP[WAHostsandParasites$M==0] 

#########################################################################################
##Start looking at diversity patterns with Strona dataset, despite missing data############
#########################################################################################
#Parasite spp richness histogram
SppRichness<-WAHostsandParasites %>% group_by(H_SP) %>% summarize(SppRichness = n())
hist(SppRichness$SppRichness)
hist(table(WAHostsandParasites$H_SP)) #k - dplyr was slower

#Make a dataframe w/ host spp as row/observation, spp richness as tally,
#and carry over info for max host length (MaxL) and some other traits 
WAHostsandParasites <- WAHostsandParasites %>% select(P_SP, H_SP, MaxL, K, Y, Ym, T, AOO)

#Because NAs are in the dataset as "na", a bunch of the quantitative
#variables are character vectors
names(WAHostsandParasites)
WAHostsandParasites[c(3:6)]<-lapply(WAHostsandParasites[c(3:6)], as.numeric)
#View(WAHostsandParasites)

SppRichness<-aggregate(WAHostsandParasites, by = list(WAHostsandParasites$H_SP), FUN=max)
View(SppRichness)
SppRichness$ParasiteRichness<-aggregate(WAHostsandParasites$H_SP, by = list(WAHostsandParasites$H_SP), FUN=length)[,2]
SppRichness<-SppRichness[,-1]

plot(SppRichness$ParasiteRichness~SppRichness$MaxL)
plot(SppRichness$ParasiteRichness~SppRichness$K)
plot(SppRichness$ParasiteRichness~SppRichness$Y)
plot(SppRichness$ParasiteRichness~SppRichness$Ym)
plot(SppRichness$ParasiteRichness~SppRichness$T)
plot(SppRichness$ParasiteRichness~SppRichness$AOO)

##################################################################################
############Make a list using NHM and helminthR##################
##################################################################################
head(FishSpp)

#For now, allow ONLY things that were identified to  spp
findHost(genus = "Acanthurus", species = "chirurgus", hostState = 1, removeDuplicates = T, speciesOnly = T)

Host_SP<-NULL
Parasite_SP<-NULL
ParasiteFull_SP<-NULL
ParasiteData<-cbind(Host_SP, Parasite_SP, ParasiteFull_SP)

for (i in 1:length(FishSpp$Genus)) {
  List<-findHost(genus = FishSpp$Genus[i], species = FishSpp$species[i], hostState = 1, removeDuplicates = T, speciesOnly = T)
  ParasiteData<-rbind(ParasiteData, List)
}

View(ParasiteData)

#For the most part, that list looks good, but there are some issues:
#In cases where the genus was changed, it gave both genera instead of Genus species
#For now, let's go w/ first listed Genus and spp
ParasiteData$Parasite[ParasiteData$Parasite=="Acanthogyrus Acanthosentis"]<-"Acanthogyrus acanthuri"
ParasiteData$Parasite[ParasiteData$Parasite=="Multitestis Multitestoides"]<-"Multitestis brasiliensis"
ParasiteData$Parasite[ParasiteData$Parasite=="Capillaria Hepatocapillaria"]<-"Capillaria cyprinodonticola"
ParasiteData$Parasite[ParasiteData$Parasite=="Paracapillaria Paracapillaria"]<-"Paracapillaria sesokoensis"
ParasiteData$Parasite[ParasiteData$Parasite=="Dichelyne Dichelyne"]<-"Dichelyne bonacii"
ParasiteData$Parasite[ParasiteData$Parasite=="Helicometra Metahelicometra"]<-"Helicometra torta"
ParasiteData$Parasite[ParasiteData$Parasite=="Dichelyne Neocucullanellus"]<-"Dichelyne sindensis"
ParasiteData$Parasite[ParasiteData$Parasite=="Capillaria Procapillaria"]<-"Capillaria gracilis"
#ParasiteData[ParasiteData$Parasite=="Capillaria Procapillaria",]

#In a handful of other cases, it reported something that wasn't IDed to the species
#level, so let's just remove those rows
ProblemEntries<-c("Eutetrarhynchid","Tetraphyllidea","Spirorchiidae", "Monogenean","Cestode","Hemiurid","Contracaecinea",
      "Nematoda -", "Spirurata gen.", "Spirurida", "Tetraphyllidean larvae", "Gyrodactyloides spp.", "Cestode larva",
      "Metacestode", "Hysterothylacium MD", "Metacercaria", "Tetraphyllidea", "Calliobothrium spp.", "Filocapsurinae",
      "Tetraphyllidea", "Acanthocephala", "Ancyrocephalidae", "Hemiuridae", "Nematoda", "Seuratoidea", "Diphyllobothrium spp", 
      "Diplostomum spp.", "Blood fluke", "Trypanorhyncha larva","Eustrongylides spp.", "Gyrodactylus spp.", "Digenea", "Hemiuroid")
#View(subset(ParasiteData, (ParasiteData$Parasite %in% ProblemEntries)))
ParasiteData<-(subset(ParasiteData, !(ParasiteData$Parasite %in% ProblemEntries)))

#Finally, in a few places, the host names that were returned had subspp that we'll take off for now
#For Atlantic Spanish mackerel, the parasites in the subspp are repeats, so remove
ParasiteData<-subset(ParasiteData, subset=ParasiteData$Host!="Scomberomorus maculatus sier"|ParasiteData$Host!="Scomberomorus maculatus sierra"|ParasiteData$Host!="Scomberomorus maculatus sier")

View(ParasiteData)

#################################################################################
##################Compare/combine Strona and NHM databases#########################
##################################################################################
WAHostsandParasites$UniqueHspPsp<-paste(WAHostsandParasites$H_SP, WAHostsandParasites$P_SP, "")
length(WAHostsandParasites$UniqueHspPsp)
head(sort(WAHostsandParasites$UniqueHspPsp), n = 10)

ParasiteData$UniqueHspPsp<-paste(ParasiteData$Host, ParasiteData$Parasite, "")
length(ParasiteData$UniqueHspPsp)
head(sort(ParasiteData$UniqueHspPsp), n = 10)

#They're the same length? But have diff spp?
WAHostsandParasites$UniqueHspPsp %in% ParasiteData$UniqueHspPsp
ParasiteData$UniqueHspPsp %in% WAHostsandParasites$UniqueHspPsp

#I suppose this is because Strona has parasites from other sources, and NHM
#has parasites that Strona excluded for some reason

#########################################################################################
#########################################################################################
#########################################################################################
#########################Only Nearctic Parasites##########################################
#########################################################################################
#########################################################################################
#########################################################################################

#########################################################################################
####Create a list of western Atlantic parasite spp using Strona dataset###################
#########################################################################################
Strona <- read_csv("~/Documents/Smithsonian Proposal/StronaFishParasiteDataset.csv")
names(Strona)

#Strona$H_SP corresponds to our FishSpp$LatinName category, and both should be based on FishBase names
#so reduce dataset to only the host spp we care about
head(Strona$H_SP) 
MAHostsandParasites<-subset(Strona, H_SP %in% FishSpp$LatinName)
MAHostsandParasites$UniqueHspPsp<-paste(MAHostsandParasites$H_SP, MAHostsandParasites$P_SP, "")

##How many species pairs have unknown geography? Strona used "na"
length(which(MAHostsandParasites$GEO=="na")) #170

#Let's keep a list of them to check later, but then remove them 
ParasitesWithoutGeo<-MAHostsandParasites$UniqueHspPsp[MAHostsandParasites$GEO=="na"]

#Just keep the confirmed Nearctic parasites
MAHostsandParasites<-MAHostsandParasites[MAHostsandParasites$GEO=="NEA",]

length(MAHostsandParasites$UniqueHspPsp)
length(unique(MAHostsandParasites$UniqueHspPsp))
View(MAHostsandParasites)

#Let's check for duplicates - first, some that aren't EXACT duplicates
#There is Caballeronema pseudoargmentosus and Caballeronema pseudoargumentatus,
#let's go with the latter, from NHM, citation Caballeronema pseudoargumentatus Appy & Dadswell, 1978   
View(MAHostsandParasites[MAHostsandParasites$P_SP=="Caballeronema pseudoargmentosus"|MAHostsandParasites$P_SP=="Caballeronema pseudoargumentatus",])
MAHostsandParasites<-MAHostsandParasites[MAHostsandParasites$P_SP!="Caballeronema pseudoargmentosus",]

#There is both (Ariopsis felis) Pseudacanthostomum panamense and Pseudoacanthostomum panamense
#which I think are the same? So let's only keep pseudo for now
View(MAHostsandParasites[MAHostsandParasites$P_SP=="Pseudacanthostomum panamense"|MAHostsandParasites$P_SP=="Pseudoacanthostomum panamense",])
MAHostsandParasites<-MAHostsandParasites[MAHostsandParasites$P_SP!="Pseudacanthostomum panamense",]

#Now exact duplicates - mostly in Alosa pseudoharengus. As far as I can tell,
#everything about these is identical, so just pick the first one in all code.
#But here are the specific pairs:
#Alosa pseudoharengus Acanthocephalus dirus
#Alosa pseudoharengus Echinorhynchus gadi
#Alosa pseudoharengus Paratenuisentis ambiguus
#icropogonias undulatus Leptorhynchoides thecatus
#Paralichthys dentatus Southwellina hispida

View(MAHostsandParasites[MAHostsandParasites$UniqueHspPsp=="Alosa pseudoharengus Acanthocephalus dirus ",])
View(Strona[Strona$P_SP=="Acanthocephalus dirus" & Strona$H_SP=="Alosa pseudoharengus",])

MAHostsandParasites<-MAHostsandParasites[!duplicated(MAHostsandParasites$UniqueHspPsp),]

#NOTE: Worried that we have a few cases where the same parasite in a host spp
#is counted as two, due to one misspecified genus. Should fix later.

#Not all of the MA host spp from the FishBase list had parasites in the Strona database
length(unique(MAHostsandParasites$H_SP)) #only 58 of the fish spp are in the Strona database
MissingHosts<-FishSpp$LatinName[!(FishSpp$LatinName %in% MAHostsandParasites$H_SP)] #list of missing host spp
MissingHosts #75 missing

#Make a dataframe w/ host spp as row/observation, spp richness as tally,
#and carry over info for max host length (MaxL) and some other traits 
MAHostsandParasites <- MAHostsandParasites %>% select(P_SP, H_SP, GEO, MaxL, K, Y, Ym, T, AOO)

#Because NAs are in the dataset as "na", a bunch of the quantitative
#variables are character vectors
names(MAHostsandParasites)
MAHostsandParasites[c(4:9)]<-lapply(MAHostsandParasites[c(4:9)], as.numeric)
#View(WAHostsandParasites)

#How many unique parasites?
length(unique(MAHostsandParasites$P_SP))

#Parasite spp richness histogram
SppRichness<-aggregate(MAHostsandParasites, by = list(MAHostsandParasites$H_SP), FUN=max)
SppRichness<-SppRichness[,-1]
SppRichness$ParasiteRichness<-aggregate(MAHostsandParasites$P_SP, by = list(MAHostsandParasites$H_SP), FUN=length)[,2]
hist(SppRichness$ParasiteRichness, breaks = 30)
min(SppRichness$ParasiteRichness); mean(SppRichness$ParasiteRichness); max(SppRichness$ParasiteRichness)
#1; 7.534483; 55
SppRichness[SppRichness$ParasiteRichness > 50,] #Micropogonias undulatus has the most known parasites (Atlantic Croaker)

plot(SppRichness$ParasiteRichness~SppRichness$MaxL)
plot(SppRichness$ParasiteRichness~SppRichness$K)
plot(SppRichness$ParasiteRichness~SppRichness$Y)
plot(SppRichness$ParasiteRichness~SppRichness$Ym)
plot(SppRichness$ParasiteRichness~SppRichness$T)
plot(SppRichness$ParasiteRichness~SppRichness$AOO)
hist(SppRichness$ParasiteRichness, breaks=30)

MissingHosts

#########################################################################################
#########################################################################################
#########################################################################################
#########################Host abundance data##################################
#########################################################################################
#########################################################################################
#########################################################################################
#http://www.iobis.org/explore/#/dataset/1435
AbundData <- read_csv("~/Documents/Western Atlantic Parasites/121cba69f62b11af10f868d0ab8913334a50f6a3/121cba69f62b11af10f868d0ab8913334a50f6a3.csv")

#Contains standardized bottom trawl surveys from all seasons and whatever "Deepwater Systematics" is
unique(AbundData$collectionCode)

#I think Autumn is the longest (1963 to present), so let's start there
AbundData<-AbundData[AbundData$collectionCode=="FALL NMFS NEFSC BOTTOM TRAWL SURVEY",]

#Only keep the fish records (no sharks, either)
unique(AbundData$class)
AbundData<-AbundData[AbundData$class=="Actinopterygii",]

#Not everything is IDed to spp. 
unique(AbundData$scientificName)

#Pull out things in the FishBase list for MA
#Not all of them are present - because they're nearshore fish?
AbundData<-subset(AbundData, AbundData$scientificName %in% FishSpp$LatinName)
unique(AbundData$scientificName)
unique(FishSpp$LatinName)

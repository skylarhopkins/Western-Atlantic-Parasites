#Skylar Hopkins, NCEAS 2017

###To Do list###
#Double check the fishbase list - are there freshwater spp?
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

#Because NAs are in the dataset as "na", a bunch of the quantitative
#variables are character vectors
WAHostsandParasites %>% select(H_SP, MaxL, K, Y, Ym, T, AOO)
names(WAHostsandParasites)
WAHostsandParasites[c(10:14,18:20)]<-lapply(WAHostsandParasites[c(10:14,18:20)], as.numeric)
#View(WAHostsandParasites)

length(unique(WAHostsandParasites$H_SP)) #only 83 of the fish spp are in the Strona database?
MissingHosts<-FishSpp$LatinName[!(FishSpp$LatinName %in% WAHostsandParasites$H_SP)] #list of missing host spp
MissingHosts

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
WAHostsandParasites <- WAHostsandParasites %>% select(H_SP, MaxL, K, Y, Ym, T, AOO)

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
############Make a full list, including host spp not in Strona##################
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

ParasiteData[ParasiteData$Host=="Scomberomorus maculatus"|ParasiteData$Host=="Scomberomorus maculatus sier"|ParasiteData$Host=="Scomberomorus maculatus sierra"|ParasiteData$Host=="Scomberomorus maculatus sier",]

#I don't know why slake is in here, because it's freshwater?
ParasiteData<-(subset(ParasiteData, subset=ParasiteData$Host != "Salvelinus fontinalis x namaycush"|ParasiteData$Host!="Salvelinus fontinalis"))

View(ParasiteData)

##################################################################################
############Use PaCO to guess which parasite spp fish will have?##################
##################################################################################
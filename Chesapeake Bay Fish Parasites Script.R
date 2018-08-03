#Skylar Hopkins, NCEAS 2018
#Chesapeake Bay fish parasites

###To Do list###

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

library(plyr)
library(dplyr)
library(tidyr)
library(readr)

library(lattice)
library(MASS)

library(rvertnet)

#########################################################################################
#########Create a list of Chesapeake Bay fish spp using FishBase###########################
#########################################################################################
##Import the list of Chesapeake Bay marine fish species that you can get from 
#the FishBase web interface. I originally used this as my main list, but later
#I switched to the list in the Field Guide to Fishes of the Chesapeake Bay
#Note: this Fishbase list doesn't include freshwater species
FishBaseFishSpp <- read_csv("~/Documents/ParasiteConservationOOS and Project/Western Atlantic Parasites/ChesapeakeFishResilience.csv")
head(FishBaseFishSpp)
length(FishBaseFishSpp$LatinName) #174 spp

#separate genus and species for use later
#We get an error for cases with subspecies, because they have three words in the latin
#name, but it doesn't affect outcome for this particular spp list
FishBaseFishSpp<-separate(data=FishBaseFishSpp, col=LatinName, into=c("Genus", "species"), sep=" ", remove=F)
FishBaseFishSpp<-as.data.frame(FishBaseFishSpp)
#View(FishSpp)

#Pull resilience category into it's own column
head(FishBaseFishSpp$Resilience)
FishBaseFishSpp$ResilienceCategory<-sapply(strsplit(FishBaseFishSpp$Resilience, ","), "[", 1)

#########################################################################################
#########Create a list of Chesapeake Bay fish spp using Field Guide###########################
#######Data about each spp then compiled from FishBase#######################
#########################################################################################
#I wasn't totally confident in the list I generated from Fishbase, so I pulled
#a new species list from the Field Guide to Fishes of the Chesapeake Bay. The book
#says it lists 211 species, but I only count 208. I copied everything by hand,
#so I need to deal with some human errors to start
FishSpp <- read_csv("~/Documents/ParasiteConservationOOS and Project/Western Atlantic Parasites/Field Guide to Chesapeake Fishes Database.csv")
head(FishSpp)
length(FishSpp$Species) #208 spp
FishSpp<-as.data.frame(FishSpp)

#Make a column that combines the binomial species name
FishSpp$LatinName<-paste(FishSpp$Genus, FishSpp$Species)

#Check to make sure the names in the Field Guide match those used by
#FishBase. Warnings tell us that some spp names are misapplied and/or have synonyms
ValidatedNames<-validate_names(FishSpp$LatinName)
warnings()
validate_names("Trachinotus falcatus") #potential issue - 2 options here
FishSpp$ValidatedLatinName<-ValidatedNames[ValidatedNames!="Trachinotus blochii"]

#Pull lots of fish spp data from FishBase
dat<-species(FishSpp$ValidatedLatinName, fields=c("FamCode", "Subfamily", "Fresh", "Brack", "Saltwater", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "LongevityWild", "Vulnerability", "Length", "CommonLength", "Weight", "Importance", "PriceCateg", "PriceReliability", "LandingStatistics", "Landings", "MainCatchingMethod", "UsedasBait", "Aquarium", "UsedforAquaculture", "GameFish", "Dangerous"))
FishSpp<-cbind(FishSpp, dat)

#IUCN status is in a different data table. Some species have multiple lines, and we 
#just take the first instance
IUCN<-stocks(FishSpp$ValidatedLatinName, fields=c("IUCN_Code"))
IUCN<-ddply(IUCN,.(sciname),function(x) head(x,1))
FishSpp<-merge(FishSpp, IUCN, by.x="ValidatedLatinName", by.y="sciname")

list_fields("Trends")

#########################################################################################
################Explore and Summarize Fish Host Data##################################
#########################################################################################

#################################Diversity########################################
length(unique(FishSpp$LatinName)) #208 spp
length(unique(FishSpp$Genus)) #150 genera
length(unique(FishSpp$Family)) #85 families

##Only 32 fish spp are year round residents
sum(FishSpp$YearRoundResidentYN)
length(unique(FishSpp$LatinName[FishSpp$YearRoundResidentYN==1])) #32 spp
length(unique(FishSpp$Genus[FishSpp$YearRoundResidentYN==1])) #24 genera
length(unique(FishSpp$Family[FishSpp$YearRoundResidentYN==1])) #19 families

########################Historical vulnerability##################################
#"FishBase computes vulnerability values using a fuzzy expert system integrating 
#biological and ecological characteristics of fish species. These values of host 
#vulnerability range from 0 (minimum vulnerability) to 100 (maximum vulnerability). 
#We used these values (divided by 100) as measures of the probabilities of host 
#species to go extinct." Also see Cheung et al. 2005. Biological Conservation.
#Roughly speaking, >50 is high to very high vulnerability
hist(FishSpp$Vulnerability, breaks=30)
xyplot(FishSpp$Vulnerability~as.factor(FishSpp$IUCN_Code))
xyplot(FishSpp$Vulnerability~jitter(as.numeric(as.factor(FishSpp$IUCN_Code))))

##########################IUCN status#########################################
table(FishSpp$IUCN_Code)
FishSpp$IUCNThreatenedYN<-0; 
FishSpp$IUCNThreatenedYN[FishSpp$IUCN_Code=="CR"|FishSpp$IUCN_Code=="EN"|FishSpp$IUCN_Code=="VU"]<-1
table(FishSpp$IUCNThreatenedYN, FishSpp$YearRoundResidentYN)
FishSpp[FishSpp$IUCN_Code=="N.A.",] #introduced from Asia
21/208
FishSpp$CommonName[FishSpp$IUCN_Code=="N.E."]

##########################US Listing#########################################
table(FishSpp$USThreatStatus, FishSpp$IUCN_Code)
FishSpp$CommonName[FishSpp$USThreatStatus=="Endangered"]

#########################################################################################
####Create a list of western Atlantic parasite spp using Strona dataset###################
#########################################################################################
#Strona et al. (2013) published a list of ~11,800 fish parasites which is available at
#DOI: 10.1890/12-1419.1 and http://esapubs.org/archive/ecol/E094/045/#data
#This probably isn't a complete list, but we can use it to get started
Strona <- read_csv("~/Documents/Postdoc and TT Job Apps/Smithsonian Proposal/StronaFishParasiteDataset.csv")
names(Strona)

#Strona$H_SP corresponds to our FishSpp$LatinName category, and both should be based on FishBase names
#so reduce dataset to only the host spp we care about
head(Strona$H_SP) 
HostsandParasites<-subset(Strona, H_SP %in% FishSpp$ValidatedLatinName)
HostsandParasites$UniqueHspPsp<-paste(HostsandParasites$H_SP, HostsandParasites$P_SP, "")

##How many species pairs have unknown geography? Strona used "na"
length(which(HostsandParasites$GEO=="na")) #391

#Let's keep a list of them to check later, but then remove them 
ParasitesWithoutGeo<-HostsandParasites$UniqueHspPsp[HostsandParasites$GEO=="na"]

#Just keep the confirmed Nearctic parasites
HostsandParasites<-HostsandParasites[HostsandParasites$GEO=="NEA",]

length(HostsandParasites$UniqueHspPsp) #1676
length(unique(HostsandParasites$UniqueHspPsp)) #1657
View(HostsandParasites)

#Let's check for duplicates - first, some that aren't EXACT duplicates
#There is Caballeronema pseudoargmentosus and Caballeronema pseudoargumentatus,
#let's go with the latter, from NHM, citation Caballeronema pseudoargumentatus Appy & Dadswell, 1978   
#View(MAHostsandParasites[MAHostsandParasites$P_SP=="Caballeronema pseudoargmentosus"|MAHostsandParasites$P_SP=="Caballeronema pseudoargumentatus",])
HostsandParasites<-HostsandParasites[HostsandParasites$P_SP!="Caballeronema pseudoargmentosus",]

#There were a few cases where the species was uncertain or not included
HostsandParasites<-HostsandParasites[HostsandParasites$P_SP!="Ameiurus nebulosus Corallobothrium parvum ?",]
HostsandParasites<-HostsandParasites[HostsandParasites$P_SP!="Ameiurus nebulosus Apophallus ?",]
HostsandParasites<-HostsandParasites[HostsandParasites$P_SP!="Amia calva Proteocephalus perplexus?",]
HostsandParasites<-HostsandParasites[HostsandParasites$P_SP!="Brevoortia tyrannus Didymozoid c",]
HostsandParasites<-HostsandParasites[HostsandParasites$P_SP!="Catostomus commersonii Caecincola ?",]

#Now exact duplicates - mostly in Alosa pseudoharengus. As far as I can tell,
#everything about these is identical, so just pick the first one in all code.
#But here are the specific pairs:
#Alosa pseudoharengus Acanthocephalus dirus
#Alosa pseudoharengus Echinorhynchus gadi
#Alosa pseudoharengus Paratenuisentis ambiguus
#Ameiurus nebulosus Leptorhynchoides thecatus
#Anguilla rostrata Echinorhynchus salmonis
#Anguilla rostrata Paratenuisentis ambiguus
#Catostomus commersonii Acanthocephalus dirus
#Catostomus commersonii Echinorhynchus lateralis
#Catostomus commersonii Glaridacris laruei
#Lophius americanus Echinorhynchus gadi
#Micropogonias undulatus Leptorhynchoides thecatus
#Paralichthys dentatus Southwellina hispida
HostsandParasites<-HostsandParasites[!duplicated(HostsandParasites$UniqueHspPsp),]

length(HostsandParasites$UniqueHspPsp)
length(unique(HostsandParasites$UniqueHspPsp)) #1656 unique host-parasite links

#NOTE: Worried that we have a few cases where the same parasite in a host spp
#is counted as two, due to one misspecified genus. Should fix later.

#########################################################################################
###################Describe parasite diversity#####################################
#########################################################################################
#Make a dataset w/ each parasite spp listed just once (ignore host info)
UniqueParasites<-HostsandParasites[!duplicated(HostsandParasites$P_SP),]
table(UniqueParasites$P_T) #* means parasite name isn't validated
#Acan = 61
#Cestodes = 130
#Monogeneans = 251
#Nematodes = 91
#Trematodes = 301
length(UniqueParasites$P_SP) #834 species
length(unique(UniqueParasites$P_F)) #125 families

#########################################################################################
###################Parasite species richness###########################################
#########################################################################################
SppRichness<-aggregate(HostsandParasites, by = list(HostsandParasites$H_SP), FUN=max)
SppRichness<-SppRichness[,-1]
SppRichness$ParasiteRichness<-aggregate(HostsandParasites$P_SP, by = list(HostsandParasites$H_SP), FUN=length)[,2]
hist(SppRichness$ParasiteRichness, breaks = 40)
min(SppRichness$ParasiteRichness); mean(SppRichness$ParasiteRichness); max(SppRichness$ParasiteRichness)
#1; 14.27586; 86
names(sort(-table(SppRichness$ParasiteRichness)))[1] #mode = 2
SppRichness[SppRichness$ParasiteRichness==86,] #"Catostomus commersonii"

Temp<-species(SppRichness$H_SP, fields=c("Vulnerability"))
SppRichness<-(merge(SppRichness, Temp, by.x="H_SP", by.y="sciname", all.x=TRUE))

#Look at potential host characteristic correlates of spp richness
#First look at each individually, then use multiple regression w/ uncorrelated
plot(SppRichness$ParasiteRichness~SppRichness$MaxL)
SppRichness$H_SP[which(SppRichness$MaxL>800)] #Let's exclude basking sharks from this - HIP
hist(SppRichness$MaxL)
hist(log(SppRichness$MaxL)) #actually that looks good without removing HIP
plot(SppRichness$ParasiteRichness~log(SppRichness$MaxL))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$MaxL)); summary(test)
FakeMaxL<-log(seq(1, max(SppRichness$MaxL), 1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeMaxL)
lines(PredRich~FakeMaxL, lwd=2, col="red")
lines(lowess(SppRichness$ParasiteRichness~log(SppRichness$MaxL)))
plot(residuals(test))
plot(residuals(test)~log(SppRichness$MaxL))

SppRichness$K<-as.numeric(SppRichness$K)
plot(SppRichness$ParasiteRichness~SppRichness$K)
hist(SppRichness$K)
hist(log(SppRichness$K+1))
plot(SppRichness$ParasiteRichness~log(SppRichness$K+1))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$K+1)); summary(test) #SIG
FakeK<-log(seq(0.0001, max(SppRichness$K, na.rm=T), 0.1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeK)
lines(PredRich~FakeK, lwd=2, col="red")
plot(residuals(test))

plot(SppRichness$ParasiteRichness~SppRichness$Y)
hist(SppRichness$Y)
hist(log(SppRichness$Y+1))
plot(SppRichness$ParasiteRichness~log(SppRichness$Y+1))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$Y+1)); summary(test)
FakeY<-log(seq(0.0001, max(SppRichness$Y, na.rm=T), 1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeY)
lines(PredRich~FakeY, lwd=2, col="red")
plot(residuals(test))

plot(SppRichness$ParasiteRichness~SppRichness$Ym)
hist(SppRichness$Ym)
hist(log(SppRichness$Y+1))
plot(SppRichness$ParasiteRichness~log(SppRichness$Ym+1))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$Ym+1)); summary(test)
FakeYm<-log(seq(0.0001, max(SppRichness$Ym, na.rm=T), 0.1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeYm)
lines(PredRich~FakeYm, lwd=2, col="red")
plot(residuals(test))

SppRichness$T<-as.numeric(SppRichness$T)
plot(SppRichness$ParasiteRichness~SppRichness$T)
hist(SppRichness$T)
test<-glm.nb(SppRichness$ParasiteRichness~SppRichness$T); summary(test)
plot(SppRichness$ParasiteRichness~SppRichness$T)
FakeT<-(seq(0.0001, max(SppRichness$T, na.rm=T), 0.1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeT)
lines(PredRich~FakeT, lwd=2, col="red")
plot(residuals(test))

SppRichness$AOO<-as.numeric(SppRichness$AOO)
plot(SppRichness$ParasiteRichness~SppRichness$AOO)
hist(SppRichness$AOO)
hist(log(SppRichness$AOO+1))
plot(SppRichness$ParasiteRichness~log(SppRichness$AOO+1)) #maybe a bit better
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$AOO+1)); summary(test) #MARG
FakeAOO<-log(seq(0, max(SppRichness$AOO, na.rm=T), 10)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeAOO)
lines(PredRich~FakeAOO, lwd=2, col="red")
plot(residuals(test))

hist(SppRichness$Vulnerability)
test<-glm.nb(SppRichness$ParasiteRichness~SppRichness$Vulnerability); summary(test)
plot(SppRichness$ParasiteRichness~SppRichness$Vulnerability)
FakeVul<-seq(0, 100, 1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeVul)
lines(PredRich~FakeVul, lwd=2, col="red")
plot(residuals(test))

?matplot
pairs(SppRichness[c(10:14, 18, 23)])
#K and Y and Y and Ym super correlated - keep K
plot(log(SppRichness$K+1)~SppRichness$Vulnerability)
names(SppRichness)
pairs(SppRichness[c(11, 14, 18, 23)])
#K w/ vulnerability, of course. So let's go w/ trophic level and K
#AOO might be correlated w/ those, but we'll check it w/ VIFs
pairs(SppRichness[c(11, 14, 18)])
plot(log(SppRichness$AOO+1)~SppRichness$T)
plot(log(SppRichness$AOO+1)~log(SppRichness$K+1))

#Multiple regression model
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$K+1) + SppRichness$T + log(SppRichness$AOO+1)); summary(test)
plot(residuals(test))

#########################################################################################
###################Number of unique psite spp per host spp##########################
#########################################################################################
UniqueParasites<-sort(unique(HostsandParasites$P_SP)) #834 unique parasites
ParasiteGeneralism<-as.data.frame(table(HostsandParasites$P_SP))
hist(ParasiteGeneralism[,2], breaks=30) #WOW. Most from just one host spp!!!!
max(ParasiteGeneralism[,2]) #shared by up to 21 host spp
SpecialistYN<-rep(1, length(ParasiteGeneralism[,2])); SpecialistYN[ParasiteGeneralism[,2] > 1]<-0 
table(SpecialistYN) #only 251 parasites are shared among two or more hosts
583/834

#I want to know how many of those are from threatened host species
ThreatenedSpp<-unique(FishSpp$ValidatedLatinName[FishSpp$IUCNThreatenedYN==1])[-1]
ThreatenedParasites<-HostsandParasites[HostsandParasites$H_SP %in% ThreatenedSpp,] 
table(ThreatenedParasites$P_T)
unique(ThreatenedParasites$P_F)
ParasiteGeneralism2<-as.data.frame(table(ThreatenedParasites$P_SP))
hist(ParasiteGeneralism2[,2]) #Mostly from a single host spp
SpecialistYN2<-rep(1, length(ParasiteGeneralism2[,2])); SpecialistYN2[ParasiteGeneralism2[,2] > 1]<-0 
table(SpecialistYN2)
78/(78+6) #93% specialists 
(78-6)/834
16/61
#Count number of unique psites per host
#this is clunky...but it works
HostsandParasites$SpecialistYN<-NA
for(i in 1:length(SpecialistYN)) {
  HostsandParasites$SpecialistYN[HostsandParasites$P_SP==ParasiteGeneralism[i,1]]<-SpecialistYN[i]
}
tail(table(HostsandParasites$P_SP, HostsandParasites$SpecialistYN))
tail(ParasiteGeneralism) #ok

SppRichness$NumUniquePsites<-aggregate(HostsandParasites$SpecialistYN, by=list(HostsandParasites$H_SP), FUN=sum)[,2]
hist(SppRichness$NumUniquePsites, breaks=30)
SppRichness$H_SP[SppRichness$NumUniquePsites==29]
min(SppRichness$NumUniquePsites); mean(SppRichness$NumUniquePsites); max(SppRichness$NumUniquePsites)
#0; 5; 42
names(sort(-table(SppRichness$NumUniquePsites)))[1] #mode = 0

#Look at potential host characteristic correlates of specialist richness
#First look at each individually, then use multiple regression w/ uncorrelated
plot(SppRichness$NumUniquePsites~SppRichness$MaxL)
hist(SppRichness$MaxL)
hist(log(SppRichness$MaxL)) #actually that looks good without removing HIP
plot(SppRichness$NumUniquePsites~log(SppRichness$MaxL))
#marginal
test<-glm.nb(SppRichness$NumUniquePsites~log(SppRichness$MaxL)); summary(test)
FakeMaxL<-log(seq(1, max(SppRichness$MaxL), 1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeMaxL)
lines(PredRich~FakeMaxL, lwd=2, col="red")
lines(lowess(SppRichness$NumUniquePsites~log(SppRichness$MaxL)))
plot(residuals(test))
plot(residuals(test)~log(SppRichness$MaxL))

SppRichness$K<-as.numeric(SppRichness$K)
plot(SppRichness$NumUniquePsites~SppRichness$K)
hist(SppRichness$K)
hist(log(SppRichness$K+1))
plot(SppRichness$NumUniquePsites~log(SppRichness$K+1))
test<-glm.nb(SppRichness$NumUniquePsites~log(SppRichness$K+1)); summary(test) #SIG
#coef for log(SppRichness$K+1) is -1.4098
FakeK<-log(seq(0.0001, max(SppRichness$K, na.rm=T), 0.1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeK)
lines(PredRich~FakeK, lwd=2, col="red")
plot(residuals(test))

plot(SppRichness$NumUniquePsites~SppRichness$Y)
hist(SppRichness$Y)
hist(log(SppRichness$Y+1))
plot(SppRichness$NumUniquePsites~log(SppRichness$Y+1))
test<-glm.nb(SppRichness$NumUniquePsites~log(SppRichness$Y+1)); summary(test)
FakeY<-log(seq(0.0001, max(SppRichness$Y, na.rm=T), 1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeY)
lines(PredRich~FakeY, lwd=2, col="red")
plot(residuals(test))

plot(SppRichness$NumUniquePsites~SppRichness$Ym)
hist(SppRichness$Ym)
hist(log(SppRichness$Y+1))
plot(SppRichness$NumUniquePsites~log(SppRichness$Ym+1))
test<-glm.nb(SppRichness$NumUniquePsites~log(SppRichness$Ym+1)); summary(test)
FakeYm<-log(seq(0.0001, max(SppRichness$Ym, na.rm=T), 0.1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeYm)
lines(PredRich~FakeYm, lwd=2, col="red")
plot(residuals(test))

SppRichness$T<-as.numeric(SppRichness$T)
plot(SppRichness$NumUniquePsites~SppRichness$T)
hist(SppRichness$T)
test<-glm.nb(SppRichness$NumUniquePsites~SppRichness$T); summary(test)
plot(SppRichness$NumUniquePsites~SppRichness$T)
FakeT<-(seq(0.0001, max(SppRichness$T, na.rm=T), 0.1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeT)
lines(PredRich~FakeT, lwd=2, col="red")
plot(residuals(test))

SppRichness$AOO<-as.numeric(SppRichness$AOO)
plot(SppRichness$NumUniquePsites~SppRichness$AOO)
hist(SppRichness$AOO)
hist(log(SppRichness$AOO+1))
plot(SppRichness$NumUniquePsites~log(SppRichness$AOO+1)) #maybe a bit better
test<-glm.nb(SppRichness$NumUniquePsites~log(SppRichness$AOO+1)); summary(test)
FakeAOO<-log(seq(0, max(SppRichness$AOO, na.rm=T), 10)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeAOO)
lines(PredRich~FakeAOO, lwd=2, col="red")
plot(residuals(test))

hist(SppRichness$Vulnerability)
test<-glm.nb(SppRichness$NumUniquePsites~SppRichness$Vulnerability); summary(test)
plot(SppRichness$NumUniquePsites~SppRichness$Vulnerability)
FakeVul<-seq(0, 100, 1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeVul)
lines(PredRich~FakeVul, lwd=2, col="red")
plot(residuals(test)) #marg

#Multiple regression model
test<-glm.nb(SppRichness$NumUniquePsites~log(SppRichness$K+1) + SppRichness$T + log(SppRichness$AOO+1)); summary(test)
#coef for log(SppRichness$K+1) is -1.8275
plot(SppRichness$NumUniquePsites~log(SppRichness$K+1))
FakeK<-log(seq(0.0001, max(SppRichness$K, na.rm=T), 0.1)+1); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeK+test$coefficients[3]*mean(SppRichness$T, na.rm=T)+test$coefficients[4]*mean(log(SppRichness$AOO + 1), na.rm=T))
lines(PredRich~FakeK, lwd=2, col="red")
plot(residuals(test))
#library(car)
vif(test)

#########################################################################################
####Missing host spp in database####################################################
#########################################################################################
#Not all of the MA host spp from the FishBase list had parasites in the Strona database
length(unique(FishSpp$ValidatedLatinName)) #208
length(unique(HostsandParasites$H_SP)) #only 116/208 of the fish spp are in the Strona database
116/208
MissingHosts<-FishSpp$LatinName[!(FishSpp$ValidatedLatinName %in% HostsandParasites$H_SP)] #list of missing host spp
MissingHosts #92 missing 
(78-6)/834
FishSpp$MissingYN<-1; FishSpp$MissingYN[(FishSpp$ValidatedLatinName %in% HostsandParasites$H_SP)]<-0
table(FishSpp$MissingYN)

table(FishSpp$IUCN_Code, FishSpp$MissingYN, useNA = "ifany")
FishSpp$LCYN<-0; FishSpp$LCYN[FishSpp$IUCN_Code=="LC"]<-1
xyplot(jitter(FishSpp$MissingYN)~as.factor(FishSpp$IUCN_Code))
table(FishSpp$MissingYN, FishSpp$LCYN)
test<-glm(FishSpp$MissingYN~FishSpp$LCYN, family="binomial"); summary(test)

table(FishSpp$CurrentPopTrend[FishSpp$MissingYN==1], FishSpp$IUCN[FishSpp$MissingYN==1])
FishSpp$RiskyMissingHostsYN<-0; FishSpp$RiskyMissingHostsYN[FishSpp$MissingYN==1 & FishSpp$IUCN!="LC" & FishSpp$IUCN!="Not Evaluated"]<-1

#There doesn't seem to be any rhyme or reason to why host spp are missing
plot(FishSpp$MissingYN~FishSpp$TrophicLevel)
plot(FishSpp$MissingYN~FishSpp$IntrinsicVulnerability)
plot(FishSpp$MissingYN~FishSpp$K)
test<-glm(FishSpp$MissingYN~FishSpp$K, family="binomial"); summary(test)
FishSpp$Lengthcm
FishSpp$LengthNumeric<-as.numeric(sapply(strsplit(FishSpp$Lengthcm, " "), "[", 1))
hist(log(FishSpp$LengthNumeric+1))
plot(FishSpp$MissingYN~log(FishSpp$LengthNumeric+1))

##########################################################################
############Predict Num of Missing Parasite Spp###########################
##########################################################################
FishSpp$LatinName[FishSpp$RiskyMissingHostsYN==1]

#These aren't a perfect match, and I don't know why. Database changes?
plot(FishSpp$K[FishSpp$MissingYN==0]~SppRichness$K)

#Make sure trends (lack thereof) look same w/ 0s included
plot(FishSpp$SpecialistRichness~log(FishSpp$LengthNumeric+1))
plot(FishSpp$SpecialistRichness~FishSpp$TrophicLevel)
plot(FishSpp$SpecialistRichness~FishSpp$IntrinsicVulnerability)
plot(FishSpp$SpecialistRichness~log(FishSpp$K+1))
xyplot(jitter(FishSpp$SpecialistRichness)~as.factor(FishSpp$CurrentPopTrend))
xyplot(jitter(FishSpp$SpecialistRichness)~as.factor(FishSpp$IUCN))

FishSpp$SpecialistRichness<-rep(0, length(FishSpp$LatinName))
for(i in 1:length(FishSpp$LatinName[FishSpp$MissingYN==0])) {
  FishSpp$SpecialistRichness[FishSpp$MissingYN==0][i]<-SppRichness$NumUniquePsites[FishSpp$LatinName[FishSpp$MissingYN==0][i]==SppRichness$H_SP]
}
plot(FishSpp$SpecialistRichness[FishSpp$MissingYN==0]~SppRichness$NumUniquePsites)

#Pred based on model for specialist/unique psite spp that have some psites
test<-glm.nb(FishSpp$SpecialistRichness[FishSpp$MissingYN==0]~log(FishSpp$K[FishSpp$MissingYN==0]+1)); summary(test)
plot(SppRichness$NumUniquePsites~log(SppRichness$K+1))
pred<-exp(test$coefficients[1]+test$coefficients[2]*log(seq(0, 2, 0.1)+1))
lines(pred~log(seq(0, 2, 0.1)+1))
points(FishSpp$SpecialistRichness[FishSpp$RiskyMissingHostsYN==1]~FishSpp$K[FishSpp$RiskyMissingHostsYN==1], col="red")

PredSpecialists<-exp(test$coefficients[1]+test$coefficients[2]*log(FishSpp$K+1))
hist(PredSpecialists)
plot(PredSpecialists~FishSpp$K, ylim=c(0, 13))
plot(PredSpecialists~FishSpp$SpecialistRichness, ylim=c(0, 13))
X<-seq(0, 30, 1)
lines(1*X~X) #That looks  awful... should be 1:1

plot(SppRichness$NumUniquePsites~log(SppRichness$K+1))
abline(h=mean(SppRichness$NumUniquePsites))
points(FishSpp$SpecialistRichness[FishSpp$RiskyMissingHostsYN==1]~FishSpp$K[FishSpp$RiskyMissingHostsYN==1], col="red")
length(which(FishSpp$RiskyMissingHostsYN==1))*mean(SppRichness$NumUniquePsites)

##########################################################################
##Specimens that could be dissected, as counted on VertNet################
##########################################################################
#You can only do up to 1000 records per search w/out emailing big lists
#So it's easier to pull from the Web
searchbyterm(FishSpp$ValidatedLatinName[1])
CBayVertSearch<-vertsearch(taxon = NULL, "Chesapeake Bay", compact = TRUE)

#already deleted rows for mammals, birds, reptiles, fossil otoliths
#genus only entries, and dry specimens
ChesapeakeBayVertNet <- read_delim("~/Documents/ParasiteConservationOOS and Project/Western Atlantic Parasites/ChesapeakeBayVertNet.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Individualcount says how many ind per jar/container/tank/lot/etc
table(ChesapeakeBayVertNet$individualcount, useNA = "ifany")
#247 NAs. Let's conservatively say NA=1
ChesapeakeBayVertNet$individualcount[is.na(ChesapeakeBayVertNet$individualcount)]<-1
table(ChesapeakeBayVertNet$individualcount, useNA = "ifany")

#7 records have individualcount=0, because they're missing. Remove them
View(ChesapeakeBayVertNet[ChesapeakeBayVertNet$individualcount==0,])
ChesapeakeBayVertNet<-ChesapeakeBayVertNet[ChesapeakeBayVertNet$individualcount>0,]
names(ChesapeakeBayVertNet)

table(ChesapeakeBayVertNet$institutioncode)
tail(sort(ChesapeakeBayVertNet$year), 100)

#Sum number of specimens per year
plot(ChesapeakeBayVertNet$individualcount~ChesapeakeBayVertNet$year, xlim=c(1850, 2018), ylab="Number of Fluid Preserved Specimens")
Year<-seq(1850, 2018, 1)
MuseumCountsYear<-aggregate(ChesapeakeBayVertNet$individualcount, by=list(ChesapeakeBayVertNet$year, ChesapeakeBayVertNet$scientificname), FUN=sum)
names(MuseumCountsYear)<-c("Year", "LatinName","Count")
plot(MuseumCountsYear$Count~MuseumCountsYear$Year, xlim=c(1850, 2018), ylab="Number of Fluid Preserved Specimens", pch=16, col=rgb(0,0,0, 0.5))

MuseumCountsYear$LCYN<-NA
for(i in 1:length(MuseumCountsYear$LCYN)) {
  ifelse(length(which(FishSpp$ValidatedLatinName==MuseumCountsYear$LatinName[i])
), MuseumCountsYear$LCYN[i]<-FishSpp$LCYN[FishSpp$ValidatedLatinName==MuseumCountsYear$LatinName[i]], MuseumCountsYear$LCYN[i]<-NA)
}

par(mar=c(2,4,1,1))
plot(MuseumCountsYear$Count[MuseumCountsYear$LCYN==1]~jitter(MuseumCountsYear$Year[MuseumCountsYear$LCYN==1], 0.2), xlim=c(1850, 2018), ylab="Number of fluid preserved specimens", pch=16, col=rgb(0,0,0, alpha=0.5))
points(MuseumCountsYear$Count[MuseumCountsYear$LCYN==0]~jitter(MuseumCountsYear$Year[MuseumCountsYear$LCYN==0], 0.2), pch=16, col=rgb(255/255,128/255, 0, alpha=0.5))

#Sum number of specimens per host spp
unique(ChesapeakeBayVertNet$scientificname) #282 spp
MuseumCounts<-aggregate(ChesapeakeBayVertNet$individualcount, by=list(ChesapeakeBayVertNet$scientificname), FUN=sum)
names(MuseumCounts)<-c("LatinName", "Count")
MuseumCounts$LatinName[MuseumCounts$LatinName=="Alosa pesudoharengus"]<-"Alosa pseudoharengus"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Alosa pseuoharengus"]<-"Alosa pseudoharengus"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Anguilla anguilla rostrata"]<-"Anguilla rostrata"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Brevoortia tryannus"]<-"Brevoortia tyrannus"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Centropristis striatus"]<-"Centropristis striata"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Chasmodes bosquianus bosquianus"]<-"Chasmodes bosquianus"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Coelorinchus coelorinchus carminatus"]<-"Coelorinchus carminatus"
MuseumCounts$LatinName[MuseumCounts$LatinName=="Esox americanusamericanus"]<-"Esox americanus americanus"

#MuseumValidatedNames<-validate_names(MuseumCounts$LatinName)
#warnings()

FishSpp$SpecimenCounts<-NA
for(i in 1:length(FishSpp$SpecimenCounts)) {
  Count<-MuseumCounts$Count[MuseumCounts$LatinName==FishSpp$ValidatedLatinName[i]]
  ifelse(length(Count), FishSpp$SpecimenCounts[i]<-Count, FishSpp$SpecimenCounts[i]<-0)
}

hist(FishSpp$SpecimenCounts, breaks=40)
hist(FishSpp$SpecimenCounts[FishSpp$SpecimenCounts > 2], breaks=40)
FishSpp[FishSpp$SpecimenCounts>3000,]

plot(FishSpp$SpecimenCounts~FishSpp$Vulnerability)
xyplot(FishSpp$SpecimenCounts~as.factor(FishSpp$IUCN_Code))

par(mar=c(2,4,1,1))
plot(FishSpp$SpecimenCounts[FishSpp$LCYN==1]~jitter(FishSpp$MissingYN[FishSpp$LCYN==1], 0.2), xlim=c(-0.5, 1.5), xaxt="n", xlab=NA, ylab="Number of fluid preserved specimens", pch=16, col=rgb(0,0,0, alpha=0.5))
points(FishSpp$SpecimenCounts[FishSpp$LCYN==0]~jitter(FishSpp$MissingYN[FishSpp$LCYN==0], 0.2), pch=16, col=rgb(255/255,128/255, 0, alpha=0.5))
axis(side = 1, at = c(0, 1), tick = 1, line = 0, labels = c("Not missing", "Missing"))

sum(FishSpp$SpecimenCounts[FishSpp$MissingYN==1]) #9598
sum(FishSpp$SpecimenCounts[FishSpp$LCYN==0]) #2291
sum(FishSpp$SpecimenCounts) #20585

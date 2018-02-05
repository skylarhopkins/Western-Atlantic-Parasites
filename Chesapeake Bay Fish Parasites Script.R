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

library(dplyr)
library(tidyr)
library(readr)

library(MASS)

#########################################################################################
#################Create a list of western Atlantic fish spp##################################
#########################################################################################
##Import the list of Chesapeake Bay marine fish species that you can from FishBase. 
#I already removed the freshwater species
FishSpp <- read_csv("~/Documents/Western Atlantic Parasites/ChesapeakeFishResilience.csv")
head(FishSpp)
length(FishSpp$LatinName) #186 spp

#separate genus and species for use later
#We get an error for cases with subspecies, because they have three words in the latin
#name, but it doesn't affect outcome for this particular spp list
FishSpp<-separate(data=FishSpp, col=LatinName, into=c("Genus", "species"), sep=" ", remove=F)
FishSpp<-as.data.frame(FishSpp)
#View(FishSpp)

#Pull resilience category into it's own column
head(FishSpp$Resilience)
FishSpp$ResilienceCategory<-sapply(strsplit(FishSpp$Resilience, ","), "[", 1)

#########################################################################################
########################Historical vulnerability##################################
#########################################################################################
#"FishBase computes vulnerability values using a fuzzy expert system integrating 
#biological and ecological characteristics of fish species18. These values of host 
#vulnerability range from 0 (minimum vulnerability) to 100 (maximum vulnerability). 
#We used these values (divided by 100) as measures of the probabilities of host 
#species to go extinct." Also see Cheung et al. 2005. Biological Conservation.

hist(FishSpp$IntrinsicVulnerability, breaks=30)
xyplot(FishSpp$IntrinsicVulnerability~as.factor(FishSpp$IUCN))
#xyplot(FishSpp$IntrinsicVulnerability~as.factor(FishSpp$ResilienceCategory))
xyplot(FishSpp$IntrinsicVulnerability~jitter(as.numeric(as.factor(FishSpp$IUCN))))

#We can also pull vulnerabilities from FishBase
#these ones aren't rounded, but a few are missing, even though I could
#find them on the web
Temp<-species(FishSpp$LatinName, fields=c("Vulnerability", "Fresh", "Brack", "Saltwater"))
FishSpp<-(merge(FishSpp, Temp, by.x="LatinName", by.y="sciname", all.x=TRUE))
plot(FishSpp$IntrinsicVulnerability~FishSpp$Vulnerability)

#I'm not sure why, but FishBase put some (35) freshwater spp in the list
#but are also marine/brackish 
table(FishSpp$Fresh, useNA = "ifany") #35 fresh
table(FishSpp$Fresh, FishSpp$Saltwater) #35 fresh and marine
table(FishSpp$Fresh, FishSpp$Brack) 

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
HostsandParasites<-subset(Strona, H_SP %in% FishSpp$LatinName)
HostsandParasites$UniqueHspPsp<-paste(HostsandParasites$H_SP, HostsandParasites$P_SP, "")

##How many species pairs have unknown geography? Strona used "na"
length(which(HostsandParasites$GEO=="na")) #285

#Let's keep a list of them to check later, but then remove them 
ParasitesWithoutGeo<-HostsandParasites$UniqueHspPsp[HostsandParasites$GEO=="na"]

#Just keep the confirmed Nearctic parasites
HostsandParasites<-HostsandParasites[HostsandParasites$GEO=="NEA",]

length(HostsandParasites$UniqueHspPsp)
length(unique(HostsandParasites$UniqueHspPsp))
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
length(unique(HostsandParasites$UniqueHspPsp)) #733 unique host-parasite links

#NOTE: Worried that we have a few cases where the same parasite in a host spp
#is counted as two, due to one misspecified genus. Should fix later.

#########################################################################################
####Missing host spp in database####################################################
#########################################################################################
#Not all of the MA host spp from the FishBase list had parasites in the Strona database
length(unique(FishSpp$LatinName))
length(unique(HostsandParasites$H_SP)) #only 84/174 of the fish spp are in the Strona database
MissingHosts<-FishSpp$LatinName[!(FishSpp$LatinName %in% HostsandParasites$H_SP)] #list of missing host spp
MissingHosts #90 missing 

#########################################################################################
###################Describe parasite diversity#####################################
#########################################################################################
#Make a dataset w/ each parasite spp listed just once (ignore host info)
UniqueParasites<-HostsandParasites[!duplicated(HostsandParasites$P_SP),]
table(UniqueParasites$P_T) #* means parasite name isn't validated
#Acan = 39
#Cestodes = 67
#Monogeneans = 113
#Nematodes = 49
#Trematodes = 172
length(UniqueParasites$P_SP) #440 species
length(unique(UniqueParasites$P_F)) #106 families

#########################################################################################
###################Parasite species richness###########################################
#########################################################################################
SppRichness<-aggregate(HostsandParasites, by = list(HostsandParasites$H_SP), FUN=max)
SppRichness<-SppRichness[,-1]
SppRichness$ParasiteRichness<-aggregate(HostsandParasites$P_SP, by = list(HostsandParasites$H_SP), FUN=length)[,2]
hist(SppRichness$ParasiteRichness, breaks = 40)
min(SppRichness$ParasiteRichness); mean(SppRichness$ParasiteRichness); max(SppRichness$ParasiteRichness)
#1; 8.72619; 55
SppRichness[SppRichness$ParasiteRichness==55,]

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
hist(log(SppRichness$K))
plot(SppRichness$ParasiteRichness~log(SppRichness$K))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$K)); summary(test)
FakeK<-log(seq(0.0001, max(SppRichness$K, na.rm=T), 0.1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeK)
lines(PredRich~FakeK, lwd=2, col="red")
plot(residuals(test))

plot(SppRichness$ParasiteRichness~SppRichness$Y)
hist(SppRichness$Y)
hist(log(SppRichness$Y+1))
plot(SppRichness$ParasiteRichness~log(SppRichness$Y+1))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$Y+1)); summary(test)
FakeY<-log(seq(0.0001, max(SppRichness$Y, na.rm=T), 1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeY)
lines(PredRich~FakeY, lwd=2, col="red")
plot(residuals(test))

plot(SppRichness$ParasiteRichness~SppRichness$Ym)
hist(SppRichness$Ym)
hist(log(SppRichness$Y+1))
plot(SppRichness$ParasiteRichness~log(SppRichness$Ym+1))
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$Ym+1)); summary(test)
FakeYm<-log(seq(0.0001, max(SppRichness$Ym, na.rm=T), 0.1)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeYm)
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
test<-glm.nb(SppRichness$ParasiteRichness~log(SppRichness$AOO+1)); summary(test)
FakeAOO<-log(seq(0, max(SppRichness$AOO, na.rm=T), 10)); PredRich<-exp(test$coefficients[1]+test$coefficients[2]*FakeAOO)
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
#K and Y and Y and Ym super correlated - still w/ Y
names(SppRichness)
pairs(SppRichness[c(10, 13:14, 18, 23)])
#MaxL and Ym w/ vulnerability, of course. So let's go w/ trophic level and vulnerability
#AOO might be correlated w/ those, but we'll check it w/ VIFs
pairs(SppRichness[c(14, 18, 23)])
plot(log(SppRichness$AOO+1)~SppRichness$T)
plot(log(SppRichness$AOO+1)~SppRichness$Vulnerability)

#Multiple regression model
test<-glm.nb(SppRichness$ParasiteRichness~SppRichness$Vulnerability + SppRichness$T + log(SppRichness$AOO+1)); summary(test)
plot(residuals(test))


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

library(lattice)
library(MASS)

library(rvertnet)
#########################################################################################
#################Create a list of western Atlantic fish spp##################################
#########################################################################################
##Import the list of Chesapeake Bay marine fish species that you can from FishBase. 
#I already removed the freshwater species
FishSpp <- read_csv("~/Documents/Western Atlantic Parasites/ChesapeakeFishResilience.csv")
head(FishSpp)
length(FishSpp$LatinName) #174 spp

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
View(FishSpp)

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
names(sort(-table(SppRichness$ParasiteRichness)))[1] #mode = 1
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
UniqueParasites<-sort(unique(HostsandParasites$P_SP)) #440 unique parasites
ParasiteGeneralism<-as.data.frame(table(HostsandParasites$P_SP))
hist(ParasiteGeneralism[,2], breaks=30) #WOW. Most from just one host spp!!!!
max(ParasiteGeneralism[,2]) #shared by up to 14 host spp
SpecialistYN<-rep(1, length(ParasiteGeneralism[,2])); SpecialistYN[ParasiteGeneralism[,2] > 1]<-0 
table(SpecialistYN) #only 95 parasites are shared among two or more hosts

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
#0; 4.107143; 29
names(sort(-table(SppRichness$NumUniquePsites)))[1] #mode = 2

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
length(unique(FishSpp$LatinName)) #174
length(unique(HostsandParasites$H_SP)) #only 84/174 of the fish spp are in the Strona database
MissingHosts<-FishSpp$LatinName[!(FishSpp$LatinName %in% HostsandParasites$H_SP)] #list of missing host spp
MissingHosts #90 missing 

FishSpp$MissingYN<-1; FishSpp$MissingYN[(FishSpp$LatinName %in% HostsandParasites$H_SP)]<-0
table(FishSpp$MissingYN)

table(FishSpp$IUCN, FishSpp$MissingYN, useNA = "ifany")
FishSpp$LCYN<-0; FishSpp$LCYN[FishSpp$IUCN=="LC"]<-1
xyplot(jitter(FishSpp$MissingYN)~as.factor(FishSpp$IUCN))
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
searchbyterm(FishSpp$LatinName[1])
CBayVertSearch<-vertsearch(taxon = NULL, "Chesapeake Bay", compact = TRUE)

#already deleted rows for mammals, birds, reptiles, fossil otoliths
#genus only entries, and dry specimens
ChesapeakeBayVertNet <- read_delim("~/Documents/Western Atlantic Parasites/ChesapeakeBayVertNet.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Individualcount says how many ind per jar/container/tank/lot/etc
table(ChesapeakeBayVertNet$individualcount, useNA = "ifany")
#141 NAs. Let's conservatively say NA=1
ChesapeakeBayVertNet$individualcount[is.na(ChesapeakeBayVertNet$individualcount)]<-1
table(ChesapeakeBayVertNet$individualcount, useNA = "ifany")

#8 records have individualcount=0, because they're missing. Remove them
View(ChesapeakeBayVertNet[ChesapeakeBayVertNet$individualcount==0,])
ChesapeakeBayVertNet<-ChesapeakeBayVertNet[ChesapeakeBayVertNet$individualcount>0,]
names(ChesapeakeBayVertNet)

table(ChesapeakeBayVertNet$institutioncode)
tail(sort(ChesapeakeBayVertNet$year), 100)

#Sum number of specimens per host spp
unique(ChesapeakeBayVertNet$scientificname) #287 spp
MuseumCounts<-aggregate(ChesapeakeBayVertNet$individualcount, by=list(ChesapeakeBayVertNet$scientificname), FUN=sum)
names(MuseumCounts)<-c("LatinName", "Count")

FishSpp$SpecimenCounts<-NA
for(i in 1:length(FishSpp$SpecimenCounts)) {
  Count<-MuseumCounts$Count[MuseumCounts$LatinName==FishSpp$LatinName[i]]
  ifelse(length(Count), FishSpp$SpecimenCounts[i]<-Count, FishSpp$SpecimenCounts[i]<-0)
}

hist(FishSpp$SpecimenCounts, breaks=40)
hist(FishSpp$SpecimenCounts[FishSpp$SpecimenCounts > 2], breaks=40)
FishSpp[FishSpp$SpecimenCounts>3000,]

hist(FishSpp$TrophicLevel)
plot(FishSpp$SpecimenCounts~FishSpp$TrophicLevel)
plot(FishSpp$SpecimenCounts~FishSpp$IntrinsicVulnerability)
xyplot(FishSpp$SpecimenCounts~as.factor(FishSpp$ResilienceCategory))
xyplot(FishSpp$SpecimenCounts~as.factor(FishSpp$IUCN))
plot(FishSpp$SpecimenCounts~FishSpp$PhylogeneticDiversityIndex)
plot(FishSpp$SpecimenCounts~FishSpp$MissingYN)
sum(FishSpp$SpecimenCounts[FishSpp$MissingYN==1]) #9511
sum(FishSpp$SpecimenCounts) #23220

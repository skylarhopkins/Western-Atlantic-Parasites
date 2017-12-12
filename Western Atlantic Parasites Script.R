#Skylar Hopkins, NCEAS 2017

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

#########################################################################################
####Create a list of western Atlantic parasite spp using Strona dataset###################
#########################################################################################
#Strona et al. (2013) published a list of ~11,800 fish parasites which is available at
#DOI: 10.1890/12-1419.1 and http://esapubs.org/archive/ecol/E094/045/#data
#This probably isn't a complete list, but we can use it to get started
Strona <- read_csv("~/Documents/Smithsonian Proposal/StronaFishParasiteDataset.csv")
names(Strona)
Strona<-as.data.f
rame(Strona)

#Strona$H_SP corresponds to our FishSpp$LatinName category, and both should be based on FishBase names
#so reduce dataset to only the host spp we care about
head(Strona$H_SP) 
WAHostsandParasites<-subset(Strona, H_SP %in% FishSpp$LatinName)

#Because NAs are in the dataset as "na", a bunch of the quantitative
#variables are character vectors
WAHostsandParasites %>% select(H_SP, MaxL, K, Y, Ym, T, AOO)
names(WAHostsandParasites)
WAHostsandParasites[c(10:14,18:20)]<-as.numeric(WAHostsandParasites[c(10:14,18:20)])

length(unique(WAHostsandParasites$H_SP)) #only 89 of the fish spp are in the Strona database?
MissingHosts<-FishSpp$LatinName[!(FishSpp$LatinName %in% WAHostsandParasites$H_SP)] #list of missing host spp
#spot checking shows that at least some missing host spp have parasites listed in Londom NHM Helminth database
findHost(genus = "Acanthurus", species = "chirurgus", removeDuplicates = T, speciesOnly = T)

#For some reason, Lobotes surinamensis isn't listed as marine
WAHostsandParasites$H_SP[WAHostsandParasites$M==0] 

#Let's look for patterns between host traits and parasite spp richness
#First make a dataframe w/ host spp as row/observation, spp richness as tally,
#and carry over info for max host length (MaxL), 
SppRichness<-WAHostsandParasites %>% group_by(H_SP) %>% summarize(SppRichness = n())
hist(SppRichness$SppRichness)
hist(table(WAHostsandParasites$H_SP)) #k - dplyr was slower

X<-aggregate(cbind(WAHostsandParasites$MaxL, WAHostsandParasites$K, WAHostsandParasites$Y, WAHostsandParasites$Ym, WAHostsandParasites$T, WAHostsandParasites$AOO), by=list(WAHostsandParasites$H_SP), FUN=max)

plot(SppRichness$SppRichness~table(table(WAHostsandParasites$H_SP)))

WAHostsandParasites %>% select(H_SP, MaxL, K, Y, Ym, T, AOO)

as.numeric(WAHostsandParasites$K)

SppRichness<-aggregate(WAHostsandParasites, by = list(WAHostsandParasites$H_SP), FUN=length)
names(WAHostsandParasites)

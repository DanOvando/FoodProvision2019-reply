#Food provision code for the Nat Geo project
#Last update: 30 October 2019
#Author: Reniel Cabral

#Clear memory
gc()
rm(list = ls())
#.rs.restartR()

saveme<-0 #if 1, meaning save activated

library(raster)
library(tidyverse)
library(sf)
library(rredlist)
library(furrr)
library(dplyr)
library(miscTools)
library(RColorBrewer)
library(doParallel)
library(reshape)
library(data.table)
library(doParallel)
library(rgdal)
library(maptools)
library(rasterVis)
library(tmap)
library(leaflet)
library(rootSolve)
#options("scipen"=-100, "digits"=4)

PlotFunction<-function(var1){
  # define projections
  behrmannCRS <- CRS('+proj=cea +lat_ts=30')
  data(wrld_simpl)
  mollCRS <- CRS('+proj=moll')
  world_ext = projectExtent(wrld_simpl, crs = behrmannCRS)
  # crop sst to extent of world to avoid overlap on the seam
  sst_crop = crop(x = var1, y=world_ext, snap='in')
  # convert sst to longlat (similar to test file)
  # somehow this gets rid of the unwanted pixels outside the ellipse
  sst_longlat = projectRaster(sst_crop, crs = ('+proj=longlat'))
  # then convert to mollweide
  sst_moll <- projectRaster(sst_longlat, crs=mollCRS, over=T)
  return(sst_moll)
}

#--check total K using the aquamaps data
#load the expert data and save as RDS for faster loading
#Aquaexpert<-read.csv("/Users/ren/Documents/CODES/FoodProvision/Aquamaps/spatial-datasets_Aquamaps_complete_current_data_all_hcaf_species_native_expert.csv")
#saveRDS(Aquaexpert, file = "/Users/ren/Documents/CODES/FoodProvision/Aquamaps/Aquaexpert.rds")
Aquaexpert2<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Aquamaps/Aquaexpert.rds")
head(Aquaexpert2)

#stack the species distribution by summing the probabilities then plot
speciesstack<-Aquaexpert2 %>% group_by(CenterLat,CenterLong) %>% summarise(S=sum(probability))
speciesstack2<-as.data.frame(speciesstack)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(speciesstack2[,2:1]))
empty_raster[cells] <- speciesstack2[,3]
plot(empty_raster,main="Sum of all species suitability, expert")

if(saveme==1){
png(file="/Users/ren/Documents/CODES/FoodProvision/Results/SumSuitabilityExpert.png", width = 6, height = 4, units = 'in', res = 300)
plot(PlotFunction(empty_raster),zlim=c(0,maxValue(empty_raster)),main="Sum of all species suitability, expert",axes=F,box=F,legend=F)
plot(empty_raster, zlim=c(0,maxValue(empty_raster)),legend.only=TRUE,legend.width=1, legend.shrink=0.75,axis.args=list(cex.axis=0.5),
     legend.args=list(text='', side=4, font=2, line=2.5, cex=0.8))
dev.off()
}

##load the other data and bind with expert-vetted data
#Aquaothers<-read.csv("/Users/ren/Documents/CODES/FoodProvision/Aquamaps/spatial-datasets_Aquamaps_complete_current_data_all_hcaf_species_native.csv")
#saveRDS(Aquaothers, file = "/Users/ren/Documents/CODES/FoodProvision/Aquamaps/Aquaothers.rds")
Aquaothers2<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Aquamaps/Aquaothers.rds")
Aquaothers2<-rbind(Aquaothers2,Aquaexpert2)
head(Aquaothers2)
dim(Aquaothers2)

speciesstackothers<-Aquaothers2 %>% group_by(CenterLat,CenterLong) %>% summarise(S=sum(probability))
speciesstack2others<-as.data.frame(speciesstackothers)
empty_rasterothers <- raster(res = 0.5)
cellsothers <- cellFromXY(empty_rasterothers, as.matrix(speciesstack2others[,2:1]))
empty_rasterothers[cellsothers] <- speciesstack2others[,3]
plot(empty_rasterothers,main="Sum of all species suitability")

if(saveme==1){
png(file="/Users/ren/Documents/CODES/FoodProvision/Results/SumSuitabilityAll.png", width = 6, height = 4, units = 'in', res = 300)
#plot(empty_rasterothers,main="Sum of all species suitability")
plot(PlotFunction(empty_rasterothers),zlim=c(0,maxValue(empty_rasterothers)),main="Sum of all species suitability",axes=F,box=F,legend=F)
plot(empty_rasterothers, zlim=c(0,maxValue(empty_rasterothers)),legend.only=TRUE,legend.width=1, legend.shrink=0.75,axis.args=list(cex.axis=0.5),
     legend.args=list(text='', side=4, font=2, line=2.5, cex=0.8))
dev.off()
}

#Load K from Costello et al. 2016 database
CostelloData<-read.csv("/Users/ren/Documents/CODES/FoodProvision/Aquamaps/UnlumpedProjectionData.csv")
#CostelloData<-read.csv("/Users/ren/Documents/CODES/FoodProvision/Aquamaps/ProjectionData.csv")
head(CostelloData,5)

Costello2012<-CostelloData %>% filter(Year=="2012")
table(Costello2012$Dbase)
table(Costello2012$Policy)
table(Costello2012$Scenario)
head(Costello2012)
#MSY from costello of RAM, FAO, and SOFIA
Costello2012 %>% group_by(Dbase,CatchShare) %>% summarise(sum(MSY))

#MSY here// explore only. can be deleted.
Costello2012$Kprime<-(Costello2012$MSY*(1.188)^(1/0.188))/Costello2012$g
plot(Costello2012$k,Costello2012$Kprime)

# #check 2050 stat
# #filter === 2050, BAU, All Stocks, not catch share, not in RAM
# Costello2050<-CostelloData %>% filter(Year=="2050", Policy=="BAU", Scenario=="All Stocks", CatchShare==0, Dbase!="RAM")
# head(Costello2050)
# table(Costello2050$Dbase)
# table(Costello2050$Policy)
# table(Costello2050$Scenario)
# table(Costello2050$CatchShare)
# head(Costello2050)
# min(Costello2050$Catch)
# min(Costello2050$Biomass)
# Costello2050 %>% filter(Biomass==0)
# plot(Costello2050$Profits)  

CostelloDataPrime<- CostelloData %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops melanostictus", "Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops caeruleus", "Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops ocellatus", "Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Merluccius capensis, M.paradoxus", "Merluccius capensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Auxis thazard, A. rochei", "Auxis thazard")) %>%
  mutate(SciName=replace(SciName, SciName=="Pleuronectes quadrituberculat.", "Pleuronectes quadrituberculat")) %>%
  mutate(SciName=replace(SciName, SciName=="Pseudopleuronectes herzenst.", "Pseudopleuronectes herzenst")) %>%
  mutate(SciName=replace(SciName, SciName=="Herklotsichthys quadrimaculat.", "Herklotsichthys quadrimaculat")) %>%
  mutate(SciName=replace(SciName, SciName=="Engraulis capensis", "Engraulis encrasicolus")) %>%
  mutate(SciName=replace(SciName, SciName=="Trachypenaeus curvirostris", "Trachysalambria curvirostris")) %>%
  mutate(SciName=replace(SciName, SciName=="Patinopecten yessoensis", "Mizuhopecten yessoensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Penaeus setiferus", "Litopenaeus setiferus")) %>%
  mutate(SciName=replace(SciName, SciName=="Loligo opalescens", "Doryteuthis opalescens")) %>%
  mutate(SciName=replace(SciName, SciName=="Larimichthys croceus", "Larimichthys crocea")) %>%
  mutate(SciName=replace(SciName, SciName=="Loligo gahi", "Doryteuthis gahi")) %>%
  mutate(SciName=replace(SciName, SciName=="Chelon haematocheilus", "Liza haematocheila")) %>%
  mutate(SciName=replace(SciName, SciName=="Anadara granosa", "Tegillarca granosa")) %>%
  mutate(SciName=replace(SciName, SciName=="Penaeus chinensis", "Fenneropenaeus chinensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Penaeus merguiensis", "Fenneropenaeus merguiensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Sebastes marinus", "Sebastes norvegicus")) %>%
  mutate(SciName=replace(SciName, SciName=="Cancer magister", "Metacarcinus magister")) %>%
  mutate(SciName=replace(SciName, SciName=="Loligo pealeii", "Doryteuthis pealeii")) %>%  
  mutate(SciName=replace(SciName, SciName=="Spisula polynyma", "Mactromeris polynyma")) %>%  
  mutate(SciName=replace(SciName, SciName=="Ommastrephes bartramii", "Ommastrephes bartramii")) %>%  
  mutate(SciName=replace(SciName, SciName=="Stichopus japonicus", "Apostichopus japonicus")) %>%  
  mutate(SciName=replace(SciName, SciName=="Penaeus notialis", "Farfantepenaeus notialis")) %>%  
  mutate(SciName=replace(SciName, SciName=="Psetta maxima", "Scophthalmus maximus")) %>%  
  mutate(SciName=replace(SciName, SciName=="Ostrea lutaria", "Ostrea chilensis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Tawera gayi", "Tawera elliptica")) %>%   
  mutate(SciName=replace(SciName, SciName=="Penaeus japonicus", "Marsupenaeus japonicus")) %>%   
  mutate(SciName=replace(SciName, SciName=="Penaeus brasiliensis","Farfantepenaeus aztecus")) %>%   
  mutate(SciName=replace(SciName, SciName=="Mytilus chilensis","Mytilus edulis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Tetrapturus audax","Kajikia audax" )) %>% 
  mutate(SciName=replace(SciName, SciName=="Cheilodactylus bergi","Nemadactylus bergi")) %>% 
  mutate(SciName=replace(SciName, SciName=="Venerupis pullastra","Venerupis corrugata")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus aztecus","Farfantepenaeus aztecus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus duorarum","Farfantepenaeus duorarum")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus kerathurus","Melicertus kerathurus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus californiensis","Farfantepenaeus californiensis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus brevirostris","Farfantepenaeus brevirostris")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus latisulcatus","Melicertus latisulcatus")) %>%     
  mutate(SciName=replace(SciName, SciName=="Penaeus occidentalis","Litopenaeus occidentalis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus vannamei","Litopenaeus vannamei")) %>% 
  mutate(SciName=replace(SciName, SciName=="Raja naevus","Leucoraja naevus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Jasus novaehollandiae","Jasus edwardsii")) %>% 
  mutate(SciName=replace(SciName, SciName=="Makaira indica","Istiompax indica")) %>% 
  mutate(SciName=replace(SciName, SciName=="Lithodes aequispina","Lithodes aequispinus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Eleginus navaga","Eleginus nawaga")) %>%
  mutate(SciName=replace(SciName, SciName=="Saxidomus giganteus","Saxidomus gigantea")) %>%
  mutate(SciName=replace(SciName, SciName=="Mugil soiuy","Liza haematocheila")) %>%
  mutate(SciName=replace(SciName, SciName=="Xiphopenaeus riveti","Xiphopenaeus kroyeri")) %>%
  mutate(SciName=replace(SciName, SciName=="Pleuronectes vetulus","Parophrys vetulus")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja radiata","Amblyraja radiata")) %>%
  mutate(SciName=replace(SciName, SciName=="Aspitrigla cuculus","Chelidonichthys cuculus")) %>%
  mutate(SciName=replace(SciName, SciName=="Valamugil seheli","Moolgarda seheli")) %>%
  mutate(SciName=replace(SciName, SciName=="Tetrapturus albidus","Kajikia albida")) %>%
  mutate(SciName=replace(SciName, SciName=="Zenopsis nebulosus","Zenopsis nebulosa")) %>%
  mutate(SciName=replace(SciName, SciName=="Arius thalassinus","Netuma thalassinus")) %>%
  mutate(SciName=replace(SciName, SciName=="Parika scaber","Meuschenia scaber")) %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops neopilchardus","Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja batis","Dipturus batis")) %>%
  mutate(SciName=replace(SciName, SciName=="Alosa pontica","Alosa immaculata")) %>%
  mutate(SciName=replace(SciName, SciName=="Conger orbignyanus","Conger orbignianus")) %>%
  mutate(SciName=replace(SciName, SciName=="Acanthopagrus schlegeli","Acanthopagrus schlegelii")) %>%
  mutate(SciName=replace(SciName, SciName=="Solea lascaris","Pegusa lascaris")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja circularis","Leucoraja circularis")) %>%
  mutate(SciName=replace(SciName, SciName=="Balistes carolinensis","Balistes capriscus")) %>%
  mutate(SciName=replace(SciName, SciName=="Plesiopenaeus edwardsianus","Aristaeopsis edwardsiana")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus flavolimbatus","Hyporthodus flavolimbatus")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus niveatus","Hyporthodus niveatus")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus nigritus","Hyporthodus nigritus")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus mystacinus","Hyporthodus mystacinus")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja oxyrinchus","Dipturus oxyrinchus")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja fullonica","Leucoraja fullonica")) %>%
  mutate(SciName=replace(SciName, SciName=="Jasus verreauxi","Sagmariasus verreauxi")) %>%
  mutate(SciName=replace(SciName, SciName=="Anadara ovalis","Lunarca ovalis")) %>%
  mutate(SciName=replace(SciName, SciName=="Pseudopentaceros richardsoni","Pentaceros richardsoni")) %>%
  mutate(SciName=replace(SciName, SciName=="Chelidonichthys lastoviza","Trigloporus lastoviza")) %>%
  mutate(SciName=replace(SciName, SciName=="Protothaca staminea","Leukoma staminea")) %>%
  mutate(SciName=replace(SciName, SciName=="Notothenia squamifrons","Lepidonotothen squamifrons")) %>%
  filter(k>0) #remove zero carrying capacity

CostelloPresentPrime<- CostelloDataPrime %>% filter(Year=="2012")
head(CostelloPresentPrime)

# #compute equilibrium 
# varB<-CostelloPresentPrime %>% mutate(varB=phi*c*(g^1.3)/(Price*MSY)) %>% select(varB)
# fun<-function (x) (x^1.188) - (1.188*x) + varB[2,]
# curve(fun(x),0,1)
# uniroot.all(fun,c(0,1))

CostelloK<-CostelloDataPrime %>% filter(Year=="2012") %>% mutate(k=Biomass/(0.4*BvBmsy)) %>% group_by(SciName) %>% summarize(K=sum(k), B=sum(Biomass), Fstatus=weighted.mean(FvFmsy, MSY), Bstatus=weighted.mean(BvBmsy, MSY)) %>% mutate(BK2012=B/K)
head(CostelloK)
dim(CostelloK)

plot(CostelloK$BK2012)

Costello2050<-CostelloDataPrime %>% filter(Year=="2050", Policy=="BAU", Scenario=="All Stocks", CatchShare==0, Dbase!="RAM") %>%
  #group_by(SciName) %>% summarize(catch2050=sum(Catch), biomass2050=sum(Biomass), k2050=sum(k)) %>% mutate(ER2050=catch2050/biomass2050, bvk2050=biomass2050/k2050)
  group_by(SciName) %>% summarize(catch2050=sum(Catch), biomass2050=sum(Biomass), k2050=sum(k)) %>% mutate(bvk2050=biomass2050/k2050)
head(Costello2050)
dim(Costello2050)

Costello2050ALL<-CostelloDataPrime %>% filter(Year=="2050", Policy=="BAU", Scenario=="All Stocks") %>%
  #group_by(SciName) %>% summarize(catch2050ALL=sum(Catch), biomass2050ALL=sum(Biomass), k2050ALL=sum(k)) %>% mutate(ER2050ALL=catch2050ALL/biomass2050ALL, bvk2050ALL=biomass2050ALL/k2050ALL)
  group_by(SciName) %>% summarize(catch2050ALL=sum(Catch), biomass2050ALL=sum(Biomass), k2050ALL=sum(k)) %>% mutate(bvk2050ALL=biomass2050ALL/k2050ALL)
head(Costello2050ALL)
dim(Costello2050ALL)

#combine the two database
CostelloPresent0<-left_join(CostelloK,Costello2050, by="SciName")
CostelloPresent1<-left_join(CostelloPresent0,Costello2050ALL, by="SciName")
CostelloPresent<-CostelloPresent1

#OLD CODE
# CostelloPresent1$ER2050prime<-CostelloPresent1$ER2050
# CostelloPresent1$ER2050prime[is.na(CostelloPresent1$ER2050prime)] <- -1
# CostelloPresent<- CostelloPresent1 %>% mutate(ERdiff=ER2050-ER2050ALL, ERcostello=(ER2050ALL*((ER2050prime==-1)*1)) + (ER2050prime*(1-((ER2050prime==-1)*1))))
# CostelloPresent %>% filter(is.nan(ERcostello)==T) #we know that expoitation rate is high so ER should be high
# CostelloPresent$ERcostello[is.nan(CostelloPresent$ERcostello)]<-100


# # check UNiQUE r per species
# head(CostelloDataPrime)
# rperspeciescostello<-CostelloDataPrime %>% select(SciName,MSY,g) %>% mutate(rcostello=(4*g)/((0.188+1)^(1/0.188)))
# rperspeciescostelloUNIQUE<-unique(rperspeciescostello[c("SciName", "rcostello")])
# head(rperspeciescostelloUNIQUE)

#rank species
CostelloPresent<-as.data.frame(CostelloPresent)
rankedsp<-CostelloPresent[order(-CostelloPresent$K),] 
#there is an <NA> in the SciName --- remove that
rankedsp<-rankedsp %>% filter(!SciName=="<NA>")

"Sardinops sagax" %in% rankedsp$SciName

head(rankedsp,5)
dim(rankedsp)# there are 1111 unique species/genus/family entries
##next step is to match species with K

#load the species id matching
spnamelookup<-read.csv("/Users/ren/Documents/CODES/FoodProvision/Aquamaps/aquamaps_spp_ref_revised.csv")
spnamelookup<-as.data.frame(spnamelookup)
head(spnamelookup)
dim(spnamelookup) 

"Clupea bentincki" %in% c(as.character(spnamelookup$resolved_scientific_name),
                          as.character(spnamelookup$aquamaps_sci_name),
                          as.character(spnamelookup$worms_sci_name),
                          as.character(spnamelookup$eol_sci_name),
                          as.character(spnamelookup$col_sci_name),
                          as.character(spnamelookup$gbif_sci_name),
                          as.character(spnamelookup$itis_sci_name))

#Species in costello db included
include <-rankedsp %>% filter((SciName %in% spnamelookup$resolved_scientific_name) | 
                                (SciName %in% spnamelookup$aquamaps_sci_name) |
                                (SciName %in% spnamelookup$worms_sci_name) |
                                (SciName %in% spnamelookup$eol_sci_name) |
                                (SciName %in% spnamelookup$col_sci_name) |
                                (SciName %in% spnamelookup$gbif_sci_name) |
                                (SciName %in% spnamelookup$itis_sci_name))
dim(include)
head(include)#these are the species in Costello DB included 

# #add B/K values --- note: Bmsy/K=0.4 so B/K = 0.4B/Bmsy
# Costello2050$BK<-0.4*Costello2050$BvBmsy
# hist(Costello2050$BK) #even when economics is added, the median and mean is 0.153 and mean 0.218.
# mean(Costello2050$BK)
# median(Costello2050$BK)
# max(Costello2050$BK)

#what are the species in costello db not included?
rankedsp %>% filter(!(rankedsp$SciName %in% include$SciName))
#Clean species name mismatch

#what are the species ID of these?
head(spnamelookup)
spID<-include
nudge<-dim(spID)[2]
spID$v1<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$resolved_scientific_name)]#this is correct!
spID$v2<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$aquamaps_sci_name)]
spID$v3<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$worms_sci_name)]
spID$v4<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$eol_sci_name)]
spID$v5<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$col_sci_name)]
spID$v6<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$gbif_sci_name)]
spID$v7<-spnamelookup$SPECIESID[match(include$SciName,spnamelookup$itis_sci_name)]
#spID$fin<-apply(spID[,3:9],1, function(x) unique(x[!is.na(x)]))
spID$fin<-apply(spID[,(1:7)+nudge],1, function(x) unique(x[!is.na(x)]))
head(spID)

#add this spID to "include" file
include$SpeciesID<-spID$fin
head(include)
plot(include$bvk2050-include$bvk2050ALL)

weighted.mean(include$bvk2050,include$K,na.rm=T)
weighted.mean(include$bvk2050ALL,include$K,na.rm=T)
head(include)
dim(include)

#fill NAs in bvk2050
include$bvk2050[is.na(include$bvk2050)] <- -1
include <- include %>% mutate(bvk_fin=(bvk2050ALL*((bvk2050==-1)*1)) + (bvk2050*(1-((bvk2050==-1)*1))))
head(include)

##TRANSFER files to VM for convertion to mollweide
#saveRDS(Aquaothers2, file = "/Users/ren/Documents/CODES/FoodProvision/Aquaothers2.rds")
#saveRDS(include, file = "/Users/ren/Documents/CODES/FoodProvision/include.rds")
#saveRDS(land_shp_moll, file = "/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")


# ###Nov22, transform to mollweide
# ##OPTION 1
# land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
# ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
# land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))
# 
# aquamaps_mollweide<-Aquaothers2 %>% select(SpeciesID,CenterLat,CenterLong,probability)
# dim(aquamaps_mollweide)
# 
# colnames(Aquaothers2)
# head(Aquaothers2)
# 
# sfc_as_cols <- function(x, names = c("x","y")) {
#   stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
#   ret <- sf::st_coordinates(x)
#   ret <- tibble::as_tibble(ret)
#   stopifnot(length(names) == ncol(ret))
#   x <- x[ , !names(x) %in% names]
#   ret <- setNames(ret,names)
#   dplyr::bind_cols(x,ret)
# }
# 
# tmp <- aquamaps_mollweide %>%
#   head(100000) %>%
#   sf::st_as_sf(coords=c("CenterLong","CenterLat")) %>%
#   sf::st_set_crs("+proj=longlat +datum=WGS84") %>%
#   sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
#   sf::st_transform("+proj=moll") %>%
#   sfc_as_cols() %>%
#   as_tibble()
# 
# tmp2 <- tmp %>%
#   #group_by(x, y) %>%
#   group_by(x, y, SpeciesID) %>%
#   summarize(mean(probability))
# 
# dim(tmp2)
# head(tmp2)
# names(tmp2) <- c("CenterLong","CenterLat","SpeciesID","probability")
# 
# Aqua3<-merge(tmp2,include,by="SpeciesID")
# head(Aqua3)
# length(unique(Aqua3$SpeciesID))
# 
# Aqua3 <- Aqua3 %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
# Aqua3stack<-Aqua3 %>% group_by(CenterLat,CenterLong) %>% mutate(Kcell=sum(probability*K/totalprob)) %>% summarise(S=sum(probability*K/totalprob), Fstat=weighted.mean(Fstatus,Kcell), Bstat=weighted.mean(Bstatus,Kcell))
# head(Aqua3stack) #S is total K per cell
# Aqua3stack<-as.data.frame(Aqua3stack)
# 
# dim(Aqua3stack) #160639
# head(Aqua3stack)
# 
# plot(Aqua3stack$CenterLon,Aqua3stack$CenterLat)
# 
# #check other plot
# Aqua3stack %>% 
#   as.data.frame(xy = T) %>% 
#   filter(!is.na(S)) %>% 
#   head(1000) %>% 
#   #set_names(c("CenterLong", "CenterLat", "S")) %>% 
#   #mutate(SpeciesID =i) %>% 
#   ggplot(aes(x=CenterLong,y=CenterLat,fill=S)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# raster_test <- Aqua3stack %>% 
#   select(CenterLong, CenterLat, S) %>% raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
#   raster::projectRaster(crs = "+proj=moll") 
# 
# plot(raster_test)
# ###############
# 
# ggplot(Aqua3stack, aes(x=CenterLong,y=CenterLat,fill=S)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# Aqua3stack %>% select(CenterLat,CenterLong,S) %>% head(1000) %>%
#   ggplot(aes(x=CenterLong,y=CenterLat,fill=S)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# 
# # ##OPTION 2: Best option
# # Aquaothers2 %>% 
# #   filter(SpeciesID == "Fis-29302") %>% 
# #   select(CenterLong, CenterLat, probability)%>% 
# #   raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
# #   raster::projectRaster(crs = "+proj=moll") %>%
# #   as.data.frame(xy = T) %>% 
# #   filter(!is.na(probability)) %>% 
# #   set_names(c("lon", "lat", "prob")) %>% 
# #   mutate(spp_id =  "Fis-29302") 
# 
# head(include)
# dim(include)
# 
# #ok, we can use the SpeciesID column in the "include" file to run our loop.
# sp_dist_mollweide_datalist = list()
# head(Aquaothers2)
# 
# for (i in include$SpeciesID[1:5]) {
#   sp_dist_mollweide<-Aquaothers2 %>% 
#     filter(SpeciesID == i) %>% 
#     select(CenterLong, CenterLat, probability)%>% 
#     raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
#     raster::projectRaster(crs = "+proj=moll") %>%
#     as.data.frame(xy = T) %>% 
#     filter(!is.na(probability)) %>% 
#     set_names(c("CenterLong", "CenterLat", "probability")) %>% 
#     mutate(SpeciesID =i) 
#   sp_dist_mollweide_datalist [[i]] <- sp_dist_mollweide# add it to your list
# }
# 
# Aquaothers2_mollweide = do.call(rbind, sp_dist_mollweide_datalist)
# head(Aquaothers2_mollweide)
# dim(Aquaothers2_mollweide)
# 
# Aquaothers2_mollweide %>% filter(SpeciesID==include$SpeciesID[5]) %>%
# ggplot(aes(x=CenterLong,y=CenterLat,fill=probability)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# Aqua3_mollweide<-merge(Aquaothers2_mollweide,include,by="SpeciesID")
# Aqua3_mollweide <- Aqua3_mollweide %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
# head(Aqua3_mollweide)
# Aqua3stack_mollweide<-Aqua3_mollweide %>% group_by(CenterLat,CenterLong) %>% mutate(Kcell=sum(probability*K/totalprob)) %>% summarise(S=sum(probability*K/totalprob), Fstat=weighted.mean(Fstatus,Kcell), Bstat=weighted.mean(Bstatus,Kcell))
# head(Aqua3stack_mollweide) #S is total K per cell
# Aqua3stack_mollweide<-as.data.frame(Aqua3stack_mollweide)
# head(Aqua3stack_mollweide)
# dim(Aqua3stack_mollweide)
# 
# Aqua3stack_mollweide %>% select(CenterLat,CenterLong,S) %>% #tail(1000) %>%
#   ggplot(aes(x=CenterLong,y=CenterLat,fill=S)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# 
# tmp <- Aqua3stack_mollweide %>%
#   group_by(CenterLong, CenterLat) %>% 
#   summarise(S = sum(S, na.rm = T)) 
# 
# tmp %>% head(1000) %>% 
#   ggplot(aes(x=CenterLong,y=CenterLat,fill=S)) + geom_raster()
# 
# 
# ##
# Aquaothers2 %>% head()
# 
# i<-include$SpeciesID[2]
# i
# 
# raster_test <- Aquaothers2 %>% 
#   filter(SpeciesID == i) %>% 
#   select(CenterLong, CenterLat, probability)%>% 
#   raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
#   raster::projectRaster(crs = "+proj=moll") 
# 
# dim(raster_test)
# plot(raster_test)
# 
# raster_test %>% 
#   as.data.frame(xy = T) %>% 
#   filter(!is.na(probability)) %>% 
#   set_names(c("CenterLong", "CenterLat", "probability")) %>% 
#   mutate(SpeciesID =i) %>% 
#   ggplot(aes(x=CenterLong,y=CenterLat,fill=probability)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# #plot same as Juan
# 
# z_pal <- list(breaks = c(0,0.5e5,1e5,1.5e5,2e5,2.5e5,3e5,5e5),
#               labels = c("0-0.5e5", "0.5-1e5", "1-1.5e5", "1.5-2e5", "2-2.5e5", "2.5-3e5", "3-5e5"),
#               colors = rev(c("#d73027","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1", "#4575b4")))
# land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
# 
# land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))
# 
# raster_test %>% 
#   raster::projectRaster(ocean_low_res_moll) %>% 
#   tmap::tm_shape()+
#   tmap::tm_raster(title = "K (MT)",
#                   palette  = z_pal$colors,
#                   breaks = z_pal$breaks,
#                   labels = z_pal$labels,
#                   legend.is.portrait = T,
#                   legend.reverse = T)+
#   tmap::tm_shape(land_shp_moll)+
#   tmap::tm_fill(col = "black", border.col = "transparent")+
#   tmap::tm_layout(title = "Carrying capacity (MT per 0.5x0.5 degree)",
#                   title.position = c("center", .95),
#                   inner.margins = c(0.12, 0, 0.08, 0.04),
#                   frame = F,
#                   legend.position = c(.99, "center"))
# 
# 
# ##
# empty_raster <- raster(res = 0.5)
# cells <- cellFromXY(empty_raster, as.matrix(Aqua3stack_mollweide[,2:1]))
# empty_raster[cells] <- Aqua3stack_mollweide[,3]
# plot(empty_raster,main="Carrying capacity per cell (MT)")
# 
# tmp2<-readRDS("/Users/ren/Documents/CODES/FoodProvision/tmp2.rds")
# head(tmp2)
# 
# tmp2 %>% filter(SpeciesID=="Fis-114088") %>% head()
# 
# Aquaothers2<-tmp2
##Add K to the Aquaothers2
head(Aquaothers2)

Aqua3<-merge(Aquaothers2,include,by="SpeciesID")
head(Aqua3)
dim(Aqua3)
length(unique(Aqua3$SpeciesID))


Aqua3 <- Aqua3 %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
Aqua3stack<-Aqua3 %>% group_by(CenterLat,CenterLong) %>% mutate(Kcell=sum(probability*K/totalprob)) %>% summarise(S=sum(probability*K/totalprob), Fstat=weighted.mean(Fstatus,Kcell), Bstat=weighted.mean(Bstatus,Kcell))
head(Aqua3stack) #S is total K per cell
Aqua3stack<-as.data.frame(Aqua3stack)

dim(Aqua3stack) #160639
head(Aqua3stack)
# 
# raster_test <- Aqua3stack %>% 
#   select(CenterLong, CenterLat, S)%>% 
#   raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
#   raster::projectRaster(crs = "+proj=moll") 
# 
# raster_test %>% 
#   as.data.frame(xy = T) %>% 
#   filter(!is.na(S)) %>% 
#   set_names(c("CenterLong", "CenterLat", "S")) %>% 
#   ggplot(aes(x=CenterLong,y=CenterLat,fill=S)) +
#   geom_raster()+
#   geom_sf(data = land_shp_moll, inherit.aes = F)
# 
# 
# empty_raster <- raster(res = 0.5)
# cells <- cellFromXY(empty_raster, as.matrix(Aqua3stack[,2:1]))
# empty_raster[cells] <- Aqua3stack[,3]
# plot(empty_raster,main="Carrying capacity per cell (MT)")
# 
# if(saveme==1){
# png(file="/Users/ren/Documents/CODES/FoodProvision/Results/K_AquaCostello.png", width = 6, height = 4, units = 'in', res = 300)
# #plot(empty_raster,main="K per cell, Aquamaps + Costello et al. (2016) data (MT)")
# plot(PlotFunction(empty_raster),zlim=c(0,maxValue(empty_raster)), main="K per cell, Aquamaps + Costello et al. (2016) data (MT)",axes=F,box=F,legend=F)
# plot(empty_raster, zlim=c(0,maxValue(empty_raster)),legend.only=TRUE,legend.width=1, legend.shrink=0.75,axis.args=list(cex.axis=0.5),
#      legend.args=list(text='Carrying capacity, K (MT)', side=4, font=2, line=2.5, cex=0.8))
# dev.off()
# }
# 
# #plot same as Juan
# crs(empty_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# maxValue(empty_raster)
# z_pal <- list(breaks = c(0,0.5e5,1e5,1.5e5,2e5,2.5e5,3e5,5e5),
#               labels = c("0-0.5e5", "0.5-1e5", "1-1.5e5", "1.5-2e5", "2-2.5e5", "2.5-3e5", "3-5e5"),
#               colors = rev(c("#d73027","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1", "#4575b4")))
# land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
# ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
# caption<-"hello world"
# land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))
# empty_raster %>% 
#   raster::projectRaster(ocean_low_res_moll) %>% 
#   tmap::tm_shape()+
#   tmap::tm_raster(title = "K (MT)",
#                   palette  = z_pal$colors,
#                   breaks = z_pal$breaks,
#                   labels = z_pal$labels,
#                   legend.is.portrait = T,
#                   legend.reverse = T)+
#   tmap::tm_shape(land_shp_moll)+
#   tmap::tm_fill(col = "black", border.col = "transparent")+
#   tmap::tm_credits(caption) +
#   tmap::tm_layout(title = "Carrying capacity (MT per 0.5x0.5 degree)",
#                   title.position = c("center", .95),
#                   inner.margins = c(0.12, 0, 0.08, 0.04),
#                   frame = F,
#                   legend.position = c(.99, "center"))
# 
# ##PLOT FISHERIES STATUS PER CELL 
# #F/Fmsy
# empty_raster <- raster(res = 0.5)
# cells <- cellFromXY(empty_raster, as.matrix(Aqua3stack[,2:1]))
# empty_raster[cells] <- Aqua3stack[,4]
# plot(empty_raster,main="F/Fmsy")
# 
# if(saveme==1){
# png(file="/Users/ren/Documents/CODES/FishCrime/FvFmsy.png", width = 6, height = 4, units = 'in', res = 300)
# levelplot(empty_raster, par.settings = RdBuTheme(),main="F/Fmsy")
# dev.off()
# }
# 
# #-----plot same as Juan, F/Fmsy
# crs(empty_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# maxValue(empty_raster)
# z_pal <- list(breaks = c(0,1,2,3,15),
#               labels = c("0-1", "1-2", "2-3", "3-15"),
#               colors = rev(c("#d73027","#fdae61","#fee090", "#4575b4")))
# land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
# ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
# caption<-""
# land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))
# empty_raster %>% 
#   raster::projectRaster(ocean_low_res_moll) %>% 
#   tmap::tm_shape()+
#   tmap::tm_raster(title = "F/Fmsy",
#                   palette  = z_pal$colors,
#                   breaks = z_pal$breaks,
#                   labels = z_pal$labels,
#                   legend.is.portrait = T,
#                   legend.reverse = T)+
#   tmap::tm_shape(land_shp_moll)+
#   tmap::tm_fill(col = "black", border.col = "transparent")+
#   tmap::tm_credits(caption) +
#   tmap::tm_layout(title = "F/Fmsy",
#                   title.position = c("center", .95),
#                   inner.margins = c(0.12, 0, 0.08, 0.04),
#                   frame = F,
#                   legend.position = c(.99, "center"))
# 
# #NOW PLOT B/BMSY
# empty_raster <- raster(res = 0.5)
# cells <- cellFromXY(empty_raster, as.matrix(Aqua3stack[,2:1]))
# empty_raster[cells] <- Aqua3stack[,5]
# plot(empty_raster,main="B/Bmsy")
# 
# if(saveme==1){
# png(file="/Users/ren/Documents/CODES/FishCrime/BvBmsy.png", width = 6, height = 4, units = 'in', res = 300)
# levelplot(empty_raster, par.settings = RdBuTheme(),main="B/Bmsy")
# dev.off()
# }
# 
# #-----plot same as Juan, B/Bmsy
# crs(empty_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# maxValue(empty_raster)
# z_pal <- list(breaks = c(0,1,2,3),
#               labels = c("0-1", "1-2", "2-3"),
#               colors = rev(c("#4575b4","#fdae61","#d73027")))
# land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
# ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
# caption<-""
# land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))
# empty_raster %>% 
#   raster::projectRaster(ocean_low_res_moll) %>% 
#   tmap::tm_shape()+
#   tmap::tm_raster(title = "B/Bmsy",
#                   palette  = z_pal$colors,
#                   breaks = z_pal$breaks,
#                   labels = z_pal$labels,
#                   legend.is.portrait = T,
#                   legend.reverse = T)+
#   tmap::tm_shape(land_shp_moll)+
#   tmap::tm_fill(col = "black", border.col = "transparent")+
#   tmap::tm_credits(caption) +
#   tmap::tm_layout(title = "B/Bmsy",
#                   title.position = c("center", .95),
#                   inner.margins = c(0.12, 0, 0.08, 0.04),
#                   frame = F,
#                   legend.position = c(.99, "center"))
# 
# levelplot(empty_raster, par.settings = RdBuTheme())
# 
# # rasterVis plot #check: https://pjbartlein.github.io/REarthSysSci/raster_intro.html
# mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")))
# plt<-levelplot(PlotFunction(empty_raster), margin = F, cuts=11, pretty=TRUE, par.settings = mapTheme)
# plt

#--this is for deriving the biological parameters c/o Chris Free
library(FishLife)
load("/Users/ren/Documents/CODES/FoodProvision/Return.RData")
# Predicted variables
# -------------------------------------------
# Loo - asymptotic length (Linf, cm)
# K - growth coefficient (K)
# Winfinity - Asymptotic mass (Winf, g)
# tmax - maximum age (Amax, yr)
# tm - age at maturity (Amat, yr)
# M - mortality rate (M, 1/yr)
# Lm - length at maturity (Lmat, cm)
# Temperature - average temperature (T, °C)
# ln_var - marginal standard deviation of recruitment variability (τ)
# rho - autocorrelation of recruitment variability (ρ)
# ln_MASPS - maximum annual spawners per spawner (r)

# Derived variables
# -------------------------------------------
# ln_margsd - standard deviation for recruitment (σ): σ = sqrt(τ^2 / (1-ρ^2))
# h / logitbound_h - steepness (h): h = ρ / (4 + ρ)
# ln_Fmsy - FMSY
# ln_Fmsy_over_m - FMSY/M ratio
# r / ln_r - Intrinsic growth rate (r): dominant eigen value for Leslie matrix w/ assumptions: length-weight b=3.04, VonB t0=-0.1, maturity ogive slope=0.25*tmat
# G / ln_G - Generation time (G, yr)

fishlife2 <- function(species){
  # Setup container
  fl <- data.frame(species=sort(unique(species)),
                   linf_cm=NA, k=NA, winf_g=NA, tmax_yr=NA, tmat_yr=NA,
                   m=NA, lmat_cm=NA, temp_c=NA, 
                   sr_var=NA, sr_rho=NA, masps=NA, sr_sd=NA, 
                   h=NA, fmsydivm=NA, fmsy=NA, r=NA, g_yr=NA, stringsAsFactors=F)
  
  # Loop through species
  for(i in 1:nrow(fl)){
    
    # Match species to FishLife
    sciname <- fl$species[i]
    genus <- stringr::word(sciname, 1)
    nwords_in_spp <- length(strsplit(sciname, " ")[[1]])
    spp <- stringr::word(sciname, start=2, end=nwords_in_spp)
    spp <- ifelse(spp=="spp", "predictive", spp)
    try(taxa_match <- FishLife::Search_species(Genus=genus, Species = spp, add_ancestors=TRUE)$match_taxonomy)
    
    # Get predictions from FishLife (mean and covariance)
    if(inherits(taxa_match, "try-error")){
      # Record blanks
      fl[i,2:ncol(fl)] <- rep(NA, ncol(fl)-1)
    }else{
      # Extract FishLife 2.0 means
      params <- colnames(Return$beta_gv)
      mus <- Return$beta_gv[rownames(Return$beta_gv)==taxa_match[[1]], ]
      mus_use <- mus[c("Loo", "K", "Winfinity", "tmax", "tm", 
                       "M", "Lm", "Temperature", "ln_var", "rho", "ln_MASPS", "ln_margsd",
                       "h", "ln_Fmsy_over_M", "ln_Fmsy", "r",  "G")]
      fl[i,2:ncol(fl)] <- mus_use
    }
    
  }
  
  # Exponentiate columns
  # These columns are not log-transformed: "temp_c", "rho", "h", "r", "g_yr", "fmsy"
  log_cols <- c("linf_cm", "k", "winf_g", "tmax_yr", "tmat_yr", "m", "lmat_cm", "sr_var", "masps", "sr_sd", "fmsydivm", "fmsy")
  fl[,log_cols] <- exp(fl[,log_cols])
  
  # Return
  return(fl)
}
#----end function for deriving the biol params

#get coordinates
coords <- read.table("/Users/ren/Documents/CODES/FoodProvision/Lat_Lon_DBEM.txt", sep = ",", col.name = c("id", "lon", "lat"))
head(coords)
dim(coords)

coordUNIQUE<-unique(coords[c("lat", "lon")])
dim(coordUNIQUE) #great!

###---REN's modification
head(Aqua3)
Aqua3<-Aqua3 %>% mutate(normprob=probability/totalprob)
Aqua3 %>% group_by(SpeciesID) %>% summarize(sumtest=sum(normprob))

#RUN ONLY ONCE / NO RUN, JUST LOAD (the commented code is important)
coords2<-coords
Aqua3Important<-Aqua3 %>% select(SpeciesID,CenterLat,CenterLong,normprob)
colnames(Aqua3Important)[which(names(Aqua3Important) == "CenterLat")] <- "lat"
colnames(Aqua3Important)[which(names(Aqua3Important) == "CenterLong")] <- "lon"
dim(Aqua3Important)
head(Aqua3Important)
TEST_UNIQUE<-unique(Aqua3Important[c("lat", "lon")])
dim(TEST_UNIQUE)

# for (i in unique(Aqua3Important$SpeciesID)){
#   Aqua3sub<-Aqua3Important %>% filter(SpeciesID==i)
#   Aqua3sub$SpeciesID<-NULL
#   colnames(Aqua3sub)[which(names(Aqua3sub) == "normprob")] <- i
#   coords2<-left_join(coords2, Aqua3sub,by=c("lon","lat"))
# }
# coords2<-coords2 %>% mutate_if(is.numeric,coalesce,0)
# saveRDS(coords2, file = "/Users/ren/Documents/CODES/FoodProvision/reshaped_costellodata.rds") #UNLUMPED DATA
#saveRDS(coords2, file = "/Users/ren/Documents/CODES/FoodProvision/reshaped_costellodata_lumped.rds") #LUMPED DATA

#load the generated data above
Aqua4<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/reshaped_costellodata.rds")
#Aqua4<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/reshaped_costellodata_lumped.rds")
dim(Aqua4)
Aqua4<-distinct(Aqua4,lon,lat, .keep_all= TRUE)
dim(Aqua4)
colSums(Aqua4,na.rm=T)#to verify that the code above is correct, we should perform colsum
#sum(coords2$normprob,na.rm = T)

# Aqua3$normprob<-Aqua3$probability/Aqua3$totalprob
# MydataLong<-Aqua3 %>% select(CenterLat,CenterLong,SpeciesID,normprob) 
# MydataLong<-dcast(MydataLong,CenterLat + CenterLong ~ SpeciesID,value.var="normprob")
# head(MydataLong)
# colSums(MydataLong)
# ####

#Merge Aqua3 and Coords.
#library(reshape2)
# Mydata<-Aqua3 %>% select(CenterLat,CenterLong,SpeciesID,probability) 
# Mydata<-dcast(Mydata,CenterLat + CenterLong ~ SpeciesID,value.var="probability")
# head(Mydata)

#Aqua4 is the rawest data. Separate managed species from the species list.
head(Aqua4) #id, lon, lat, species
#"include" contains SciName, K, SpeciesID
dim(Aqua4)

TEST_UNIQUE<-unique(Aqua4[c("lat", "lon")])
dim(TEST_UNIQUE)

##MANAGEMENT
#ManagementLayer<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerData.rds")
ManagementLayer<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerv3.rds") #v3 is the one with stockid
names(ManagementLayer)[names(ManagementLayer) == 'x'] <- 'lon'
names(ManagementLayer)[names(ManagementLayer) == 'y'] <- 'lat'
names(ManagementLayer)[names(ManagementLayer) == 'species'] <- 'SciName'
head(ManagementLayer) #x is lon, y is lat

unique(ManagementLayer$stockid)
#There are NAs in the stockid! Remove them.
ManagementLayer <- na.omit(ManagementLayer)

#add species id code into the "ManagementLayer" file. We can get the id from "include"
head(include)

#ManagementLayer2<-left_join(ManagementLayer,include,by="SciName") %>% select(lon,lat,SpeciesID)
#try to add stockid
ManagementLayer2<-left_join(ManagementLayer,include,by="SciName") %>% select(lon,lat,stockid,SpeciesID)
ManagementLayer2$Value<-1
head(ManagementLayer2) 
ManagementLayer2trans<-ManagementLayer2 %>% select(lon,lat,stockid,Value)
head(ManagementLayer2trans)
unique(ManagementLayer2$stockid)
unique(ManagementLayer2$SpeciesID)

##---reshape the management layer
##NOTE: This can take some time so I will save the output and just load it. 
##when new species is added, rerun the "cast" code below

#THIS IS NOT RIGHT because we have several stocks per species --- REVISED BELOW
#ManagementLayer3<-cast(ManagementLayer2,lon+lat~SpeciesID)
#saveRDS(ManagementLayer3, file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayer3.rds")

#attempt failed --- memory not enough
#ManagementLayer4<-cast(ManagementLayer2trans,lon+lat~stockid)
#saveRDS(ManagementLayer4, file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayer4.rds")

##OK, THIS IS THE FINAL CODE!!! RUN ONLY ONCE --- FAST RUN. FEW MINUTES.
# count<-0
# ManagementLayer4<-coords
# for (i in unique(ManagementLayer2trans$stockid)){
#   ML2Tprime<-ManagementLayer2trans %>% filter(stockid==i)
#   ML2Tprime$stockid<-NULL
#   colnames(ML2Tprime)[which(names(ML2Tprime) == "Value")] <- i
#   ManagementLayer4<-left_join(ManagementLayer4, ML2Tprime,by=c("lon","lat"))
#   count<-count+1
#   print(count)
# }
# ManagementLayer4<-ManagementLayer4 %>% mutate_if(is.numeric,coalesce,0)
# saveRDS(ManagementLayer4, file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayer4.rds")

##FROM HERE, I CHANGED MNGT LAYER 3 to 4
ManagementLayer4<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayer4.rds")
head(ManagementLayer4)
plot(ManagementLayer4$'BGRDRSE')
#ManagementLayer3 is our areas of management

# managedspecieslist<-colnames(ManagementLayer4)
# managedspecieslist<-managedspecieslist[-which(managedspecieslist %in% c("id","lon","lat"))]
# managedspecieslist<-as.data.frame(managedspecieslist)
# colnames(managedspecieslist) <- "stockid"

#what are the species ids of the stocks?
#Reference- stockid and SpeciesID
ReferenceStockSpeciesID<-ManagementLayer2 %>% select(stockid,SpeciesID) %>% group_by(stockid,SpeciesID) %>% summarise(n=n())
ReferenceStockSpeciesID2<-ReferenceStockSpeciesID %>% filter(is.na(SpeciesID)==F)
#524 of the species in Costello et al. species list have stock assessments

#normalization function
NormFunction<-function(rawfile){
  # remove id, lat, lon
  rawfile$id <- NULL
  rawfile$lon <- NULL
  rawfile$lat <- NULL
  rawfile[is.na(rawfile)] <- 0
  #total_K_cell<-rowSums(rawfile)
  total_K_species<-colSums(rawfile)
  Norm_K<-t(t(rawfile)/total_K_species)
  return(Norm_K)
}

#separate managed layer from our main data (Aqua4)
head(Aqua4)
Aqua4[is.na(Aqua4)] <- 0

Aqua4Norm<-as.data.frame(NormFunction(Aqua4))
colSums(Aqua4Norm) #ok, answer correct
#bring the coordinates back using cbind
Aqua4Norm<-cbind(Aqua4[c("id", "lon", "lat")],Aqua4Norm)

#Species that are not in the management layer/poorly managed
#Aqua4poor<-Aqua4Norm[ , -which(names(Aqua4Norm) %in% colnames(ManagementLayer3))]
Aqua4poor<-Aqua4Norm[ , -which(names(Aqua4Norm) %in% ReferenceStockSpeciesID2$SpeciesID)]
head(Aqua4poor)

#Species in the management layer//not yet disaggregated
#Aqua4other<-Aqua4Norm[ , which(names(Aqua4Norm) %in% colnames(ManagementLayer3))]
Aqua4other<-Aqua4Norm[ , which(names(Aqua4Norm) %in% ReferenceStockSpeciesID2$SpeciesID)]
Aqua4other<-cbind(Aqua4[c("id", "lon", "lat")],Aqua4other)
head(Aqua4other)

#I just want the coordinates
AquaPoor_other<- Aqua4other %>% select(c(lon,lat))
AquaManaged_other<- Aqua4other %>% select(c(lon,lat))

#separating managed and unmanaged
#managed
for (j in ReferenceStockSpeciesID2$stockid){
  i<-ReferenceStockSpeciesID2$SpeciesID[which(ReferenceStockSpeciesID2$stockid==j)]
  #i="Fis-10768"
  Layer1<-Aqua4other %>% select(c(lon,lat,i))
  Layer2<-ManagementLayer4 %>% select(c(lon,lat,j))
  Layer3<-left_join(Layer1,Layer2,by=c("lon","lat"))
  Layer3[is.na(Layer3)] <- 0
  AquaManaged_other<-AquaManaged_other %>% mutate(myval=Layer3[,3]*(Layer3[,4]==1))
  names(AquaManaged_other)[names(AquaManaged_other) == 'myval'] <- j
}
head(AquaManaged_other)

#unmanaged
for (i in unique(ReferenceStockSpeciesID2$SpeciesID)){
  j<-ReferenceStockSpeciesID2$stockid[which(ReferenceStockSpeciesID2$SpeciesID==i)]
  #i="Fis-22832" #this is with duplicate
  Layer1<-Aqua4other %>% select(c(lon,lat,i))
  Layer2<-ManagementLayer4 %>% select(c(lon,lat))
  Layer2prime<-ManagementLayer4 %>% select(c(j))
  Layer2$isum<-rowSums(Layer2prime,na.rm =T)
  
  Layer3<-left_join(Layer1,Layer2,by=c("lon","lat"))
  Layer3[is.na(Layer3)] <- 0
  AquaPoor_other <- AquaPoor_other %>% mutate(myval=Layer3[,3]*(Layer3[,4]!=1))
  names(AquaPoor_other)[names(AquaPoor_other) == 'myval'] <- i
}

# ###THIS IS THE OLD CODE OF THE TWO CHUNKS OF CODES ABOVE
# for (i in ReferenceStockSpeciesID2$SpeciesID){
#   j<-ReferenceStockSpeciesID2$stockid[which(ReferenceStockSpeciesID2$SpeciesID==i)]
#     #i="Fis-10768"
#   Layer1<-Aqua4other %>% select(c(lon,lat,i))
#   Layer2<-ManagementLayer4 %>% select(c(lon,lat,j))
#   Layer3<-left_join(Layer1,Layer2,by=c("lon","lat"))
#   Layer3[is.na(Layer3)] <- 0
#   AquaPoor_other <- AquaPoor_other %>% mutate(myval=Layer3[,3]*(Layer3[,4]!=1))
#   names(AquaPoor_other)[names(AquaPoor_other) == 'myval'] <- i
#   
#   AquaManaged_other<-AquaManaged_other %>% mutate(myval=Layer3[,3]*(Layer3[,4]==1))
#   names(AquaManaged_other)[names(AquaManaged_other) == 'myval'] <- j
# }

#we can plot the managed layer to see if it is the same as expected (and not mess up the coordinates)  

head(AquaManaged_other)
dim(AquaManaged_other)

#plot(AquaManaged_other$'Fis-10768')

nmanageareas<-as.data.frame((AquaManaged_other[,3:dim(AquaManaged_other)[2]]>0)*1)
#plot(nmanageareas$'Fis-10768')

summanagedlayer<-rowSums(nmanageareas)
max(summanagedlayer)

#THIS IS THE PLOT OF THE MANAGEMENT LAYER. NOTICE THAT IT IS LESS THAN THE RAW DATA
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(AquaManaged_other[,1:2]))
empty_raster[cells] <- summanagedlayer
head(empty_raster)
plot(PlotFunction(empty_raster),main="Management",axes=F,box=F)

#Alternative management layer plot based on ggplot
ManagementLayerPlot<-cbind(AquaManaged_other[,1:2], summanagedlayer)
head(ManagementLayerPlot)

land_shp_moll<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")
ManagementLayerPlotFin<- ManagementLayerPlot %>% 
  select(lon, lat, summanagedlayer)%>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
  raster::projectRaster(crs = "+proj=moll") %>% 
  as.data.frame(xy = T) %>%
  filter(!is.na(summanagedlayer)) %>%
  set_names(c("lon", "lat", "Count")) %>%
  ggplot(aes(x=lon,y=lat,fill=Count)) + scale_fill_gradient(low="white", high="#00539CFF")+#guides(fill=guide_legend())+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+
  geom_raster()+
  geom_sf(data = land_shp_moll,fill="darkgray", lwd = 0.1,  inherit.aes = F)
ManagementLayerPlotFin
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/ManagementLayerPlotFin.png", ManagementLayerPlotFin,width = 10, height = 8, dpi = 300, units = "in")#resolution not great

#Saving the management layer for the paper
if(saveme==1){
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/ManagementLayer.png", width = 6, height = 4, units = 'in', res = 300)
plot(PlotFunction(empty_raster),zlim=c(0,maxValue(empty_raster)), axes=F,box=F,legend=F)
plot(empty_raster, zlim=c(0,maxValue(empty_raster)),legend.only=TRUE,legend.width=1, legend.shrink=0.75,axis.args=list(cex.axis=0.5),
     legend.args=list(text='Number', side=4, font=2, line=2.5, cex=0.8))
dev.off()
}

#these are three relevant files
head(AquaPoor_other) #unmanaged layer that have species in managed #have lon lat
head(AquaManaged_other) #managed layer #have lon,lat
head(Aqua4poor)#completely unmanaged species ##have id

#This is for calculating the multipliers of K
KNorm_Aqua4poor<-as.data.frame(colSums(Aqua4poor[,!(names(Aqua4poor) %in% c('id','lon', 'lat'))])) #ok, answer correct
setDT(KNorm_Aqua4poor, keep.rownames = "SpeciesID")
colnames(KNorm_Aqua4poor) <- c("SpeciesID","Kfrac")
KNorm_Aqua4poor$Manage<-0
table(KNorm_Aqua4poor$Kfrac)

KNorm_AquaPoor_other<-as.data.frame(colSums(AquaPoor_other[,!(names(AquaPoor_other) %in% c('id','lon', 'lat'))]))
setDT(KNorm_AquaPoor_other, keep.rownames = "SpeciesID")
colnames(KNorm_AquaPoor_other) <- c("SpeciesID","Kfrac")
KNorm_AquaPoor_other$Manage<-0
table(KNorm_AquaPoor_other$Kfrac)#1 species with no K, we can remove this.
#What is that species?
removespeciesAquaPoor<-KNorm_AquaPoor_other %>% filter(Kfrac==0)
removespeciesAquaPoor
KNorm_AquaPoor_other<-KNorm_AquaPoor_other %>% filter(Kfrac!=0)

KNorm_AquaManaged_other<-as.data.frame(colSums(AquaManaged_other[,!(names(AquaManaged_other) %in% c('id','lon', 'lat'))]))
setDT(KNorm_AquaManaged_other, keep.rownames = "stockid")
colnames(KNorm_AquaManaged_other) <- c("stockid","Kfrac")
KNorm_AquaManaged_other$Manage<-1
table(KNorm_AquaManaged_other$Kfrac) #there are 10 stocks with zero K. We can remove this. [1 stock completely managed]
#What is that species?
removespeciesAquaManaged<-KNorm_AquaManaged_other %>% filter(Kfrac==0)
removespeciesAquaManaged
dim(KNorm_AquaManaged_other)
KNorm_AquaManaged_other<-KNorm_AquaManaged_other %>% filter(Kfrac!=0)
dim(KNorm_AquaManaged_other)


stocktoSpID<-ReferenceStockSpeciesID2 %>% select(-n)
KNorm_AquaManaged_other_SPID<-left_join(KNorm_AquaManaged_other,stocktoSpID, by="stockid") %>%select(SpeciesID,Kfrac,Manage,stockid)
KNorm_Aqua4poor$stockid<-KNorm_Aqua4poor$SpeciesID
KNorm_AquaPoor_other$stockid<-KNorm_AquaPoor_other$SpeciesID
KfracFile<-rbind(KNorm_Aqua4poor,KNorm_AquaPoor_other,KNorm_AquaManaged_other_SPID)
#perfect! I've calculated K multipliers. The next step is to us SpeciesID and Manage as matching columns to get Kfrac.

#The above are just K fractions, not the main file!!! 
#Below we will partition the file
#i will drop the id in Aqua4poor and merge it with AquaPoor_other
Aqua4poor$id <- NULL

#merge unmanaged fishery by using cbind
PoorlyManagedComb<-cbind(AquaPoor_other,Aqua4poor[,3:dim(Aqua4poor)[2]]) #coordinates are included... I removed the coors from Aqua4poor
head(PoorlyManagedComb)
max(colSums(PoorlyManagedComb))
dim(PoorlyManagedComb)
dim(AquaPoor_other)
#ok, the above looks good. The Managed fishery is just the "AquaManaged_other"
ManagedComb<-AquaManaged_other

#remove columns we identified above in the calculation of Kfrac
removespeciesAquaPoor$SpeciesID
PoorlyManagedComb$"ITS-97075" <- NULL #Manual removal because other ways are not working!!!! =(
dim(PoorlyManagedComb)

dim(ManagedComb)
removespeciesAquaManaged$stockid
ManagedComb$"BWHITCH"<- NULL
ManagedComb$"CMACKWA"<- NULL
ManagedComb$"FLSOLEBSAI"<- NULL
ManagedComb$"FLSOLEGA"<- NULL
ManagedComb$"HMACKCH"<- NULL
ManagedComb$"PRCODMEDGSA9"<- NULL
ManagedComb$"RKCRABNS"<- NULL
ManagedComb$"RMULLMEDGSA25"<- NULL
ManagedComb$"RROCKLOBSTERSAUSNZ"<- NULL
ManagedComb$"RROCKLOBSTERSAUSSZ"<- NULL
dim(ManagedComb)  

#PoorlyManagedComb[ , !(names(PoorlyManagedComb) %in% removespeciesAquaPoor$SpeciesID)] #this is slow!! NOT WORKING!!!
#dim(PoorlyManagedComb)
#dim(ManagedComb)
#ManagedComb[ , !(names(ManagedComb) %in% removespeciesAquaManaged$stockid)] #this is slow!! ##NOT WORKING!!!
#dim(ManagedComb)

#These are our normalized files
Norm_PoorlyManagedComb<-NormFunction(PoorlyManagedComb)
Norm_ManagedComb<-NormFunction(ManagedComb)
colSums(Norm_ManagedComb) #ok, answer correct

#We could bring back the coordinates, compute K, remove K==0 to save computing space.
Coord_ManagedComb<-ManagedComb %>% select(lon,lat)
Coord_PoorlyManagedComb<-PoorlyManagedComb %>% select(lon,lat)

#coord added to normalized file ##We could decide later if we want to work on this together or run separately
CoordNorm_PoorlyManagedComb<-cbind(Coord_PoorlyManagedComb,Norm_PoorlyManagedComb)
CoordNorm_ManagedComb<-cbind(Coord_ManagedComb,Norm_ManagedComb)

#the two lines below proves that the coordinates are the same above
sum(CoordNorm_PoorlyManagedComb$lat-CoordNorm_ManagedComb$lat)
sum(CoordNorm_PoorlyManagedComb$lon-CoordNorm_ManagedComb$lon)

megacell<-cbind(Norm_PoorlyManagedComb,Norm_ManagedComb)
colnames(Norm_PoorlyManagedComb)
colnames(megacell) #it is working --- same filenames
dim(megacell)

#add sum per cell, then remove 
Reduced_megacell<-cbind(megacell,NKsum=rowSums(megacell,na.rm=T)) 
#add coordinates to the megacell
Reduced_megacell2<-cbind(Coord_PoorlyManagedComb,Reduced_megacell)
dim(Reduced_megacell2)
Reduced_megacell3 <- Reduced_megacell2[ which(Reduced_megacell2$NKsum>0),]  
dim(Reduced_megacell3)
kpercellforplot<-Reduced_megacell3$NKsum
Reduced_megacell3$NKsum <- NULL

#coordinates
CleanCoordmegacell<-Reduced_megacell3[c("lon", "lat")]
head(CleanCoordmegacell)
dim(CleanCoordmegacell)
CleanCoordmegacellUNIQUE<-unique(CleanCoordmegacell[c("lon", "lat")])
dim(CleanCoordmegacellUNIQUE)

#remove lat long
Reduced_megacell3$lat <- NULL
Reduced_megacell3$lon <- NULL
Cleanmegacell<-Reduced_megacell3
head(Cleanmegacell)

colSums(Cleanmegacell)
##add sum per cell, then remove 
#Reduced_CoordNorm_PoorlyManagedComb<-cbind(CoordNorm_PoorlyManagedComb,NKsum=rowSums(Norm_PoorlyManagedComb,na.rm=T))%>%filter(NKsum>0)%>%select (-NKsum)  
#Reduced_CoordNorm_ManagedComb<-cbind(CoordNorm_ManagedComb,NKsum=rowSums(Norm_ManagedComb,na.rm=T))%>%filter(NKsum>0)%>%select (-NKsum)

dim(Cleanmegacell)

# ##these are the reference files!!!
# CleanCoordPoor<-Reduced_CoordNorm_PoorlyManagedComb %>% select(lon,lat)
# CleanPoor<-Reduced_CoordNorm_PoorlyManagedComb %>% select(-c(lon,lat))
# CleanCoordManage<-Reduced_CoordNorm_ManagedComb %>% select(lon,lat)
# CleanManage<-Reduced_CoordNorm_ManagedComb %>% select(-c(lon,lat))
# dim(CleanPoor)
# dim(CleanManage)

#get species ids in prep for making the MegaData biological parameters.
PoorMng<-as.data.frame(colnames(Norm_PoorlyManagedComb))
PoorMng<-cbind(PoorMng,0)
colnames(PoorMng) <- c("SpeciesID","Manage")
head(PoorMng)
dim(PoorMng)
PoorMng$stockid<-PoorMng$SpeciesID #this is for merging purpose later

Mng<-as.data.frame(colnames(Norm_ManagedComb))
Mng<-cbind(Mng,1)
colnames(Mng) <- c("stockid","Manage")
head(Mng)
dim(Mng)
#bind this with the real species id
head(ReferenceStockSpeciesID2)
dim(ReferenceStockSpeciesID2)
stocktoSpID<-ReferenceStockSpeciesID2 %>% select(-n)
Mng2<-left_join(Mng,stocktoSpID, by="stockid") %>%select(SpeciesID,Manage,stockid)
head(Mng2)
dim(Mng2)

#Next is to prepare a table of the biological layers? 
MegaData<-rbind(PoorMng,Mng2)

#add species name?
MegaData<-left_join(MegaData,include,by="SpeciesID")

#Add Kfrac and Kfinal
KfracFile <- KfracFile %>% select(stockid,Kfrac) #i just need these since stockid's are unique
MegaData<-left_join(MegaData,KfracFile,by="stockid")
#MegaData$Kfin<-MegaData$K*MegaData$Kfrac

#add r
biolparams<-fishlife2(as.character(MegaData$SciName))
biolparams2<-biolparams %>% select(species,r)
colnames(biolparams2)[colnames(biolparams2)=="species"] <- "SciName"
MegaData<-left_join(MegaData,biolparams2,by="SciName") #ok

#load file with m estimate
#mfile<-read.csv("/Users/ren/Documents/CODES/FoodProvision/SpeciesNamesCostello_m.csv")
mfile<-read.csv("/Users/ren/Documents/CODES/FoodProvision/mobility_data - data.csv")
mfile$m<-mfile$m_final
mfile<-mfile %>% mutate(m=replace(m,m==1,0.1),
                        m=replace(m,m==2,0.3),
                        m=replace(m,m==3,0.9))
mfile<-mfile %>% select(SpeciesID,m)
MegaData<-left_join(MegaData,mfile,by="SpeciesID")
head(MegaData)

#ER is exploitation rate, E is escapement. This is for stock-assessed.
ERmanage<-read.csv("/Users/ren/Documents/CODES/FoodProvision/MatchedER - MatchedERFin.csv")
MegaData<-left_join(MegaData,ERmanage,by="stockid")
#ERmanage is the exploitation rate of managed fishery

#if ERmanage is > r, make ER==r. AT ER=r, biomass and catch will be zero 
MegaData <- MegaData %>% mutate(ERset= (ER*(ER<=r)) + (r*(ER>r))) %>% mutate(E=1-ERset)
head(MegaData)

#calculate E that will result to bvk_fin, add E at msy, calculate E that will make BvK=0.1
MegaData <- MegaData %>% mutate(ER_costello=r-(bvk_fin*r)) %>% mutate(E_costello=1-ER_costello, Emsy=1-(0.5*r), EBvK01=1-(0.9*r))
head(MegaData)

min(MegaData$bvk_fin)
max(MegaData$bvk_fin)
max(MegaData$ER_costello)
min(MegaData$ER_costello)
max(MegaData$E_costello)
min(MegaData$E_costello)

##REVISED ASSUMPTION 1: Default is E and E_costello otherwise --- this means
#E costello 2050 + E constant (BAU2 assumption)
MegaData$Emanage<-MegaData$E
min(MegaData$Emanage,na.rm=T)
MegaData$Emanage[is.na(MegaData$Emanage)] <- -1
min(MegaData$Emanage)

MegaData <- MegaData %>% mutate(Efin=(Emanage*(Emanage!=-1)) + (E_costello*(Emanage==-1)))
head(MegaData)

#Assumption 2
#E assumption that would make BvK 0.1 for all stocks, add also Emsy
MegaData <- MegaData %>% mutate(EBvK01fin=(Emanage*(Emanage!=-1)) + (EBvK01*(Emanage==-1)))
head(MegaData)

MegaData <- MegaData %>% mutate(EBvK01_msy=(Emsy*(Emanage!=-1)) + (EBvK01*(Emanage==-1)))
head(MegaData)
#this is ER/ERmsy
#weighted.mean((1-MegaData$Efin)/(1-MegaData$Emsy), MegaData$MSYfin)

# #E assumption that would make BvK 0.1 for all stocks
# #if E is NA, then OA
# #MegaDataV2<-MegaData %>% mutate(E = replace_na(E, 1-(0.9*MegaData$r)))#mutate(E=replace(E, E==NA",1-(0.9*r)))
# managedE<-MegaData$E*(is.na(MegaData$E)==F) #this is just the managed E. Need to do this to convert NA to zero
# managedE[is.na(managedE)] <- 0
# MegaData$E1<-(1-(0.9*MegaData$r))*(is.na(MegaData$E)==T) + managedE #E1 is mixed escapement, both managed and OA included
# head(MegaData)
# 
# ###the stock will collapse if exploitation rate is greater than the growth rate of the stock
# #ERcheck<-MegaData %>% select(SpeciesID,stockid,Kfin, r, Efin,Ropt,Hopt)
# ER<-1-MegaData$E1
# ERratio<-ER/(0.9*MegaData$r)
# min(ERratio)
# max(ERratio)
# MegaData$Efin<-MegaData$E1*(ERratio<=1) + ((1-(0.9*MegaData$r))*(ERratio>1)) #managed or open access.
# hist(1-MegaData$Efin) #histogram of exploitation rate
# head(MegaData)

# #add E-MSY
# MegaData$ERfin<-1-MegaData$Efin
# MegaData$ERmsy<-0.5*MegaData$r
# MegaData$ERfinvERmsy<-MegaData$ERfin/MegaData$ERmsy
# hist(MegaData$ERfinvERmsy)
# head(MegaData)

##add Efin + MSY assumption for species with stock assessment
max(MegaData$ER_costello)
min(MegaData$ER_costello)
# #step 1 is to compute E
# E2050xmsy<-((1-MegaData$ERcostello)*(MegaData$Manage==0)) + ((1-MegaData$ERmsy)*(MegaData$Manage==1))
# #step 2 is cap data
# MegaData$E2050xmsyfin<-(E2050xmsy*(((1-E2050xmsy)/MegaData$r)<=1)) + ((1-MegaData$r)*(((1-E2050xmsy)/MegaData$r)>1))
MegaData<-MegaData %>% mutate(Efin_msy= (Efin*(Manage==0)+ Emsy*(Manage==1)))
head(MegaData)

# ##add E2050 data + current escapement assumption for species with stock assessment
# #step 1 is to compute E
# E2050xcurrent<-((1-MegaData$ERcostello)*(MegaData$Manage==0)) + ((1-MegaData$ERfin)*(MegaData$Manage==1))
# #step 2 is cap data
# MegaData$E2050xcurrentfin<-(E2050xcurrent*(((1-E2050xcurrent)/MegaData$r)<=1)) + ((1-MegaData$r)*(((1-E2050xcurrent)/MegaData$r)>1))
# head(MegaData)
# 
# #use ER=r(1-B2050/K) -- I think this is the best!
# BK2050<-MegaData$biomass2050/MegaData$k2050
# BK2050[is.na(BK2050)] <- -1
# BK2050ALL<-MegaData$biomass2050ALL/MegaData$K2050ALL
# BoverK2050<- ((BK2050ALL*(BK2050==-1)) + (BK2050*(BK2050!=-1)))
# plot(BoverK2050)
# #exploitation rate (ER)
# ER2050<-MegaData$r*(1-BoverK2050)
# #if ER>r, biomass will be zero. To be sure, reset ER==r if ER>r.
# plot(ER2050/MegaData$r)
# ER2050<- (MegaData$r*(ER2050>MegaData$r)) +  (ER2050*(ER2050<=MegaData$r))
# 
# #oks, now calculate E
# MegaData$E_BoverK2050<-(1-ER2050)
# #check ER for managed
# min(MegaData$ER,na.rm=T)
# MegaData$ER[is.na(MegaData$ER)]<- -1 #there are NAs
# 
# ER2050managed<-MegaData$ER
# ER2050managed<- (MegaData$r*(ER2050managed>MegaData$r)) +  (ER2050managed*(ER2050managed<=MegaData$r))
# #ok, now if ER2050managed == -1, it should be the unmanaged. Otherwise, use the managed values
# ER2050best<- (ER2050*(ER2050managed == -1))  + (ER2050managed*(ER2050managed >= 0))
# table(ER2050best) #13 stocks with exploitation rate == 0 (MPA will have no benefit for those stocks)
# MegaData$E2050best<-1-ER2050best
# head(MegaData)
# 
# plot(MegaData$E_BoverK2050-MegaData$E2050best)
# 
# ##add E2050 data + MSY for others
# head(MegaData)
# MegaData$E2050bestxmsy<-MegaData$E2050best*(MegaData$Manage==0) + MegaData$Emsy*(MegaData$Manage==1)
# plot(MegaData$E2050bestxmsy-MegaData$E2050best)

##add scorched earth + current E for others, and corched earth + MSY
MegaData<-MegaData %>% mutate(Escorched_current= (((1-r)*(Manage==0))+ (Efin*(Manage==1))), Escorched_msy= (((1-r)*(Manage==0))+ (Emsy*(Manage==1))))
head(MegaData)
# MegaData$Escorchedearth<-(1-MegaData$r)*(MegaData$Manage==0) + MegaData$E2050best*(MegaData$Manage==1)
# head(MegaData)


#50% of the poorly managed will be managed at msy
MegaData$randomnum<-runif(dim(MegaData)[1])
head(MegaData)
MegaData<-MegaData %>% mutate(Efinhalf_msy= (Efin*(Manage==0 & randomnum<=0.5)+ Emsy*(Manage==0 & randomnum>0.5)+ Emsy*(Manage==1)))
head(MegaData)

#BAU1: check Fstatus (then F current forever)
MegaData <- MegaData %>% mutate(Efin_BAU1=Efin*(Manage==1)+Efin*((Fstatus>1 | Bstatus<1) & Manage==0)+ (1-r+(BK2012*r))*(Fstatus<1 & Bstatus>1 & Manage==0))
head(MegaData)
plot(MegaData$Efin,MegaData$Efin_BAU1)

#check this later
halfearth<-MegaData %>% filter(Manage==0) %>% select(MSYfin,Efin,Emsy) %>% mutate(ERratio=(1-Efin)/(1-Emsy))
head(halfearth)
plot(halfearth$MSYfin,halfearth$ERratio)

#MSY per species from costello et al.
MSYperspeciescostello<-CostelloPresentPrime %>% select(SciName,MSY) %>% filter(SciName %in% MegaData$SciName) %>% group_by(SciName) %>% summarize(MSYtotal=sum(MSY))
MegaData<-left_join(MegaData,MSYperspeciescostello,by="SciName")
MegaData$MSYfin<-MegaData$MSYtotal*MegaData$Kfrac
MegaData$Kfin<-4*MegaData$MSYfin/MegaData$r
sum(MegaData$MSYfin)
sum(MegaData$Kfin)
head(MegaData)
plot(MegaData$Kfin,MegaData$K*MegaData$Kfrac)
abline(0,1)

#include costello r (not working!)
rperspeciescostelloUNIQUE_2<-rperspeciescostelloUNIQUE %>% filter(SciName %in% MegaData$SciName) 
#MegaData_withcostellor<-left_join(MegaData,rperspeciescostelloUNIQUE,by="SciName")
#add r
rparams<-fishlife2(as.character(rperspeciescostelloUNIQUE_2$SciName))
rparams2<-rparams %>% select(species,r)
colnames(rparams2)[colnames(rparams2)=="species"] <- "SciName"
rperspeciescostelloUNIQUE_3<-left_join(rperspeciescostelloUNIQUE_2,rparams2,by="SciName") #ok
plot(rperspeciescostelloUNIQUE_3$rcostello,rperspeciescostelloUNIQUE_3$r)

#plot K per stock per species for SI
head(MegaData)
# change fill color by groups and add text labels
MegaDataKplot<-MegaData[order(-MegaData$Kfin),] %>% slice(1:50)
plotMegaDataKplot<-ggplot(MegaDataKplot, aes(x = reorder(stockid, Kfin), y = Kfin)) +
  geom_bar(fill="steelblue",stat = "identity") +
  coord_flip() +
  geom_text(aes(label = SciName,size=14), nudge_y = 9e6, color = "black")+
  labs(y = "Carrying capacity, K (MT)", x="Fish stock")+ ylim(0, 4.5e7)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")
plotMegaDataKplot
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/KperStock.png", width = 10, height = 10,units = 'in', res = 300) 
plotMegaDataKplot
dev.off()

#plot K per species for SI
head(MegaData)
sum((MegaData$Manage==0)*1)
MegaDataKplot<-MegaData %>% filter(Manage==0) %>% mutate(KfinTot=4*MSYtotal/r) 
MegaDataKplot<-MegaDataKplot[order(-MegaDataKplot$KfinTot),] %>% slice(1:50)
plotMegaDataKplot<-ggplot(MegaDataKplot, aes(x = reorder(SciName, KfinTot), y = KfinTot)) +
  geom_bar(fill="steelblue",stat = "identity") +
  coord_flip() +
  #geom_text(aes(label = SciName,size=14), nudge_y = 8e6, color = "black")+
  labs(y = "Carrying capacity, K (MT)", x="Species")+ #ylim(0, 4.7e7)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")
plotMegaDataKplot
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/KperSpecies.png", width = 10, height = 10,units = 'in', res = 300) 
plotMegaDataKplot
dev.off()

#Check total MSY and compare to costello et al.
sum((MegaData$r*MegaData$Kfin)/4) #MSYtotal
sum(MegaData$Kfin) #total K

#MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")

#Ropt, Hopt
head(MegaData)
MegaData$Ropt<-((MegaData$m*MegaData$r) + (((2*MegaData$Efin)-2)*MegaData$m)) / (((MegaData$Efin-1)*MegaData$r)+(((2*MegaData$Efin)-2)*MegaData$m))
hist(MegaData$Ropt,xlab="Ropt",main="")

#SI plot for paper
optimalMPAsize<-as.data.frame(MegaData$Ropt*100)
names(optimalMPAsize) <- c("Ropt")
optimalMPAsize<-optimalMPAsize %>% filter(Ropt>0) %>% filter(Ropt<=100)
head(optimalMPAsize)
hist(optimalMPAsize,xlab="Ropt",main="")
mu<-mean(optimalMPAsize$Ropt)
optimalMPAsize<-as.data.frame(optimalMPAsize)
p<-ggplot(optimalMPAsize, aes(x=Ropt)) +geom_histogram()+
  #geom_histogram(fill="white", position="dodge")+
  geom_vline(xintercept=mu,
             linetype="dashed")+
  theme(legend.position="top")+labs(x="MPA size (0-100%)",y="Number of species")
p
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Ropt.png", width = 6, height = 6,units = 'in',res = 300) 
p
dev.off()

#Given Ropt, what is Hopt???
MegaData$Hopt<-((1-MegaData$Efin)*((MegaData$m*MegaData$Kfin*(1-MegaData$Ropt))/(MegaData$Ropt-(MegaData$Efin*MegaData$Ropt)+MegaData$m))*(1-(((1-MegaData$Efin)*(1-MegaData$Ropt)*MegaData$m)/((MegaData$Ropt-(MegaData$Efin*MegaData$Ropt)+MegaData$m)*MegaData$r)))) - ((1-MegaData$Efin)*((MegaData$r+MegaData$Efin-1)/MegaData$r)*MegaData$Kfin)
hist(MegaData$Hopt)


##What proportion of MSY is managed vs unmanaged?
MegaData %>% group_by(Manage) %>% summarise(msy=sum(MSYfin))
#21/60 --- 35% of the stocks have stock assessment

#plot curve for different MPA size
dH<-vector()
Rvec<-vector()
count<-0
FracMPA<-seq(0,1,0.01)
E<-MegaData$Efin[1]
m<-MegaData$m[1]
K<-MegaData$Kfin[1]
r<-MegaData$r[1]
for (R in FracMPA){
  count<-count+1
  dH[count]<-((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r)))) - ((1-E)*((r+E-1)/r)*K)
  Rvec[count]<-R
}
Mycurve<-cbind(Rvec,dH)
Mycurve<-as.data.frame(Mycurve)
plot(Mycurve$Rvec,Mycurve$dH)
#write.csv(Mycurve, file = "/Users/ren/Documents/CODES/FoodProvision/Curve_explore.csv")

#curve fitting
#install.packages("nls2")
#install.packages("minpack.lm")
library(nls2)
library(minpack.lm)
x  <- Mycurve$Rvec[1:37]
y <- Mycurve$dH[1:37]
p <- plot(x,y,pch=19)
nlsFit <- nlsLM(y ~b1*((((1489.9105/b1)^(1/b2))/0.3578660)*x)^b2,start=list(b1 = y[2]/x[2],b2=0.3))
newdata <- data.frame(x = seq(min(x),max(x),len=100))
predictLine <- lines(newdata$x,predict(nlsFit,newdata=newdata),col="red")
print(predictLine)

#derive parameters for Juan// Zonation
head(MegaData)
table(MegaData$Ropt)
ZonationMegaData<-MegaData %>% filter(Ropt>0) %>% filter(Ropt<=1)

hist(ZonationMegaData$Ropt)
max(ZonationMegaData$Ropt)
max(ZonationMegaData$Hopt)
min(ZonationMegaData$Hopt)

#other parameters
Zonation_others<- MegaData %>% filter(! (stockid %in% ZonationMegaData$stockid))
dim(Zonation_others)

#THIS IS FOR ALL THE SPECIES #unmanaged
#w1param<-vector()
xparam<-vector()
#Tparam<-vector()
FracMPA<-seq(0,1,0.0001)
pdf("/Users/ren/Documents/CODES/FoodProvision/Results/curvefit.pdf")
for (i in 1:dim(ZonationMegaData)[1]){
  dH<-vector()
  Rvec<-vector()
  count<-0
  E<-ZonationMegaData$Efin[i]
  m<-ZonationMegaData$m[i]
  K<-ZonationMegaData$Kfin[i]
  r<-ZonationMegaData$r[i]
  Hopt<-ZonationMegaData$Hopt[i]
  Ropt<-ZonationMegaData$Ropt[i]
  for (R in FracMPA){
    count<-count+1
    dH[count]<-((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r)))) - ((1-E)*((r+E-1)/r)*K)
    Rvec[count]<-R
  }
  Mycurve<-cbind(Rvec,dH)
  Mycurve<-as.data.frame(Mycurve)
  #plot(Mycurve$Rvec,Mycurve$dH)
  #write.csv(Mycurve, file = "/Users/ren/Documents/CODES/FoodProvision/Curve_explore.csv")
  #curve fitting
  
  maxposition<-which(Mycurve$dH==max(Mycurve$dH))
  
  x  <- Mycurve$Rvec[1:maxposition]
  y <- Mycurve$dH[1:maxposition]
  #  p <- plot(x,y,pch=19)
  #nlsFit <- nlsLM(y ~b1*((((Hopt/b1)^(1/b2))/Ropt)*x)^b2,start=list(b1 = y[2]/x[2],b2=0.5))
  nlsFit <- nlsLM(y ~Hopt*(x/Ropt)^b2,start=list(b2=0.5))
  newdata <- data.frame(x = seq(min(x),max(x),len=100))
  #predictLine <- lines(newdata$x,predict(nlsFit,newdata=newdata),col="red")
  #print(predictLine)
  plot(x,y,pch=19,main=ZonationMegaData$stockid[i])+
    lines(newdata$x,predict(nlsFit,newdata=newdata),col="red")  
  #w1param[i]<-coef(nlsFit)[1]
  #xparam[i]<-coef(nlsFit)[2]
  #Tparam[i]<-1/(((Hopt/coef(nlsFit)[1])^(1/coef(nlsFit)[2]))/Ropt)
  xparam[i]<-coef(nlsFit)
}
dev.off()

#ZonationMegaData$w1param<-w1param
ZonationMegaData$xparam<-xparam
#ZonationMegaData$Tparam<-Tparam
head(ZonationMegaData)
ZonationMegaData$ExploitationRate<-1-ZonationMegaData$Efin
ForZonationMegaData<-ZonationMegaData %>% select(stockid,Kfin,Ropt,Hopt,xparam,ExploitationRate,Kfin,m,r)
head(ForZonationMegaData)
write.csv(ForZonationMegaData, file = "/Users/ren/Documents/CODES/FoodProvision/ForZonationMegaData_Unmanaged.csv")


#THIS IS FOR OTHER SPECIES #MANAGED
w2param<-vector()
yparam<-vector()
FracMPA<-seq(0,1,0.0001)
pdf("/Users/ren/Documents/CODES/FoodProvision/Results/curvefitMANAGED.pdf")
for (i in 1:dim(Zonation_others)[1]){
  dH<-vector()
  Rvec<-vector()
  count<-0
  E<-Zonation_others$Efin[i]
  m<-Zonation_others$m[i]
  K<-Zonation_others$Kfin[i]
  r<-Zonation_others$r[i]
  Hopt<-Zonation_others$Hopt[i]
  Ropt<-Zonation_others$Ropt[i]
  for (R in FracMPA){
    count<-count+1
    dH[count]<-((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r)))) - ((1-E)*((r+E-1)/r)*K)
    Rvec[count]<-R
  }
  Mycurve<-cbind(Rvec,dH)
  Mycurve<-as.data.frame(Mycurve)
  #  plot(Mycurve$Rvec,Mycurve$dH,main=Zonation_others$stockid[i])
  maxposition<-which(Mycurve$dH==max(Mycurve$dH))
  x  <- Mycurve$Rvec
  y <- Mycurve$dH
  nlsFit <- nlsLM(y ~(w2*(x^y1)),start=list(w2=(y[2]/x[2])-0.1,y1=0.5))
  newdata <- data.frame(x = seq(min(x),max(x),len=100))
  plot(x,y,pch=19,main=Zonation_others$stockid[i])+
    lines(newdata$x,predict(nlsFit,newdata=newdata),col="red")  
  w2param[i]<-coef(nlsFit)[1]
  yparam[i]<-coef(nlsFit)[2]
}
dev.off()
Zonation_others$w2param<-w2param
Zonation_others$yparam<-yparam
Zonation_others$ExploitationRate<-1-Zonation_others$Efin
ForZonationMegaData_Managed<-Zonation_others %>% select(stockid,Kfin,Ropt,Hopt,w2param,yparam,ExploitationRate,Kfin,m,r)
write.csv(ForZonationMegaData_Managed, file = "/Users/ren/Documents/CODES/FoodProvision/ForZonationMegaData_Managed.csv")

####For Zonation BAU1---------BAU1 -------------BAU1
#Ropt, Hopt
MegaData$Ropt<-((MegaData$m*MegaData$r) + (((2*MegaData$Efin_BAU1)-2)*MegaData$m)) / (((MegaData$Efin_BAU1-1)*MegaData$r)+(((2*MegaData$Efin_BAU1)-2)*MegaData$m))

#Given Ropt, what is Hopt???
MegaData$Hopt<-((1-MegaData$Efin_BAU1)*((MegaData$m*MegaData$Kfin*(1-MegaData$Ropt))/(MegaData$Ropt-(MegaData$Efin_BAU1*MegaData$Ropt)+MegaData$m))*(1-(((1-MegaData$Efin_BAU1)*(1-MegaData$Ropt)*MegaData$m)/((MegaData$Ropt-(MegaData$Efin_BAU1*MegaData$Ropt)+MegaData$m)*MegaData$r)))) - ((1-MegaData$Efin_BAU1)*((MegaData$r+MegaData$Efin_BAU1-1)/MegaData$r)*MegaData$Kfin)

#derive parameters for Juan// Zonation
ZonationMegaData<-MegaData %>% filter(Ropt>0) %>% filter(Ropt<=1)

#other parameters
Zonation_others<- MegaData %>% filter(! (stockid %in% ZonationMegaData$stockid))

#THIS IS FOR ALL THE SPECIES #unmanaged
xparam<-vector()
FracMPA<-seq(0,1,0.0001)
pdf("/Users/ren/Documents/CODES/FoodProvision/Results/curvefit.pdf")
for (i in 1:dim(ZonationMegaData)[1]){
  dH<-vector()
  Rvec<-vector()
  count<-0
  E<-ZonationMegaData$Efin_BAU1[i]
  m<-ZonationMegaData$m[i]
  K<-ZonationMegaData$Kfin[i]
  r<-ZonationMegaData$r[i]
  Hopt<-ZonationMegaData$Hopt[i]
  Ropt<-ZonationMegaData$Ropt[i]
  for (R in FracMPA){
    count<-count+1
    dH[count]<-((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r)))) - ((1-E)*((r+E-1)/r)*K)
    Rvec[count]<-R
  }
  Mycurve<-cbind(Rvec,dH)
  Mycurve<-as.data.frame(Mycurve)
  
  maxposition<-which(Mycurve$dH==max(Mycurve$dH))
  
  x  <- Mycurve$Rvec[1:maxposition]
  y <- Mycurve$dH[1:maxposition]

  nlsFit <- nlsLM(y ~Hopt*(x/Ropt)^b2,start=list(b2=0.5))
  newdata <- data.frame(x = seq(min(x),max(x),len=100))

  plot(x,y,pch=19,main=ZonationMegaData$stockid[i])+
    lines(newdata$x,predict(nlsFit,newdata=newdata),col="red")  

  xparam[i]<-coef(nlsFit)
}
dev.off()

ZonationMegaData$xparam<-xparam
head(ZonationMegaData)
ZonationMegaData$ExploitationRate<-1-ZonationMegaData$Efin_BAU1
ForZonationMegaData_BAU1<-ZonationMegaData %>% select(stockid,Kfin,Ropt,Hopt,xparam,ExploitationRate,Kfin,m,r)
write.csv(ForZonationMegaData_BAU1, file = "/Users/ren/Documents/CODES/FoodProvision/ForZonationMegaData_Unmanaged_BAU1.csv")


#THIS IS FOR OTHER SPECIES #MANAGED
w2param<-vector()
yparam<-vector()
FracMPA<-seq(0,1,0.0001)
pdf("/Users/ren/Documents/CODES/FoodProvision/Results/curvefitMANAGED.pdf")
for (i in 1:dim(Zonation_others)[1]){
  dH<-vector()
  Rvec<-vector()
  count<-0
  E<-Zonation_others$Efin_BAU1[i]
  m<-Zonation_others$m[i]
  K<-Zonation_others$Kfin[i]
  r<-Zonation_others$r[i]
  Hopt<-Zonation_others$Hopt[i]
  Ropt<-Zonation_others$Ropt[i]
  for (R in FracMPA){
    count<-count+1
    dH[count]<-((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r)))) - ((1-E)*((r+E-1)/r)*K)
    Rvec[count]<-R
  }
  Mycurve<-cbind(Rvec,dH)
  Mycurve<-as.data.frame(Mycurve)

  maxposition<-which(Mycurve$dH==max(Mycurve$dH))
  x  <- Mycurve$Rvec
  y <- Mycurve$dH
  nlsFit <- nlsLM(y ~(w2*(x^y1)),start=list(w2=(y[2]/x[2])-0.1,y1=0.5))
  newdata <- data.frame(x = seq(min(x),max(x),len=100))
  plot(x,y,pch=19,main=Zonation_others$stockid[i])+
    lines(newdata$x,predict(nlsFit,newdata=newdata),col="red")  
  w2param[i]<-coef(nlsFit)[1]
  yparam[i]<-coef(nlsFit)[2]
}
dev.off()
Zonation_others$w2param<-w2param
Zonation_others$yparam<-yparam
Zonation_others$ExploitationRate<-1-Zonation_others$Efin_BAU1
ForZonationMegaData_Managed_BAU1<-Zonation_others %>% select(stockid,Kfin,Ropt,Hopt,w2param,yparam,ExploitationRate,Kfin,m,r)
write.csv(ForZonationMegaData_Managed_BAU1, file = "/Users/ren/Documents/CODES/FoodProvision/ForZonationMegaData_Managed_BAU1.csv")





###PLOT K using the new data!!!!!!!!!!!!!!!!!!!!!!!!
#files needed: Aquaothers2, MegaData 
#saveRDS(Aquaothers2, file = "/Users/ren/Documents/CODES/FoodProvision/Aquaothers2.rds")
Aquaothers2<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Aquaothers2.rds")
MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")

head(Aquaothers2)
#K and SpeciesID
head(MegaData)
MagaData_K<-MegaData %>% filter(Manage==0) 
includeREV<-MagaData_K %>% select(SpeciesID,r,MSYtotal) %>% mutate(Ktotal=4*MSYtotal/r) %>% select(SpeciesID,Ktotal)
Aqua3Rev<-merge(Aquaothers2,includeREV,by="SpeciesID")
head(Aqua3Rev)
###
#length(unique(Aqua3$SpeciesID))
Aqua3Rev <- Aqua3Rev %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
Aqua3stackRev<-Aqua3Rev %>% group_by(CenterLat,CenterLong) %>% summarise(S=sum(probability*Ktotal/totalprob))
#head(Aqua3stackRev) #S is total K per cell

Aqua3stackRev<-as.data.frame(Aqua3stackRev)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(Aqua3stackRev[,2:1]))
empty_raster[cells] <- Aqua3stackRev[,3]
plot(empty_raster,main="Carrying capacity per cell (MT)")

png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all_flat.png", width = 8, height = 5, units = 'in', res = 300)
plot(empty_raster)
dev.off()

#----K PLOT - BASED ON GGPLOT
head(Aqua3stackRev)
land_shp_moll<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")
KPLOTFin<- Aqua3stackRev %>% 
  set_names(c("lat", "lon", "K")) %>%
  select(lon, lat, K)%>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
  raster::projectRaster(crs = "+proj=moll") %>% 
  as.data.frame(xy = T) %>%
  filter(!is.na(K)) %>%
  set_names(c("lon", "lat", "K")) %>%
  ggplot(aes(x=lon,y=lat,fill=K)) + labs(fill="K (MT/cell)")+scale_fill_gradient(low="white", high="#00539CFF")+
  #guides(fill=guide_legend())+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
  geom_raster()+
  geom_sf(data = land_shp_moll, inherit.aes = F)
KPLOTFin
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/KPLOTFin.png", KPLOTFin,width = 10, height = 8, dpi = 300, units = "in")#resolution not great

#--Kplot based on Juan's code
max(Aqua3stackRev$S)
min(Aqua3stackRev$S)

#Qresult<-result[result>0]
T1<-0
T2<-100
T3<-500
T4<-5000
T5<-10000
T6<-50000
T7<-100000.0
T8<-max(Aqua3stackRev$S)#335857.3

library(tmap)
library(leaflet)
ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
crs(empty_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
maxValue(empty_raster)
z_pal <- list(breaks = c(T1,T2,T3,T4,T5,T6,T7,T8),
              labels = c(paste(T1,"-",T2), paste(T2,"-",T3), paste(T3,"-",T4), paste(T4,"-",T5), paste(T5,"-",T6), paste(T6,"-",T7), paste(T7,"-",round(T8))),
              colors = rev(c("#d73027","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1", "#4575b4")))
land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
caption<-"Pixel-level food provisioning potential"
land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))

Kplot<-empty_raster %>% 
  raster::projectRaster(ocean_low_res_moll) %>% 
  tmap::tm_shape()+
  tmap::tm_raster(title = "K (MT/cell)",
                  palette  = z_pal$colors,
                  breaks = z_pal$breaks,
                  labels = z_pal$labels,
                  legend.is.portrait = T,
                  legend.reverse = T)+
  tmap::tm_shape(land_shp_moll)+
  tmap::tm_fill(col = "black", border.col = "transparent")+
  #tmap::tm_credits(caption) +
  tmap::tm_layout(#title = "Food provision potential (MT)",
    #title.position = c("center", .95),
    inner.margins = c(0.12, 0, 0.08, 0.04),
    frame = F,
    legend.position = c(.99, "center"))
Kplot

#ggsave("/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all.tiff", plot=Kplot,dpi=300)

tiff(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all.tiff", width = 12, height = 6, units = 'in', res = 300)
Kplot
dev.off()


#This is a data request. Ok to ignore next time.
#Question: 


#test if I can combine tmap with ggplot
library(cowplot)
library(ggplot2)
library(magick)
library(png)
library(grid)
library(gridExtra)
library(tiff)
p2<-readTIFF("/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all.tiff")
grid.arrange(rasterGrob(p2),rasterGrob(p2),ncol=1)


#OK, INSERT code for identifying ocean areas for country-level analysis
head(Aqua3stackRev)
oceancoord<-Aqua3stackRev %>% select(CenterLat,CenterLong)
colnames(oceancoord) <- c("lat","lon")
#load EEZ file from Juan
highreseezgrid<-read.csv("/Users/ren/Documents/CODES/FoodProvision/EEZfile/high_res_eez_grid.csv")
#convert to 0.5 resolution
res<-0.5
highreseezgrid$CenterLat<-floor(highreseezgrid$lat_bin_center/res)*res+ 0.5*res
highreseezgrid$CenterLong<-floor(highreseezgrid$lon_bin_center/res)*res+ 0.5*res

highreseezgrid<-highreseezgrid %>% select(territory_iso3,sovereign_iso3,CenterLat,CenterLong)
colnames(highreseezgrid) <- c("territory","sovereign","lat","lon")
highreseezgrid2<-left_join(oceancoord,highreseezgrid,by=c("lat","lon"))
head(highreseezgrid2)
table(highreseezgrid2$sovereign)

countries<-c("AUS", "CAN", "CHL", "FJI", "GHA", "IDN", "IND", "JAM", "JPN", "KEN", "MEX", "NAM", "NOR", "PLW", "PRT")

EEZs<-highreseezgrid2 %>% filter(is.na(sovereign)==F)#%>% filter(sovereign %in% countries)
head(EEZs)
EEZs_coord<-unique(EEZs[c("lat", "lon")])
head(EEZs_coord)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(EEZs_coord[,2:1]))
empty_raster[cells] <- 1
plot(empty_raster,main="EEZs")
#save EEZs_coord
saveRDS(EEZs_coord,file = "/Users/ren/Documents/CODES/FoodProvision/EEZfile/EEZs_coord.rds")

HighSeas<-highreseezgrid2 %>% filter(is.na(sovereign)==T)#%>% filter(sovereign %in% countries)
HighSeas_coord<-unique(HighSeas[c("lat", "lon")])
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(HighSeas_coord[,2:1]))
empty_raster[cells] <- 1
plot(empty_raster,main="High Seas")
saveRDS(HighSeas_coord,file = "/Users/ren/Documents/CODES/FoodProvision/EEZfile/HighSeas_coord.rds")

#Example
AUS<-highreseezgrid2 %>% filter(sovereign=="AUS")
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(AUS[,2:1]))
empty_raster[cells] <- 1
plot(empty_raster,main="AUS")

CAN<-highreseezgrid2 %>% filter(sovereign=="CAN")
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(CAN[,2:1]))
empty_raster[cells] <- 1
plot(empty_raster,main="CAN")

#load MPA file and just check what it looks like
MPAcurrent<-read.csv("/Users/ren/Documents/CODES/FoodProvision/EEZfile/politically_correct_mpas.csv")
head(MPAcurrent)
table(MPAcurrent$mpa)
#convert to 0.5 resolution
res<-0.5
MPAcurrent$lon<-floor(MPAcurrent$lon/res)*res+ 0.5*res
MPAcurrent$lat<-floor(MPAcurrent$lat/res)*res+ 0.5*res
#get only mpa==1
MPAcurrent<-MPAcurrent %>% filter(mpa==1)
dim(MPAcurrent)
MPA_coord<-unique(MPAcurrent[c("lat", "lon")])
dim(MPA_coord)
head(MPA_coord)
saveRDS(MPA_coord,file = "/Users/ren/Documents/CODES/FoodProvision/EEZfile/MPA_coord.rds")

#---compute K per m!!!!!!!!!!!!!!! (for SI)
#carrying capacity, m=0.1
MagaData_K<-MegaData %>% filter(Manage==0) %>% filter(m==0.1) #managed only
head(MagaData_K)
includeREV<-MagaData_K %>% select(SpeciesID,r,MSYtotal) %>% mutate(Ktotal=4*MSYtotal/r) %>% select(SpeciesID,Ktotal)
Aqua3Rev<-merge(Aquaothers2,includeREV,by="SpeciesID")
Aqua3Rev <- Aqua3Rev %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
Aqua3stackRev<-Aqua3Rev %>% group_by(CenterLat,CenterLong) %>% summarise(S=sum(probability*Ktotal/totalprob))
Aqua3stackRev<-as.data.frame(Aqua3stackRev)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(Aqua3stackRev[,2:1]))
empty_raster[cells] <- Aqua3stackRev[,3]
plot(PlotFunction(empty_raster),main="Carrying capacity per cell (MT), m=0.1")
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all_flat_m01.png", width = 8, height = 5, units = 'in', res = 300)
plot(empty_raster)
dev.off()

land_shp_moll<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")
head(Aqua3stackRev)
max(Aqua3stackRev$S)
m01<- Aqua3stackRev %>% 
  set_names(c("lat", "lon", "K")) %>%
  select(lon, lat, K)%>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
  raster::projectRaster(crs = "+proj=moll") %>% 
  as.data.frame(xy = T) %>%
  filter(!is.na(K)) %>%
  set_names(c("lon", "lat", "K")) %>%
  ggplot(aes(x=lon,y=lat,fill=K)) + scale_fill_gradient(low="white", high="#00539CFF",name="K (MT/pixel)",limit=c(0,max(Aqua3stackRev$S)))+#guides(fill=guide_legend())+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+ #theme_bw()+
  geom_raster()+
  #geom_sf(data = land_shp_moll, inherit.aes = F)
  geom_sf(data = land_shp_moll, fill="darkgray", lwd = 0.1, inherit.aes = F)
m01
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/m01.png", m01, width = 10, height = 8, dpi = 300, units = "in")#resolution not great


#carrying capacity, m=0.3
MagaData_K<-MegaData %>% filter(Manage==0) %>% filter(m==0.3) #managed only
head(MagaData_K)
includeREV<-MagaData_K %>% select(SpeciesID,r,MSYtotal) %>% mutate(Ktotal=4*MSYtotal/r) %>% select(SpeciesID,Ktotal)
Aqua3Rev<-merge(Aquaothers2,includeREV,by="SpeciesID")
Aqua3Rev <- Aqua3Rev %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
Aqua3stackRev<-Aqua3Rev %>% group_by(CenterLat,CenterLong) %>% summarise(S=sum(probability*Ktotal/totalprob))
Aqua3stackRev<-as.data.frame(Aqua3stackRev)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(Aqua3stackRev[,2:1]))
empty_raster[cells] <- Aqua3stackRev[,3]
plot(empty_raster,main="Carrying capacity per cell (MT), m=0.3")
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all_flat_m03.png", width = 8, height = 5, units = 'in', res = 300)
plot(empty_raster)
dev.off()

m3<- Aqua3stackRev %>% 
  set_names(c("lat", "lon", "K")) %>%
  select(lon, lat, K)%>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
  raster::projectRaster(crs = "+proj=moll") %>% 
  as.data.frame(xy = T) %>%
  filter(!is.na(K)) %>%
  set_names(c("lon", "lat", "K")) %>%
  ggplot(aes(x=lon,y=lat,fill=K)) + scale_fill_gradient(low="white", high="#00539CFF",name="K (MT/pixel)",limit=c(0,max(Aqua3stackRev$S)))+#guides(fill=guide_legend())+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+ #theme_bw()+
  geom_raster()+
  #geom_sf(data = land_shp_moll, inherit.aes = F)
  geom_sf(data = land_shp_moll, fill="darkgray", lwd = 0.1, inherit.aes = F)
m3
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/m3.png", m3, width = 10, height = 8, dpi = 300, units = "in")#resolution not great

#carrying capacity, m=9
MagaData_K<-MegaData %>% filter(Manage==0) %>% filter(m==0.9) #managed only
head(MagaData_K)
includeREV<-MagaData_K %>% select(SpeciesID,r,MSYtotal) %>% mutate(Ktotal=4*MSYtotal/r) %>% select(SpeciesID,Ktotal)
Aqua3Rev<-merge(Aquaothers2,includeREV,by="SpeciesID")
Aqua3Rev <- Aqua3Rev %>% group_by(SpeciesID) %>% mutate(totalprob=sum(probability))
Aqua3stackRev<-Aqua3Rev %>% group_by(CenterLat,CenterLong) %>% summarise(S=sum(probability*Ktotal/totalprob))
Aqua3stackRev<-as.data.frame(Aqua3stackRev)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(Aqua3stackRev[,2:1]))
empty_raster[cells] <- Aqua3stackRev[,3]
plot(empty_raster,main="Carrying capacity per cell (MT), m=0.9")
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Kplot_all_flat_m09.png", width = 8, height = 5, units = 'in', res = 300)
plot(empty_raster)
dev.off()

m9<- Aqua3stackRev %>% 
  set_names(c("lat", "lon", "K")) %>%
  select(lon, lat, K)%>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
  raster::projectRaster(crs = "+proj=moll") %>% 
  as.data.frame(xy = T) %>%
  filter(!is.na(K)) %>%
  set_names(c("lon", "lat", "K")) %>%
  ggplot(aes(x=lon,y=lat,fill=K)) + scale_fill_gradient(low="white", high="#00539CFF",name="K (MT/pixel)",limit=c(0,max(Aqua3stackRev$S)))+#guides(fill=guide_legend())+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+ #theme_bw()+
  geom_raster()+
  #geom_sf(data = land_shp_moll, inherit.aes = F)
  geom_sf(data = land_shp_moll, fill="darkgray", lwd = 0.1, inherit.aes = F)
m9
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/m9.png", m9, width = 10, height = 8, dpi = 300, units = "in")#resolution not great

##

###Compute spillover---PIXEL-LEVEL spillover
dim(Cleanmegacell)
dim(MegaData)
head(Cleanmegacell)
head(MegaData)

#UNMANAGED data
UNMNG_MegaData<-MegaData %>% filter(Manage==0)
UNMNG_Cleanmegacell<-Cleanmegacell %>% select(UNMNG_MegaData$stockid)
#Managed data
MNG_MegaData<-MegaData %>% filter(Manage==1)
MNG_Cleanmegacell<-Cleanmegacell %>% select(MNG_MegaData$stockid)


#1. UNMANAGED only
UNMNG_numcell<-dim(UNMNG_Cleanmegacell)[1]
K<-UNMNG_MegaData$Kfin #k per species
m<-UNMNG_MegaData$m #mobility per species
#Harvest without MPA
R <-0 #MPA size
r<-UNMNG_MegaData$r
E<-UNMNG_MegaData$Efin
#----Harvest with no MPA, BAU, no climate
HBAU <- sum((1-E)*((r+E-1)/r)*K)

MPAselect0<-matrix(0, nrow=UNMNG_numcell, ncol=1)
TUNMNG_Cleanmegacell<-t(UNMNG_Cleanmegacell)

cores<-detectCores()
registerDoParallel(cores)
system.time({
  UNMNG_result <- foreach(iter = 1:240, .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[iter]<-1
    
    keeps<-which(MPAselect==1)
    MPAselect2<-as.matrix(MPAselect[keeps,])
    TUNMNG_Cleanmegacell2<-as.matrix(TUNMNG_Cleanmegacell[,keeps])
    Kprotected<-as.data.frame(TUNMNG_Cleanmegacell2 %*% MPAselect2)
    
    R<-Kprotected$V1
    hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
    hmpa<-hmpa*(hmpa>0)
    HMPA<-sum(hmpa)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
    HMPA-HBAU
  }
})
plot(UNMNG_result)
stopImplicitCluster()

#2. MANAGED only
MNG_numcell<-dim(MNG_Cleanmegacell)[1]
K<-MNG_MegaData$Kfin #k per species
m<-MNG_MegaData$m #mobility per species
#Harvest without MPA
R <-0 #MPA size
r<-MNG_MegaData$r
E<-MNG_MegaData$Efin
#----Harvest with no MPA, BAU, no climate
HBAU <- sum((1-E)*((r+E-1)/r)*K)

MPAselect0<-matrix(0, nrow=MNG_numcell, ncol=1)
TMNG_Cleanmegacell<-t(MNG_Cleanmegacell)

cores<-detectCores()
registerDoParallel(cores)
system.time({
  MNG_result <- foreach(iter = 1:24, .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[iter]<-1
    Kprotected<-as.data.frame(TMNG_Cleanmegacell %*% MPAselect)
    R<-Kprotected$V1
    hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
    hmpa<-hmpa*(hmpa>0)
    HMPA<-sum(hmpa)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
    HMPA-HBAU
  }
})
MNG_result
stopImplicitCluster()

#Files to transfer to bigger machine 
#MegaData, Cleanmegacell, and the coordinates --
saveRDS(Cleanmegacell, file = "/Users/ren/Documents/CODES/FoodProvision/Cleanmegacell.rds")
saveRDS(MegaData, file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")
saveRDS(CleanCoordmegacell, file = "/Users/ren/Documents/CODES/FoodProvision/CleanCoordmegacell.rds")
#colnames(Cleanmegacell)

##YOU CAN RESTART R NOW TO REFRESH MEMORY

#load big files
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Cleanmegacell.rds")
MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")
CleanCoordmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/CleanCoordmegacell.rds")

#3.COMBINED MANAGED AND UNMANAGED
numcell<-dim(Cleanmegacell)[1]
dH<-matrix(0, nrow=numcell, ncol=1)
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
#Harvest without MPA
R <-0 #MPA size
r<-MegaData$r
E<-MegaData$Efin
#----Harvest with no MPA, BAU, no climate
HBAU <- sum((1-E)*((r+E-1)/r)*K)
HBAUpixel<-(1-E)*((r+E-1)/r)*K
deltaHpixel<-(K<0)*1
#under business as usual, top x species 


MPAselect0<-matrix(0, nrow=numcell, ncol=1)
#Norm_K_filter: row is species name, col is pixel id, content is normaized K
#TNorm_K_filter<-t(Norm_K_filter)
TCleanmegacell<-t(Cleanmegacell)

# #response per pixel. Not useful!
# for (iter in 1:numcell) {
#   MPAselect<-MPAselect0
#   MPAselect[iter]<-1
#   
#   keeps<-which(MPAselect==1)
#   MPAselect2<-as.matrix(MPAselect[keeps,])
#   TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
#   Kprotected<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
#   
#   R<-Kprotected$V1
#   HMPApixel<-(1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r)))
#   HMPApixel[is.na(HMPApixel)]<-0
#   deltaHpixel<-deltaHpixel + HMPApixel - HBAUpixel
# }
# plot(deltaHpixel)
# sum(deltaHpixel)
# #plot K per stock per species for SI
# StockList<-MegaData %>% select(stockid,SciName,MSYfin,Kfin)
# StockList$deltaH<-deltaHpixel
# StockList$deltaH_MSY<-StockList$deltaH/StockList$MSYfin
# plot(StockList$deltaH_MSY)
# head(StockList)
# # Horizontal bar plots, 
# # change fill color by groups and add text labels
# deltaHpixelplot<-StockList[order(-StockList$deltaH),] %>% slice(1:50)
# plotdeltaHpixelplot<-ggplot(deltaHpixelplot, aes(x = reorder(stockid, deltaH), y = deltaH)) +
#   geom_bar(fill="steelblue",stat = "identity") +
#   coord_flip() +
#   geom_text(aes(label = SciName,size=14), nudge_y = 0, color = "black")+
#   labs(y = "Carrying capacity, K (MT)", x="Fish stock")+ #ylim(0, 4.5e7)+
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=16,face="bold"),
#         legend.position="none")
# plotdeltaHpixelplot #this is global


#MAKE A LOOK-UP TABLE, each pixel have values of how much of the geog range of each species is in MPA
# ##NO NEED TO RERUN
# KprotectedPerCell<-vector()
# MPAselect0<-matrix(0, nrow=numcell, ncol=1)
# for (iter in 1:numcell){
#   MPAselect<-MPAselect0
#   MPAselect[iter]<-1
#   keeps<-which(MPAselect==1)
#   MPAselect2<-as.matrix(MPAselect[keeps,])
#   TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
#   KprotectedPerCell[iter]<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
# }
# KprotectedPerCell_Library<-as.data.frame(KprotectedPerCell)
# names(KprotectedPerCell_Library) <- 1:numcell
# head(KprotectedPerCell_Library)
# dim(KprotectedPerCell_Library)
# sum(KprotectedPerCell_Library[1000])
# saveRDS(KprotectedPerCell_Library, file = "/Users/ren/Documents/CODES/FoodProvision/KprotectedPerCell_Library.rds")


cores<-detectCores()
registerDoParallel(cores)
system.time({
  result <- foreach(iter = 1:numcell, .combine = cbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[iter]<-1
    
    keeps<-which(MPAselect==1)
    MPAselect2<-as.matrix(MPAselect[keeps,])
    TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
    Kprotected<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
    
    R<-Kprotected$V1
    HMPA<-sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
    HMPA-HBAU
  }
})
max(result)
min(result)
stopImplicitCluster()
#stopCluster(myCluster)

#pixellevelresult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Results/foodprovision2July.rds")
#plot(pixellevelresult)

#quantile(result,0)

# Or you can pass in your own quantiles:
#quantile(result, q = c(0.125,0.25,0.375,0.5,0.625,0.75))

#Qresult<-result[result>0]
T1<-min(result)
T2<-0
T3<-100
T4<-500
T5<-1000
T6<-5000
T7<-10000
T8<-max(result)

#plot same as Juan
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(CleanCoordmegacell[,1:2]))
empty_raster[cells] <- result
plot(empty_raster)

library(tmap)
library(leaflet)
ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
crs(empty_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
maxValue(empty_raster)
z_pal <- list(breaks = c(T1,T2,T3,T4,T5,T6,T7,T8),
              labels = c(paste(round(T1),"-",T2), paste(T2,"-",T3), paste(T3,"-",T4), paste(T4,"-",T5), paste(T5,"-",T6), paste(T6,"-",T7), paste(T7,"-",round(T8))),
              colors = rev(c("#d73027","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1", "#4575b4")))
land_shp <-st_read("/Users/ren/Documents/CODES/FoodProvision/landshp_moll/spatial-datasets-land-land_50.shp")
ocean_low_res_moll<-raster::raster("/Users/ren/Documents/CODES/FoodProvision/ocean-low-res-moll.tiff")
caption<-"Pixel-level food provisioning potential"
land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))

pixellevelfood<-empty_raster %>% 
  raster::projectRaster(ocean_low_res_moll) %>% 
  tmap::tm_shape()+
  tmap::tm_raster(title = expression(paste(Delta, "H (MT)")),
                  palette  = z_pal$colors,
                  breaks = z_pal$breaks,
                  labels = z_pal$labels,
                  legend.is.portrait = T,
                  legend.reverse = T)+
  tmap::tm_shape(land_shp_moll)+
  tmap::tm_fill(col = "black", border.col = "transparent")+
  #tmap::tm_credits(caption) +
  tmap::tm_layout(#title = "Food provision potential (MT)",
                  #title.position = c("center", .95),
                  inner.margins = c(0.12, 0, 0.08, 0.04),
                  frame = F,
                  legend.position = c(.99, "center"))
pixellevelfood

png(file="/Users/ren/Documents/CODES/FoodProvision/Results/Fig2_PixelLevelFoodProv.png", width = 12, height = 6, units = 'in', res = 300)
pixellevelfood
dev.off()



###OK to restart R and load
MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")
head(MegaData)
SpeciesInfo <- MegaData %>% select(stockid,Manage,SciName,MSYfin)

#Performance of per species as we close more of the ocean
plotmarginal2<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/plotmarginal2.rds")
head(plotmarginal2)  
colnames(plotmarginal2) <- c("stockid","MPA","deltaH")
plotmarginal3<-left_join(plotmarginal2,SpeciesInfo,by="stockid")
head(plotmarginal3)
ggplot(plotmarginal3, aes(x=MPA,y=deltaH))+geom_point()+facet_wrap( ~ Manage)

#I want to plot just the managed
manageddeltaH<-plotmarginal3 %>% filter(Manage==1)

pdf("/Users/ren/Documents/CODES/FoodProvision/Results/deltaHpersp.pdf")
for (i in unique(manageddeltaH$stockid)){
  manageddeltaHsp<-manageddeltaH %>% filter(stockid==i)  

  plot(manageddeltaHsp$MPA,manageddeltaHsp$deltaH,main=paste(i,"/",manageddeltaHsp$SciName[1]),ylim=c(min(manageddeltaHsp$deltaH),manageddeltaHsp$MSYfin[1]))
  abline(h=manageddeltaHsp$MSYfin[1], col="blue")
  #ggplot(manageddeltaHsp, aes(x=MPA,y=deltaH))+geom_point()+labs(title=i)
}
dev.off()

#Iplot the unmanaged
unmanageddeltaH<-plotmarginal3 %>% filter(Manage==0)
pdf("/Users/ren/Documents/CODES/FoodProvision/Results/deltaHpersp_unmanaged.pdf")
for (i in unique(unmanageddeltaH$stockid)){
  unmanageddeltaHsp<-unmanageddeltaH %>% filter(stockid==i)  
  
  plot(unmanageddeltaHsp$MPA,unmanageddeltaHsp$deltaH,main=paste(i,"/",unmanageddeltaHsp$SciName[1]),ylim=c(min(unmanageddeltaHsp$deltaH),unmanageddeltaHsp$MSYfin[1]))
  abline(h=unmanageddeltaHsp$MSYfin[1], col="blue")
  #ggplot(manageddeltaHsp, aes(x=MPA,y=deltaH))+geom_point()+labs(title=i)
}
dev.off()

#check Managed result with the Megadata file
MegaDataManaged<-MegaData %>% filter(Manage==1)

#--------------------------
####Network code -- PICK 1000 at a time
#there are 168,712 cells so 1:168 #per 1000 is 0.593%. Up to 10%, it will be 16.87
#files needed: 
KprotectedPerCell_Library<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/KprotectedPerCell_Library.rds")

##TRY new approach
numcell<-dim(Cleanmegacell)[1]
celltoiterateFULL<-1:numcell
celltoiterate<-celltoiterateFULL
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
#TCleanmegacell<-t(Cleanmegacell)
PriorityAreas<-c()
NetworkResult<-vector()

###Compute spillover---PIXEL-LEVEL spillover #in case not yet computed
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
#Harvest without MPA
R <-0 #MPA size
r<-MegaData$r
E<-MegaData$Efin
#----Harvest with no MPA, BAU, no climate
hbau <-(1-E)*((r+E-1)/r)*K
hbau <-hbau*(hbau>0)
#table(hbau) #negative harvest should be zero harvest! check code tomorrow.
HBAU <- sum(hbau)#sum((1-E)*((r+E-1)/r)*K)
HBAU

nmax<-20
PerSpDeltaH<-matrix(nrow=nmax,ncol=1342)
#head(PerSpDeltaH)

###this block is for implementing EEZ only selection
head(EEZs_coord)
EEZs_coord$EEZ<-1
CleanCoordmegacell_EEZ<-left_join(CleanCoordmegacell,EEZs_coord,by=c("lon","lat"))
head(CleanCoordmegacell_EEZ)
dim(CleanCoordmegacell_EEZ)
#positions of 1s
EEZposition<-which(CleanCoordmegacell_EEZ$EEZ==1)
celltoiterate<-EEZposition#celltoiterateFULL
length(celltoiterate)

cores<-detectCores()
registerDoParallel(cores)
for (i in 1:nmax){ 
  MPAselectPrev<-rowSums(KprotectedPerCell_Library[,which(MPAselect0==1),drop=FALSE])
  result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[celltoiterate[iter]]<-1
    R<-MPAselectPrev+KprotectedPerCell_Library[,celltoiterate[iter]]
    hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
    hmpa<-hmpa*(hmpa>0)
    HMPA<-sum(hmpa)
    HMPA-HBAU
  }
  #1. find the location of the top 1000 highest pixel-level
  myorderHightoLow<-order(-result)#positions
  cellselected<-myorderHightoLow[1:1000] #but these are the position of the temporary pixels, not our reference pixels
  #convert coord to scale comparable to priority areas
  Prioritycellselected<-celltoiterate[cellselected]
  #3. block those additional 1000 in MPAselect
  MPAselect0[Prioritycellselected]<-1
  #3. save them for our priority areas
  PriorityAreas<-append(PriorityAreas,Prioritycellselected)
  #4. Calculate food prov of the additional 1000 cells
  MPAselect<-MPAselect0
  R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
  hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
  HMPA-HBAU
  
  #save result
  PerSpDeltaH[i,]<-hmpa-hbau
  NetworkResult[i]<-HMPA-HBAU
  
  #pass this to the top
  celltoiterate<-celltoiterateFULL[-PriorityAreas]
  print(c(i,NetworkResult[i]))
  rm(result,myorderHightoLow,cellselected,Prioritycellselected, MPAselect,keeps,MPAselect2,TCleanmegacell2,Kprotected,R,hmpa,HMPA)
}
plot(NetworkResult)


cores<-detectCores()
registerDoParallel(cores)
for (i in 1:nmax){ 
#for (i in 1:168){ 
  #be sure to make celltoiterate adaptive
  
  MPAselectPrev<-rowSums(KprotectedPerCell_Library[,which(MPAselect0==1),drop=FALSE])

  result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[celltoiterate[iter]]<-1
    
    R<-MPAselectPrev+KprotectedPerCell_Library[,celltoiterate[iter]]
    
    #R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])

    # keeps<-which(MPAselect==1)
    # MPAselect2<-as.matrix(MPAselect[keeps,])
    # TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
    # Kprotected<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
    
    #R<-Kprotected$V1
    hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
    hmpa<-hmpa*(hmpa>0)
    HMPA<-sum(hmpa)
    HMPA-HBAU
  }
  
  #1. find the location of the top 1000 highest pixel-level
  myorderHightoLow<-order(-result)#positions
  cellselected<-myorderHightoLow[1:1000] #but these are the position of the temporary pixels, not our reference pixels
  #convert coord to scale comparable to priority areas
  Prioritycellselected<-celltoiterate[cellselected]
  #plot(result[myorderHightoLow][1:1000])#plot values for demo
  
  #3. block those additional 1000 in MPAselect
  MPAselect0[Prioritycellselected]<-1
  #3. save them for our priority areas
  PriorityAreas<-append(PriorityAreas,Prioritycellselected)
  #4. Calculate food prov of the additional 1000 cells
  MPAselect<-MPAselect0
  
  # keeps<-which(MPAselect==1)
  # MPAselect2<-as.matrix(MPAselect[keeps,])
  # TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
  # Kprotected<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
  
  R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
  
  #Kprotected<-as.data.frame(TCleanmegacell %*% MPAselect)
  #R<-Kprotected$V1
  hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
  HMPA-HBAU
  
  PerSpDeltaH[i,]<-hmpa-hbau
  #save result
  NetworkResult[i]<-HMPA-HBAU
  
  #pass this to the top
  celltoiterate<-celltoiterateFULL[-PriorityAreas]
  print(c(i,NetworkResult[i]))
  rm(result,myorderHightoLow,cellselected,Prioritycellselected, MPAselect,keeps,MPAselect2,TCleanmegacell2,Kprotected,R,hmpa,HMPA)
}
plot(NetworkResult)
stopImplicitCluster()

head(PerSpDeltaH)
DeltaPerSpDeltaH<-PerSpDeltaH[1:nmax-1,]
DeltaPerSpDeltaH<-rbind(0,DeltaPerSpDeltaH)
DeltaPerSpDeltaH<-PerSpDeltaH-DeltaPerSpDeltaH
DeltaPerSpDeltaH2<-as.data.frame(DeltaPerSpDeltaH)
#melt this
colnames(DeltaPerSpDeltaH2)<-1:1342
DeltaPerSpDeltaH2$ID<-1:nmax

#combine with megadata
head(MegaData)

LongDeltaHpersp <- melt(DeltaPerSpDeltaH2, id=c("ID"))
head(LongDeltaHpersp)


MegaCategory<-MegaData %>% select(Manage,stockid,SciName,m)
MegaCategory$variable<-as.factor(1:1342)
head(MegaCategory)
LongDeltaHpersp2<- left_join(LongDeltaHpersp,MegaCategory,by="variable")
LongDeltaHpersp2$Manage<-as.factor(LongDeltaHpersp2$Manage)
LongDeltaHpersp2$m<-as.factor(LongDeltaHpersp2$m)
head(LongDeltaHpersp2)

#this is one of the plots I want
ggplot(LongDeltaHpersp2, aes(x=ID,y=value,colour=m,shape=Manage)) + geom_point(size=5) +
  scale_shape(solid = FALSE) 

#add face_wrap
ggplot(LongDeltaHpersp2, aes(x=ID,y=value,colour=m,shape=Manage)) + geom_point(size=5) +
  scale_shape(solid = FALSE) +facet_wrap(~Manage)

#save delta H per species --- also fun!
#I want to plot just the managed
manageddeltaH<-LongDeltaHpersp2 %>% filter(Manage==1)

pdf("/Users/ren/Documents/CODES/FoodProvision/Results/deltaHpersp_managed.pdf")
for (i in unique(manageddeltaH$stockid)){
  manageddeltaHsp<-manageddeltaH %>% filter(stockid==i)  
  
  plot(manageddeltaHsp$ID,manageddeltaHsp$value,main=paste(i,"/",manageddeltaHsp$SciName[1]))
  #abline(h=manageddeltaHsp$MSYfin[1], col="blue")
  #ggplot(manageddeltaHsp, aes(x=MPA,y=deltaH))+geom_point()+labs(title=i)
}
dev.off()

#Iplot the unmanaged
unmanageddeltaH<-LongDeltaHpersp2  %>% filter(Manage==0)
pdf("/Users/ren/Documents/CODES/FoodProvision/Results/deltaHpersp_unmanaged.pdf")
for (i in unique(unmanageddeltaH$stockid)){
  unmanageddeltaHsp<-unmanageddeltaH %>% filter(stockid==i)  
  plot(unmanageddeltaHsp$ID,unmanageddeltaHsp$value,main=paste(i,"/",unmanageddeltaHsp$SciName[1]))
}
dev.off()

#check Managed result with the Megadata file
MegaDataManaged<-MegaData %>% filter(Manage==1)


###CALCULARTE PIXEL-LEVEL CATCH for JUAN


###
numcell<-dim(Cleanmegacell)[1]
celltoiterateFULL<-1:numcell
celltoiterate<-celltoiterateFULL
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
TCleanmegacell<-t(Cleanmegacell)
PriorityAreas<-c()
NetworkResult<-vector()

###Compute spillover---PIXEL-LEVEL spillover #in case not yet computed
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
#Harvest without MPA
R <-0 #MPA size
r<-MegaData$r
E<-MegaData$Efin
#----Harvest with no MPA, BAU, no climate
hbau <-(1-E)*((r+E-1)/r)*K
hbau <-hbau*(hbau>0)
table(hbau) #negative harvest should be zero harvest! check code tomorrow.
HBAU <- sum(hbau)#sum((1-E)*((r+E-1)/r)*K)
HBAU

cores<-detectCores()
registerDoParallel(cores)
for (i in 1:20){ 
  #be sure to make celltoiterate adaptive
  result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[celltoiterate[iter]]<-1
    
    keeps<-which(MPAselect==1)
    MPAselect2<-as.matrix(MPAselect[keeps,])
    TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
    Kprotected<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
    
    R<-Kprotected$V1
    hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
    hmpa<-hmpa*(hmpa>0)
    HMPA<-sum(hmpa)
    HMPA-HBAU
  }
  
  #1. find the location of the top 1000 highest pixel-level
  myorderHightoLow<-order(-result)#positions
  cellselected<-myorderHightoLow[1:100] #but these are the position of the temporary pixels, not our reference pixels
  #convert coord to scale comparable to priority areas
  Prioritycellselected<-celltoiterate[cellselected]
  #plot(result[myorderHightoLow][1:1000])#plot values for demo
  
  #3. block those additional 1000 in MPAselect
  MPAselect0[Prioritycellselected]<-1
  #3. save them for our priority areas
  PriorityAreas<-append(PriorityAreas,Prioritycellselected)
  #4. Calculate food prov of the additional 1000 cells
  MPAselect<-MPAselect0
  
  keeps<-which(MPAselect==1)
  MPAselect2<-as.matrix(MPAselect[keeps,])
  TCleanmegacell2<-as.matrix(TCleanmegacell[,keeps])
  Kprotected<-as.data.frame(TCleanmegacell2 %*% MPAselect2)
  
  #Kprotected<-as.data.frame(TCleanmegacell %*% MPAselect)
  R<-Kprotected$V1
  hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
  HMPA-HBAU
  
  #save result
  NetworkResult[i]<-HMPA-HBAU
  
  #pass this to the top
  celltoiterate<-celltoiterateFULL[-PriorityAreas]
  print(c(i,NetworkResult[i]))
  rm(result,myorderHightoLow,cellselected,Prioritycellselected, MPAselect,keeps,MPAselect2,TCleanmegacell2,Kprotected,R,hmpa,HMPA)
  }
plot(NetworkResult)
stopImplicitCluster()

BenefitCurve<-as.data.frame(NetworkResult)/1000000
BenefitCurve$MPA <- (seq.int(nrow(BenefitCurve))/168712)*100*100
head(BenefitCurve)
zerozero<-data.frame(0,0)
names(zerozero)<-c("NetworkResult","MPA")
BenefitCurve<-rbind(BenefitCurve,zerozero)
p <- ggplot(BenefitCurve, aes(MPA, NetworkResult))
benefitplot<-p + geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 2)+
  labs(x="% ocean protected",y="Change in catch (million metric tons)")
benefitplot
#png(file="~/Food provision/benefitfunctionFood.png", width = 6, height = 4, units = 'in', res = 300)
#benefitplot
#dev.off()

#plot priority areas
plot(PriorityAreas)
saveRDS(PriorityAreas, file = "/Users/ren/Documents/CODES/FoodProvision/PriorityAreas.rds")


##RAM Legacy database here
library(devtools)
#install_github("ropensci/ramlegacy")
library(ramlegacy)
#download_ramlegacy(version="4.44") #downloading the latest version, 4.44
load_ramlegacy()
RAMDATA<-load_ramlegacy(tables = "timeseries_values_views")
head(RAMDATA$timeseries_values_views)
RAMDATA2<-RAMDATA$timeseries_values_views
head(RAMDATA2)
colnames(RAMDATA2)
RAMDATA3<-RAMDATA2 %>% select(stockid,year,ERbest,ER)
#remove entries with no ER values
terminalER<-RAMDATA3 %>% filter(! (ER=='NA')) %>%  group_by(stockid) %>% slice(which.max(year))
#terminalERtest<-RAMDATA2 %>% filter(! (ER=='NA')) %>%  group_by(stockid) %>% slice(which.max(year))
#head(terminalERtest)
head(terminalER)
plot(terminalER$ERbest,terminalER$ER)
hist(terminalER$year)
table(terminalER$ER)
terminalER$stockid ##terminalER is the file containing the stock assessments with terminal Exploitation Rate

#get our stockid them match
head(Mng2)
MatchedER<-left_join(Mng2,terminalER,by="stockid")
head(MatchedER)
hist(MatchedER$year)
hist(MatchedER$ER)
sum(MatchedER$ERbest-MatchedER$ER,na.rm=T) #ok. This proves that ERbest and ER are the same

MatchedERHOST<-MatchedER %>% select(stockid,year,ER) %>% filter(! (ER=='NA')) 
head(MatchedERHOST)


#Fill gaps
#1. filter entries with no match
head(MatchedER)
Gap1<-MatchedER %>% filter(is.na(ERbest)==T)
head(Gap1)
dim(Gap1)
#2. Use RAMDATA2 to get matchings
# then remove TCbest == NA, TB==NA
head(RAMDATA2)
ematch1<-RAMDATA2 %>% filter(stockid %in% Gap1$stockid) %>% filter(TCbest>=0) %>% filter(TB>=0) %>%  group_by(stockid) %>% slice(which.max(year)) %>% select(stockid,year,TCbest,TB)
ematch1$ER<-ematch1$TCbest/ematch1$TB
ematch1 <- ematch1 %>% select(stockid,year,ER)
head(ematch1)
MatchedERHOST2 <- rbind(as.data.frame(MatchedERHOST),as.data.frame(ematch1))

#3.matching based on SSB
ematch2<-RAMDATA2 %>% filter(stockid %in% Gap1$stockid) %>% filter(TCbest>=0) %>% filter(SSB>=0) %>%  group_by(stockid) %>% slice(which.max(year)) %>% select(stockid,year,TCbest,SSB)
ematch2$ER<-ematch2$TCbest/ematch2$SSB
ematch2<-ematch2 %>% select(stockid,year,ER) %>% filter(! stockid %in% MatchedERHOST2$stockid)
head(ematch2)
MatchedERHOST3 <- rbind(as.data.frame(MatchedERHOST2),as.data.frame(ematch2))
head(MatchedERHOST3)

#save MatchedERHOST3 and load in google doc for manual entries
write.csv(MatchedERHOST3, file = "/Users/ren/Documents/CODES/FoodProvision/MatchedER.csv")

#what are the stockid with no match?
Gap2<-Gap1 %>% filter(! stockid %in% MatchedERHOST3$stockid)
head(Gap2)

withmatch<-c("DOYSFS","HERR4RFA","HERR4RSP","HERRPWS","HERRSITKA")

nomatchv44<-RAMDATA2 %>% filter(stockid %in% Gap2$stockid) %>% filter(! stockid %in% withmatch) %>% select(stockid,year,TBbest,TCbest,ERbest,TB,SSB,TC,ER)# %>% filter(year>=2000)


###NEXT is to load the RAM version consistent with Chris Free's shapefiles
download_ramlegacy(version = "4.3")
load_ramlegacy(version="4.3")
RAMDATA43<-load_ramlegacy(tables = "timeseries_values_views")
head(RAMDATA43$timeseries_values_views)
RAMDATA2_43<-RAMDATA43$timeseries_values_views
head(RAMDATA2_43)

Testme<-RAMDATA2_43 %>% filter(stockid %in% Gap2$stockid) %>% filter(! stockid %in% withmatch) %>% select(stockid,year,TBbest,TCbest,ERbest,TB,SSB,TC,ER) %>% filter(year>=2000)



xxxxxxxxxx
#read Chris Free metadata file of the management layer
ChrisMetadata<-read.csv("/Users/ren/Documents/CODES/FoodProvision/ramldb_v3.8_stock_boundary_table_v2_formatted.csv")
head(ChrisMetadata)
ChrisMetadata<-ChrisMetadata %>% select(stockid,assessid)

MatchedER<-left_join(ChrisMetadata,terminalER,by="stockid")
head(MatchedER)
MatchedER
#save this and load this to google sheet?
sum(MatchedER$ERbest-MatchedER$ER,na.rm=T) #ok. This proves that ERbest and ER are the same


ManagementLayer<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayer.rds")
head(ManagementLayer)
unique(ManagementLayer$species)

#Below is for adding the stock ID in Chris Free file
#match species list from Chris Free Species list
###SHAPEFILES
shapefolder <- list.files(path="/Users/ren/Documents/CODES/FoodProvision/ramldb_boundaries",pattern="*.shp")
typeof(shapefolder)
head(shapefolder)
shapenames<- as.data.frame(shapefolder) %>% separate(shapefolder,c("filename", "rest"),sep=".shp", remove=TRUE) %>% select (-c("rest"))
shapenames<- unique(shapenames) #there are duplicates from .hml and we want a character
shapenames<- as.character(shapenames$filename)
head(shapenames)
shapenames

#This is for regenerating the management layer
# ref_raster<- raster("/Users/ren/Documents/CODES/FoodProvision/referenceraster")#i got this from the version 1 code
# datamanagelist<-list()
# count=0
# for (i in shapenames){
#   count<-count+1
#   #i=shapenames[1]
#   shape1 <- read_sf(dsn = "/Users/ren/Documents/CODES/FoodProvision/ramldb_boundaries", layer = i)
#   ##this is how you will get the species name
#   #shape1$species
#   #shape1$stockid for the stock id
# 
#   # #this is for moll transform. I will work on
#   # datashape1<-shape1 %>%
#   #   sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
#   #   sf::st_transform("+proj=moll") %>%
#   #   raster::rasterize(ocean_low_res_moll,getCover = T) %>%
#   #   raster::as.data.frame(xy = T)
#   #this is for regular, wgs
#   datashape1<-shape1 %>%
#     sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
#     sf::st_transform("+proj=longlat +datum=WGS84") %>%
#     raster::rasterize(ref_raster,getCover = T) %>%
#     raster::as.data.frame(xy = T)
#   #plot(datashape1)
# 
#   datafilter<-datashape1 %>% filter(layer>0) %>% mutate(species=shape1$species,stockid=shape1$stockid)
#   #manageareas<-rbind(manageareas,datafilter)
#   datamanagelist[[count]]<-datafilter
#   print(c(i,count))
# }
# 
# ManagementLayer<-dplyr::bind_rows(datamanagelist)
# table(ManagementLayer$species)
# head(ManagementLayer)
# saveRDS(ManagementLayer, file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerv3.rds")
###- use ManagementLayer3. It has an additional entry, i.e., species and stockid

ManagementLayer<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerv3.rds")

stocklistChrisF<-ManagementLayer %>% group_by(species, stockid) %>%summarize(n=n())
ChrisMetadata<-read.csv("/Users/ren/Documents/CODES/FoodProvision/ramldb_v3.8_stock_boundary_table_v2_formatted.csv")
ChrisMetadata<-ChrisMetadata %>% select(stockid,assessid)
stocklistChrisF<-left_join(stocklistChrisF,ChrisMetadata,by="stockid")

MatchedER<-left_join(stocklistChrisF,terminalER,by="stockid") %>% select (-c(n,ERbest)) %>% filter(is.na(species)==F)
head(MatchedER)
MatchedER
dim(MatchedER)
dim(MatchedER %>% filter(is.na(year)==T))[1]/dim(MatchedER)[1]
table(MatchedER$ER)
write.csv(MatchedER, file = "/Users/ren/Documents/CODES/FoodProvision/ERwithStockAssessmentTEST.csv")
##This is the file that we are using!!!

#Isolate stockid with no maching ---
NomatchER<-MatchedER %>% filter(is.na(ER)==T)
#match with costello data
NomatchER_costello<-left_join(NomatchER,terminaldataCostello,by="assessid")
#NomatchER_costellowithRAM295<-left_join(NomatchER_costello,RAM295matching3,by="assessid") ##not useful information
NomatchER_costello_Nomatch<-NomatchER_costello%>% filter(is.na(Biomass)==T)

#RAMDATA2 is the rawest Assessment data
head(RAMDATA2)
RAMDATA_nomatch<-RAMDATA2 %>% filter(stockid %in% NomatchER_costello_Nomatch$stockid)



#Check costello DB #costello et al. used v.2.95
download_ramlegacy(version="2.95") #downloading the latest version, 4.44
load_ramlegacy(version="2.95")
RAM295<-load_ramlegacy(tables = "assessment")$assessment %>% select(assessid,stockid)
head(RAM295)
RAM295matching1<-stocklistChrisF %>% select(-c(assessid,n))
RAM295matching2<-left_join(RAM295matching1,RAM295,by="stockid")
RAM295matching3<-left_join(RAM295matching2,terminaldataCostello,by="assessid")

head(RAMonly)
table(RAMonly$Policy)
table(RAMonly$Scenario)
subRAMCostello<-RAMonly  %>% filter(Policy=="Historic") %>% select(IdOrig,SciName,CatchShare,Year,Catch,Biomass)
terminaldataCostello<-subRAMCostello %>%  group_by(IdOrig) %>% slice(which.max(Year))
head(terminaldataCostello)
colnames(terminaldataCostello)[colnames(terminaldataCostello)=="IdOrig"] <- "assessid"

head(ChrisMetadata)
mergedCostelloFree<-left_join(ChrisMetadata,terminaldataCostello, by="assessid")
head(mergedCostelloFree)
write.csv(mergedCostelloFree, file = "/Users/ren/Documents/CODES/FoodProvision/ER_CostelloFree.csv")

# ##PARALLEL VERSION OF ABOVE (unstable)
# cores<-detectCores()
# registerDoParallel(cores)
# system.time({
#   #ManagementLayer <- foreach(i=1:length(shapenames), .combine = rbind) %dopar% {
#   ManagementLayer <- foreach(i=199:length(shapenames), .combine = rbind) %dopar% {
#     shape1 <- read_sf(dsn = "/Users/ren/Documents/CODES/FoodProvision/ramldb_boundaries", layer = shapenames[i])
#     datashape1<-shape1 %>% 
#       sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>% 
#       sf::st_transform("+proj=longlat +datum=WGS84") %>% 
#       raster::rasterize(ref_raster,getCover = T) %>% 
#       raster::as.data.frame(xy = T)  
#   
#     datafilter<-datashape1 %>% filter(layer>0) %>% mutate(species=shape1$species,stockid=shape1$stockid)
#     datafilter
#   }
# })
# head(ManagementLayer)
# dim(ManagementLayer)
# stopImplicitCluster()


#save rasterized files
saveRDS(ManagementLayer, file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerv2.rds")
ManagementLayer<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerv2.rds")

StockIdinChrisFreeDB<-unique(ManagementLayer$stockid)

#xxxxxx-----------------------

#remove coords for now
head(Aqua4)
combined_df<-Aqua4[,-c(1:3)]
combined_df[is.na(combined_df)] <- 0
head(combined_df)

#total carrying capacity per cell
total_K_cell<-rowSums(combined_df)
head(total_K_cell)

#total K per species. Question: is this K 
total_K_species<-colSums(combined_df)
head(total_K_species)

#Normalize K. Divide elements by the total K of the species.
Norm_K<-t(t(combined_df)/total_K_species)
#this is for checking if the answer is right
colSums(Norm_K)
#ok the answer is right

#Let us just work on non-zero data and see if it will work if we plot it
ksum_with_coords <- cbind(Aqua4[,c(1:3)], TotalK=rowSums(Norm_K))  #this is merging coords and sum of K per pixel
head(ksum_with_coords)

##remove total K = 0
#ksum_with_coords_filter<-ksum_with_coords %>% filter(TotalK>0)
#dim(ksum_with_coords_filter)

#>>this is the variable that contains the data, remove K_sum==0
k_with_coords <- cbind(ksum_with_coords, Norm_K)
dim(k_with_coords)
#I will make two files in case I will need the coordinates
Norm_K_filter0<-k_with_coords %>% filter(TotalK>0) 
head(Norm_K_filter0)

#remove id, lon, lat, and TotalK
Norm_K_filter<-Norm_K_filter0[,-c(1:4)]
head(Norm_K_filter)
dim(Norm_K_filter)

plot(Norm_K_filter0[,1])

#MPA selection algorithm
numcell<-nrow(Norm_K_filter) #just choose the raw data - no coordinates

#10% protection for now
MPAselect<-matrix(round(ifelse(runif(numcell,min=0,max=1)<0.5,1,0)), nrow=numcell, ncol=1)

dim(MPAselect)
dim(Norm_K_filter)
# this is how much proportion of the K we are protecting per species
Kprotected<-t(t(Norm_K_filter) %*% MPAselect)
Kprotected
dim(Kprotected)
Kprotected<-as.data.frame(t(Kprotected))
colnames(Kprotected)[which(names(Kprotected)=="V1")]<-"Kprotected"
head(Kprotected)
#Now, if we close a pixel, that should give us a food provision value
#assumptions for now
#1. all open access - will simplify things for now
#2. global catch under BAU will be a single value (no climate for now) - i can compute this
#3. delta_catch will be network-level catch - catch under BAU

MainData<-Kprotected
MainData$SpeciesID<-row.names(MainData)
MainData<-left_join(MainData,include,by="SpeciesID")
biolparams<-fishlife2(as.character(MainData$SciName))
biolparams<-biolparams %>% select(species,r) %>% rename(SciName=species)
head(biolparams)
MainData<-left_join(MainData,biolparams,by="SciName")
head(MainData)

#this code just save the species list for collection of m
head(spnamelookup)
SpeciesNamesCostello<-MainData %>% select(SpeciesID,SciName)
FBname<-spnamelookup %>% select(SPECIESID,family,FBname)
colnames(FBname)[which(names(FBname) == "SPECIESID")] <- "SpeciesID"
SpeciesNamesCostello<-left_join(SpeciesNamesCostello,FBname,by="SpeciesID")
head(SpeciesNamesCostello)
write.csv(SpeciesNamesCostello, file = "/Users/ren/Documents/CODES/FoodProvision/SpeciesNamesCostello.csv")

#load file with m estimate
mfile<-read.csv("/Users/ren/Documents/CODES/FoodProvision/SpeciesNamesCostello_m.csv")
mfile<-mfile %>% mutate(m=replace(m,m==1,0.1),
                        m=replace(m,m==2,0.5),
                        m=replace(m,m==3,0.9))
head(mfile) 
MainData$m<-mfile$m



###----xxx-----xxx-----xxx---PIXEL-LEVEL spillover
dH<-matrix(0, nrow=numcell, ncol=1)
K<-MainData$K #k per species
m<-MainData$m #mobility per species
#Harvest without MPA
R <-0 #MPA size
r<-MainData$r
E <-1-(0.9*r) #escapement, amount retained
###if E is negative, make it zero --- meaning you harvest more. negative is also ok meaning you are borrowing from the growth.
#E <- (E>0)*E
#hist(E)

Ropt<-((m*r) + (((2*E)-2)*m)) / (((E-1)*r)+(((2*E)-2)*m))
MainData$E<-E
MainData$Ropt<-Ropt
hist(MainData$Ropt,xlab="Ropt",main="")

#Given Ropt, what is Hopt???
Hopt<-((1-E)*((m*K*(1-Ropt))/(Ropt-(E*Ropt)+m))*(1-(((1-E)*(1-Ropt)*m)/((Ropt-(E*Ropt)+m)*r)))) - ((1-E)*((r+E-1)/r)*K)


#----Harvest with no MPA, BAU, no climate
HBAU <- sum((1-E)*((r+E-1)/r)*K)

MPAselect0<-matrix(0, nrow=numcell, ncol=1)
#Norm_K_filter: row is species name, col is pixel id, content is normaized K
TNorm_K_filter<-t(Norm_K_filter)
#saveRDS(TNorm_K_filter, file = "/Users/ren/Documents/CODES/FoodProvision/fTNorm_K_filter.rds")
#saveRDS(MainData, file = "/Users/ren/Documents/CODES/FoodProvision/MainData.rds")

head(MPAselect0)
#try parallel programming
#install.packages("doParallel")
cores<-detectCores()
registerDoParallel(cores)
system.time({
  result <- foreach(iter = 1000:1012, .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[iter]<-1
    Kprotected<-as.data.frame(TNorm_K_filter %*% MPAselect)
    R<-Kprotected$V1
    HMPA<-sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
    HMPA-HBAU
  }
})
result
max(result)
stopImplicitCluster()
#stopCluster(myCluster)

pixellevelspill<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/pixellevelspill.rds")
head(pixellevelspill)
plot(pixellevelspill)
#numcell is 168712. 1% is 1687, 0.5% is 844.
head(Norm_K_filter0[,2:3])

##########################################################################

##----with Management DRAFT
ManagementLayer<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/ManagementLayerData.rds")
head(ManagementLayer)
managementmap<-ManagementLayer %>% group_by(x,y) %>% summarise(nmanage=n())

head(managementmap)
xyz<-rasterFromXYZ(managementmap)
plot(xyz,main="Management Layer (number of assessed stocks per pixel)")
#Norm_K_filter0 contains the lon lat

#for now, just check the species
speciesmanaged<-unique(ManagementLayer$species)
head(include)
sum((speciesmanaged %in% include$SciName)*1)
#188 species are managed

empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(managementmap[,1:2]))
empty_raster[cells] <- managementmap$nmanage
head(empty_raster)
plot(PlotFunction(empty_raster),main="Food provision potential (MT)",axes=F,box=F)

##################################
#plot for the slides
head(include)
options("scipen"=100, "digits"=4)
barplot(include$K,log="y",main="K distribution", 
        xlab="", ylab="K (MT)")

# ggplot(data=barplotme, aes(x=SciName, y=K)) +
#   geom_bar(stat="identity", fill="steelblue")+
#   theme_minimal()

#plot r
head(MainData)
options("scipen"=100, "digits"=4)

rplotme<-MainData[order(-r),]
barplot(rplotme$r, main="r distribution")

#plot family and number of species
head(mfile)
familysummary_costello<-mfile %>% group_by(family) %>% summarize(n=n())
sort(-familysummary_costello$n)
write.csv(familysummary_costello,"/Users/ren/Documents/CODES/FoodProvision/Results/familysummary_costello.csv")
ggplot(mfile) + geom_bar(aes(m))


#######################
#toy model Fishery is managed
K<-1000
M<-0.9
a <-1 #surv. I think this should just be 1
R <-0 #MPA size
m<-M*(1-R) #frac biomass moving out
r<-0.5
#E <-1/(0.9*r+1) #escapement, unmanaged
E <- 1-(r/2) #managed

Bout= -(E*K + 2*E^2*K*a^2 - E^2*K*a^3 - 3*E*K*R - 2*E*K*a + 3*E*K*R^2 - E*K*R^3 + E*K*a^2 - E^2*K*a - E^2*K*r - 3*E*K*R*a^2 - 6*E*K*R^2*a + 3*E^2*K*R*a + 2*E*K*R^3*a + 3*E*K*R^2*r + 4*E^2*K*R*r - 3*E*K*R^3*r + E*K*R^4*r - 2*E*K*a^2*m + 2*E^2*K*a*r + 3*E*K*R^2*a^2 - 6*E^2*K*R*a^2 - 3*E^2*K*R^2*a - E*K*R^3*a^2 + 3*E^2*K*R*a^3 + E^2*K*R^3*a - 6*E^2*K*R^2*r + 4*E^2*K*R^3*r - E^2*K*R^4*r + E*K*a^2*m^2 - 2*E^2*K*a^2*m + 2*E^2*K*a^3*m - E^2*K*a^2*r + 6*E*K*R*a - E*K*R*r + 2*E*K*a*m + 6*E^2*K*R^2*a^2 - 3*E^2*K*R^2*a^3 - 2*E^2*K*R^3*a^2 + E^2*K*R^3*a^3 - E^2*K*a^3*m^2 - 5*E*K*R*a*m + E*K*R*a*r + E*K*R^2*a^2*m^2 + E^2*K*R*a^2*m^2 + E^2*K*R*a^3*m^2 + 2*E^2*K*R^2*a^3*m - E^2*K*R^3*a^2*m - 3*E^2*K*R^2*a^2*r + E^2*K*R^3*a^2*r - E^2*K*a^2*m^2*r + 5*E*K*R*a^2*m + 4*E*K*R^2*a*m + E^2*K*R*a*m - E*K*R^3*a*m - 3*E*K*R^2*a*r - 7*E^2*K*R*a*r + 3*E*K*R^3*a*r - E*K*R^4*a*r - E^2*K*R^2*a^2*m^2 - 2*E^2*K*a*m*r - 2*E*K*R*a^2*m^2 - 4*E*K*R^2*a^2*m + 3*E^2*K*R*a^2*m - 2*E^2*K*R^2*a*m + E*K*R^3*a^2*m - 4*E^2*K*R*a^3*m + E^2*K*R^3*a*m + 3*E^2*K*R*a^2*r + 9*E^2*K*R^2*a*r - 5*E^2*K*R^3*a*r + E^2*K*R^4*a*r + 2*E^2*K*a^2*m*r - 4*E^2*K*R*a^2*m*r - 4*E^2*K*R^2*a*m*r + E^2*K*R^3*a*m*r - E*K*R*a*m*r + E^2*K*R*a^2*m^2*r + 2*E^2*K*R^2*a^2*m*r + 2*E*K*R^2*a*m*r + 5*E^2*K*R*a*m*r - E*K*R^3*a*m*r)/(r*E^2*R^4 + 2*r*E^2*R^3*a - 4*r*E^2*R^3 + r*E^2*R^2*a^2 + 2*r*E^2*R^2*a*m - 6*r*E^2*R^2*a + 6*r*E^2*R^2 + 2*r*E^2*R*a^2*m - 2*r*E^2*R*a^2 - 4*r*E^2*R*a*m + 6*r*E^2*R*a - 4*r*E^2*R + r*E^2*a^2*m^2 - 2*r*E^2*a^2*m + r*E^2*a^2 + 2*r*E^2*a*m - 2*r*E^2*a + r*E^2 - 2*r*E*R^4 - 2*r*E*R^3*a + 6*r*E*R^3 - 2*r*E*R^2*a*m + 4*r*E*R^2*a - 6*r*E*R^2 + 2*r*E*R*a*m - 2*r*E*R*a + 2*r*E*R + r*R^4 - 2*r*R^3 + r*R^2)
#----Harvest with no MPA, BAU, no climate
HBAU <- sum((1-E)*Bout)
HBAU

HBAU_OA = 31.03
HBAU_Managed = 83.33

K<-1000
M<-1
a <-1 #surv. I think this should just be 1
r<-0.5
H_managed<-vector()
count<-0
E<-0.5
#E <- 1/(1+(0.5*r)) #managed
#E <-1/(0.9*r+1)
FracMPA<-seq(0,.95,0.05)
for (R in FracMPA){
  count<-count+1
  m<-M*(1-R)
  Bout= -(E*K + 2*E^2*K*a^2 - E^2*K*a^3 - 3*E*K*R - 2*E*K*a + 3*E*K*R^2 - E*K*R^3 + E*K*a^2 - E^2*K*a - E^2*K*r - 3*E*K*R*a^2 - 6*E*K*R^2*a + 3*E^2*K*R*a + 2*E*K*R^3*a + 3*E*K*R^2*r + 4*E^2*K*R*r - 3*E*K*R^3*r + E*K*R^4*r - 2*E*K*a^2*m + 2*E^2*K*a*r + 3*E*K*R^2*a^2 - 6*E^2*K*R*a^2 - 3*E^2*K*R^2*a - E*K*R^3*a^2 + 3*E^2*K*R*a^3 + E^2*K*R^3*a - 6*E^2*K*R^2*r + 4*E^2*K*R^3*r - E^2*K*R^4*r + E*K*a^2*m^2 - 2*E^2*K*a^2*m + 2*E^2*K*a^3*m - E^2*K*a^2*r + 6*E*K*R*a - E*K*R*r + 2*E*K*a*m + 6*E^2*K*R^2*a^2 - 3*E^2*K*R^2*a^3 - 2*E^2*K*R^3*a^2 + E^2*K*R^3*a^3 - E^2*K*a^3*m^2 - 5*E*K*R*a*m + E*K*R*a*r + E*K*R^2*a^2*m^2 + E^2*K*R*a^2*m^2 + E^2*K*R*a^3*m^2 + 2*E^2*K*R^2*a^3*m - E^2*K*R^3*a^2*m - 3*E^2*K*R^2*a^2*r + E^2*K*R^3*a^2*r - E^2*K*a^2*m^2*r + 5*E*K*R*a^2*m + 4*E*K*R^2*a*m + E^2*K*R*a*m - E*K*R^3*a*m - 3*E*K*R^2*a*r - 7*E^2*K*R*a*r + 3*E*K*R^3*a*r - E*K*R^4*a*r - E^2*K*R^2*a^2*m^2 - 2*E^2*K*a*m*r - 2*E*K*R*a^2*m^2 - 4*E*K*R^2*a^2*m + 3*E^2*K*R*a^2*m - 2*E^2*K*R^2*a*m + E*K*R^3*a^2*m - 4*E^2*K*R*a^3*m + E^2*K*R^3*a*m + 3*E^2*K*R*a^2*r + 9*E^2*K*R^2*a*r - 5*E^2*K*R^3*a*r + E^2*K*R^4*a*r + 2*E^2*K*a^2*m*r - 4*E^2*K*R*a^2*m*r - 4*E^2*K*R^2*a*m*r + E^2*K*R^3*a*m*r - E*K*R*a*m*r + E^2*K*R*a^2*m^2*r + 2*E^2*K*R^2*a^2*m*r + 2*E*K*R^2*a*m*r + 5*E^2*K*R*a*m*r - E*K*R^3*a*m*r)/(r*E^2*R^4 + 2*r*E^2*R^3*a - 4*r*E^2*R^3 + r*E^2*R^2*a^2 + 2*r*E^2*R^2*a*m - 6*r*E^2*R^2*a + 6*r*E^2*R^2 + 2*r*E^2*R*a^2*m - 2*r*E^2*R*a^2 - 4*r*E^2*R*a*m + 6*r*E^2*R*a - 4*r*E^2*R + r*E^2*a^2*m^2 - 2*r*E^2*a^2*m + r*E^2*a^2 + 2*r*E^2*a*m - 2*r*E^2*a + r*E^2 - 2*r*E*R^4 - 2*r*E*R^3*a + 6*r*E*R^3 - 2*r*E*R^2*a*m + 4*r*E*R^2*a - 6*r*E*R^2 + 2*r*E*R*a*m - 2*r*E*R*a + 2*r*E*R + r*R^4 - 2*r*R^3 + r*R^2)
  H_managed[count]<-(1-E)*Bout
}
plot(FracMPA,H_managed,ylim=c(0,max(na.omit(H_managed))))

#what is the effect of different escapement level?
H_managed<-vector()
count<-0
R<-0
for (E in FracMPA){
  count<-count+1
  m<-0.9*(1-R)
  Bout= -(E*K + 2*E^2*K*a^2 - E^2*K*a^3 - 3*E*K*R - 2*E*K*a + 3*E*K*R^2 - E*K*R^3 + E*K*a^2 - E^2*K*a - E^2*K*r - 3*E*K*R*a^2 - 6*E*K*R^2*a + 3*E^2*K*R*a + 2*E*K*R^3*a + 3*E*K*R^2*r + 4*E^2*K*R*r - 3*E*K*R^3*r + E*K*R^4*r - 2*E*K*a^2*m + 2*E^2*K*a*r + 3*E*K*R^2*a^2 - 6*E^2*K*R*a^2 - 3*E^2*K*R^2*a - E*K*R^3*a^2 + 3*E^2*K*R*a^3 + E^2*K*R^3*a - 6*E^2*K*R^2*r + 4*E^2*K*R^3*r - E^2*K*R^4*r + E*K*a^2*m^2 - 2*E^2*K*a^2*m + 2*E^2*K*a^3*m - E^2*K*a^2*r + 6*E*K*R*a - E*K*R*r + 2*E*K*a*m + 6*E^2*K*R^2*a^2 - 3*E^2*K*R^2*a^3 - 2*E^2*K*R^3*a^2 + E^2*K*R^3*a^3 - E^2*K*a^3*m^2 - 5*E*K*R*a*m + E*K*R*a*r + E*K*R^2*a^2*m^2 + E^2*K*R*a^2*m^2 + E^2*K*R*a^3*m^2 + 2*E^2*K*R^2*a^3*m - E^2*K*R^3*a^2*m - 3*E^2*K*R^2*a^2*r + E^2*K*R^3*a^2*r - E^2*K*a^2*m^2*r + 5*E*K*R*a^2*m + 4*E*K*R^2*a*m + E^2*K*R*a*m - E*K*R^3*a*m - 3*E*K*R^2*a*r - 7*E^2*K*R*a*r + 3*E*K*R^3*a*r - E*K*R^4*a*r - E^2*K*R^2*a^2*m^2 - 2*E^2*K*a*m*r - 2*E*K*R*a^2*m^2 - 4*E*K*R^2*a^2*m + 3*E^2*K*R*a^2*m - 2*E^2*K*R^2*a*m + E*K*R^3*a^2*m - 4*E^2*K*R*a^3*m + E^2*K*R^3*a*m + 3*E^2*K*R*a^2*r + 9*E^2*K*R^2*a*r - 5*E^2*K*R^3*a*r + E^2*K*R^4*a*r + 2*E^2*K*a^2*m*r - 4*E^2*K*R*a^2*m*r - 4*E^2*K*R^2*a*m*r + E^2*K*R^3*a*m*r - E*K*R*a*m*r + E^2*K*R*a^2*m^2*r + 2*E^2*K*R^2*a^2*m*r + 2*E*K*R^2*a*m*r + 5*E^2*K*R*a*m*r - E*K*R^3*a*m*r)/(r*E^2*R^4 + 2*r*E^2*R^3*a - 4*r*E^2*R^3 + r*E^2*R^2*a^2 + 2*r*E^2*R^2*a*m - 6*r*E^2*R^2*a + 6*r*E^2*R^2 + 2*r*E^2*R*a^2*m - 2*r*E^2*R*a^2 - 4*r*E^2*R*a*m + 6*r*E^2*R*a - 4*r*E^2*R + r*E^2*a^2*m^2 - 2*r*E^2*a^2*m + r*E^2*a^2 + 2*r*E^2*a*m - 2*r*E^2*a + r*E^2 - 2*r*E*R^4 - 2*r*E*R^3*a + 6*r*E*R^3 - 2*r*E*R^2*a*m + 4*r*E*R^2*a - 6*r*E*R^2 + 2*r*E*R*a*m - 2*r*E*R*a + 2*r*E*R + r*R^4 - 2*r*R^3 + r*R^2)
  H_managed[count]<-(1-E)*Bout
}
plot(FracMPA,H_managed)

#How's Bout behaving for diff MPA size? Managed and unmanaged
#PART OF PRESENTATION
K<-1000
#M<-0.1
a <-1 #surv. I think this should just be 1
r<-0.1
H_managed<-vector()
Rvec<-vector()
Mvec<-vector()
count<-0
#E<-0.5
#E <- 1/(1+(0.5*r)) #managed
E <-1/(0.9*r+1)
FracMPA<-seq(0,0.99,0.01)
for (R in FracMPA){
  for (M in c(0.1,0.5,0.9)){
    count<-count+1
    m<-M*(1-R)
    Bout= -(E*K + 2*E^2*K*a^2 - E^2*K*a^3 - 3*E*K*R - 2*E*K*a + 3*E*K*R^2 - E*K*R^3 + E*K*a^2 - E^2*K*a - E^2*K*r - 3*E*K*R*a^2 - 6*E*K*R^2*a + 3*E^2*K*R*a + 2*E*K*R^3*a + 3*E*K*R^2*r + 4*E^2*K*R*r - 3*E*K*R^3*r + E*K*R^4*r - 2*E*K*a^2*m + 2*E^2*K*a*r + 3*E*K*R^2*a^2 - 6*E^2*K*R*a^2 - 3*E^2*K*R^2*a - E*K*R^3*a^2 + 3*E^2*K*R*a^3 + E^2*K*R^3*a - 6*E^2*K*R^2*r + 4*E^2*K*R^3*r - E^2*K*R^4*r + E*K*a^2*m^2 - 2*E^2*K*a^2*m + 2*E^2*K*a^3*m - E^2*K*a^2*r + 6*E*K*R*a - E*K*R*r + 2*E*K*a*m + 6*E^2*K*R^2*a^2 - 3*E^2*K*R^2*a^3 - 2*E^2*K*R^3*a^2 + E^2*K*R^3*a^3 - E^2*K*a^3*m^2 - 5*E*K*R*a*m + E*K*R*a*r + E*K*R^2*a^2*m^2 + E^2*K*R*a^2*m^2 + E^2*K*R*a^3*m^2 + 2*E^2*K*R^2*a^3*m - E^2*K*R^3*a^2*m - 3*E^2*K*R^2*a^2*r + E^2*K*R^3*a^2*r - E^2*K*a^2*m^2*r + 5*E*K*R*a^2*m + 4*E*K*R^2*a*m + E^2*K*R*a*m - E*K*R^3*a*m - 3*E*K*R^2*a*r - 7*E^2*K*R*a*r + 3*E*K*R^3*a*r - E*K*R^4*a*r - E^2*K*R^2*a^2*m^2 - 2*E^2*K*a*m*r - 2*E*K*R*a^2*m^2 - 4*E*K*R^2*a^2*m + 3*E^2*K*R*a^2*m - 2*E^2*K*R^2*a*m + E*K*R^3*a^2*m - 4*E^2*K*R*a^3*m + E^2*K*R^3*a*m + 3*E^2*K*R*a^2*r + 9*E^2*K*R^2*a*r - 5*E^2*K*R^3*a*r + E^2*K*R^4*a*r + 2*E^2*K*a^2*m*r - 4*E^2*K*R*a^2*m*r - 4*E^2*K*R^2*a*m*r + E^2*K*R^3*a*m*r - E*K*R*a*m*r + E^2*K*R*a^2*m^2*r + 2*E^2*K*R^2*a^2*m*r + 2*E*K*R^2*a*m*r + 5*E^2*K*R*a*m*r - E*K*R^3*a*m*r)/(r*E^2*R^4 + 2*r*E^2*R^3*a - 4*r*E^2*R^3 + r*E^2*R^2*a^2 + 2*r*E^2*R^2*a*m - 6*r*E^2*R^2*a + 6*r*E^2*R^2 + 2*r*E^2*R*a^2*m - 2*r*E^2*R*a^2 - 4*r*E^2*R*a*m + 6*r*E^2*R*a - 4*r*E^2*R + r*E^2*a^2*m^2 - 2*r*E^2*a^2*m + r*E^2*a^2 + 2*r*E^2*a*m - 2*r*E^2*a + r*E^2 - 2*r*E*R^4 - 2*r*E*R^3*a + 6*r*E*R^3 - 2*r*E*R^2*a*m + 4*r*E*R^2*a - 6*r*E*R^2 + 2*r*E*R*a*m - 2*r*E*R*a + 2*r*E*R + r*R^4 - 2*r*R^3 + r*R^2)
    H_managed[count]<-(1-E)*Bout
    Rvec[count]<-R
    Mvec[count]<-M
  }}
Result<-cbind(H_managed,Rvec,Mvec)
head(Result)
p1<-ggplot(data=as.data.frame(Result), aes(x=Rvec,y=H_managed,colour=factor(Mvec)))+geom_line()+
  labs(x="MPA size",y="Catch",color="mobility")+theme_minimal()+ ggtitle("Managed species (r=1)")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text=element_text(size=16),legend.title=element_text(size=16),plot.title = element_text(size = 16, face = "bold"))
p1
ggsave(filename="/Users/ren/Documents/CODES/FoodProvision/Results/managed_r1.png",p1,width=6, height=4,dpi=300)




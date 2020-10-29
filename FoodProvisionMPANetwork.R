#GITHUB filr
#Network-level food provisioning calculation
#Author: Reniel Cabral
#December 2019

##Clean environment
gc()
rm(list = ls())

##Load libraries
library(doParallel)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)
library(pryr)
library(ggplot2)
library(cowplot)
library(reshape)
library(here)

##Define scenarios. 

scenes <- c("BAU1","OAconstant","all managed","Efin_msy","EBvK01fin", "EBvK01_msy")

# scenario<-"BAU1" # Business as usual scenario used in our paper. Based on Costello et al. "concervation concern" scenario.
#scenario<-"OAconstant" # Costello et al. "all stocks" scenario.
#scenario<-"all managed" #All MSY scenario
#scenario<-"Efin_msy" #Costello et al. "all stock" + MSY scenario
#scenario<-"EBvK01fin" #"Collapse+current" scenario
#scenario<-"EBvK01_msy" #"Collapse+MSY" scenario

#Load files
MegaData<-readRDS(file = here("MegaData.rds"))
Cleanmegacell<-readRDS(file = "Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "CleanCoordmegacell_mollweide.rds")
KprotectedPerCell_Library<-readRDS(file = "KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="MPA_coord_mollweide.rds")
land_shp_moll<-readRDS(file = "land_shp_moll.rds")
head(MPA_coord)
dim(MPA_coord)

#get MPA positions
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)
sum(CleanCoordmegacell_MPA$MPA,na.rm=T)

#positions of 1s (MPAs)
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
head(MPAposition)
length(MPAposition)#2931 --- 2.44% are MPAs
length(MPAposition)*100/dim(Cleanmegacell)[1]

numcell<-dim(Cleanmegacell)[1]
celltoiterateFULL<-1:numcell
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
PriorityAreas<-c()
NetworkResult<-vector()

#Make MPAselect0==1 for MPAs
MPAselect0[MPAposition]<-1
head(MPAselect0)
sum(MPAselect0)

#remove MPAs from celltoiterateFULL
celltoiterateFULL<-celltoiterateFULL[-MPAposition]
celltoiterate<-celltoiterateFULL
ncell<-length(celltoiterate)

###Compute spillover---PIXEL-LEVEL spillover 
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
r<-MegaData$r

for (s in scenes){
  
  scenario <- scenes[s]

if (scenario=="all managed"){
  E<-MegaData$Emsy
}else if(scenario=="OAconstant"){
  E<-MegaData$Efin
}else if(scenario=="BAU1"){  
  E<-MegaData$Efin_BAU1
}else if(scenario=="Efin_msy"){ 
  E<-MegaData$Efin_msy
}else if(scenario=="EBvK01fin"){ 
  E<-MegaData$EBvK01fin 
}else if(scenario=="EBvK01_msy"){ 
  E<-MegaData$EBvK01_msy  
}

MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)
HBAU

PICKSIZE<-100

nmax<-floor(length(celltoiterate)/PICKSIZE)
nmax #this is the number of iterations needed for PICKSIZE at a time!
PerSpDeltaH<-matrix(nrow=nmax,ncol=1342)
PerSpDeltaH_EEZ<-matrix(nrow=nmax,ncol=1342)
PerSpDeltaH_HS<-matrix(nrow=nmax,ncol=1342)

cores<-detectCores() - 2
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
  cellselected<-myorderHightoLow[1:PICKSIZE] #but these are the position of the temporary pixels, not our reference pixels
  #convert coord to scale comparable to priority areas
  Prioritycellselected<-celltoiterate[cellselected]
  #plot(result[myorderHightoLow][1:1000])#plot values for demo
  
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
  
  #save result.
  PerSpDeltaH[i,]<-hmpa-hbau
  NetworkResult[i]<-HMPA-HBAU
  
  #pass this to the top
  celltoiterate<-celltoiterateFULL[!celltoiterateFULL %in% PriorityAreas]#celltoiterateFULL[-PriorityAreas]#bacause Prioritycellselected are real numbers, not rank
  
  #celltoiterate<- intersect(celltoiterate,EEZposition)
  print(c(i,NetworkResult[i]))
  rm(result,myorderHightoLow,cellselected,Prioritycellselected, MPAselect,R,hmpa,HMPA)
}
plot(NetworkResult)
stopImplicitCluster()

#saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH10_BAU1_mollweide.rds")
#saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult10_BAU1_mollweide.rds")
#saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas10_BAU1_mollweide.rds") 

if(scenario=="BAU1"){   
  saveRDS(PerSpDeltaH,file = "PerSpDeltaH100_BAU1_mollweide.rds")
  saveRDS(NetworkResult,file = "NetworkResult100_BAU1_mollweide.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_BAU1_mollweide.rds")  
}else if(scenario=="all managed"){  
  saveRDS(PerSpDeltaH,file = "PerSpDeltaH100_allmanaged.rds")
  saveRDS(NetworkResult,file = "NetworkResult100_allmanaged.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_allmanaged.rds") 
}else if(scenario=="OAconstant"){
  saveRDS(PerSpDeltaH,file = "PerSpDeltaH100_OAconstant_mollweide.rds")
  saveRDS(NetworkResult,file = "NetworkResult100_OAconstant_mollweide.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_OAconstant_mollweide.rds")   
}else if(scenario=="Efin_msy"){
  saveRDS(PerSpDeltaH,file = "PerSpDeltaH100_Efin_msy_mollweide.rds")
  saveRDS(NetworkResult,file = "NetworkResult100_Efin_msy_mollweide.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_Efin_msy_mollweide.rds")  
}else if(scenario=="EBvK01fin"){
  saveRDS(PerSpDeltaH,file = "PerSpDeltaH100_EBvK01fin_mollweide.rds")
  saveRDS(NetworkResult,file = "NetworkResult100_EBvK01fin_mollweide.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_EBvK01fin_mollweide.rds") 
}else if(scenario=="EBvK01_msy"){
  saveRDS(PerSpDeltaH,file = "PerSpDeltaH100_EBvK01_msy_mollweide.rds")
  saveRDS(NetworkResult,file = "NetworkResult100_EBvK01_msy_mollweide.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_EBvK01_msy_mollweide.rds") 
}

NetworkResult<-readRDS(file = "NetworkResult100.rds")
PriorityAreas<-readRDS(file = "PriorityAreas100.rds")

PerSpDeltaH<-readRDS(file = "PerSpDeltaH100.rds")
dim(PerSpDeltaH)
plot(rowSums(PerSpDeltaH))#this will give us the same result 

#BENEFIT CURVE FOR 100 at a time? Next block for 1000 at a time.
PICKSIZE<-100
BenefitCurve<-as.data.frame(NetworkResult)/1000000
MPAsize<-(length(MPAposition)+1)*100/dim(Cleanmegacell)[1]
#(MPAinEEZ+1)*100/length(EEZposition)#there is +1 because the next pixel starts with +1
BenefitCurve$MPA <- rescale(seq.int(nmax), to = c(MPAsize, 100))
zerozero<-data.frame(0,0)
names(zerozero)<-c("NetworkResult","MPA")
zerozero[1,]<-c(-HBAU/(1000000),100)
BenefitCurve<-rbind(BenefitCurve,zerozero)
theme_set(theme_cowplot())
benefitplot<-ggplot(BenefitCurve, aes(MPA, NetworkResult)) +geom_line(col="blue")+# theme_classic()+
  labs(x="% EEZ protected",y="Change in catch (MMT)",title=paste("Global (max =", round(max(BenefitCurve$NetworkResult),2),"MMT)"))#+geom_hline(yintercept = 0)
benefitplot

Priority<-as.data.frame(PriorityAreas)
Priority$rank <- (1-(seq.int(nrow(Priority))/ncell))*100
PriorityFrame <- as.data.frame(seq.int(nrow(CleanCoordmegacell)))
names(PriorityFrame) <- "PriorityAreas"
#add the priority
PriorityFrame2<-left_join(PriorityFrame,Priority, by="PriorityAreas")
PriorityFrame2[is.na(PriorityFrame2)] <- 0
head(PriorityFrame2)
dim(PriorityFrame2)

ShortCoord<-CleanCoordmegacell
ShortCoord$rank<-PriorityFrame2$rank
ShortCoord$ID<-row.names(ShortCoord)

dim(ShortCoord)
head(ShortCoord)

GlobalMap<-ShortCoord %>% ggplot(aes(x=lon,y=lat,fill=rank)) + scale_fill_gradient(low="white", high="#00539CFF",limits=c(0,100),name="Rank")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  geom_raster()+ geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="#EEA47FFF")+
  geom_sf(data = land_shp_moll, inherit.aes = F)
GlobalMap

#ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities10_BAU1.png", width = 12, height = 6, units = 'in', dpi= 300)

if(scenario=="BAU1"){   
  ggsave(here("FoodResults","FoodProvPriorities100_BAU1.png"), width = 12, height = 6, units = 'in', dpi= 300)
}else if(scenario=="all managed"){
  ggsave(here("FoodResults","FoodProvPriorities100_allmanaged.png"), width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="OAconstant"){
  ggsave(here("FoodResults","FoodProvPriorities100_OAconstant.png"), width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="Efin_msy"){
  ggsave(here("FoodResults","FoodProvPriorities100_Efin_msy.png"), width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="EBvK01fin"){
  ggsave(here("FoodResults","FoodProvPriorities100_EBvK01fin.png"), width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="EBvK01_msy"){
  ggsave(here("FoodResults","FoodProvPriorities100_EBvK01_msy.png"), width = 12, height = 6, units = 'in', dpi= 600)
}

}

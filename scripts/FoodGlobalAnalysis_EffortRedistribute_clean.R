#GLOBAL RUN
#Network-level food provisioning calculation
#Effort redistribution based on Beverton and Holt 1957
#Reniel Cabral
#4 Mar 2020
#gc()
#rm(list = ls())
#.rs.restartR()

library(doParallel)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)
library(pryr)
library(ggplot2)
library(cowplot)
library(reshape)
library(data.table)

##SELECT SCENARIO --- there are four scenarios
scenario<-"BAU1"
#scenario<-"OAconstant"
#scenario<-"EBvK01fin"
#scenario<-"all managed"

#Load MOLLWEIDE projected data
MegaData<-readRDS(file = "MegaData.rds")
Cleanmegacell<-readRDS(file = "Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "CleanCoordmegacell_mollweide.rds")
KprotectedPerCell_Library<-readRDS(file = "KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="MPA_coord_mollweide.rds") #this is my code
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

##TRY new approach
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
K<-MegaData$Kfin # K per species
m<-MegaData$m # mobility per species
r<-MegaData$r

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
}

ER<-1-E
ER<-1*(ER>1) + ER*(ER<=1)
max(ER)
min(ER)

MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
ER_redistribute<-1-(1-ER)^(1/(1-R))


b_outside_bau <-
  ((m * K * (1 - R)) / (ER_redistribute * R + m)) * (1 - (ER_redistribute * (1 - R) * m) /
                                                       ((ER_redistribute + m) * r)) 

hbau<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)
HBAU

PICKSIZE<-100 #number of MPA sites selected

nmax<-floor(length(celltoiterate)/PICKSIZE)
nmax #this is the number of iterations needed for PICKSIZE at a time!

Eevolve<-matrix(nrow=nmax,ncol=dim(MegaData)[1])
# k_per_cell <- (as.data.table(KprotectedPerCell_Library))

cores<-detectCores() - 2
registerDoParallel(cores)
for (i in 1:nmax){ 
  MPAselectPrev<-rowSums(KprotectedPerCell_Library[,which(MPAselect0==1),drop=FALSE])
  a <- Sys.time()
  result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {
    MPAselect<-MPAselect0
    MPAselect[celltoiterate[iter]]<-1
    R<-MPAselectPrev+KprotectedPerCell_Library[,celltoiterate[iter]]
    ER_redistribute<-1-(1-ER)^(1/(1-R))
    
    
    hmpa<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
    hmpa<-hmpa*(hmpa>0)
    HMPA<-sum(hmpa)
    HMPA-HBAU
  }
  Sys.time() - a
  
  #1. find the location of the top 1000 highest pixel-level
  myorderHightoLow<-order(-result)#positions
  cellselected<-myorderHightoLow[1:PICKSIZE] #but these are the position of the temporary pixels, not our reference pixels
  #convert coord to scale comparable to priority areas
  Prioritycellselected<-celltoiterate[cellselected]
  
  #3. block those additional 100 in MPAselect
  MPAselect0[Prioritycellselected]<-1
  
  #4. save them for our priority areas
  PriorityAreas<-append(PriorityAreas,Prioritycellselected)
  
  #5. Calculate food prov of the additional 100 cells
  MPAselect<-MPAselect0
  R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
  ER_redistribute<-1-(1-ER)^(1/(1-R))

  hmpa<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)
  
  #save result. Comment other parts not needed now.
  NetworkResult[i]<-HMPA-HBAU
  Eevolve[i,]<-ER_redistribute
  
  #pass this to the top
  celltoiterate<-celltoiterateFULL[!celltoiterateFULL %in% PriorityAreas]#celltoiterateFULL[-PriorityAreas]#bacause Prioritycellselected are real numbers, not rank
  
  print(c(i,NetworkResult[i]))
  rm(result,myorderHightoLow,cellselected,Prioritycellselected, MPAselect,R,hmpa,HMPA)
}
plot(NetworkResult)
stopImplicitCluster()

if(scenario=="BAU1"){
  saveRDS(NetworkResult,file = "NetworkResult100_BAU1_mollweide.rds")
  saveRDS(PriorityAreas,file = "PriorityAreas100_BAU1_mollweide.rds")  
}else if(scenario=="all managed"){  
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_allmanaged_redistribute.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_allmanaged_redistribute.rds") 
}else if(scenario=="OAconstant"){
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_OAconstant_mollweide_redistribute.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_OAconstant_mollweide_redistribute.rds")   
}else if(scenario=="EBvK01fin"){
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_EBvK01fin_mollweide_redistribute.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_EBvK01fin_mollweide_redistribute.rds") 
}

##LOAD FILES
#BAU1
NetworkResult<-readRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_BAU1_mollweide_redistribute.rds")
PriorityAreas<-readRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_BAU1_mollweide_redistribute.rds") 

#OAconstant
#NetworkResult<-readRDS(file = "~/foodGCEfile/NetworkResult100_OAconstant_mollweide_redistribute.rds")
#PriorityAreas<-readRDS(file = "~/foodGCEfile/PriorityAreas100_OAconstant_mollweide_redistribute.rds")

#collapse
#NetworkResult<-readRDS(file = "~/foodGCEfile/NetworkResult100_EBvK01fin_mollweide_redistribute.rds")
#PriorityAreas<-readRDS(file = "~/foodGCEfile/PriorityAreas100_EBvK01fin_mollweide_redistribute.rds")

#MSY
#NetworkResult<-readRDS(file = "~/foodGCEfile/NetworkResult100_allmanaged_redistribute.rds")
#PriorityAreas<-readRDS(file = "~/foodGCEfile/PriorityAreas100_allmanaged_redistribute.rds")

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
Priority$rank <- ((seq.int(nrow(Priority))/ncell))*100

#add dh
NetworkResult_prime<-as.data.frame(NetworkResult)
dH_prime<-NetworkResult_prime %>% mutate(dH = NetworkResult - lag(NetworkResult, default = 0))
plot(dH_prime$dH)
Priority$NetworkResult<-rep(NetworkResult, each=PICKSIZE) ## this is adding delta H
Priority$dH<-rep(dH_prime$dH/PICKSIZE, each=PICKSIZE) ## this is adding delta H
head(Priority)
dim(Priority)

PriorityFrame <- as.data.frame(seq.int(nrow(CleanCoordmegacell)))
names(PriorityFrame) <- "PriorityAreas"
#add the priority
PriorityFrame2<-left_join(PriorityFrame,Priority, by="PriorityAreas")
PriorityFrame2[is.na(PriorityFrame2)] <- 0
head(PriorityFrame2)
dim(PriorityFrame2)

ShortCoord<-CleanCoordmegacell
ShortCoord$rank<-PriorityFrame2$rank
ShortCoord$NetworkResult<- PriorityFrame2$NetworkResult#this is for deriving dh
ShortCoord$dH<- PriorityFrame2$dH#this is for deriving dh
ShortCoord$ID<-row.names(ShortCoord)

dim(ShortCoord)
head(ShortCoord)

ShortCoord_Sort <- ShortCoord[order(-ShortCoord$rank),] %>% filter(rank!=-1000)

ShortCoord_Sort$sign<-ShortCoord_Sort$dH>0
InflectPoint<-min(which(ShortCoord_Sort$sign == TRUE))
InflectMPA<-ShortCoord_Sort$rank[InflectPoint]

MPAcoverage<-ShortCoord_Sort %>% ggplot(aes(x=lon,y=lat,fill=rank)) + #scale_fill_gradient2(low="darkred", mid="white",high="#00539CFF",midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar", aesthetics = "fill",name=expression(paste(Delta, "H (MT)")))+
  scale_fill_gradientn(colours = c("forestgreen", "white", "orange"), limits=c(0,100), values = scales::rescale(c(min(ShortCoord_Sort$rank), InflectMPA, max(ShortCoord_Sort$rank))),name="Protection sequence")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position=c(0.63, 0.05), legend.direction = "horizontal")+ #"bottom
  geom_raster()+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="cyan")+  #"#EEA47FFF"
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent'))
MPAcoverage

#ggsave(file="~/foodGCEfile/FoodResults/MPAcoverageBAU1_redistribute.png", MPAcoverage, width = 10, height = 8, dpi = 600, units = "in")
#ggsave(file="~/foodGCEfile/FoodResults/MPAcoverageOAconstant_redistribute.png", MPAcoverage, width = 10, height = 8, dpi = 600, units = "in")
#ggsave(file="~/foodGCEfile/FoodResults/MPAcoverageCollapse_redistribute.png", MPAcoverage, width = 10, height = 8, dpi = 600, units = "in")
#ggsave(file="~/foodGCEfile/FoodResults/MPAcoverageAllmanaged_redistribute.png", MPAcoverage, width = 10, height = 8, dpi = 600, units = "in")
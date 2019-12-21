#FigurePlot_Github
#plot code for the food provision paper
#Author: Reniel Cabral

gc()
rm(list = ls())
#.rs.restartR()

library(doParallel)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)
library(ggplot2)
library(cowplot)
library(reshape)
library(magick)
library(png)
library(grid)
library(gridExtra)
library(tiff)
library(tmap)
library(leaflet)
library(sf)
library(scales)

#FIGURE 1 - Pixel-level
MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/CleanCoordmegacell_mollweide.rds")
KprotectedPerCell_Library<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="/Users/ren/Documents/CODES/FoodProvision/MPA_coord_mollweide.rds")
land_shp_moll<-readRDS(file="/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")

#get MPA positions
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)

#positions of 1s
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
CleanCoordmegacell_noMPA<-CleanCoordmegacell_MPA %>% filter(is.na(MPA)==TRUE) %>% select(lon,lat)
head(CleanCoordmegacell_noMPA)
CleanCoordmegacell_MPAcoord<-CleanCoordmegacell_MPA %>% filter(is.na(MPA)==FALSE) %>% select(lon,lat)
CleanCoordmegacell_mpaEND<-rbind(CleanCoordmegacell_noMPA,CleanCoordmegacell_MPAcoord)

length(MPAposition)#2931--- 2.4% are MPAs

numcell<-dim(Cleanmegacell)[1]
celltoiterateFULL<-1:numcell
celltoiterate_noMPA<-celltoiterateFULL
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
MPAselect0_noMPA<-MPAselect0
PriorityAreas<-c()
NetworkResult<-vector()

#Make MPAselect0==1 for MPAs
MPAselect0[MPAposition]<-1
sum(MPAselect0)

#remove MPAs from celltoiterateFULL
celltoiterateFULL<-celltoiterateFULL[-MPAposition]
celltoiterate<-celltoiterateFULL
ncell<-length(celltoiterate)

#load EEZ (high res) grid and remove MPA position
EEZs_coord<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/EEZs_coord_mollweide.rds")
head(EEZs_coord)

CleanCoordmegacell_EEZ<-left_join(CleanCoordmegacell,EEZs_coord,by=c("lon","lat"))
#compare if arrangements maintained
head(CleanCoordmegacell)
head(CleanCoordmegacell_EEZ)#this is the file with EEZ=1 as EEZ and EEZ=NA as HS
sum(CleanCoordmegacell_EEZ$EEZ,na.rm=T)
dim(CleanCoordmegacell_EEZ)
CleanCoordmegacell_EEZ_noMPA<-CleanCoordmegacell_EEZ[-MPAposition,]#the comma is necessary here
dim(CleanCoordmegacell_EEZ_noMPA)
head(CleanCoordmegacell_EEZ_noMPA)

###Compute PIXEL-LEVEL spillover 
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
r<-MegaData$r
E<-MegaData$Efin_BAU1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
HBAU

#Harvest BAU with no MPA
sum((1-E)*((r+E-1)/r)*K)
#current MPA benefits
HBAU-sum((1-E)*((r+E-1)/r)*K)

result<-readRDS("/Users/ren/Documents/CODES/FoodProvision/pixellevelfoodresult_mollweide.rds")
npick<-100 #this does not matter since the cells are in order --- no reshuffling
nmax<-floor(length(result)/npick)
head(result)

#Plot1b
Plot1b<-as.data.frame(result)
CleanCoordmegacell_EEZ_noMPA$EEZ[CleanCoordmegacell_EEZ_noMPA$EEZ==1]<-"EEZ"
CleanCoordmegacell_EEZ_noMPA$EEZ[is.na(CleanCoordmegacell_EEZ_noMPA$EEZ)==T]<-"High Seas"
Plot1b$EEZ<-CleanCoordmegacell_EEZ_noMPA$EEZ
head(Plot1b)

Plot1b %>% group_by(EEZ) %>% summarize(mean=mean(V1)) #EEZ is 10 times higher than HS

FigS1<-ggplot(Plot1b,aes(x=EEZ,y=V1,fill=EEZ))+
  geom_bar(stat = "summary", fun.y = "mean",fill="grey")+labs(x="",y=expression(paste("Average ",Delta,"H (MMT)")))+
  theme_bw()+theme(axis.text=element_text(size=35),axis.title=element_text(size=35),legend.position = "none")
FigS1
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/FigS1.png", FigS1,dpi=300)#resolution not great

#plot here --- map using Juan code, food per cell, benefit curve
result<-readRDS("/Users/ren/Documents/CODES/FoodProvision/pixellevelfoodresult_mollweide.rds")
hist(result)
T0<-min(result)-10^10
T1<-min(result)
T2<-0
T3<-100
T4<-500
T5<-1000
T6<-5000
T7<-10000
T8<-max(result)

head(result)
result2<-result[1:dim(CleanCoordmegacell_MPAcoord)[1]]
head(result2)
result<-as.data.frame(result)
result2<-as.data.frame(result2)
names(result2) <- "deltaH"
names(result) <- "deltaH"
result3<-rbind(result,result2)

#revised plotting of Fig 1a
Fig1Data<-cbind(CleanCoordmegacell_mpaEND[,1:2],result3)
head(Fig1Data)

options("scipen"=100, "digits"=1)
root<-3
pixeldHplot<-Fig1Data %>% 
  mutate(tmp = abs(deltaH)^(1/root),
         tmp_2 = ifelse(deltaH < 0, -tmp, tmp)) %>% 
  ggplot()+
  geom_raster(aes(lon, lat, fill = tmp_2))+
  scale_fill_gradient2(labels = function(x){x^root},
                       low = "red", mid = "white",
                       high = "#00539CFF", space = "Lab",
                       name=expression(paste(Delta, "H (MT)")))+
  labs(title = "", fill = "", y = "", x = "")+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="cyan")+  #"#EEA47FFF"
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent'))
pixeldHplot
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig1_pixeldHplot.png", pixeldHplot, width = 8, height = 4, dpi = 600, units = "in")

###########################################################################################################################

#FIGURE 2: Network-Level Analysis
MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/CleanCoordmegacell_mollweide.rds")
KprotectedPerCell_Library<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="/Users/ren/Documents/CODES/FoodProvision/MPA_coord_mollweide.rds")
head(MPA_coord)
dim(MPA_coord)
colSums(Cleanmegacell)
#get MPA positions
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)

#Add eez coord here
EEZs_coord<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/EEZs_coord_mollweide.rds")
head(EEZs_coord)
CleanCoordmegacell_MPA_EEZ<-left_join(CleanCoordmegacell_MPA,EEZs_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA_EEZ)  
table(CleanCoordmegacell_MPA_EEZ$EEZ)
dim(CleanCoordmegacell_MPA_EEZ)

#positions of 1s
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
head(MPAposition)
length(MPAposition)#4491 --- 2.80% are MPAs
length(MPAposition)*100/dim(Cleanmegacell)[1]

###Compute PIXEL-LEVEL spillover
numcell<-dim(Cleanmegacell)[1]
celltoiterateFULL<-1:numcell
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
PriorityAreas<-c()
NetworkResultRandom<-vector()

#Make MPAselect0==1 for MPAs
MPAselect0[MPAposition]<-1
sum(MPAselect0)

#remove MPAs from celltoiterateFULL
celltoiterateFULL<-celltoiterateFULL[-MPAposition]
celltoiterate<-celltoiterateFULL
ncell<-length(celltoiterate)

K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
r<-MegaData$r
E<-MegaData$Efin_BAU1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
HBAU

nmax<-floor(length(celltoiterate)/1000)
PerSpDeltaH<-matrix(nrow=nmax,ncol=1342)

NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_BAU1_mollweide.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_BAU1_mollweide.rds")
head(MegaData)

#this is for generating how much food will be generated from managed stocks vs unmanaged.
PerSpeciesDH<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PerSpDeltaH100_OAconstant_mollweide.rds")
dim(PerSpeciesDH)
max(which(MegaData$Manage == 0))
unmanagedDH<-rowSums(PerSpeciesDH[,1:max(which(MegaData$Manage == 0))]) #unmanaged
plot(unmanagedDH)
max(unmanagedDH)

managedDH<-rowSums(PerSpeciesDH[,min(which(MegaData$Manage == 1)):max(which(MegaData$Manage == 1))]) #unmanaged
plot(managedDH)
max(managedDH)

#proportion of unmanaged stocks
max(unmanagedDH)/(max(unmanagedDH)+max(managedDH))
PICKSIZE<-100
nmax<-floor(ncell/PICKSIZE)
BenefitCurve3<-as.data.frame(NetworkResult)/1000000
MPAsize<-(length(MPAposition)+1)*100/dim(Cleanmegacell)[1]
MPAsize0<-length(MPAposition)*100/dim(Cleanmegacell)[1]
#(MPAinEEZ+1)*100/length(EEZposition)#there is +1 because the next pixel starts with +1
BenefitCurve3$MPA <- rescale(seq.int(nmax), to = c(MPAsize, dim(BenefitCurve3)[1]*100/(ncell/100)   ))
zerozero<-data.frame(0,0)
names(zerozero)<-c("NetworkResult","MPA")
zerozero[1,]<-c(-HBAU/(1000000),100)
zerozero[2,]<-c(0,MPAsize0)
BenefitCurve3<-rbind(BenefitCurve3,zerozero)
theme_set(theme_cowplot())
benefitplot3<-ggplot(BenefitCurve3, aes(MPA, NetworkResult)) +geom_line(col="blue")+# theme_classic()+
  labs(x="% MPA coverage",y="Change in catch (MMT)")
benefitplot3


#all Managed scenario
NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_allmanaged.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_allmanaged.rds")
PICKSIZE<-100
nmax<-floor(ncell/PICKSIZE)
BenefitCurve_allmanaged<-as.data.frame(NetworkResult)/1000000
MPAsize<-(length(MPAposition)+1)*100/dim(Cleanmegacell)[1]
MPAsize0<-length(MPAposition)*100/dim(Cleanmegacell)[1]
BenefitCurve_allmanaged$MPA <- rescale(seq.int(nmax), to = c(MPAsize, dim(BenefitCurve_allmanaged)[1]*100/(ncell/100)   ))
zerozero<-data.frame(0,0)
names(zerozero)<-c("NetworkResult","MPA")
zerozero[1,]<-c(-60.6,100)
zerozero[2,]<-c(0,MPAsize0)
BenefitCurve_allmanaged<-rbind(BenefitCurve_allmanaged,zerozero)
theme_set(theme_cowplot())
benefitplot_allmanaged<-ggplot(BenefitCurve_allmanaged, aes(MPA, NetworkResult)) +geom_line(col="blue")+# theme_classic()+
  labs(x="% MPA coverage",y="Change in catch (MMT)")
benefitplot_allmanaged

##BBAU2
E<-MegaData$Efin
numcell<-dim(Cleanmegacell)[1]
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
MPAselect0[MPAposition]<-1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
HBAU

NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_OAconstant_mollweide.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_OAconstant_mollweide.rds")
PICKSIZE<-100
nmax<-floor(ncell/PICKSIZE)
BenefitCurve_BAU2<-as.data.frame(NetworkResult)/1000000
MPAsize<-(length(MPAposition)+1)*100/dim(Cleanmegacell)[1]
MPAsize0<-length(MPAposition)*100/dim(Cleanmegacell)[1]
BenefitCurve_BAU2$MPA <- rescale(seq.int(nmax), to = c(MPAsize, dim(BenefitCurve_BAU2)[1]*100/(ncell/100)   ))
zerozero<-data.frame(0,0)
names(zerozero)<-c("NetworkResult","MPA")
zerozero[1,]<-c(-HBAU/1000000,100)
zerozero[2,]<-c(0,MPAsize0)
BenefitCurve_BAU2<-rbind(BenefitCurve_BAU2,zerozero)
theme_set(theme_cowplot())
benefitplot_BAU2<-ggplot(BenefitCurve_BAU2, aes(MPA, NetworkResult)) +geom_line(col="blue")+# theme_classic()+
  labs(x="% MPA coverage",y="Change in catch (MMT)")
benefitplot_BAU2

###Collapsed (0.1k) assumption
E<-MegaData$EBvK01fin
numcell<-dim(Cleanmegacell)[1]
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
MPAselect0[MPAposition]<-1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)
HBAU

NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_EBvK01fin_mollweide.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_EBvK01fin_mollweide.rds")
PICKSIZE<-100
nmax<-floor(ncell/PICKSIZE)
BenefitCurve_EBvK01fin<-as.data.frame(NetworkResult)/1000000
MPAsize<-(length(MPAposition)+1)*100/dim(Cleanmegacell)[1]
MPAsize0<-length(MPAposition)*100/dim(Cleanmegacell)[1]
BenefitCurve_EBvK01fin$MPA <- rescale(seq.int(nmax), to = c(MPAsize, dim(BenefitCurve_EBvK01fin)[1]*100/(ncell/100)   ))
zerozero<-data.frame(0,0)
names(zerozero)<-c("NetworkResult","MPA")
zerozero[1,]<-c(-HBAU/1000000,100)
zerozero[2,]<-c(0,MPAsize0)
BenefitCurve_EBvK01fin<-rbind(BenefitCurve_EBvK01fin,zerozero)
theme_set(theme_cowplot())
benefitplot_EBvK01fin<-ggplot(BenefitCurve_EBvK01fin, aes(MPA, NetworkResult)) +geom_line(col="blue")+# theme_classic()+
  labs(x="% MPA coverage",y="Change in catch (MMT)")
benefitplot_EBvK01fin

benefitplot_combined<-ggplot(BenefitCurve3, aes(MPA, NetworkResult)) +geom_line(col="#00A087FF",size=0.5)+ 
  labs(x="% MPA coverage",y="Change in catch (MMT)")+theme_bw()+ylim(min(BenefitCurve_allmanaged$NetworkResult),max(BenefitCurve_EBvK01fin$NetworkResult))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14))+
  geom_line(data=BenefitCurve_allmanaged, aes(MPA, NetworkResult),col="red",size=0.5,linetype = "dotted")+theme(legend.position = "none")+
  geom_line(data=BenefitCurve_EBvK01fin, aes(MPA, NetworkResult),col="black",size=0.5,linetype = "dotdash")+theme(legend.position = "none")+
  geom_line(data=BenefitCurve_BAU2, aes(MPA, NetworkResult),col="black",size=0.5,linetype = "dashed")+theme(legend.position = "none")
benefitplot_combined
#ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig2Benefit.png", benefitplot_combined,dpi=600)


#Fig 2a.
NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult10_BAU1_mollweide.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas10_BAU1_mollweide.rds")
length(PriorityAreas)
length(NetworkResult)
head(PriorityAreas)
head(NetworkResult)

Priority<-as.data.frame(PriorityAreas)
Priority$rank <- (seq.int(nrow(Priority))/ncell)*100

#add dh
picksize<-10
NetworkResult_prime<-as.data.frame(NetworkResult)
dH_prime<-NetworkResult_prime %>% mutate(dH = NetworkResult - lag(NetworkResult, default = 0))
plot(dH_prime$dH)
Priority$NetworkResult<-rep(NetworkResult, each=picksize) ## this is adding delta H
Priority$dH<-rep(dH_prime$dH/picksize, each=picksize) ## this is adding delta H
head(Priority)
dim(Priority)

PriorityFrame <- as.data.frame(seq.int(nrow(CleanCoordmegacell)))
names(PriorityFrame) <- "PriorityAreas"
#add the priority
PriorityFrame2<-left_join(PriorityFrame,Priority, by="PriorityAreas")
PriorityFrame2[is.na(PriorityFrame2)] <- -1000 #0
head(PriorityFrame2)
dim(PriorityFrame2)

##--Where are those MPAs --- in EEZ or HS?
PriorityAreas<-as.data.frame(PriorityAreas)
PriorityAreas$Rank<-rep(1:(dim(PriorityAreas)[1]/100), each=100)
head(PriorityAreas)
dim(PriorityAreas)
RankPrior<-PriorityAreas
RankPrior$PriorityAreas<-as.factor(RankPrior$PriorityAreas)
head(RankPrior)
dim(RankPrior)

#attach EEZ/HS
CleanCoordmegacell_MPA_EEZ$PriorityAreas<-row.names(CleanCoordmegacell_MPA_EEZ)
head(CleanCoordmegacell_MPA_EEZ)

#add the deltaH
GlobaldH<-as.data.frame(NetworkResult)/1000000
head(GlobaldH)
dim(GlobaldH)

ShortCoord<-CleanCoordmegacell
ShortCoord$rank<-PriorityFrame2$rank
ShortCoord$NetworkResult<- PriorityFrame2$NetworkResult#this is for deriving dh
ShortCoord$dH<- PriorityFrame2$dH#this is for deriving dh
ShortCoord$ID<-row.names(ShortCoord)
head(ShortCoord)
coordsplot<-ShortCoord %>% select(lon,lat)

#Are ShortCoord lat lon in EEZ or HS?
#EEZ only selection (activate)
EEZs_coord<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/EEZs_coord_mollweide.rds")
head(EEZs_coord)
ShortCoordEEZ<-left_join(ShortCoord,EEZs_coord,by=c("lon","lat"))
head(ShortCoordEEZ)  
table(ShortCoordEEZ$EEZ)
dim(ShortCoordEEZ)

SortShortCoordEEZ<-ShortCoordEEZ[order(-ShortCoordEEZ$rank),]
head(SortShortCoordEEZ)
SortShortCoordEEZ$EEZ[is.na(SortShortCoordEEZ$EEZ)] <- 0
cumsumrank<-SortShortCoordEEZ %>% mutate(EEZcs = cumsum(EEZ)) %>% mutate(HScs = cumsum(1-EEZ)) %>% mutate(id = row_number())
cumsumrank$id <- rescale(seq.int(max(cumsumrank$id)), to = c(MPAsize0, 100))
head(cumsumrank)

sum(cumsumrank$EEZ==1)#eez
sum(cumsumrank$EEZ==0)#hs
cumsumrank$EEZcs<-cumsumrank$EEZcs/sum(cumsumrank$EEZ==1)
cumsumrank$HScs<-cumsumrank$HScs/sum(cumsumrank$EEZ==0)

cumsumrankplot<-ggplot(cumsumrank)+geom_line(aes(x=id, y=EEZcs),col="#F39B7FFF",size=1) + geom_line(aes(x=id, y=HScs),col="#8491B4FF",size=1)+labs(x="% MPA coverage",y="Protected/Available pixels")+
  theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=14))+
  annotate("text", x = 50, y = 0.75, label = "High Seas",size=4)+annotate("text", x = 50, y = 0.25, label = "EEZs",size=4)
cumsumrankplot

min(ShortCoord$rank)
max(ShortCoord$rank)
ShortCoord_Sort <- ShortCoord[order(-ShortCoord$rank),] %>% filter(rank!=-1000)
head(ShortCoord_Sort)
dim(ShortCoord_Sort)
min(ShortCoord_Sort$rank)

ShortCoord_Sort$sign<-ShortCoord_Sort$dH>0
InflectPoint<-min(which(ShortCoord_Sort$sign == TRUE))
InflectMPA<-ShortCoord_Sort$rank[InflectPoint]

MPAcoverage<-ShortCoord_Sort %>% ggplot(aes(x=lon,y=lat,fill=rank)) + #scale_fill_gradient2(low="darkred", mid="white",high="#00539CFF",midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar", aesthetics = "fill",name=expression(paste(Delta, "H (MT)")))+
  scale_fill_gradientn(colours = c("#00539CFF", "white", "red"), limits=c(0,100), values = scales::rescale(c(min(ShortCoord_Sort$rank), InflectMPA, max(ShortCoord_Sort$rank))),name="% MPA coverage")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position=c(0.65, 0.05), legend.direction = "horizontal")+ #"bottom
  geom_raster()+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="cyan")+  #"#EEA47FFF"
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent'))
MPAcoverage
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/MPAcoverage.png", MPAcoverage, width = 10, height = 8, dpi = 600, units = "in")#resolution not great

p1<-MPAcoverage
p2<-benefitplot_combined
p3<-cumsumrankplot
fig2<-grid.arrange(p1,p2,p3, layout_matrix = rbind(c(1,1),c(1,1),c(2,3)))
fig2
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig2.png", fig2, width = 10, height = 8, dpi = 600, units = "in")

##------------PLOT FIGURE 3-------------
head(MegaData)
ER_ratio_EBvK01fin<-weighted.mean((1-MegaData$EBvK01fin)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_EBvK01_msy<-weighted.mean((1-MegaData$EBvK01_msy)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_OA_constant<-weighted.mean((1-MegaData$Efin)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_Efin_msy<-weighted.mean((1-MegaData$Efin_msy)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_Efin_BAU1<-weighted.mean((1-MegaData$Efin_BAU1)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_OAhalf_msy<-weighted.mean((1-MegaData$Efinhalf_msy)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_EBvK01fin
ER_ratio_EBvK01_msy
ER_ratio_Efin_BAU1
ER_ratio_OA_constant
ER_ratio_OA_msy
ER_ratio_OAhalf_msy

dH_EBvK01fin<-max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_EBvK01fin_mollweide.rds"))/1000000
dH_EBvK01_msy<-max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_EBvK01_msy_mollweide.rds"))/1000000
dH_OA_constant<-max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_OAconstant_mollweide.rds"))/1000000
dH_Efin_msy<-max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_Efin_msy_mollweide.rds"))/1000000
dH_Efin_BAU1<-max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_BAU1_mollweide.rds"))/1000000

dH_OA_constant/dH_Efin_BAU1
dH_EBvK01fin/dH_Efin_BAU1

NetworkResult100_EBvK01_msy_mollweide

require(ggplot2)
require(ggrepel)
testdata <- data.frame(x = c(ER_ratio_EBvK01fin,ER_ratio_EBvK01_msy,ER_ratio_OA_constant,ER_ratio_Efin_msy,ER_ratio_Efin_BAU1,1),
                       Policy = c("Collapse + current E","Collapse + MSY", "BAU \"all stocks\"", "BAU \"all stocks\" + MSY", "BAU \"conservation concern\"", "All MSY"),  # Create example data
                       y = c(dH_EBvK01fin, dH_EBvK01_msy,dH_OA_constant, dH_Efin_msy, dH_Efin_BAU1, 0))
testdata$Policy <- as.factor(testdata$Policy)

dodge <- position_dodge(1)
Fig3Science<-ggplot(testdata, aes(x = x, y = y, label = Policy)) + geom_line(linetype = "dotted")+
  geom_point(
    #mapping = aes(color = "#EE000099"),
    color = "#EE000099",
    position = dodge,
    size = 5,
    alpha = 0.5
  ) +
  geom_text_repel(
    nudge_x       = 0.1,
    segment.size  = 0.2,
    size=7,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0
  ) +
  labs(y="MPA benefits (change in catch in MMT)") +
  ylim(-2,dH_EBvK01fin) + 
  xlab(bquote('E/E'[msy]))+
  theme_minimal()+
  theme(text = element_text(size=20))+ 
  scale_x_reverse()+  xlim(ER_ratio_EBvK01fin,.8) +annotate(geom="text", x=1.25, y=-1.5, label=expression("Poorly managed fisheries" %->% "Well-managed fisheries"),size=7)
Fig3Science
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig3.png", Fig3Science,width = 10, height = 8, dpi = 300, units = "in")
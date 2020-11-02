#GITHUB
#Pixel-level food provisioning calculation
#Author: Reniel Cabral
#December 2019

# gc()
# rm(list = ls())

library(doParallel)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)
library(pryr)
library(ggplot2)
library(cowplot)
library(reshape)

#MOLLWEIDE
MegaData<-readRDS(file = "MegaData.rds")
Cleanmegacell<-readRDS(file = "Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "CleanCoordmegacell_mollweide.rds")
KprotectedPerCell_Library<-readRDS(file = "KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="MPA_coord_mollweide.rds") #this is my code
#MPA_coord<-readRDS(file="~/foodGCEfile/MPA_coord_mollweide_JuanMatched.rds") #juan's code
land_shp_moll<-readRDS(file = "land_shp_moll.rds")
head(MPA_coord)
dim(MPA_coord)

#get MPA positions
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)

#positions of 1s
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
head(MPAposition)
length(MPAposition)#4491 --- 2.80% are MPAs
length(MPAposition)*100/dim(Cleanmegacell)[1]

##TRY new approach
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

#remove MPAs from celltoiterateFULL ##For now, fine
celltoiterateFULL<-celltoiterateFULL[-MPAposition]
celltoiterate<-celltoiterateFULL
ncell<-length(celltoiterate)

###Compute spillover---PIXEL-LEVEL spillover 
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
r<-MegaData$r
E<-MegaData$Efin_BAU1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)
HBAU

cores<-detectCores() - 2
registerDoParallel(cores)
MPAselectPrev<-rowSums(KprotectedPerCell_Library[,which(MPAselect0==1),drop=FALSE])#needed since there are existing MPAs!!!
result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {
  MPAselect<-MPAselect0
  MPAselect[celltoiterate[iter]]<-1
  R<-MPAselectPrev+KprotectedPerCell_Library[,celltoiterate[iter]]
  hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)
  HMPA-HBAU
}
stopImplicitCluster()

head(result) #this is the ranking --- use this ranking to prioritize

#save pixel-level result
saveRDS(result,file = "pixellevelfoodresult_mollweide.rds")

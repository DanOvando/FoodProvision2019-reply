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

##Define scenarios. 
scenario<-"BAU1"
#scenario<-"OAconstant"
#scenario<-"all managed"
#scenario<-"Escorched_current"
#scenario<-"Efin_msy"
#scenario<-"EBvK01fin"
#scenario<-"EBvK01_msy"

#Load files
MegaData<-readRDS(file = "~/foodGCEfile/MegaData.rds")
Cleanmegacell<-readRDS(file = "~/foodGCEfile/Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "~/foodGCEfile/CleanCoordmegacell_mollweide.rds")
KprotectedPerCell_Library<-readRDS(file = "~/foodGCEfile/KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="~/foodGCEfile/MPA_coord_mollweide.rds")
land_shp_moll<-readRDS(file = "~/foodGCEfile/land_shp_moll.rds")
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
#celltoiterate<-celltoiterateFULL #can erase this if we activate remove MPAs from celltoiterate
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
PriorityAreas<-c()
NetworkResult<-vector()

#Make MPAselect0==1 for MPAs
MPAselect0[MPAposition]<-1
head(MPAselect0)
sum(MPAselect0)

#remove MPAs from celltoiterateFULL ##For now, fine
celltoiterateFULL<-celltoiterateFULL[-MPAposition]
celltoiterate<-celltoiterateFULL
ncell<-length(celltoiterate)

###Compute spillover---PIXEL-LEVEL spillover 
K<-MegaData$Kfin #k per species
m<-MegaData$m #mobility per species
r<-MegaData$r

if (scenario=="all managed"){
  E<-MegaData$Emsy
}else if(scenario=="2012xmsy"){
  E<-MegaData$E2050xmsyfin
}else if(scenario=="constant"){
  E<-MegaData$E2050xcurrentfin
}else if(scenario=="OAconstant"){
  E<-MegaData$Efin
}else if(scenario=="OAconstantxmsy"){
  E<-MegaData$E2050bestxmsy
}else if(scenario=="Escorched_current"){
  E<-MegaData$Escorched_current
}else if(scenario=="OAhalfxmsy"){  
  E<-MegaData$Efinhalf_msy
}else if(scenario=="BAU1"){  
  E<-MegaData$Efin_BAU1
}else if(scenario=="Efin_msy"){ 
  E<-MegaData$Efin_msy
}else if(scenario=="EBvK01fin"){ 
  E<-MegaData$EBvK01fin 
}else if(scenario=="EBvK01_msy"){ 
  E<-MegaData$EBvK01_msy  
}else if(scenario=="Efinhalf_msy"){   
  E<-MegaData$Efinhalf_msy
}

MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
HBAU

PICKSIZE<-10

nmax<-floor(length(celltoiterate)/PICKSIZE)
nmax #this is the number of iterations needed for PICKSIZE at a time!
PerSpDeltaH<-matrix(nrow=nmax,ncol=1342)
PerSpDeltaH_EEZ<-matrix(nrow=nmax,ncol=1342)
PerSpDeltaH_HS<-matrix(nrow=nmax,ncol=1342)

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
  cellselected<-myorderHightoLow[1:PICKSIZE] #but these are the position of the temporary pixels, not our reference pixels
  #convert coord to scale comparable to priority areas
  Prioritycellselected<-celltoiterate[cellselected]
  #plot(result[myorderHightoLow][1:1000])#plot values for demo
  
  #3. block those additional 1000 in MPAselect
  MPAselect0[Prioritycellselected]<-1
  
  ##3.1.Compute fraction per species within EEZ using the cleanmegacee
  #this is working but i will comment now --- not needed
  #RangeinEEZ<-t((CleanCoordmegacell_MPA_EEZ$EEZ)*(1-MPAselect0)) %*% as.matrix(Cleanmegacell) #EEZ x not in MPA  
  #SpeciesRangeinEEZ<-t(RangeinEEZ)
  
  ###for NOW, just plot EEZ
  #RangeinHS<-t((1-CleanCoordmegacell_MPA_EEZ$EEZ)*(1-MPAselect0)) %*% as.matrix(Cleanmegacell) #EEZ x not in MPA  
  #SpeciesRangeinHS<-t(RangeinHS)
  
  #3. save them for our priority areas
  PriorityAreas<-append(PriorityAreas,Prioritycellselected)
  #4. Calculate food prov of the additional 1000 cells
  MPAselect<-MPAselect0
  R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
  hmpa<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
  #HMPA-HBAU
  
  #save result. Comment other parts not needed now.
  PerSpDeltaH[i,]<-hmpa-hbau
  #PerSpDeltaH_EEZ[i,]<-(hmpa-hbau)*SpeciesRangeinEEZ
  #PerSpDeltaH_HS[i,]<-(hmpa-hbau)*SpeciesRangeinHS
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
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_BAU1_mollweide.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_BAU1_mollweide.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_BAU1_mollweide.rds")  
}else if(scenario=="all managed"){  
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_allmanaged.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_allmanaged.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_allmanaged.rds") 
}else if(scenario=="OAconstant"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_OAconstant_mollweide.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_OAconstant_mollweide.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_OAconstant_mollweide.rds")   
}else if(scenario=="Escorched_current"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_Escorched_current_mollweide.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_Escorched_current_mollweide.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_Escorched_current_mollweide.rds")
}else if(scenario=="Efin_msy"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_Efin_msy_mollweide.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_Efin_msy_mollweide.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_Efin_msy_mollweide.rds")  
}else if(scenario=="EBvK01fin"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_EBvK01fin_mollweide.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_EBvK01fin_mollweide.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_EBvK01fin_mollweide.rds") 
}else if(scenario=="EBvK01_msy"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_EBvK01_msy_mollweide.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_EBvK01_msy_mollweide.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_EBvK01_msy_mollweide.rds") 
}



if (scenario=="all managed"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_allmanaged.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_allmanaged.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_allmanaged.rds")
}else if(scenario=="2012xmsy"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_2012xmsy.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_2012xmsy.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_2012xmsy.rds")
}else if(scenario=="constant"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_constant.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_constant.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_constant.rds")
}else if(scenario=="OAconstant"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_OAconstant.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_OAconstant.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_OAconstant.rds")
}else if(scenario=="OAconstant10"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH10_OAconstant.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult10_OAconstant.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas10_OAconstant.rds")
}else if(scenario=="OAconstantxmsy"){
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_OAconstantxmsy.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_OAconstantxmsy.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_OAconstantxmsy.rds")  
}else if(scenario=="scorchedearth"){  
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_scorchedearth.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_scorchedearth.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_scorchedearth.rds") 
}else if(scenario=="OAhalfxmsy"){  
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_OAhalfxmsy.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_OAhalfxmsy.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_OAhalfxmsy.rds") 
}else if(scenario=="BAU1"){   
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100_BAU1.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100_BAU1.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100_BAU1.rds")   
}else {
  saveRDS(PerSpDeltaH,file = "~/foodGCEfile/PerSpDeltaH100.rds")
  saveRDS(NetworkResult,file = "~/foodGCEfile/NetworkResult100.rds")
  saveRDS(PriorityAreas,file = "~/foodGCEfile/PriorityAreas100.rds")
}

NetworkResult<-readRDS(file = "~/foodGCEfile/NetworkResult100.rds")
PriorityAreas<-readRDS(file = "~/foodGCEfile/PriorityAreas100.rds")

PerSpDeltaH<-readRDS(file = "~/foodGCEfile/PerSpDeltaH100.rds")
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


if (scenario=="all managed"){
  
}else if(scenario=="2012xmsy"){
  png(file=paste("~/foodGCEfile/HLP/benefitfunctionFoodGlobal_2012xmsy.png",sep = ""), width = 6, height = 4.5, units = 'in', res = 300)
  benefitplot
  dev.off()
}else if(scenario=="constant"){  
  png(file=paste("~/foodGCEfile/HLP/benefitfunctionFoodGlobal_constant.png",sep = ""), width = 6, height = 4.5, units = 'in', res = 300)
  benefitplot
  dev.off()
}else if(scenario=="OAconstant"){  
  png(file=paste("~/foodGCEfile/HLP/benefitfunctionFoodGlobal_OAconstant.png",sep = ""), width = 6, height = 4.5, units = 'in', res = 300)
  benefitplot
  dev.off()
}else {
  png(file=paste("~/foodGCEfile/HLP/benefitfunctionFoodGlobal.png",sep = ""), width = 6, height = 4.5, units = 'in', res = 300)
  benefitplot
  dev.off()
}

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
##green = "#008B45FF"
#red -= "#BB0021FF"
#BLEU GREEN  "#00828099"
#low="white", high="#BB0021FF"

GlobalMap<-ShortCoord %>% ggplot(aes(x=lon,y=lat,fill=rank)) + scale_fill_gradient(low="white", high="#00539CFF",limits=c(0,100),name="Rank")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_raster()+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="#EEA47FFF")+
  geom_sf(data = land_shp_moll, inherit.aes = F)
GlobalMap

#ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities10_BAU1.png", width = 12, height = 6, units = 'in', dpi= 300)


if(scenario=="BAU1"){   
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_BAU1.png", width = 12, height = 6, units = 'in', dpi= 300)
}else if(scenario=="all managed"){
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_allmanaged.png", width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="OAconstant"){
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_OAconstant.png", width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="Escorched_current"){
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_Escorched_current.png", width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="Efin_msy"){
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_Efin_msy.png", width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="EBvK01fin"){
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_EBvK01fin.png", width = 12, height = 6, units = 'in', dpi= 600)
}else if(scenario=="EBvK01_msy"){
  ggsave("~/foodGCEfile/FoodResults/FoodProvPriorities100_EBvK01_msy.png", width = 12, height = 6, units = 'in', dpi= 600)
}



# #ShortCoord<-ShortCoord %>% select(ID,rank)
# coordsplot<-ShortCoord %>% select(lon,lat)
# empty_raster <- raster(res = 0.5)
# cells <- cellFromXY(empty_raster, as.matrix(coordsplot))
# empty_raster[cells] <- ShortCoord$rank#PriorityFrame2$rank
# plot(empty_raster,main="Food provision potential (MT)",axes=F,box=F)
# 
# # #load long coord
# # LongCoord<-readRDS(file = "~/foodGCEfile/Long_CleanCoordmegacell.rds")
# # LongCoord$ID<-row.names(LongCoord)
# # LongCoord2<-left_join(LongCoord,ShortCoord,by="ID")
# # LongCoord2[is.na(LongCoord2)] <- min(PriorityFrame2$rank,na.rm=T)
# # #------------------------
# # #plot
# # 
# # coordsplot<-LongCoord2 %>% select(lon,lat)
# # empty_raster <- raster(res = 0.5)
# # cells <- cellFromXY(empty_raster, as.matrix(coordsplot))
# # empty_raster[cells] <- LongCoord2$rank#PriorityFrame2$rank
# # plot(empty_raster,main="Food provision potential (MT)",axes=F,box=F)
# 
# #plot same as Juan
# library(tmap)
# library(leaflet)
# library(sf)
# #install.packages(c("tmap","leaflet","sf"))
# crs(empty_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# maxValue(empty_raster)
# z_pal <- list(breaks = c(0,50,60,70,80,90,95,100),
#               labels = c("0-50", "50-60", "60-70", "70-80", "80-90", "90-95", "95-100"),
#               colors = rev(c("#d73027","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1", "#4575b4")))
# land_shp <-st_read("~/foodGCEfile/landshp_moll/spatial-datasets-land-land_50.shp")
# ocean_low_res_moll<-raster::raster("~/foodGCEfile/ocean-low-res-moll.tiff")
# land_shp_moll <- land_shp %>% st_transform(crs = projection(ocean_low_res_moll))
# 
# FoodProvPriorities<-empty_raster %>% 
#   raster::projectRaster(ocean_low_res_moll) %>% 
#   tmap::tm_shape()+
#   tmap::tm_raster(title = "Priority score",
#                   palette  = z_pal$colors,
#                   breaks = z_pal$breaks,
#                   labels = z_pal$labels,
#                   legend.is.portrait = T,
#                   legend.reverse = T)+
#   tmap::tm_shape(land_shp_moll)+
#   tmap::tm_fill(col = "black", border.col = "transparent")+
#   #tmap::tm_credits(caption) +
#   tmap::tm_layout(title = "Food provision",
#                   title.position = c("center", .95),
#                   inner.margins = c(0.12, 0, 0.08, 0.04),
#                   frame = F,
#                   legend.position = c(.99, "center"))
# FoodProvPriorities


if (scenario=="all managed"){
  
}else if(scenario=="2012xmsy"){
  png(file="~/Food provision/FoodProvPriorities100_2012xmsy.png", width = 12, height = 6, units = 'in', res = 300)
  FoodProvPriorities
  dev.off()
}else if(scenario=="constant"){
  png(file="~/Food provision/FoodProvPriorities100_constant.png", width = 12, height = 6, units = 'in', res = 300)
  FoodProvPriorities
  dev.off()
}else if(scenario=="OAconstant"){
  png(file="~/Food provision/FoodProvPriorities100_OAconstant.png", width = 12, height = 6, units = 'in', res = 300)
  FoodProvPriorities
  dev.off()  
}else if(scenario=="OAconstantxmsy"){
  png(file="~/Food provision/FoodProvPriorities100_OAconstant.png", width = 12, height = 6, units = 'in', res = 300)
  FoodProvPriorities
  dev.off()    
}else {
  png(file="~/Food provision/FoodProvPriorities100_EEZ.png", width = 12, height = 6, units = 'in', res = 300)
  FoodProvPriorities
  dev.off()
}


#Plot here
Priority<-as.data.frame(PriorityAreas)
Priority$rank <- (length(MPAposition)+1):(dim(Priority)[1]+length(MPAposition))#(MPAinEEZ+1):length(EEZposition)
coord<-na.omit(CleanCoordmegacell)
coord2 <- cbind(PriorityAreas = rownames(coord), coord)
Priority$PriorityAreas<-as.factor(Priority$PriorityAreas)
Priority2<-left_join(Priority,coord2, by="PriorityAreas")

#ggplot(Priority2, aes(x=lon,y=lat,colour=rank))+geom_point()
#juan_plot
JuanPlot<-Priority2 %>% select(lat,lon,rank)
JuanPlot$MPA<-0
#bind MPA
JuanPlot<-rbind(MPA_coord,JuanPlot)
JuanPlot$rank <- rescale(JuanPlot$rank, to = c(0, 100)) #((seq.int(nrow(Priority))/nmax))*100
JuanPlot
#ggplot(JuanPlot, aes(x=lon,y=lat,colour=rank))+geom_point()
#dim(unique(JuanPlot[c("lat", "lon")]))

##Save
write.csv(JuanPlot,file = paste("~/foodGCEfile/HLP/Food_",ISO[countryNum],".csv",sep = ""))

#plot same as Juan
coordsplot<-JuanPlot %>% select(lon,lat)
empty_raster <- raster(res = 0.5)
cells <- cellFromXY(empty_raster, as.matrix(coordsplot))
empty_raster[cells] <- JuanPlot$rank

png(file=paste("~/foodGCEfile/HLP/FoodMAP_",ISO[countryNum],".png",sep = ""), width = 8, height = 8, units = 'in', res = 300)
colfunc<-c("#d73027","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1", "#4575b4")
plot(empty_raster, xlim = c(min(JuanPlot$lon),max(JuanPlot$lon)), ylim = c(min(JuanPlot$lat), max(JuanPlot$lat)),col=colfunc, breaks=c(0,5,10,20,30,40,50,100), interpolate=F, main=countries[countryNum],legend.args=list(text='Top % of EEZ', side=4,font=1, line=2.5, cex=1.1))
map("world", add=TRUE, lwd=0.5, interior = FALSE, col = "black")
dev.off()




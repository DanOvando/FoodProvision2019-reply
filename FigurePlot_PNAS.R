#FigurePlot_Github
#Plot code used for the food provision paper
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
library(matrixStats)
library(ggpubr)

# #check Juan's code for incorporating uncertainty in E
# MegaData_UncertaintyAnalysis<-read.csv(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData_UncertaintyAnalysis.csv")
# head(MegaData_UncertaintyAnalysis)
# 
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "Manage")] <- "is_managed"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "Fstatus")] <- "f_status"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "Bstatus")] <- "b_status"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "Emanage")] <- "e_manage"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "ER")] <- "er"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "r_fin")] <- "r_run"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "Fstatus")] <- "f_status"
# colnames(MegaData_UncertaintyAnalysis)[which(names(MegaData_UncertaintyAnalysis) == "BK2012")] <- "bk_2012"
# 
#   
# recalculate_ex_rate <- function(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012){
#   
#   if(scenario == "ex_rate_bau1"){
#     
#     ex_rate_run <- if_else(is_managed | f_status >= 1 | b_status <= 1,
#                            if_else(e_manage != -1 & er > r_run,
#                                    r_run, 
#                                    if_else(e_manage != -1 & er <= r_run,
#                                            er,
#                                            (1 - bvk_fin)*r_run)),
#                            (1 - bk_2012)*r_run)
#     
#   } else if (scenario == "ex_rate_oa_cons"){
#     
#     ex_rate_run <- if_else(e_manage != -1 & er > r_run,
#                            r_run, 
#                            if_else(e_manage != -1 & er <= r_run,
#                                    er,
#                                    (1 - bvk_fin)*r_run))
#     
#   } else if (scenario == "ex_rate_all_msy"){
#     
#     ex_rate_run <- 0.5*r_run
#     
#   } else if (scenario == "ex_rate_efin_msy"){
#     
#     ex_rate_run <- if_else(!is_managed,
#                            if_else(e_manage != -1 & er > r_run,
#                                    r_run, 
#                                    if_else(e_manage != -1 & er <= r_run,
#                                            er,
#                                            (1 - bvk_fin)*r_run)),
#                            0.5*r_run)
#     
#   } else if (scenario == "ex_rate_worm_oa"){
#     
#     ex_rate_run <- if_else(e_manage != -1 & er > r_run,
#                            r_run, 
#                            if_else(e_manage != -1 & er <= r_run,
#                                    er, 
#                                    0.9*r_run))
#     
#   } else if (scenario == "ex_rate_worm_msy"){
#     
#     ex_rate_run <- if_else(e_manage != -1, 
#                            0.5*r_run, 
#                            0.9*r_run)
#     
#   }
#   
#   ex_rate_run[ex_rate_run > 1] <- 1 
#   
#   return(ex_rate_run)
#   
# }
# 
# 
# scenario<-"ex_rate_all_msy"
# MegaData_UncertaintyAnalysis<-MegaData_UncertaintyAnalysis %>% mutate(recalc_msy=recalculate_ex_rate(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012))
# plot(MegaData_UncertaintyAnalysis$ExploitationRate_AllMSY,MegaData_UncertaintyAnalysis$recalc_msy)  
# 
# scenario<-"ex_rate_worm_msy"
# MegaData_UncertaintyAnalysis<-MegaData_UncertaintyAnalysis %>% mutate(recalc_wormmsy=recalculate_ex_rate(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012))
# plot(MegaData_UncertaintyAnalysis$ExploitationRate_WormMSY,MegaData_UncertaintyAnalysis$recalc_wormmsy)  
# 
# scenario<-"ex_rate_worm_oa"
# MegaData_UncertaintyAnalysis<-MegaData_UncertaintyAnalysis %>% mutate(recalc_wormoa=recalculate_ex_rate(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012))
# plot(MegaData_UncertaintyAnalysis$ExploitationRate_WormOA,MegaData_UncertaintyAnalysis$recalc_wormoa)  
# 
# scenario<-"ex_rate_bau1"
# MegaData_UncertaintyAnalysis<-MegaData_UncertaintyAnalysis %>% mutate(recalc_bau1=recalculate_ex_rate(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012))
# plot(MegaData_UncertaintyAnalysis$ExploitationRate_BAU1,MegaData_UncertaintyAnalysis$recalc_bau1)  
# 
# scenario<-"ex_rate_oa_cons"
# MegaData_UncertaintyAnalysis<-MegaData_UncertaintyAnalysis %>% mutate(recalc_oacons=recalculate_ex_rate(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012))
# plot(MegaData_UncertaintyAnalysis$ExploitationRate_OAcons,MegaData_UncertaintyAnalysis$recalc_oacons)  
# 
# scenario<-"ex_rate_efin_msy"
# MegaData_UncertaintyAnalysis<-MegaData_UncertaintyAnalysis %>% mutate(recalc_efinmsy=recalculate_ex_rate(scenario, is_managed, f_status, b_status, e_manage, er, r_run, bvk_fin, bk_2012))
# plot(MegaData_UncertaintyAnalysis$ExploitationRate_EfinMSY,MegaData_UncertaintyAnalysis$recalc_efinmsy)  


#Check the results sent by Juan and PLOT (FIG2B)
#BAU1_uncertain<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_oa_cons.rds")
# results_ex_rate_bau1<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/20_percent_K_uncertain/results_ex_rate_bau1.rds")
# results_ex_rate_oa_cons<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/20_percent_K_uncertain/results_ex_rate_oa_cons.rds")
# results_ex_rate_worm_oa<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/20_percent_K_uncertain/results_ex_rate_worm_oa.rds")
# results_ex_rate_all_msy<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/20_percent_K_uncertain/results_ex_rate_all_msy.rds")

results_ex_rate_bau1<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_ex_rate_bau1.rds")
results_ex_rate_oa_cons<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_ex_rate_oa_cons.rds")
results_ex_rate_worm_oa<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_ex_rate_worm_oa.rds")
results_ex_rate_all_msy<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_ex_rate_all_msy.rds")

results_ex_rate_efin_msy<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_ex_rate_efin_msy.rds")
results_ex_rate_worm_msy<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Juan_uncertainty/results_ex_rate_worm_msy.rds")

head(results_ex_rate_efin_msy)
max(results_ex_rate_efin_msy$dH)

head(results_ex_rate_bau1)
tail(results_ex_rate_bau1)
npixels<-table(results_ex_rate_bau1$iter==1)[2]

results_ex_rate_bau1$ID<-rep_len(1:npixels, length.out=dim(results_ex_rate_bau1)[1])
results_ex_rate_oa_cons$ID<-rep_len(1:npixels, length.out=dim(results_ex_rate_oa_cons)[1])
results_ex_rate_worm_oa$ID<-rep_len(1:npixels, length.out=dim(results_ex_rate_worm_oa)[1])
results_ex_rate_all_msy$ID<-rep_len(1:npixels, length.out=dim(results_ex_rate_all_msy)[1])
results_ex_rate_efin_msy$ID<-rep_len(1:npixels, length.out=dim(results_ex_rate_efin_msy)[1])  
results_ex_rate_worm_msy$ID<-rep_len(1:npixels, length.out=dim(results_ex_rate_worm_msy)[1])

results_ex_rate_bau1<- results_ex_rate_bau1 %>% mutate(dH=dH/10^6)
results_ex_rate_oa_cons<- results_ex_rate_oa_cons %>% mutate(dH=dH/10^6)
results_ex_rate_worm_oa<- results_ex_rate_worm_oa %>% mutate(dH=dH/10^6)
results_ex_rate_all_msy<- results_ex_rate_all_msy %>% mutate(dH=dH/10^6)
results_ex_rate_efin_msy<- results_ex_rate_efin_msy %>% mutate(dH=dH/10^6)
results_ex_rate_worm_msy<- results_ex_rate_worm_msy %>% mutate(dH=dH/10^6)

#ggplot()+geom_line(data=results_ex_rate_bau1, aes(x=fraction_protected, y=dH, group_by(iter)))
  

bau1_df<-results_ex_rate_bau1 %>% group_by(ID) %>% summarise(FracProtect=mean(fraction_protected),MeanDH=mean(dH), Sd=sd(dH)) %>% select(FracProtect,MeanDH,Sd,ID)
oa_cons_df<-results_ex_rate_oa_cons %>% group_by(ID) %>% summarise(FracProtect=mean(fraction_protected),MeanDH=mean(dH), Sd=sd(dH)) %>% select(FracProtect,MeanDH,Sd,ID)
worm_oa_df<-results_ex_rate_worm_oa %>% group_by(ID) %>% summarise(FracProtect=mean(fraction_protected),MeanDH=mean(dH), Sd=sd(dH)) %>% select(FracProtect,MeanDH,Sd,ID)
all_msy_df<-results_ex_rate_all_msy %>% group_by(ID) %>% summarise(FracProtect=mean(fraction_protected),MeanDH=mean(dH), Sd=sd(dH)) %>% select(FracProtect,MeanDH,Sd,ID)
efin_msy_df<-results_ex_rate_efin_msy %>% group_by(ID) %>% summarise(FracProtect=mean(fraction_protected),MeanDH=mean(dH), Sd=sd(dH)) %>% select(FracProtect,MeanDH,Sd,ID)
worm_msy_df<-results_ex_rate_worm_msy %>% group_by(ID) %>% summarise(FracProtect=mean(fraction_protected),MeanDH=mean(dH), Sd=sd(dH)) %>% select(FracProtect,MeanDH,Sd,ID)

max(efin_msy_df$MeanDH)

#remove NaN
tail(bau1_df)
bau1_df <- na.omit(bau1_df)
maxID<-max(bau1_df$ID)#142871
oa_cons_df <- oa_cons_df %>% filter(ID<=maxID) #na.omit(oa_cons_df)
worm_oa_df <- worm_oa_df %>% filter(ID<=maxID)
all_msy_df <- all_msy_df %>% filter(ID<=maxID)
efin_msy_df <- efin_msy_df %>% filter(ID<=maxID)
worm_msy_df <- worm_msy_df %>% filter(ID<=maxID)

maxdH_bau1_df<-bau1_df[which.max(bau1_df$MeanDH),]
maxdH_bau1_df
maxdH_oa_cons_df<-oa_cons_df[which.max(oa_cons_df$MeanDH),]
maxdH_worm_oa_df<-worm_oa_df[which.max(worm_oa_df$MeanDH),]
maxdH_all_msy_df<-all_msy_df[which.max(all_msy_df$MeanDH),]
maxdH_efin_msy_df<-efin_msy_df[which.max(efin_msy_df$MeanDH),]
maxdH_worm_msy_df<-worm_msy_df[which.max(worm_msy_df$MeanDH),]

#stretch FracProtect
bau1_df$FracProtect_rescale <- rescale(bau1_df$FracProtect, to = c(bau1_df$FracProtect[1], 1))
oa_cons_df$FracProtect_rescale <- rescale(oa_cons_df$FracProtect, to = c(oa_cons_df$FracProtect[1], 1))
worm_oa_df$FracProtect_rescale <- rescale(worm_oa_df$FracProtect, to = c(worm_oa_df$FracProtect[1], 1))
all_msy_df$FracProtect_rescale <- rescale(all_msy_df$FracProtect, to = c(all_msy_df$FracProtect[1], 1))
efin_msy_df$FracProtect_rescale <- rescale(efin_msy_df$FracProtect, to = c(efin_msy_df$FracProtect[1], 1))
worm_msy_df$FracProtect_rescale <- rescale(worm_msy_df$FracProtect, to = c(worm_msy_df$FracProtect[1], 1))

head(bau1_df)
tail(bau1_df)

max(bau1_df$MeanDH,na.rm=T)
max(oa_cons_df$MeanDH,na.rm=T)
max(worm_oa_df$MeanDH,na.rm=T)
max(all_msy_df$MeanDH,na.rm=T)
max(efin_msy_df$MeanDH,na.rm=T)
max(worm_msy_df$MeanDH,na.rm=T)

(max(oa_cons_df$MeanDH,na.rm=T)-max(bau1_df$MeanDH,na.rm=T))*100/max(bau1_df$MeanDH,na.rm=T)
(max(worm_oa_df$MeanDH,na.rm=T)-max(bau1_df$MeanDH,na.rm=T))*100/max(bau1_df$MeanDH,na.rm=T)

# ggplot()+geom_line(data=results_ex_rate_bau1[seq(1, nrow(results_ex_rate_bau1), 100), ],aes(x=fraction_protected,y=dH,group=ID),col="#F8766D",size=0.5,alpha=0.01)+
#   geom_line(data=bau1_df[seq(1, nrow(bau1_df), 100), ], aes(x=FracProtect_rescale, y=MeanDH),col="#F8766D",size=0.5)

#  geom_ribbon(data=bau1_df[seq(1, nrow(bau1_df), 100), ], aes(x=FracProtect_rescale, y=MeanDH, ymin=MeanDH-Sd, ymax=MeanDH+Sd),alpha=0.25,fill="black")

#  theme(legend.position="none")


benefitplot_combined_juan<-ggplot(data=bau1_df[seq(1, nrow(bau1_df), 100), ], aes(x=FracProtect_rescale, y=MeanDH)) + geom_line(col="black",size=0.4) +
  geom_ribbon(data=bau1_df, aes(x=FracProtect_rescale, y=MeanDH, ymin=MeanDH-Sd, ymax=MeanDH+Sd),alpha=0.25,fill="black") +
  geom_line(data=oa_cons_df[seq(1, nrow(oa_cons_df), 100), ], aes(x=FracProtect_rescale, y=MeanDH), color="magenta",size=0.4,linetype="longdash") + geom_ribbon(data=oa_cons_df, aes(x=FracProtect_rescale, y=MeanDH, ymin=MeanDH-Sd, ymax=MeanDH+Sd),alpha=0.25,fill="magenta") +
  geom_line(data=worm_oa_df[seq(1, nrow(worm_oa_df), 100), ], aes(x=FracProtect_rescale, y=MeanDH), color="brown",size=0.4,linetype = "dotted") + geom_ribbon(data=worm_oa_df, aes(x=FracProtect_rescale, y=MeanDH, ymin=MeanDH-Sd, ymax=MeanDH+Sd),alpha=0.25,fill="brown") +
  geom_line(data=all_msy_df[seq(1, nrow(all_msy_df), 100), ], aes(x=FracProtect_rescale, y=MeanDH), color="black",size=0.4,linetype = "dotdash") + geom_ribbon(data=all_msy_df, aes(x=FracProtect_rescale, y=MeanDH, ymin=MeanDH-Sd, ymax=MeanDH+Sd),alpha=0.25,fill="black")+
  labs(x="% MPA coverage",y="Change in catch (MMT)")+theme_bw()+#ylim(min(BenefitCurve_allmanaged$NetworkResult),max(BenefitCurve_EBvK01fin$NetworkResult))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14))+
  scale_y_continuous(breaks = seq(-60, 30, 20))
benefitplot_combined_juan
#ggplot(BAU1_uncertain,aes(x=fraction_protected,y=dH, group=iter))+geom_line()

#sample plot individual runs
ggplot()+geom_line(data=results_ex_rate_worm_oa[seq(1, nrow(results_ex_rate_worm_oa), 100), ],aes(x=fraction_protected,y=dH,group=ID),col="brown",size=0.5,alpha=0.01)+
  geom_line(data=results_ex_rate_bau1[seq(1, nrow(results_ex_rate_bau1), 100), ],aes(x=fraction_protected,y=dH,group=ID),col="black",size=0.5,alpha=0.01)

#plot here, individual  

#PLOT OTHER DATA---------------
MegaData<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/MegaData.rds")
KprotectedPerCell_Library<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/KprotectedPerCell_Library_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/CleanCoordmegacell_mollweide.rds")
MPA_coord<-readRDS(file="/Users/ren/Documents/CODES/FoodProvision/MPA_coord_mollweide.rds")
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Cleanmegacell_mollweide.rds")
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
MPAsize0<-length(MPAposition)*100/dim(Cleanmegacell)[1]

#proportion of unassessed stocks
MegaData %>% group_by(Manage) %>% summarise(msy=sum(MSYfin), k=sum(Kfin)) %>% mutate(proportion_msy=msy/sum(msy),proportion_k=k/sum(k))

#SI figure, exploitation rate
###Compute spillover---PIXEL-LEVEL spillover 
numcell<-dim(Cleanmegacell)[1]
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
MPAselect0[MPAposition]<-1
K<-MegaData$Kfin #k per species
m<-MegaData$m_fin #mobility per species
r<-MegaData$r_fin
#E<-MegaData$Efin
E<-MegaData$Efin_BAU1
ER<-1-E
ER<-1*(ER>1) + ER*(ER<=1)
max(ER)
min(ER)

MPAselect<-MPAselect0

R<-0
ER_redistribute_0<-data.frame(E0 = 1-(1-ER)^(1/(1-R)))
head(ER_redistribute_0)

R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])
ER_redistribute_2pt4<-data.frame(EBAU = 1-(1-ER)^(1/(1-R)))
head(ER_redistribute_2pt4)

Eevolve<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Eevolve100_BAU1_mollweide_redistribute.rds")
head(Eevolve)
dim(Eevolve)
plot(rowMedians(Eevolve, na.rm=TRUE))#this will give us the same result 


Eevolve_median_0<-data.frame(area=0,median=median(ER_redistribute_0$E0))

Eevolve_median<-data.frame(area=1:1173)
Eevolve_median$median<-rowMedians(Eevolve, na.rm=TRUE)
head(Eevolve_median)

Eevolve_median_fin<-rbind(Eevolve_median_0,Eevolve_median)


xaxis<-(Eevolve_median$area*100*100/dim(CleanCoordmegacell)[1])+MPAsize0
head(xaxis)
tail(xaxis)
medianEplot<-ggplot(Eevolve_median, aes(x = (area*100*100/dim(CleanCoordmegacell)[1])+MPAsize0, y=median)) + geom_line()+labs(x="% MPA coverage", y="Median exploitation rate") + ylim(c(0,1))
medianEplot
head(Eevolve_median)
tail(Eevolve_median)
#round(1173*100*100/dim(CleanCoordmegacell)[1],1)

Eevolve_df<-as.data.frame(t(Eevolve))
head(Eevolve_df)

hist_evolve_1<-ggplot(ER_redistribute_0, aes(x = E0)) + geom_histogram() +
  geom_vline(xintercept = median(Eevolve_df$V1), color = "red", linetype = "dashed")+labs(x="Exploitation rate (E)", y="Number of stocks", title="% MPA coverage = 0%")
hist_evolve_1

hist_evolve_2<-ggplot(ER_redistribute_2pt4, aes(x = EBAU)) + geom_histogram() +
  geom_vline(xintercept = median(Eevolve_df$V1), color = "red", linetype = "dashed")+labs(x="Exploitation rate (E)", y="Number of stocks", title="% MPA coverage = 2.4%")
hist_evolve_2

hist_evolve_3<-ggplot(Eevolve_df, aes(x = V1173)) + geom_histogram()+
  geom_vline(xintercept = median(Eevolve_df$V1173), color = "red", linetype = "dashed")+labs(x="Exploitation rate (E)", y="Number of stocks", title="% MPA coverage = 99.9%")
hist_evolve_3

ggsave(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/FigSI_Eevolve_part1.tiff", medianEplot, width = 10, height = 8, dpi = 600, units = "in")
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/FigSI_Eevolve_part2.tiff", hist_evolve_2, width = 3.1, height = 2.5, dpi = 600, units = "in")
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/FigSI_Eevolve_part3.tiff", hist_evolve_3, width = 3.1, height = 2.5, dpi = 600, units = "in")

FigSI_Eevolve<-grid.arrange(medianEplot,hist_evolve_2,hist_evolve_3, layout_matrix = rbind(c(1,1),c(1,1),c(2,3)))
#FigSI_Eevolve<-grid.arrange(medianEplot,hist_evolve_1,hist_evolve_2,hist_evolve_3, ncol=2)#layout_matrix = rbind(c(1,1),c(1,1),c(2,3)))
FigSI_Eevolve
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/FigSI_Eevolve.png", FigSI_Eevolve, width = 10, height = 8, dpi = 600, units = "in")
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/FigSI_Eevolve.tiff", FigSI_Eevolve, width = 10, height = 8, dpi = 600, units = "in")

#install.packages("matrixStats")
#library("matrixStats")

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
m<-MegaData$m_fin #mobility per species
r<-MegaData$r_fin
E<-MegaData$Efin_BAU1

ER<-1-E
ER<-1*(ER>1) + ER*(ER<=1)
max(ER)
min(ER)

MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])

ER_redistribute<-1-(1-ER)^(1/(1-R))
hbau<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
#hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
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

Plot1b %>% group_by(EEZ) %>% summarize(mean=mean(V1)) #EEZ is 12 times higher than HS

FigS1<-ggplot(Plot1b,aes(x=EEZ,y=V1,fill=EEZ))+
  geom_bar(stat = "summary", fun.y = "mean",fill="grey")+labs(x="",y=expression(paste("Average ",Delta,"H (MT)")))+
  theme_bw()+theme(axis.text=element_text(size=35),axis.title=element_text(size=35),legend.position = "none")
FigS1
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/FigS1.png", FigS1,dpi=300,width = 8, height = 8, units = "in")#resolution not great

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
max(Fig1Data$deltaH)
min(Fig1Data$deltaH)
20000^(1/2.5)

saveRDS(Fig1Data, file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig1_PixelLeveDH_raw.rds")

#options("scipen"=100, "digits"=1)
root<-3
pixeldHplot<-Fig1Data %>% 
  mutate(tmp = abs(deltaH)^(1/root),
         tmp_2 = ifelse(deltaH < 0, -tmp, tmp)) %>% 
  ggplot()+
  geom_raster(aes(lon, lat, fill = tmp_2))+
  scale_fill_gradient2(labels = function(x){x^root},
                       low = "red", mid = "white",
                       #high = "dodgerblue", space = "Lab",
                       high = "#00539CFF", space = "Lab",
                       name=expression(paste(Delta, "H (MT)")),midpoint = 0,limits=c(-20,30))+
  labs(title = "", fill = "", y = "", x = "")+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="cyan")+  #"#EEA47FFF"
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent'))
pixeldHplot



ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig1_pixeldHplot.tiff", pixeldHplot, width = 22, height = 18, dpi = 600, units = "cm")

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
m<-MegaData$m_fin #mobility per species
r<-MegaData$r_fin
E<-MegaData$Efin_BAU1

ER<-1-E
ER<-1*(ER>1) + ER*(ER<=1)
max(ER)
min(ER)

MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])

ER_redistribute<-1-(1-ER)^(1/(1-R))
hbau<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
#hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
HBAU

nmax<-floor(length(celltoiterate)/1000)
PerSpDeltaH<-matrix(nrow=nmax,ncol=1342)

NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_BAU1_mollweide_redistribute.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_BAU1_mollweide_redistribute.rds")
head(MegaData)



#load uncertainty analysis file and add a ribbon plot
Uncertainty_BAU1<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvision2019/UncertaintyAnalysis/Uncertainty_BAU1_10.rds")
Mean_NetworkResult<-rowMeans(Uncertainty_BAU1, na.rm = FALSE)
Sd_NetworkResult<-rowSds(as.matrix(Uncertainty_BAU1), na.rm=TRUE)
Uncertainty_BAU1$ID <- seq.int(nrow(Uncertainty_BAU1))
head(Uncertainty_BAU1)
Uncertainty_BAU1_melt <- melt(Uncertainty_BAU1, id="ID")
head(Uncertainty_BAU1_melt)
#ggplot(Uncertainty_BAU1_melt, aes(x=ID, y=value, group=variable))+geom_line()

testplot<-data.frame(Mean=Mean_NetworkResult)
testplot$ID <- seq.int(nrow(testplot))
testplot$Sd <- Sd_NetworkResult
max(testplot$Mean)
ggplot(testplot, aes(x=ID, y=Mean, ymin=Mean-Sd, ymax=Mean+Sd)) + 
  geom_line() + 
  geom_ribbon(alpha=0.5)


#this is for generating how much food will be generated from managed stocks vs unmanaged.
PerSpeciesDH<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PerSpDeltaH100_BAU1_mollweide_redistribute.rds")
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

max(BenefitCurve3$NetworkResult)

#check BenefitCurve3, extract the position for 5% additional MPAs
head(BenefitCurve3)
dim(BenefitCurve3)
BenefitCurve3$MPA[1]+5 #5% more MPA
BenefitCurve3 %>% filter(MPA<=BenefitCurve3$MPA[1]+5) #61st entry
BenefitCurve3$NetworkResult[61]

#now, check the PerSpeciesDH data
dim(PerSpeciesDH) #1173, 1338

#plot(PerSpeciesDH[,7])

MegaData$dH_five_percent<-PerSpeciesDH[61,]
MegaData$dHoverK<-MegaData$dH_five_percent/MegaData$Kfin
head(MegaData)

#Add geog range in MegaData
#colSums((Cleanmegacell>0)*3000)#in KM
MegaData$geograngekm<-colSums((Cleanmegacell>0)*3000)
MegaData$Efin_over_Emsy<-(1-MegaData$Efin)/(1-MegaData$Emsy)
head(MegaData)

Top15dH<-MegaData %>% select(SpeciesID,Manage,stockid,SciName,Kfin,r_fin,Efin_over_Emsy,geograngekm,dH_five_percent)
#Top10dHoverK<-MegaData %>% select(SpeciesID,Manage,stockid,SciName,Kfin,r_fin,geograngekm,dHoverK)

Top15dHplot<-Top15dH[order(-Top15dH$dH_five_percent),] %>% slice(1:15)
#Top10dHoverKplot<-Top10dHoverK[order(-Top10dHoverK$dHoverK),] %>% slice(1:10)
#Top10dHplot$dH_five_percent[1]

plotTop15dH<-ggplot(Top15dHplot, aes(x = reorder(stockid, dH_five_percent), y = dH_five_percent)) +
  geom_bar(fill="coral",stat = "identity") +
  coord_flip() +
  geom_text(aes(label = SciName,size=14), nudge_y = 0.5e6, color = "black")+
  labs(y = expression(paste(Delta*H~(MT))), x=expression(Fish~stock),title="+5% MPA")+ ylim(0, Top15dHplot$dH_five_percent[1]+1e6)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")
plotTop15dH

png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/Additional5percent.png", width = 10, height = 10,units = 'in', res = 300) 
plotTop15dH
dev.off()

rKplotTop15<-ggplot(MegaData, aes(y=Kfin,x=r_fin))+geom_point()+geom_point(data=Top15dHplot,aes(x=r_fin,y=Kfin),col="coral")+
  labs(x=expression(r),y=expression(K))+  theme(axis.text=element_text(size=14),
                            axis.title=element_text(size=16,face="bold"),
                            legend.position="none")
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/rKplotTop15.png", width = 6, height = 6,units = 'in', res = 300) 
rKplotTop15
dev.off()

rangeEplotTop15<-ggplot(MegaData, aes(y=Efin_over_Emsy,x=geograngekm))+geom_point()+geom_point(data=Top10dHplot,aes(x=geograngekm,y=Efin_over_Emsy),col="coral")+
  labs(x=expression(paste(Stock~range~(km^2))),y=expression(paste(E/E[MSY])))+  theme(axis.text=element_text(size=14),
                            axis.title=element_text(size=16,face="bold"),
                            legend.position="none")
png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/rangeEplotTop15.png", width = 6, height = 6,units = 'in', res = 300) 
rangeEplotTop15
dev.off()

#CompositeTop15<-grid.arrange(plotTop10dH,rKplotTop20,rangeEplotTop20, layout_matrix = rbind(c(1,1),c(1,1),c(2,3)))
CompositeTop15<-ggarrange(plotTop15dH,                                            
          ggarrange(rKplotTop15,rangeEplotTop20, ncol = 2, labels = c("B", "C")), 
          nrow = 2, labels = "A", heights=c(2,1))
CompositeTop15

ggsave(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/CompositeTop15.tiff", CompositeTop15, width = 26, height = 26, dpi = 300, units = "cm")


#ADD the plot of R and K// or fisheries status and M??


# plotMegaDataKplot<-ggplot(Top10dHplot, aes(x = reorder(SciName, dH_five_percent), y = dH_five_percent)) +
#   geom_bar(fill="steelblue",stat = "identity") +
#   coord_flip() +
#   #geom_text(aes(label = SciName,size=14), nudge_y = 8e6, color = "black")+
#   labs(y = "dH (MT)", x="Species")+ #ylim(0, 4.7e7)+
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=16,face="bold"),
#         legend.position="none")
# plotMegaDataKplot
# 
# png(file="/Users/ren/Documents/CODES/FoodProvision/SupplementInfo/KperSpecies.png", width = 10, height = 10,units = 'in', res = 300) 
# plotMegaDataKplot
# dev.off()

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

ER<-1-E
ER<-1*(ER>1) + ER*(ER<=1)
max(ER)
min(ER)

numcell<-dim(Cleanmegacell)[1]
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
MPAselect0[MPAposition]<-1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])

ER_redistribute<-1-(1-ER)^(1/(1-R))
hbau<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
#hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)#sum((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))), na.rm=T)
HBAU

#Harvest BAU with no MPA
sum((1-E)*((r+E-1)/r)*K)
#current MPA benefits
HBAU-sum((1-E)*((r+E-1)/r)*K)

NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_OAconstant_mollweide_redistribute.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_OAconstant_mollweide_redistribute.rds")
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

max(BenefitCurve_BAU2$NetworkResult)

###Collapsed (0.1k) assumption
E<-MegaData$EBvK01fin

ER<-1-E
ER<-1*(ER>1) + ER*(ER<=1)
max(ER)
min(ER)

numcell<-dim(Cleanmegacell)[1]
MPAselect0<-matrix(0, nrow=numcell, ncol=1)
MPAselect0[MPAposition]<-1
MPAselect<-MPAselect0
R<-rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])

ER_redistribute<-1-(1-ER)^(1/(1-R))
hbau<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
#hbau<-na.omit((1-E)*((m*K*(1-R))/(R-(E*R)+m))*(1-(((1-E)*(1-R)*m)/((R-(E*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)
HBAU

#Harvest BAU with no MPA
sum((1-E)*((r+E-1)/r)*K)
#current MPA benefits
HBAU-sum((1-E)*((r+E-1)/r)*K)

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

max(BenefitCurve_EBvK01fin$NetworkResult)

benefitplot_combined<-ggplot(BenefitCurve3, aes(MPA, NetworkResult)) +geom_line(col="#00A087FF",size=0.5)+ 
  labs(x="% MPA coverage",y="Change in catch (MMT)")+theme_bw()+ylim(min(BenefitCurve_allmanaged$NetworkResult),max(BenefitCurve_EBvK01fin$NetworkResult))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14))+
  geom_line(data=BenefitCurve_allmanaged, aes(MPA, NetworkResult),col="red",size=0.5,linetype = "dotted")+theme(legend.position = "none")+
  geom_line(data=BenefitCurve_EBvK01fin, aes(MPA, NetworkResult),col="black",size=0.5,linetype = "dotdash")+theme(legend.position = "none")+
  geom_line(data=BenefitCurve_BAU2, aes(MPA, NetworkResult),col="black",size=0.5,linetype = "dashed")+theme(legend.position = "none")
benefitplot_combined
#ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig2Benefit.png", benefitplot_combined,dpi=600)

benefitplot_main<-ggplot(BenefitCurve3, aes(MPA, NetworkResult)) +geom_line(col="#00A087FF",size=2)+ 
  labs(x="% ocean protected",y="Change in catch (MMT)")+theme_bw()+ylim(min(BenefitCurve_allmanaged$NetworkResult),max(BenefitCurve_EBvK01fin$NetworkResult))+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=30))
benefitplot_main
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/benefitplot_main.png", benefitplot_main,dpi=300)


benefitplot_top3<-ggplot(BenefitCurve3, aes(MPA, NetworkResult)) +geom_line(col="#00A087FF",size=02)+ 
  labs(x="% ocean protected",y="Change in catch (MMT)")+theme_bw()+ylim(min(BenefitCurve_allmanaged$NetworkResult),max(BenefitCurve_EBvK01fin$NetworkResult))+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=30))+
  geom_line(data=BenefitCurve_EBvK01fin, aes(MPA, NetworkResult),col="black",size=2,linetype = "dotdash")+theme(legend.position = "none")+
  geom_line(data=BenefitCurve_BAU2, aes(MPA, NetworkResult),col="black",size=2,linetype = "dashed")+theme(legend.position = "none")
benefitplot_top3
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/benefitplot_top3.png", benefitplot_top3,dpi=300)

#Fig 2a.
NetworkResult<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_BAU1_mollweide_redistribute_10.rds")
PriorityAreas<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/PriorityAreas100_BAU1_mollweide_redistribute_10.rds")
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


cumsumrankplot<-ggplot(cumsumrank[seq(1, nrow(cumsumrank), 100), ])+geom_line(aes(x=id, y=EEZcs),col="black",size=1,linetype = "dotted") + geom_line(aes(x=id, y=HScs),col="black",size=1,linetype = "longdash")+labs(x="% MPA coverage",y="Protected/Available pixels")+
  theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=14))+
  annotate("text", x = 25, y = 0.50, label = "EEZs",size=4)+annotate("text", x = 50, y = 0.25, label = "High Seas",size=4)
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
  scale_fill_gradientn(colours = c("forestgreen", "white", "orange"),#c("#00539CFF", "white", "red"), 
                       limits=c(0,100), values = scales::rescale(c(min(ShortCoord_Sort$rank), InflectMPA, max(ShortCoord_Sort$rank))),name="Protection sequence")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position=c(0.63, 0.05), legend.direction = "horizontal")+ #"bottom
  geom_raster()+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="cyan")+  #"#EEA47FFF"
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent'))
MPAcoverage



ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/MPAcoverage.png", MPAcoverage, width = 10, height = 8, dpi = 600, units = "in")#resolution not great


#library(ggpubr)
p1<-MPAcoverage
p2<-benefitplot_combined_juan
p3<-cumsumrankplot
# fig2<-ggarrange(p1,                                                 # First row with scatter plot
#                 ggarrange(p2, p3, ncol = 2), #labels = c("B", "C")), # Second row with box and dot plots
#                 nrow = 2)#, 
#                 #labels = "A")                                        # Labels of the scatter plot
#                 #font.label = list(size = 24)
#                 #) 
#fig2<-grid.arrange(p1,p2,p3, layout_matrix = rbind(c(1,1),c(1,1),c(2,3)))
fig2<-grid.arrange(p1,p2,p3, layout_matrix = rbind(c(1,1),c(2,3)))
fig2
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig2.tiff", fig2, width = 22, height = 18, dpi = 600, units = "cm")

##------------PLOT FIGURE 3-------------
head(MegaData)
max(MegaData$Efin_BAU1)
min(MegaData$Efin_BAU1)
ER_ratio_EBvK01fin<-weighted.mean((1-MegaData$EBvK01fin)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_EBvK01_msy<-weighted.mean((1-MegaData$EBvK01_msy)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_OA_constant<-weighted.mean((1-MegaData$Efin)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_Efin_msy<-weighted.mean((1-MegaData$Efin_msy)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_Efin_BAU1<-weighted.mean((1-MegaData$Efin_BAU1)/(1-MegaData$Emsy), MegaData$MSYfin)
#ER_ratio_OAhalf_msy<-weighted.mean((1-MegaData$Efinhalf_msy)/(1-MegaData$Emsy), MegaData$MSYfin)
ER_ratio_EBvK01fin
ER_ratio_EBvK01_msy
ER_ratio_Efin_BAU1
ER_ratio_OA_constant
ER_ratio_Efin_msy
#ER_ratio_OA_msy
#ER_ratio_OAhalf_msy

dH_EBvK01fin<-maxdH_worm_oa_df$MeanDH#max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_EBvK01fin_mollweide.rds"))/1000000
dH_EBvK01_msy<-maxdH_worm_msy_df$MeanDH#max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_EBvK01_msy_mollweide.rds"))/1000000
dH_OA_constant<-maxdH_oa_cons_df$MeanDH#max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_OAconstant_mollweide_redistribute.rds"))/1000000
dH_Efin_msy<-maxdH_efin_msy_df$MeanDH#max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_Efin_msy_mollweide.rds"))/1000000
dH_Efin_BAU1<-maxdH_bau1_df$MeanDH#max(readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/PaperFigures/NetworkResult100_BAU1_mollweide_redistribute.rds"))/1000000

sd_bau1<-maxdH_bau1_df$Sd
sd_oa_cons<-maxdH_oa_cons_df$Sd
sd_efin_msy<-maxdH_efin_msy_df$Sd
sd_worm_oa<-maxdH_worm_oa_df$Sd
sd_worm_msy<-maxdH_worm_msy_df$Sd

dH_OA_constant/dH_Efin_BAU1
dH_EBvK01fin/dH_Efin_BAU1

#NetworkResult100_EBvK01_msy_mollweide

require(ggplot2)
require(ggrepel)
testdata <- data.frame(x = c(ER_ratio_EBvK01fin,ER_ratio_OA_constant,ER_ratio_Efin_BAU1,1),
                       Policy = c("collapse", "BAU (all stocks)", "BAU (conservation concern)", "MSY"),  # Create example data
                       y = c(dH_EBvK01fin, dH_OA_constant, dH_Efin_BAU1, 0),
                       Sd = c(sd_worm_oa,sd_oa_cons,sd_bau1,0))
testdata$Policy <- as.factor(testdata$Policy)

dodge <- position_dodge(1)
Fig3PNAS<-ggplot(testdata, aes(x = x, y = y, label = Policy)) + geom_line(linetype = "dotted")+
  geom_point(
    #mapping = aes(color = "#EE000099"),
    color = "#EE000099",
    position = dodge,
    size = 5,
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin=y-Sd, ymax=y+Sd))+
  geom_text_repel(
    #nudge_x       = 0.1,
    #nudge_y = 0.2,
    #segment.size  = 0.4,
    size=4,
    #segment.color = "grey50",
    direction     = "y",
    vjust         = -1
  ) +
  labs(y="Maximum MPA benefit (MMT)") +
  ylim(-2,(dH_EBvK01fin+3)) + 
  xlab(bquote('E/E'[MSY]))+
  theme_minimal()+
  theme(text = element_text(size=12))+ 
  scale_x_reverse()+  xlim((ER_ratio_EBvK01fin+0.06),.81) +annotate(geom="text", x=1.25, y=-1.5, label=expression("Poorly managed fisheries" %->% "Well-managed fisheries"),size=4)
Fig3PNAS
ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig3.tiff", Fig3PNAS,width = 11, height = 11, dpi = 600, units = "cm")
# testdata <- data.frame(x = c(ER_ratio_EBvK01fin,ER_ratio_EBvK01_msy,ER_ratio_OA_constant,ER_ratio_Efin_msy,ER_ratio_Efin_BAU1,1),
#                        Policy = c("collapse","", "BAU (all stocks)", "", "BAU (conservation concern)", "MSY"),  # Create example data
#                        y = c(dH_EBvK01fin, dH_EBvK01_msy,dH_OA_constant, dH_Efin_msy, dH_Efin_BAU1, 0),
#                        Sd = c(sd_worm_oa,sd_worm_msy,sd_oa_cons,sd_efin_msy,sd_bau1,0))
# testdata$Policy <- as.factor(testdata$Policy)
# 
# dodge <- position_dodge(1)
# Fig3PNAS<-ggplot(testdata, aes(x = x, y = y, label = Policy)) + geom_line(linetype = "dotted")+
#   geom_point(
#     #mapping = aes(color = "#EE000099"),
#     color = "#EE000099",
#     position = dodge,
#     size = 5,
#     alpha = 0.5
#   ) +
#   geom_errorbar(aes(ymin=y-Sd, ymax=y+Sd))+
#   geom_text_repel(
#     nudge_x       = 0.1,
#     segment.size  = 0.2,
#     size=7,
#     segment.color = "grey50",
#     direction     = "y",
#     hjust         = 0
#   ) +
#   labs(y="Maximum MPA benefit (MMT)") +
#   ylim(-2,(dH_EBvK01fin+2)) + 
#   xlab(bquote('E/E'[msy]))+
#   theme_minimal()+
#   theme(text = element_text(size=20))+ 
#   scale_x_reverse()+  xlim((ER_ratio_EBvK01fin+0.05),.76) +annotate(geom="text", x=1.25, y=-1.5, label=expression("Poorly managed fisheries" %->% "Well-managed fisheries"),size=7)
# Fig3PNAS
# ggsave(file="/Users/ren/Documents/CODES/FoodProvision/PaperFigures/Fig3.tiff", Fig3PNAS,width = 10, height = 8, dpi = 300, units = "in")
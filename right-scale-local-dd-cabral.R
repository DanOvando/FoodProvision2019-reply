#copied and then modified to allow reproducibility from FoodGlobalAnalysis_EffortRedistribute_clean.R supplied by Ren through
# https://ucsb.box.com/s/mj6gnsh111c0nrdw1yt4nutvkts76zi4

library(foreach)
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
library(here)
library(scales)
library(tidyverse)
library(countrycode)
library(patchwork)
# library(marlin)
library(devtools)
#install_github("ropensci/ramlegacy")
library(ramlegacy)
library(sf)
library(smoothr)
library(rnaturalearth)
library(furrr)
library(Rcpp)
#idea: create correlation matrix of habitat by species and scale up effort via technical interaction in proportion to correlation 

warning("Each 'run_` takes about 10 hours on 12 cores, be prepared to wait if running everything. Results are stored via github since they are small so you can set every run_ to FALSE. run_teleportation is the only exception, only takes a minute or two.")

Rcpp::sourceCpp(here('src',"sim_mpa.cpp"))

rename <- dplyr::rename

divide_stocks <- FALSE

local_dd <- 1

mpa_sim_years <- 50 # number of years to run MPA simulation

run_cabral_et_al <- TRUE

get_fao_data <- TRUE

results_name <- "v0.5"

results_path <- here("results", results_name)

if (!dir.exists(results_path)){
  dir.create(results_path, recursive = TRUE)
}

if (!dir.exists("data")){
  
  download.file(
    "https://www.dropbox.com/s/vrewj7ryqbbs0a4/food-provision-data.zip?dl=1",
    destfile = here("tmp.zip"),
    mode = "wb"
  )
  
  unzip(here("tmp.zip")) # unzip = 'unzip' needed for windows
  
  # file.rename("food-provision-data","data")
  
  file.remove("tmp.zip")
  
  if (dir.exists("__MACOSX")){
    unlink("__MACOSX", recursive = TRUE)
  }
  
}

if (get_fao_data | !dir.exists(here("data", "fao"))) {
  if (!dir.exists(here("data", "fao"))) {
    dir.create(here("data", "fao"))
    
    download.file(
      "http://www.fao.org/fishery/static/Data/Capture_2019.1.0.zip",
      destfile = here("data", "fao.zip"),
      mode = "wb"
    )
    
    unzip(here("data", "fao.zip"), exdir = here("data", "fao"))
    
    file.remove(here("data", "fao.zip"))
    
    download.file(
      "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip",
      destfile = here("data", "asfis.zip"),
      mode = "wb"
    )
    
    unzip(here("data", "asfis.zip"), exdir = here("data", "fao"))
    
    file.remove(here("data", "asfis.zip"))
    
    
  }
  
  
  asfis <-
    read_delim(here("data", "fao", "ASFIS_sp_2020.txt"), delim = ",") %>%
    janitor::clean_names() %>%
    rename(isscaap_code = isscaap) %>%
    select(isscaap_code, scientific_name, taxocode) %>%
    unique()
  
  # major issue with NEIs here. There is no database that has both isscaap group and isscaap code, so you need
  # to do a complicated merge based on scientific name.
  
  fao_capture <-
    read_csv(here("data", "fao", "TS_FI_CAPTURE.csv")) %>%
    janitor::clean_names()
  
  sp_groups <-
    read_csv(here("data", "fao", "CL_FI_SPECIES_GROUPS.csv")) %>%
    janitor::clean_names() %>%
    select(x3alpha_code:identifier, contains("_en"), author:cpc_group) %>%
    rename(species_name_en = name_en) %>%
    left_join(asfis, by = c("taxonomic_code" = "taxocode"))
  
  # sp_groups %>%
  #   group_by(x3alpha_code) %>%
  #   summarise(ni = n_distinct(isscaap_group)) %>%
  #   arrange(desc(ni))
  
  country_groups <-
    read_csv(here("data", "fao", "CL_FI_COUNTRY_GROUPS.csv")) %>%
    janitor::clean_names() %>%
    mutate(un_code = as.numeric(un_code)) %>%
    select(un_code:iso3_code, contains("_en")) %>%
    rename(country_name_en = name_en,
           country_official_name_en = official_name_en)
  
  fao_areas <-
    read_csv(here("data", "fao", "CL_FI_WATERAREA_GROUPS.csv")) %>%
    janitor::clean_names() %>%
    mutate(fishing_area = as.numeric(code)) %>%
    select(fishing_area, contains("_en"), contains("group"))
  
  fao_capture <- fao_capture %>%
    left_join(sp_groups, by = c("species" = "x3alpha_code"))
  
  fao_capture <- fao_capture %>%
    left_join(country_groups, by = c("country" = "un_code")) %>%
    left_join(fao_areas, by = "fishing_area")
  
  fao_capture$fao_country_name <-
    countrycode::countrycode(fao_capture$country_name_en, "country.name", "un.name.en")
  
  fao_capture <- fao_capture %>%
    mutate(country = case_when(
      is.na(fao_country_name) ~ country_name_en,
      TRUE ~ fao_country_name
    )) %>%
    mutate(continent = countrycode::countrycode(country, "country.name", "continent"))
  
  fao_capture <- fao_capture %>%
    rename(
      isscaap_number = isscaap_code,
      common_name = species_name_en,
      capture = quantity,
      capture_units = unit,
      fao_area_code = fishing_area,
      fao_area = name_en
    ) %>%
    mutate(fao_stock = paste(common_name, country, fao_area, sep = '_'))
  
  fao_capture <- fao_capture %>%
    group_by(fao_stock) %>%
    nest() %>%
    ungroup() %>%
    mutate(id = 1:nrow(.)) %>%
    unnest(cols = data)
  
  fao_capture <- fao_capture %>%
    select(id, fao_stock, everything())
  
  fao <- fao_capture %>%
    filter(capture_units == "t",
           isscaap_number < 67)
  
  assign("fao", fao, envir = .GlobalEnv)
  
  
  fao_stock_lookup <- fao %>%
    select(scientific_name,
           common_name,
           country,
           fao_area,
           fao_area_code) %>%
    unique()
  
  assign("fao_stock_lookup", fao_stock_lookup, envir = .GlobalEnv)
  
  
  fao_species <- fao %>%
    select(scientific_name, common_name, isscaap_group, isscaap_number) %>%
    unique()
  
  assign("fao_species", fao_species, envir = .GlobalEnv)
  
  fao_genus <-
    str_split(fao_species$scientific_name, ' ', simplify = TRUE)[, 1]
  
  fao_genus <-  fao_species %>%
    mutate(genus = fao_genus) %>%
    group_by(genus, isscaap_group) %>%
    count() %>%
    group_by(genus) %>%
    filter(n == max(n)) %>%
    select(-n) %>%
    ungroup()
  
  write_rds(fao_capture, file = here("data", "fao", "fao-capture.rds"))
  
  
} else {
  fao_capture <-
    read_rds(file = here("data", "fao", "fao-capture.rds"))
  
  
}

# get FAO shapefile

if (!dir.exists(here("data", "FAO_AREAS_NOCOASTLINE"))) {
  download.file(url = "http://www.fao.org/figis/geoserver/area/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=area:FAO_AREAS_NOCOASTLINE&outputFormat=SHAPE-ZIP",
                destfile = here("data", "FAO_AREAS_NOCOASTLINE.zip"),
                mode = "wb"
  )
  
  unzip(
    here("data", "FAO_AREAS_NOCOASTLINE.zip"),
    exdir = here("data", "FAO_AREAS_NOCOASTLINE")
  )
  
}


fao_areas <- sf::st_read(here('data', "FAO_AREAS_NOCOASTLINE")) %>%
  janitor::clean_names()

fao_areas <- fao_areas %>%
  group_by(f_area) %>%
  nest() %>%
  mutate(geometry = map(data, sf::st_union)) %>%
  select(-data)


fao_areas = fao_areas %>%
  unnest(cols = geometry) %>%
  ungroup() %>%
  sf::st_as_sf() %>%
  # sf::st_simplify() %>%
  mutate(fao_area_code = as.numeric(f_area)) #%>% 
# st_transform(crs = "+proj=moll")
# 
ggplot(fao_areas) +
  geom_sf()

##SELECT SCENARIO --- there are four scenarios
scenario<-"BAU1"
#scenario<-"OAconstant"
#scenario<-"EBvK01fin"
#scenario<-"all managed"


upsides <- read_csv(here("data","upsides","ProjectionData.csv")) %>% 
  janitor::clean_names()

# upsides %>%
#   filter(dbase == "RAM", year > 2016,
#          policy == "BAU",
#          scenario == "Con. Concern") %>%
#   ggplot(aes(year, pmin(4,fv_fmsy), color = scenario, group = interaction(scenario, id_orig)))+
#   geom_line(alpha = 0.5) +
#   facet_wrap(~policy,scales = "free_y")

upsides_bau <- upsides %>% 
  filter(
    policy == "BAU", 
    scenario == "Con. Concern") %>% 
  filter(year == max(year)) %>%
  dplyr::rename(bvbmsy_upsides = bv_bmsy,
                fvfmsy_upsides = fv_fmsy) %>% 
  mutate(manage = ifelse(dbase == "FAO",0, 1))

# upsides_bau %>% 
#   filter(manage == 1) %>% 
#   ggplot(aes(fvfmsy_upsides)) + 
#   geom_histogram()

# sum(upsides_bau$catch) # costello et al. 2016 58.2

upsides_bau$check <- (upsides_bau$bvbmsy_upsides * upsides_bau$fvfmsy_upsides) * upsides_bau$msy


#Load MOLLWEIDE projected data
MegaData<-readRDS(file = here("data","MegaData.rds"))
Cleanmegacell<-readRDS(file = here("data","Cleanmegacell_mollweide.rds"))
CleanCoordmegacell<-readRDS(file = here("data","CleanCoordmegacell_mollweide.rds"))
KprotectedPerCell_Library<-readRDS(file = here("data","KprotectedPerCell_Library_mollweide.rds"))
MPA_coord<-readRDS(file= here("data","MPA_coord_mollweide.rds")) #this is my code
land_shp_moll<-readRDS(file = here("data","land_shp_moll.rds"))
head(MPA_coord)
dim(MPA_coord)

eezs <-
  sf::read_sf(here("data", "World_EEZ_v11_20191118_LR", "eez_v11_lowres.shp")) %>%
  mutate(iso3_code = countrycode(SOVEREIGN1, "country.name",  "iso3c")) %>% 
  mutate(iso3c_name = countrycode(iso3_code, "iso3c",  "country.name")) %>%
  filter(!is.na(iso3_code)) %>%
  sf::st_transform(sf::st_crs(land_shp_moll))

#get MPA positions
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)
sum(CleanCoordmegacell_MPA$MPA,na.rm=T)



# assign upsides to cabral map --------------------------------------------

upsides_bau$iso3_country_code <- 
  countrycode::countrycode(upsides_bau$country, "country.name", "iso3c")

upsides_bau$iso3_country_name <- 
  countrycode::countrycode(upsides_bau$iso3_country_code, "iso3c", "country.name")


# write a function to take the upsides, and then for each row, generate
# megadata and the updated KprotectedPerCell_Library


ua_megadata <- MegaData %>% 
  filter(Manage == 0)

ua_upsides_bau <- upsides_bau %>% 
  filter(sci_name %in% unique(ua_megadata$SciName), dbase == "FAO",id_level == "Species")

# stock <- ua_upsides_bau[ua_upsides_bau$sci_name ==,]


tmp <- CleanCoordmegacell %>% 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = sf::st_crs(land_shp_moll)) 


divide_stock <- function(stock, MegaData, KprotectedPerCell_Library, tmp){


upsides_country <- stock$country[1]
  
taxa <- stock$sci_name[1]

tmp_eez_code <- stock$iso3_country_code[1]

tmp_fao_area_code <- stock$region_fao[1]

tmp_mega <- ua_megadata %>% 
  filter(SciName == taxa)

# generate map:
# pull out the row for that taxa from Kprotected....
# convert to shapefile and mask with EEZ and FAO region
# set habitat to zero in all cells outside of intersected area
# return row

global_occurance <- as.numeric(KprotectedPerCell_Library[which(MegaData$SciName == taxa & MegaData$Manage == 0)[1],])

tmp$habitat <- global_occurance

tmp_eez <- eezs %>% 
  filter(iso3_code == tmp_eez_code)

tmp_fao <- fao_areas %>% 
  filter(fao_area_code == tmp_fao_area_code) %>% 
  sf::st_transform(sf::st_crs(land_shp_moll))


tmp_layer <- st_intersects(tmp,tmp_eez, sparse = FALSE)

tmp_eez_layer <- apply(tmp_layer, 1, any)

tmp_layer <- st_intersects(tmp,tmp_fao, sparse = FALSE)

tmp_fao_layer <- apply(tmp_layer, 1, any)

if (upsides_country == "Multinational"){
  
  mask <- tmp_fao_layer
  
} else {
  mask <- tmp_eez_layer & tmp_fao_layer
  
  if (all(mask == FALSE)){
    mask <- tmp_fao_layer
  }
  
}

stock_occurance <- global_occurance

stock_occurance[!mask] <- 0

if (sum(stock_occurance) == 0){
  stock_occurance[mask] <- 1
}

# test <- t(as.matrix(stock_occurance))

# tmp %>%
#   mutate(init = tmp_layer & tmp_fao_layer,
#          hab = stock_occurance) %>%
#   ggplot(aes(color = hab))+
#   geom_sf()

# stock_occurance <- t(as.matrix(stock_occurance / sum(stock_occurance, na.rm = TRUE)))

stock_occurance <- (stock_occurance / sum(stock_occurance, na.rm = TRUE))

if (any(is.na(stock_occurance))){
  stop()
}
# OK that generates a habitat layer for that specific stock

# now update "MegaData" with the correct 
# MSYfin
# Efin_BAU1
# The BAU exploitation rate for the unassessed stocks appears to be set such that B/B~MSY~ BAU equals the MSY weighted mean B/B~MSY~ BAU from Costello et al. 2016

tmp_mega$MSYfin <- stock$msy

tmp_mega$Kfin <- stock$k

tmp_mega$r <- stock$msy * 4 / stock$k # hacky conversion from PT to Schaefer

f_fmsy_bau <- 2 - min(2,stock$bvbmsy_upsides)

tmp_mega$Efin_BAU1 <- 1 - f_fmsy_bau * (tmp_mega$r / 2)

# (1 - tmp_mega$Efin_BAU1) / (tmp_mega$r / 2)

out <- list(tmp_mega = tmp_mega, temp_occurance =stock_occurance )
}
if (divide_stocks | (!file.exists(here("data","divided-stocks.rds")))){
  
message("dividings stocks, this can take a while, like up to an hour")

  divided_stocks <- ua_upsides_bau %>% 
  group_by(id_orig) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(ds = map(data,divide_stock, MegaData = MegaData, KprotectedPerCell_Library = KprotectedPerCell_Library, tmp = tmp))

write_rds(divided_stocks,here("data","divided-stocks.rds"))

} else {
  
  divided_stocks <- read_rds(here("data","divided-stocks.rds"))
  
}

ram_stocks <- MegaData %>% 
  filter(Manage == 1)

ram_range <- KprotectedPerCell_Library[which(MegaData$Manage == 1),]


# create unassessed

divided_megadata <- map_df(divided_stocks$ds, "tmp_mega")

f_fmsy <- (1 - divided_megadata$Efin_BAU1) / (divided_megadata$r / 2)


divided_k <- matrix(NA, nrow = nrow(divided_megadata), ncol = ncol(KprotectedPerCell_Library))

for (i in 1:nrow(divided_stocks)){
  if (any(is.na( as.numeric(divided_stocks$ds[[i]]$temp_occurance)))){
    stop()
  }
  divided_k[i,] <- as.numeric(divided_stocks$ds[[i]]$temp_occurance) # much faster this way
  
}

# divided_k <- map(divided_stocks$ds, "temp_occurance") %>% 
#   map_df(as.data.frame)


MegaData <- ram_stocks %>% 
  bind_rows(divided_megadata)

KprotectedPerCell_Library <- ram_range %>% 
  as.matrix() %>% 
  rbind(divided_k)

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
R<- pmax(1e-6,pmin(0.999,rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE], na.rm = TRUE)))
ER_redistribute<- pmin(0.999,1-(1-ER)^(1/(1-R)))


b_outside_bau <-
  ((m * K * (1 - R)) / (ER_redistribute * R + m)) * (1 - (ER_redistribute * (1 - R) * m) /
                                                       ((ER_redistribute * R + m) * r)) 

hbau<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
hbau<-hbau*(hbau>0)
HBAU<-sum(hbau)
HBAU

PICKSIZE<-100 #number of MPA sites selected

nmax<-floor(length(celltoiterate)/PICKSIZE)
nmax #this is the number of iterations needed for PICKSIZE at a time!

Eevolve<-matrix(nrow=nmax,ncol=dim(MegaData)[1])
# k_per_cell <- t(as.datKprotectedPerCell_Library)

bau <- sim_mpa(r = r, k = K, m = m, u = ER_redistribute,p_mpa = R,local_dd = local_dd,years = mpa_sim_years)

sum(bau) / HBAU

# sum(bau$outside_b) /  sum(b_outside_bau)

HBAU <- sum(bau)


if (run_cabral_et_al){
  
  cores<-detectCores() - 2
  registerDoParallel(cores)
  for (i in 1:nmax){ 
    MPAselectPrev<-rowSums(KprotectedPerCell_Library[,which(MPAselect0==1),drop=FALSE], na.rm = TRUE)
    a <- Sys.time()
    result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {
      MPAselect<-MPAselect0
      MPAselect[celltoiterate[iter]]<-1
      R<-pmax(1e-6,pmin(0.999,MPAselectPrev+KprotectedPerCell_Library[,celltoiterate[iter]]))
      ER_redistribute<- pmin(.999,1-(1-ER)^(1/(1-R)))
      
      results <- sim_mpa(r = r, k = K, m = m, u = ER_redistribute,p_mpa = R,local_dd = local_dd,years = mpa_sim_years)
      # wtf <- is.na(results)
      
      # R[wtf]
      # hmpa<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
      HMPA <- sum(results)
      
#       huh <- results$yield - hmpa
#       
# plot(results$yield, hmpa)
# abline(a = 0, b = 1)
# 
      # hmpa<-hmpa*(hmpa>0)
      # HMPA<-sum(hmpa)


      # sum(results$yield) / HMPA
      # 
      # (sum(results$yield) - HBAU) 
      
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
    R<- pmax(1e-6,pmin(.999,rowSums(KprotectedPerCell_Library[,which(MPAselect==1),drop=FALSE])))
    ER_redistribute<- pmin(.999,1-(1-ER)^(1/(1-R)))
    
    results <- sim_mpa(r = r, k = K, m = m, u = ER_redistribute,p_mpa = R,local_dd = local_dd,years = mpa_sim_years)
    
    # if (any((R == .999))){
    #   stop()
    # }
    # 
    # if (i == 7){
    #   browser()
    # }
    
    # hmpa<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
    # hmpa<-hmpa*(hmpa>0)
    # HMPA<-sum(hmpa)
    
    HMPA <- sum(results)
    
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
    saveRDS(NetworkResult,file = file.path(results_path,"local_dd_downsized_NetworkResult100_BAU1_mollweide.rds"))
    saveRDS(PriorityAreas,file = file.path(results_path, "local_dd_downsized_PriorityAreas100_BAU1_mollweide.rds"))
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
  
  
} else {
  
  NetworkResult <-readRDS(file = file.path(results_path,"local_dd_downsized_NetworkResult100_BAU1_mollweide.rds"))
  
  PriorityAreas <- readRDS(file = file.path(results_path,"local_dd_downsized_PriorityAreas100_BAU1_mollweide.rds") )
  
}

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

downsized_BenefitCurve <- BenefitCurve

downsized_benefitplot <- benefitplot

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

downsized_ShortCoord_Sort <- ShortCoord_Sort

downsized_MPAcoverage<-downsized_ShortCoord_Sort %>% ggplot(aes(x=lon,y=lat,fill=rank)) + #scale_fill_gradient2(low="darkred", mid="white",high="#00539CFF",midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar", aesthetics = "fill",name=expression(paste(Delta, "H (MT)")))+
  scale_fill_gradientn(colours = c("forestgreen", "white", "orange"), limits=c(0,100), values = scales::rescale(c(min(ShortCoord_Sort$rank), InflectMPA, max(ShortCoord_Sort$rank))),name="Protection sequence")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position=c(0.63, 0.05), legend.direction = "horizontal")+ #"bottom
  geom_raster()+
  geom_raster(data=MPA_coord, aes(x=lon, y=lat),fill="cyan")+  #"#EEA47FFF"
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent'))
downsized_MPAcoverage

# load original cabral

NetworkResult <-readRDS(file = file.path(results_path,"NetworkResult100_BAU1_mollweide.rds"))

PriorityAreas <- readRDS(file = file.path(results_path,"PriorityAreas100_BAU1_mollweide.rds") )


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


# compare

og_priority <- ShortCoord_Sort %>% 
  mutate(ID = as.integer(ID))


downscaled_priority <- downsized_ShortCoord_Sort %>% 
  mutate(ID = as.integer(ID))

compare_priority <- og_priority %>% 
  left_join(downscaled_priority %>% select(rank, ID), by = "ID")

compare_priority %>% 
  ggplot(aes(rank.x, rank.y)) + 
  geom_point(alpha = 0.25) + 
  labs(x = "Cell Prioritization Rank with Global Stocks",
       y = "Cell Prioritization Rank with Taxa + EEZ + FAO Region Stocks")

(MPAcoverage + labs(title = "Cell Prioritization Rank with Global Stocks") ) / 
  (downsized_MPAcoverage + labs(title = "Cell Prioritization Rank with Taxa + EEZ + FAO Region Stocks"))


(benefitplot + labs(subtitle = "Global Stocks") ) / 
  (downsized_benefitplot + labs(subtitle = "Taxa + EEZ + FAO Region Stocks"))

# sigh <- ShortCoord_Sort %>% 
#   sf::st_as_sf(coords = c("lon", "lat"),
#                crs = sf::st_crs(land_shp_moll))  %>% 
#   sf::st_intersection(eezs %>% st_simplify())
# 


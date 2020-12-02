library(tidyverse)
library(sf)
library(here)


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


MegaData<-readRDS(file = here("data","MegaData.rds"))


CleanCoordmegacell<-readRDS(file = here("data","CleanCoordmegacell_mollweide.rds"))

KprotectedPerCell_Library<-readRDS(file = here("data","KprotectedPerCell_Library_mollweide.rds"))

land_shp_moll<-readRDS(file = here("data","land_shp_moll.rds"))


ShortCoord<-CleanCoordmegacell

stockid_to_map <- "Fis-23873" #MegaData$stockid[3]


tmp <- ShortCoord %>% 
  mutate(k_per_cell = as.numeric(KprotectedPerCell_Library[which(MegaData$stockid == stockid_to_map),])) %>% 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = sf::st_crs(land_shp_moll)) %>% 
  filter(k_per_cell > 0) 

tmp %>% 
  ggplot(aes(color = k_per_cell)) + 
  geom_sf(data = land_shp_moll, fill="black", lwd = 0, inherit.aes = F)+ theme(panel.grid.major = element_line(colour = 'transparent')) +
  geom_sf(size = .1) + 
  scale_color_viridis_c() + 
  labs(title = stockid_to_map)
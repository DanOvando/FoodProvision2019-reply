# FoodProvision2019
 Global MPA Network
 This file contains the metadata
 
 Files: 
  MegaData.rds - contains the species list, biological parameters, and exploitation rates. See below for the metadata or description of the column names.
  
 CleanCoordmegacell_mollweide.rds. - coordinates of the management units in mollweide projection. The resolution of the cell is approximately 50km by 50 km.
 
 Cleanmegacell_mollweide.rds - contains the species distribution of the modeled stocks. Column names are stock ids "stockid column in Megadata.rds" while rows are normalized distribution of stock.
  
 Column descriptions for MegaData.rds:
 SpeciesID - Aquamaps species ID
 Manage - 0 for unassessed stocks, 1 for assessed stocks
 stockid - Aquamaps species ID for unassessed stocks, RAM database ID for assessed stocks
 Sci Name - Scientific name
 r - instrinsic growth rate
 m - mobility
 Kfin - total carrying capacity of the stock
 MSYfin - maximum sustainable yield of the stock

 

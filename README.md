Please read this document before proceeding to use the other files.

This is the repository page for the code and the data used in the paper
"A global network of marine protected areas for food"
by: R.B. Cabral, D. Bradley, J. Mayorga, W. Goodell, A.M. Friedlander, E. Sala, C. Costello, S.D. Gaines
(in press, PNAS)
 
This file contains the metadata
 
Files: 

MegaData.rds - contains the species list, biological parameters, and exploitation rates. See below for the metadata or description of the column names.
  
CleanCoordmegacell_mollweide.rds. - coordinates of the management units in mollweide projection. The resolution of the cell is ~55 km by 55 km.
 
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

 

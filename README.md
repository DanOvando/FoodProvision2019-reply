
# Reproducing Results

All materials needed to reproduce our results and manuscript are contained in this repository. In order to reproduce

1. Fork the repository and clone to your machine

2. Open R and set your working directory of the cloned repository (or just use RStudio projects)

3. This project is set up with [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package dependencies. Inside R (and with your working directory set correctly) run `renv::restore()`. This will install the correct versions of all the packages needed to replicate our results. Packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else. 

This project has a lot of package dependencies so depending on what you already have installed the `renv` process might take a while (like 10-20 minutes). 

Once you've successfully run `renv::restore()` you can reproduce our results by running cabral-etal-exploration.Rmd. This will knit the reply stored in cabral-etal-reply.Rmd.

After running the renv steps, you should not have to change anything on your end in order to reproduce our analysis (saved versions of results are pushed to github so you don't have to re-run everything). This has been tested on MacOS, Linux, and Windows. 


# Cabral et al. README
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

 

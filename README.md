
# Reproducing Results

This is a forked version of the repository used to produce Cabral *et al.* 2020 "A global network of marine protected areas for food". The repository has been modified to produce the results presented in Ovando *et al.* In Review "Models of Marine Protected Areas Must Explicitly Address Spatial Dynamics". The original repository for Cabral *et al.* 2020 can be found [here](https://github.com/rencabral/FoodProvision2019), please see documentation there for details on use of their materials. 

All materials needed to reproduce our results and manuscript are contained in this repository. In order to reproduce

1. Fork the repository and clone to your machine

2. Open R and set your working directory of the cloned repository (or just use RStudio projects)

3. This project is set up with [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package dependencies. Inside R (and with your working directory set correctly) run `renv::restore()`. This will install the correct versions of all the packages needed to replicate our results. Packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else. 

This project has a lot of package dependencies so depending on what you already have installed the `renv` process might take a while (like 10-20 minutes). 

Once you've successfully run `renv::restore()` you can reproduce our results by running cabral-etal-exploration.Rmd. This will knit the reply stored in cabral-etal-reply.Rmd.

After running the renv steps, you should not have to change anything on your end in order to reproduce our analysis (saved versions of results are pushed to github so you don't have to re-run everything). This has been tested on MacOS, Linux, and Windows. 


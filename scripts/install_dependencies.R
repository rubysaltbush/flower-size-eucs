# run to install all packages needed for analysis
# R version for original analysis 4.3.0

install.packages("tidyverse") # for data manipulation, run version 2.0.0
install.packages("remotes") # package for installing from GitHub
remotes::install_github("traitecoevo/austraits", upgrade = "ask") # access to austraits database for australian plant trait data, not on CRAN, version 2.1.2
#remotes::install_github("traitecoevo/APCalign") # taxonomic name matching to the Aus Plant Census v0.0.2.9
install.packages("sf") # for spatial data, run version 1.0-12
install.packages("terra") # does both raster and vector data, compatible with changes to R spatial data, v1.7-39
install.packages("ape") # basic package for phylogenetic trees and analysis, v5.7-1
install.packages("ggpubr") # for publication ready ggplots v0.6.0
install.packages("RColorBrewer") # for plot colours, v1.1-3
install.packages("GGally") # for correlation plots, v2.1.2

# run to install all packages needed for analysis
# R version for original analysis 4.3.0

install.packages("tidyverse") # for data manipulation, run version 2.0.0
install.packages("sf") # for spatial data, run version 1.0-14
install.packages("terra") # does both raster and vector data, compatible with changes to R spatial data, v1.7-55
install.packages("ape") # basic package for phylogenetic trees and analysis, v5.7-1
install.packages("phytools") # package for phylogenetic visualisations, v2.0-3
install.packages("phylolm") # phylogenetic logistic regressions, v2.6.2
install.packages("caper") # to test phylogenetic signal in binary trait, v1.0.3
install.packages("ggpubr") # for publication ready ggplots v0.6.0
install.packages("RColorBrewer") # for plot colours, v1.1-3
install.packages("GGally") # for correlation plots, v2.1.2
install.packages("plotrix") # for drawing circles on phylogenies, v3.8-2
install.packages("nlme") # for PGLS, a special case of mixed-effects models, v3.1-163
install.packages("AICcmodavg") # for model averaging v 2.3-2
install.packages("rr2") # for PGLS R2 per Ives (2019), v1.1.1
install.packages("galah") # for insect pollinator map, v2.0.0
install.packages("spThin") # for thinning euc occurrences, v0.2.0

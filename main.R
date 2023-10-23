# uncomment below if you need to install packages
#source("scripts/install_dependencies.R")

library(tidyverse)
library(austraits)
library(ape)
library(phytools)
library(sf)
library(terra)
library(ggpubr)
library(RColorBrewer)
library(GGally)
library(car)
library(plotrix)
# library(APCalign) was used for aligning taxonomy but not needed in current workflow

# function to cache pre-prepared R data. If RDS already in cache will read data
source("scripts/functions/cache_RDS.R")

#### GET DATA #####

# euc ranges raster from Stuart Allen, cleaned by Ruby Stephens
rangerast <- list(cell_id = terra::rast("data_input/euc_raster/aus_100x100_cells.tif"),
                  taxa = readr::read_csv("data_input/euc_raster/aus_100x100_taxa.csv"),
                  cell_taxa = readr::read_csv("data_input/euc_raster/aus_100x100_cell_taxa.csv"))

# Thornhill et al. (2019) phylogeny, dated ML1 version with tips cleaned
treeML1 <- ape::read.tree("data_input/Eucalypts_ML1_dated_r8s.phy")

# tidied, species level eucalypt trait and environment data with taxonomic name 
# resolution between phylogeny, ranges, EUCLID and Australian Plant Census (via APCalign)
source("scripts/data_prep/final_data.R")

# standard colours for plots
source("scripts/my_colours.R")

#### ANALYSES ####

# initial regression analyses and figures
source("scripts/analyses/single_regressions.R")

# main analysis, multiple regressions
source("scripts/analyses/multiple_regression.R")

# supporting phylogenetic regressions for evolutionary relationships
source("scripts/analyses/PGLS.R")

#### FIGURES ####

# supporting phylogenetic figure with evolutionary modelling
source("scripts/figures/phylogeny.R")

# trait maps for figures
source("scripts/figures/maps.R")


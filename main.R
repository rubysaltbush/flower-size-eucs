# uncomment below if you need to install packages
#source("scripts/install_dependencies.R")
library(tidyverse)
library(austraits)
#library(APCalign) # not sure if necessary? used for data but not using now
library(ape)
library(sf)
library(terra)
library(ggpubr)
library(RColorBrewer)
library(GGally)
library(car)

# function to cache pre-prepared R data. If RDS already in cache will read data
source("scripts/functions/cache_RDS.R")

#### GET DATA #####

# euc ranges raster from Stuart Allen, cleaned by Ruby Stephens
rangerast <- list(cell_id = terra::rast("data_input/euc_raster/aus_100x100_cells.tif"),
                  taxa = readr::read_csv("data_input/euc_raster/aus_100x100_taxa.csv"),
                  cell_taxa = readr::read_csv("data_input/euc_raster/aus_100x100_cell_taxa.csv"))

# Thornhill et al. (2019) phylogeny, dated and with tips cleaned
treeML1 <- ape::read.tree("data_input/Eucalypts_ML1_dated_r8s.phy")

# tidied, species level eucalypt trait and environment data with taxonomic name resolution
source("scripts/data_prep/final_data.R")

#### ANALYSES ####

source("scripts/analysis/multiple_regression.R")

source("scripts/analysis/phylogenetic_ls_regression.R")

# NEXT - PGLS???
#      - evolutionary correlation between colour and size? phylogenetic logistic regression?
#      - produce final figures, maps etc

# uncomment below if you need to install packages
#source("scripts/install_dependencies.R")

library(tidyverse)
library(ape)
library(phytools)
library(phylolm)
library(caper)
library(sf)
library(terra)
library(spThin)
library(ggpubr)
library(RColorBrewer)
library(GGally)
library(car)
library(plotrix)
library(nlme)
library(AICcmodavg)
library(rr2)
library(galah)
# library(APCalign) was used for aligning taxonomy but not needed in current workflow

# function to cache pre-prepared R data. If RDS already in cache will read data
source("scripts/functions/cache_RDS.R")

#### GET DATA #####

# euc ranges raster from Acknowledged_SA, cleaned by Lead Author
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

# initial regression analyses and figures for relationships with environment
source("scripts/analyses/single_regressions.R")

# main analysis, multiple regressions
source("scripts/analyses/multiple_regression.R")

# supports main analysis, model averaging
source("scripts/analyses/model_averaging.R")

# supporting test of phylogenetic signal for bud size and flower colour
source("scripts/analyses/phylogenetic_signal.R")

# supporting phylogenetic regressions for evolutionary relationships
source("scripts/analyses/PGLS.R")

#### FIGURES ####

# supporting phylogenetic figure with evolutionary modelling
source("scripts/figures/phylogeny.R")

# trait maps for figures
source("scripts/figures/maps.R")

# map of insect pollinators across Australia
source("scripts/figures/insect_pollinators_map.R")

#### SUPPORTING ####

source("scripts/analyses/field_data.R")

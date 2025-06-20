# flower-size-eucs
Data and analysis R code for the paper [*Birds, Bats or Climate? Eucalypt Floral Traits Reflect Pollination Over Abiotic Environment*](https://doi.org/10.1002/ece3.71449)


## Re-running analysis

Using RStudio open [main.R](https://github.com/rubysaltbush/flower-size-eucs/blob/main/main.R) 
and run scripts in order given in this main script.

Necessary packages can be installed by running [install_dependencies.R](https://github.com/rubysaltbush/flower-size-eucs/blob/main/scripts/install_dependencies.R).
Check package versions mentioned in this script are consistent with your installed versions. Originally run in R version 4.3.0

The code caches several steps of the data cleaning and results. 
Original files for some environmental data are not provided as they are too large for GitHub and/or not allowed to be shared but processed rasters are available in [data_output/rasters](https://github.com/rubysaltbush/flower-size-eucs/tree/main/data_output/rasters).

Feel free to re-use scripts and functions for your own analyses, e.g.

[arclabel.R](https://github.com/rubysaltbush/flower-size-eucs/blob/main/scripts/functions/arclabel.R) adds custom labels to circular phylogenies by providing the tips to draw the label between

Citation: Stephens, R. E., Sauquet, H., Laugier, B., Gosper, C. R., & Gallagher, R. V. (2025). Birds, Bats or Climate? Eucalypt Floral Traits Reflect Pollination Over Abiotic Environment. Ecology and Evolution, 15(6), e71449. https://doi.org/10.1002/ece3.71449

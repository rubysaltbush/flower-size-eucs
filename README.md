# flower-size-eucs
Data and analysis R code for the paper *Eucalypt flower size and colour shaped more by pollination than abiotic environment*

Authors: Ruby E. Stephens (1,2), Hervé Sauquet (2,3), Bree Laugier (4), Carl R. Gosper (5), , Rachael V. Gallagher (1,4)
+ Corresponding author: Ruby E. Stephens, stephenseruby@gmail.com

Author addresses:

1. School of Natural Sciences, Macquarie University, Sydney, Australia
2. National Herbarium of New South Wales (NSW), Royal Botanic Gardens and Domain Trust, Sydney, Australia
3. Evolution and Ecology Research Centre, University of New South Wales, Sydney, Australia
4. Hawkesbury Institute for the Environment, Western Sydney University, Sydney, Australia
5. Biodiversity and Conservation Science, Department of Biodiversity, Conservation and Attractions, Perth, WA, Australia

## Re-running analysis

Using RStudio open [main.R](https://github.com/rubysaltbush/flower-size-eucs/blob/main/main.R) 
and run scripts in order given in this main script.

Necessary packages can be installed by running [install_dependencies.R](https://github.com/rubysaltbush/flower-size-eucs/blob/main/scripts/install_dependencies.R).
Check package versions mentioned in this script are consistent with your installed versions. Originally run in R version 4.1.3

The code caches several steps of the data cleaning and results. 
Original files for some environmental data are not provided as they are too large for GitHub and/or not allowed to be shared but processed rasters are available in [data_output/rasters](https://github.com/rubysaltbush/flower-size-eucs/tree/main/data_output/rasters).

Feel free to re-use scripts and functions for your own analyses, e.g.

[arclabel.R](https://github.com/rubysaltbush/flower-size-eucs/blob/main/scripts/functions/arclabel.R) adds custom labels to circular phylogenies by providing the tips to draw the label between

Citation: 
euc_raster - Stuart Allen, Rachael Gallagher and Ruby Stephens - added 2023-08-22 - 100x100m cells across Australia, clipped to within coastline, with corresponding csv's that list the eucalypt species that occur within each cell according to modelled species ranges:

"-cell_id: a raster at 100 x 100km resolution in Albers Equal Area projection, with values being unique cell IDs

-taxa: a data.frame of taxon names and IDs. This also includes a column denoting the type of model used to generate the estimated range, which was dependent on the number of available occurrence points for the species (PPM = point process model; RangeBag = range bagging; Points = no modelling, occurrence points only)

-cell_taxa: a data.frame of taxon IDs and the corresponding cell IDs in which they are estimated to occur"

occurrence_data_Ruby_fromBree.csv - Bree Laugier-Kitchener - added 2023-08-22 - eucalypt species occurrences downloaded from AVH and cleaned by manual comparison to distributions on EUCLID by Bree Laugier-Kitchener. "I used EUCLID to compare maps for the species and finally D. Nicolle to determine dubious species and combine species that EUCLID treats as separate."
occurrence_data
taxon = includes all subspecies
MAT = mean annual temperature of occurrence record (from CHELSA https://chelsa-climate.org/)
MTWM = mean temperature of the warmest month (from CHELSA https://chelsa-climate.org/)
MAP = mean annual precipitation (from CHELSA https://chelsa-climate.org/)
MPDM = mean precipitation warmest month (from CHELSA https://chelsa-climate.org/)
speciesLevel = amalgamated at species level (e.g. Angophora bakeri subsp. crassifolia = Angophora bakeri)
speciesLevelUpdated = some species I further amalgamated because they were considered another species by D. Nicolles classification of eucalypts (e.g. Eucalyptus sp. Flinders Ranges (D.Nicolle 562) = Eucalyptus cajuputea)

Eucalypts_ML1_dated_r8s.phy - Andrew Thornhill et al. 2019 - added 2023-08-22 - main dated eucalypt phylogeny from Thornhill, A. H., Crisp, M. D., Külheim, C., Lam, K. E., Nelson, L. A., Yeates, D. K., Miller, J. T. (2019). A dated molecular perspective of eucalypt taxonomy, evolution and diversification. Australian Systematic Botany, 32(1), 29–48. https://doi.org/10.1071/SB18015, downloaded from https://doi.org/10.25919/5c6a53564d748 CSIRO data portal and processed to clean tip names of r8s version and make usable

euc_traits_nosubsp_filled.csv - Ruby Stephens - added 2023-08-22 - eucalypt trait data assembled by text mining using regular expressions to extract data from descriptions on EUCLID (https://apps.lucidcentral.org/euclid/text/intro/index.html, for bud and fruit dimensions, flower colour, number of buds and most max height) combined with AusTraits data (max height). Data have been manually checked with missing values filled in and outlying values corrected, and species names have been matched to the Australian Plant Census using APCalign and then by manually cross checking taxa with the APC to align taxonomy of EUCLID, plant occurrences and ranges, and the Thornhill et al. phylogeny

euclid_buds_fruit_checked.csv - Ruby Stephens - added 2023-02-09 - updated 2023-04-13 - data scraped from EUCLID for bud and fruit dimensions of all eucalypt species of Australia and manually checked by Ruby Stephens, with names updated manually from APC for those taxa that didn't automatically match using APCalign

euclid_colours_checked.csv - Ruby Stephens - added 2023-02-16 - data scraped from EUCLID for flower colour with non-standard colour terms manually resolved by Ruby Stephens' inspection of flower images on iNaturalist and in Dean Nicolle's book series
Note to self: scoring colours in colours_austraits by checking images or descriptions from multiple sources, might ultimately want to include yellow flowers as part of white_cream as often marginal but have left separate here in most cases. Scored dominant flower colour (so ignoring anther colour where this is different and favouring filament colour which is more dominant). If pink to red I've classified as red.



range_data_Ruby_fromBree.csv - Bree Laugier-Kitchener and Rachael Gallagher - added 2023-03-27 - eucalypt species area of occurrence and extent of occurrence from Stuart Allen and Rachael Gallagher's 2020 calculations I think?
range_data
n = number of occurrences per species - this includes subspecies
aoo = area of occupancy (km2)
eoo = extent of occurrence (km2)
dubious species = species of dubious identification according to D.Nicolles classification of eucalypts, includes hybrids or intergrades



taxonomic_matching.csv - Ruby Stephens - added 2023-05-16 - updated manual taxonomic matching between EUCLID names, Australian Plant Census (APC), Thornhill phylogeny (this one uses tip names from Eucalypts_ML1_dated_r8s.tre) and range data from Bree. Aligns some spelling errors in tip names to correct names



Aus_outline.shp - Ruby Stephens - added 2023-06-19 - shapefile with WGS84 projection of coastline of Australia including all islands for plotting and cropping raster

batmaps folder - ​Australasian Bat Society - BatMap. http://ausbats.org.au/batmap. Accessed 18/07/2023. Distribution maps for flower-visiting bats across Australia

Australian_Bird_Data_Version_1.xlsx - Garnett et al. 2015 - added 2023-07-21 - dataset for biological, ecological, conservation and legal information for every species and subspecies of Australian bird, with which I can classify nectar and pollen eating (I.e. flower visiting) Australian bird species to include in my maps of the species richness of flower visiting birds across Australia. Garnett, S. T., Duursma, D. E., Ehmke, G., Guay, P.-J., Stewart, A., Szabo, J. K., Weston, M. A., Bennett, S., Crowley, G. M., Drynan, D., Dutson, G., Fitzherbert, K., & Franklin, D. C. (2015). Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data, 2(1), Article 1. https://doi.org/10.1038/sdata.2015.61


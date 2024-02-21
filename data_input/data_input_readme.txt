Aus_outline.shp - Lead Author - added 2023-06-19 - shapefile with WGS84 projection of coastline of Australia including all islands for plotting and cropping raster

Australian_Bird_Data_Version_1.xlsx - Garnett et al. 2015 - added 2023-07-21 - dataset for biological, ecological, conservation and legal information for every species and subspecies of Australian bird, with which I can classify nectar and pollen eating (i.e. flower visiting) Australian bird species to include in my maps of the species richness of flower visiting birds across Australia. Garnett, S. T., Duursma, D. E., Ehmke, G., Guay, P.-J., Stewart, A., Szabo, J. K., Weston, M. A., Bennett, S., Crowley, G. M., Drynan, D., Dutson, G., Fitzherbert, K., & Franklin, D. C. (2015). Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data, 2(1), Article 1. https://doi.org/10.1038/sdata.2015.61

batmaps folder - ​Australasian Bat Society - BatMap. http://ausbats.org.au/batmap. Accessed 18/07/2023. Distribution maps for flower-visiting bats across Australia, species as per Table 7 in Armstrong (1979) review of biotic pollination in Australia.

euc_raster - Acknowledged_SA, Final Author and Lead Author - added 2023-08-22 - 100x100m cells across Australia, clipped to within coastline, with corresponding csv's that list the eucalypt species that occur within each cell according to modelled species ranges:
"-cell_id: a raster at 100 x 100km resolution in Albers Equal Area projection, with values being unique cell IDs
-taxa: a data.frame of taxon names and IDs. This also includes a column denoting the type of model used to generate the estimated range, which was dependent on the number of available occurrence points for the species (PPM = point process model; RangeBag = range bagging; Points = no modelling, occurrence points only)
-cell_taxa: a data.frame of taxon IDs and the corresponding cell IDs in which they are estimated to occur"

euc_traits_nosubsp_filled.csv - Lead Author - added 2023-08-22 - updated 2023-10-19 - eucalypt trait data assembled by text mining using regular expressions to extract data from descriptions on EUCLID (https://apps.lucidcentral.org/euclid/text/intro/index.html, for bud and fruit dimensions, flower colour, number of buds and most max height) combined with AusTraits data (max height). Data have been manually checked with missing values filled in and outlying values corrected, and species names have been matched to the Australian Plant Census using APCalign and then by manually cross checking taxa with the APC to align taxonomy of EUCLID, plant occurrences and ranges, and the Thornhill et al. phylogeny. Subgenus updated to match Thornhill et al. (2019) phylogeny.

Eucalypts_ML1_dated_r8s.phy - Andrew Thornhill et al. 2019 - added 2023-08-22 - main dated eucalypt phylogeny from Thornhill, A. H., Crisp, M. D., Külheim, C., Lam, K. E., Nelson, L. A., Yeates, D. K., Miller, J. T. (2019). A dated molecular perspective of eucalypt taxonomy, evolution and diversification. Australian Systematic Botany, 32(1), 29–48. https://doi.org/10.1071/SB18015, downloaded from https://doi.org/10.25919/5c6a53564d748 CSIRO data portal and processed to clean tip names of r8s version and make usable

occurrence_data_Ruby_fromBree.csv - Third Author - added 2023-08-22 - eucalypt species occurrences downloaded from AVH and cleaned by manual comparison to distributions on EUCLID by Third Author. "I used EUCLID to compare maps for the species and finally D. Nicolle to determine dubious species and combine species that EUCLID treats as separate."
occurrence_data
taxon = includes all subspecies
MAT = mean annual temperature of occurrence record (from CHELSA https://chelsa-climate.org/)
MTWM = mean temperature of the warmest month (from CHELSA https://chelsa-climate.org/)
MAP = mean annual precipitation (from CHELSA https://chelsa-climate.org/)
MPDM = mean precipitation warmest month (from CHELSA https://chelsa-climate.org/)
speciesLevel = amalgamated at species level (e.g. Angophora bakeri subsp. crassifolia = Angophora bakeri)
speciesLevelUpdated = some species I further amalgamated because they were considered another species by D. Nicolles classification of eucalypts (e.g. Eucalyptus sp. Flinders Ranges (D.Nicolle 562) = Eucalyptus cajuputea)
** note - columns for climate data contain errors and should be discarded

clades_forlabelling.csv - Lead Author and Acknowledged_DC - added 2023-10-19 - Thornhill et al (2019) tree names, subgenus and position in tree matched to eucalypt section and selected subgenus and section labels excluding smaller clades and paraphyletic taxa

marsupial_pollinator_ranges - Lead Author and Marsh et al. (2022) - added 2024-01-10 - ranges for flower-visiting marsupials in Australia, 14 species from Table 8 in Armstrong (1979) chosen for presence of nectar in their diet according to Wilman et al. (2014) EltonTraits 1.0. 
Ranges from Marsh, C. J., Sica, Y. V., Burgin, C. J., Dorman, W. A., Anderson, R. C., del Toro Mijares, I., Vigneron, J. G., Barve, V., Dombrowik, V. L., Duong, M., Guralnick, R., Hart, J. A., Maypole, J. K., McCall, K., Ranipeta, A., Schuerkmann, A., Torselli, M. A., Lacher Jr, T., Mittermeier, R. A., … Jetz, W. (2022). Expert range maps of global mammal distributions harmonised to three taxonomic authorities. Journal of Biogeography, 49(5), 979–992. https://doi.org/10.1111/jbi.14330

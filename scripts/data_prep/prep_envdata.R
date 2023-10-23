# script to get environmental data rasters and calculate species means using
# cleaned eucalypt occurrence data, for: 
#                     1) soil available phosphorus
#                     2) mean annual temperature
#                     3) mean annual precipitation
#                     4) flower-visiting bat species richness and 
#                     5) flower-visiting bird species richness

# resulting species mean file has been cached because original data too large to
# share over GitHub, but below script shows steps used to extract data

spmean_env <- cache_RDS("data_output/euc_species_mean_env.csv", 
                        read_function = readr::read_csv,
                        save_function = write_csv, function() {

# read in cleaned euc herbarium occurrences from Bree-Anne Laugier Kitchener
occurrences <- readr::read_csv("data_input/occurrence_data_Ruby_fromBree.csv") 
occurrences <- occurrences %>%
  dplyr::select(range_names = speciesLevelUpdated, latitude, longitude, year,
                locality)
                          
#### 1 - PHOSPHORUS ####                        
# read in 3 rasters of available phosphorus across Australia (90m resolution)
# from the Soil and Landscape Grid of Australia (SLGA)
# https://esoil.io/TERNLandscapes/Public/Pages/SLGA/index.html
# and take 1 mean of top 30cm, where P most available to plants

# available soil phosphorus #
# read in 3 rasters downloaded from SLGA using terra stack function
avp <- terra::rast(list.files("data_cache/AVP_EV_N_P_AU_TRN_N_20220826/", 
                              full.names = TRUE))

# check them out
plot(avp)

# take mean of all depths
mean_avp <- terra::app(avp, fun = mean)
rm(avp)

# how does this look?
plot(mean_avp)
# generally higher along east coast but a bit variable

# read in shapefile of Aus outline for plotting
aus <- terra::vect("data_input/Aus_outline.shp")
# bit slow and over complex, simplify to make faster
aus <- terra::simplifyGeom(aus, tolerance = 0.01)
# crop to remove Norfolk and Christmas Islands from outline
aus <- terra::crop(aus, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# change aus outline projection to match raster
aus <- terra::project(aus, crs(mean_avp))

# export raster image with base R as resolution too high for ggplot
pdf(file = "figures/maps/aus_mean_available_phosphorus_0to30cm_SLGA.pdf",
    width = 10, height = 10)
plot(mean_avp, main = "Mean Available Phosphorus 0-30 cm (mg/kg)",
     box = FALSE)
plot(aus, add = TRUE)
dev.off()

# export this raster to use in analysis!
terra::writeRaster(mean_avp, "data_cache/mean_AVP_0to30cm_SLGA.tiff",
                   overwrite = TRUE)
# because it's huge have to write to cache, can't sync via GitHub

# now calculate mean AVP for each species by intersecting occurrences
# with mean modelled available phosphorus for Australia at 0-30cm metres depth

# extract available phosphorus value for each cleaned eucalypt occurrence
# first make occurrences spatial
euc_occurr <- terra::vect(occurrences, geom = c("longitude", "latitude"), 
                          crs = crs(mean_avp))
# check it out
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_avp <- terra::extract(mean_avp, euc_occurr)

# and join back to data frame
occurrences$mean_AVP <- euc_occurr_mean_avp[, -1]

# now summarise by species!
species_meanAVP <- occurrences %>%
  dplyr::group_by(range_names) %>%
  dplyr::summarise(meanAVP = mean(mean_AVP, na.rm = TRUE))

# check it out
summary(species_meanAVP$meanAVP)
hist(species_meanAVP$meanAVP)

rm(mean_avp, euc_occurr_mean_avp)

#### 2,3 - TEMPERATURE AND PRECIPITATION ####  

# using occurrence data and provided Mean Annual Temperature and Mean Annual
# Precipitation calculate MAT and MAP per species as well

# read in raster of global Mean Annual Air Temperature for 1981-2010
# from CHELSA v2.1 
# Brun, P., Zimmermann, N.E., Hari, C., Pellissier, L., Karger, D. (2022): Data from: CHELSA-BIOCLIM+ A novel set of global climate-related predictors at kilometre-resolution. EnviDat. https://doi.org/10.16904/envidat.332
# Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, P., Kessler, M. (2017): Climatologies at high resolution for the Earth land surface areas. Scientific Data. 4 170122. https://doi.org/10.1038/sdata.2017.122
# too large for GitHub, cached
mat <- terra::rast("data_cache/CHELSA_bio1_1981-2010_V.2.1.tif")
# clip to Australia
mat <- terra::crop(mat, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# check it out
plot(mat, main = "Mean Annual Air Temperature (ºC)")
plot(aus, add = TRUE)

# export raster image with base R as resolution too high for ggplot
pdf(file = "figures/maps/aus_mean_annual_temperature_CHELSAv2.pdf",
    width = 10, height = 10)
plot(mat, main = "Mean Annual Air Temperature (ºC)", box = FALSE)
plot(aus, add = TRUE)
dev.off()

# extract MAT value for each cleaned eucalypt occurrence
# check it out
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_MAT <- terra::extract(mat, euc_occurr)
# and join back to data frame
occurrences$mat_chelsav2 <- euc_occurr_mean_MAT[, -1]

# summarise MAT by species!
species_meanMAT <- occurrences %>%
  dplyr::group_by(range_names) %>%
  dplyr::summarise(meanMAT = mean(mat_chelsav2, na.rm = TRUE))
summary(species_meanMAT$meanMAT)
hist(species_meanMAT$meanMAT)

rm(mat, euc_occurr_mean_MAT)

# read in raster of global Mean Annual Precipitation for 1981-2010
# from CHELSA v2.1 
map <- terra::rast("data_cache/CHELSA_bio12_1981-2010_V.2.1.tif")
# clip to Australia
map <- terra::crop(map, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# check it out
plot(map, main = "Annual Precipitation Amount (kg m-2 year-1)")
plot(aus, add = TRUE)

# export raster image with base R as resolution too high for ggplot
pdf(file = "figures/maps/aus_mean_annual_precipitation_CHELSAv2.pdf",
    width = 10, height = 10)
plot(map, main = "Annual Precipitation Amount (kg m-2 year-1)", box = FALSE)
plot(aus, add = TRUE)
dev.off()

# extract MAP value for each cleaned eucalypt occurrence
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_MAP <- terra::extract(map, euc_occurr)
# and join back to data frame
occurrences$map_chelsav2 <- euc_occurr_mean_MAP[, -1]

# summarise MAP by species!
species_meanMAP <- occurrences %>%
  dplyr::group_by(range_names) %>%
  dplyr::summarise(meanMAP = mean(map_chelsav2, na.rm = TRUE))
summary(species_meanMAP$meanMAP)
hist(species_meanMAP$meanMAP)

rm(euc_occurr_mean_MAP, map)

#### 4 - BAT RICHNESS ####  

# create raster of flower-visiting bat species richness across Australia

# first read in range maps for Australian flower visiting bat species
# as per Table 7 in Armstrong (1979)'s review of Australian pollination
files <- list.files(path = "data_input/batmaps", pattern = ".shp$", full.names = TRUE)
batranges <- lapply(files, FUN = terra::vect)
files <- gsub("data_input/batmaps/", "", files)
files <- gsub("\\.shp", "", files)
names(batranges) <- files
rm(files)
# make sure projections same as Euc ranges raster (standard for project)
for(name in names(batranges)) {
  batranges[[name]] <- terra::project(batranges[[name]], crs(rangerast$cell_id))
}
rm(name)

# want to use whole possible range including outliers
# make 1m buffers around outlier points to change them to polygons,
# then unite with ranges maps
temp <- terra::buffer(batranges$Dobsonia_magna_outlier, width = 1)
plot(temp)
batranges$Dobsonia_magna <- terra::combineGeoms(temp, batranges$Dobsonia_magna)
plot(batranges$Dobsonia_magna)
temp <- terra::buffer(batranges$Pteropus_alecto_outlier, width = 1)
plot(temp)
batranges$Pteropus_alecto <- terra::combineGeoms(temp, batranges$Pteropus_alecto)
plot(batranges$Pteropus_alecto)
temp <- terra::buffer(batranges$Pteropus_poliocephalus_outlier, width = 1)
plot(temp)
batranges$Pteropus_poliocephalus <- terra::combineGeoms(temp, batranges$Pteropus_poliocephalus)
plot(batranges$Pteropus_poliocephalus)
temp <- terra::buffer(batranges$Pteropus_scapulatus_outlier, width = 1)
plot(temp)
batranges$Pteropus_scapulatus <- terra::combineGeoms(temp, batranges$Pteropus_scapulatus)
plot(batranges$Pteropus_scapulatus)
rm(temp)
# then drop outlier shapefiles from list
batranges <- batranges[!(names(batranges) %in% c("Dobsonia_magna_outlier", 
                                                 "Pteropus_alecto_outlier",
                                                 "Pteropus_poliocephalus_outlier", 
                                                 "Pteropus_scapulatus_outlier"))]

# generate blank raster at finer scale than eucalypt ranges raster
# 20km*20km, each cell with unique integer value
# to intersect with bat and bird ranges
blankrast <- terra::rast(matrix(1:39975, 205),
                         extent = terra::ext(rangerast$cell_id),
                         crs = terra::crs(rangerast$cell_id))
plot(blankrast)

batrange_cells <- dplyr::tibble()
for(name in names(batranges)) {
  temp <- terra::extract(x = blankrast, y = batranges[[name]])
  temp$species <- name
  batrange_cells <- rbind(batrange_cells, temp)
}
rm(temp, name)
# filter out NAs and remove unnecessary ID column
batrange_cells <- batrange_cells %>%
  dplyr::select(-ID) %>%
  dplyr::filter(!(is.na(lyr.1)))

# prepare map figure of flower-visiting bat species richness across Aus
# calculate bat richness per grid cell 
batrichness <- batrange_cells %>%
  dplyr::group_by(lyr.1) %>%
  dplyr::summarise(richness = n())
summary(batrichness)
rm(batranges, batrange_cells)

# turn into batrichness raster
rbatrichness <- terra::subst(blankrast, from = batrichness$lyr.1, 
                             to = batrichness$richness, others = 0)
names(rbatrichness) <- "richness"
plot(rbatrichness, main = "Flower-visiting bat species richness", box = FALSE)
rm(batrichness)
# change aus outline projection to match raster
aus <- terra::project(aus, crs(rangerast$cell_id))
plot(aus, add = TRUE)
# save raster as output
terra::writeRaster(rbatrichness, "data_output/rasters/flvisbat_sprichness_aus.tif",
                   overwrite = TRUE)

# then have to convert this back to df to plot with ggplot
# tried plotting in base R, resolution very bad for some reason
rbatrichnessdf <- as.data.frame(rbatrichness, xy = TRUE) %>%
  na.omit()
head(rbatrichnessdf)
# convert aus to sf for ggplot plotting
aus <- sf::st_as_sf(aus)

# plot using ggplot and R colour scale with Aus coastline
pdf("figures/maps/aus_mean_flower-visiting_bat_richness.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = rbatrichnessdf, aes(x = x, y = y, fill = richness, colour = richness)) +
  scale_fill_gradientn(colours = c("#F2F2F2","#EEBEAC", "#EBB065", "#E7CA24", "#C0DD00", "#74CB01", "#36B700", "#00A600")) +
  scale_colour_gradientn(colours = c("#F2F2F2","#EEBEAC", "#EBB065", "#E7CA24", "#C0DD00", "#74CB01", "#36B700", "#00A600")) +
  geom_sf(data = aus, fill = NA, linewidth = 0.5, colour = "black") +
  theme_void() +
  labs(fill = "Flower-visiting bat\nspecies richness", colour = "Flower-visiting bat\nspecies richness")
dev.off()

# as many zeroes, will just use bat richness as binary i.e. bats or no bats
# reduce raster to 1 (bats present) or 0 (no bats in area)
rbatrichness <- terra::ifel(rbatrichness$richness >=1, 1, 0)
plot(rbatrichness)

# convert back to df to ggplot
rbatrichnessdf <- as.data.frame(rbatrichness, xy = TRUE) %>%
  na.omit()
head(rbatrichnessdf)

# plot using ggplot and custom colour scale with Aus coastline
pdf("figures/maps/aus_mean_flower-visiting_bat_presence.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = rbatrichnessdf, aes(x = x, y = y, fill = richness, colour = richness)) +
  scale_fill_gradientn(colours = c("white", "#9457eb")) +
  scale_colour_gradientn(colours = c("white", "#9457eb")) +
  geom_sf(data = aus, fill = NA, linewidth = 0.5, colour = "black") +
  theme_void() +
  labs(fill = "Flower-visiting\nbat presence", colour = "Flower-visiting\nbat presence")
dev.off()
rm(rbatrichnessdf)

# extract bat presence value for each cleaned eucalypt occurrence
# change projection so raster and vector match
euc_occurr <- terra::project(euc_occurr, crs(rbatrichness))
# check it out
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_batp <- terra::extract(rbatrichness, euc_occurr)

# and join back to data frame
occurrences$batpresence <- euc_occurr_mean_batp[, -1]

# now summarise by species!
species_meanbatpres <- occurrences %>%
  dplyr::group_by(range_names) %>%
  dplyr::summarise(meanbatpres = mean(batpresence, na.rm = TRUE))

# check it out
summary(species_meanbatpres$meanbatpres)
hist(species_meanbatpres$meanbatpres) # more 0s with finer resolution
rm(rbatrichness, euc_occurr_mean_batp)

#### 5 - BIRD RICHNESS #### 

# extract Australian flower-visiting bird ranges from BirdLife International
# range maps (too large to share), then calculate diversity of flower-visiting
# birds across Australia for 100x100km eucalypt range grid

# to get Bird Range maps emailed BirdLife International who provided geodatabase
# of global bird species ranges, too large for R to handle so in QGIS I have: 
#         1) fixed geometries
#         2) clipped ranges to outline of Australia

# below assembles list of Australian flower-visiting bird species (extant
# and extinct), reads in ranges of all Australian birds and filters to only
# flower-visiting Australian birds, then exports ranges for each species as
# separate shapefile to be read in later to create a raster of diversity of
# flower-visiting bird species across Australia

#* list flower-visiting Australian bird species ----

# Table 6 The main flower visiting families of birds (after Proctor & Yeo 1973). 
# Bird family                               Distribution
# COEREBIDAE (Honeycreepers)                Tropical America 
# DICAEIDAE (Flowerpeckers)                 Asia, Australasia
# DREPANIDIDAE (Hawaiian honeycreepers)     Hawaiian Islands
# MELIPHAGIDAE (Sugar-birds)                South Africa
#               (Honey-eaters)              Australasia
# NECTARINIIDAE (Sunbirds)                  South-west Africa, Asia, Philippines, Australasia
# PSITTACIDAE: subfamily Loriinae 
#               (Brush-tongued parakeets)   Australasia
# TROCHILIDAE (Hummingbirds)                North & South America
# ZOSTEROPIDAE (White-eyes)                 Africa, Asia, Australasia
# 
# bird taxa I will want to include (Table 6 from Armstrong 1979, checked
# against ALA taxonomy and descriptions):
#    
# not Dicaeidae - only species is Dicaeum hirundinaceum, mistletoe specialist
# not Ashbyia or Epthianura (MELIPHAGIDAE) - eat invertebrates
# not Zosterops citrinella or Zosterops albogularis - eat berries and insects
# 
# subfamily Meliphaginae, all checked for nectar in diet (some, e.g. Glycichaera 
# fallax, have very little but still some nectar in diet)
# all names those accepted by ALA

flowervisitingbirds <- tibble(species = c("Acanthagenys rufogularis", 
                                              "Acanthorhynchus superciliosus",
                                              "Acanthorhynchus tenuirostris", 
                                              "Anthochaera phrygia",
                                              "Anthochaera chrysoptera", 
                                              "Anthochaera lunulata",
                                              "Anthochaera paradoxa", 
                                              "Anthochaera carunculata",
                                              "Bolemoreus hindwoodi", 
                                              "Bolemoreus frenatus",
                                              "Caligavis chrysops", 
                                              "Certhionyx variegatus",
                                              "Cissomela pectoralis", 
                                              "Conopophila whitei",
                                              "Conopophila albogularis", 
                                              "Conopophila rufogularis",
                                              "Entomyzon cyanotis", 
                                              "Gavicalis virescens",
                                              "Gavicalis fasciogularis", 
                                              "Gavicalis versicolor",
                                              "Glyciphila melanops", 
                                              "Glycichaera fallax",
                                              "Grantiella picta", 
                                              "Lichenostomus cratitius",
                                              "Lichenostomus melanops", 
                                              "Lichmera indistincta",
                                              "Manorina melanocephala", 
                                              "Manorina melanotis",
                                              "Manorina flavigula", 
                                              "Manorina melanophrys",
                                              "Meliphaga notata", 
                                              "Meliphaga lewinii",
                                              "Meliphaga albilineata", 
                                              "Meliphaga gracilis",
                                              "Melithreptus gularis", 
                                              "Melithreptus validirostris",
                                              "Melithreptus albogularis", 
                                              "Melithreptus lunatus",
                                              "Melithreptus affinis", 
                                              "Melithreptus brevirostris",
                                              "Myzomela obscura", 
                                              "Myzomela erythrocephala",
                                              "Myzomela sanguinolenta", 
                                              "Nesoptilotis flavicollis",
                                              "Nesoptilotis leucotis", 
                                              "Philemon buceroides",
                                              "Philemon argenticeps", 
                                              "Philemon corniculatus",
                                              "Philemon citreogularis", 
                                              "Phylidonyris pyrrhopterus",
                                              "Phylidonyris novaehollandiae", 
                                              "Phylidonyris niger",
                                              "Plectorhyncha lanceolata", 
                                              "Ptilotula plumula",
                                              "Ptilotula penicillata", 
                                              "Ptilotula ornata", 
                                              "Ptilotula keartlandi", 
                                              "Ptilotula fusca",
                                              "Ptilotula flavescens", 
                                              "Purnella albifrons",
                                              "Ramsayornis fasciatus", 
                                              "Ramsayornis modestus",
                                              "Stomiopera unicolor", 
                                              "Stomiopera flava",
                                              "Sugomel nigrum", 
                                              "Trichodere cockerelli",
                                              "Xanthotis flaviventer", 
                                              "Xanthotis macleayanus"))
flowervisitingbirds$family <- "Meliphagidae"

flowervisitingbirds <- dplyr::bind_rows(flowervisitingbirds, 
                                        tibble(species = "Nectarinia jugularis",
                                                   family = "Nectariniidae"))
flowervisitingbirds <- dplyr::bind_rows(flowervisitingbirds, 
                                        tibble(species = c("Cyclopsitta diophthalma", 
                                                               "Glossopsitta concinna",
                                                               "Parvipsitta porphyrocephala",
                                                               "Parvipsitta pusilla", 
                                                               "Psitteuteles versicolor",
                                                               "Trichoglossus haematodus", 
                                                               "Trichoglossus chlorolepidotus"),
                                                   family = "Psittacidae"))

flowervisitingbirds <- dplyr::bind_rows(flowervisitingbirds, 
                                        tibble(species = c("Zosterops tenuirostris", # Norfolk Island
                                                               "Zosterops natalis", # Christmas Island
                                                               "Zosterops luteus",
                                                               "Zosterops lateralis"),
                                                   family = "Zosteropidae"))

# now add in data from Australian bird database, Garnett et al. 2015
ausbirds <- readxl::read_xlsx("data_input/Australian_Bird_Data_Version_1.xlsx",
                              guess_max = 2056)

# reduce to columns of interest
ausbirds <- ausbirds %>%
  dplyr::filter(`7_Taxon_scientific_name_CandB_2` != "NA") %>% # filter out subspecies (NA in species column)
  # then filter out non-native birds (not important in evolutionary time)
  dplyr::filter(!(`29_Population_description_4` %in% c("Vagrant", 
                                                       "Taxonomy uncertain",
                                                       "Hoax, fraud or mislabelled",
                                                       "No confirmed record",
                                                       "No record",
                                                       "Introduced",
                                                       "Failed introduction",
                                                       "Vagrant (from introduction to NZ)"))) %>%
  dplyr::filter(`164_Food_Nectar_or_pollen_10` %in% c(1, "NA")) %>% # filter to only flower-visiting birds and NAs
  dplyr::filter(!(`10_Family_scientific_name_2` %in% c("Casuariidae", # filter out families highly unlikely to be pollinators
                                                       "Procellariidae", #inc many seabirds
                                                       "Rallidae",
                                                       "Diomedeidae",
                                                       "Spheniscidae",
                                                       "Sulidae",
                                                       "Anatidae",
                                                       "Fregatidae",
                                                       "Oceanitidae",
                                                       "Phaethontidae",
                                                       "Laridae",
                                                       "Acanthizidae"))) %>%
  dplyr::select(species = `7_Taxon_scientific_name_CandB_2`,
                commonname = `3_Taxon_common_name_2`,
                family = `10_Family_scientific_name_2`,
                familycommon = `9_Family_common_name_2`,
                nectarpollen = `164_Food_Nectar_or_pollen_10`)

# have checked all taxa with "NA" for nectar/pollen in diet, many extinct and
# distributed on islands but possibility they would have acted as pollinators 
# so will leave in this list for now

# check if any of my above species missing from this list
sum(flowervisitingbirds$species %in% ausbirds$species)
mismatches <- flowervisitingbirds[!(flowervisitingbirds$species %in% ausbirds$species),]
# taxa that don't match look like taxonomic synonyms 
# e.g. Lichenostomus instead of Gavicalis in database
# will leave these in my final list (not sure what taxonomy BirdLife follow exactly)
rm(mismatches)

flowervisitingbirds <- dplyr::mutate(flowervisitingbirds, nectarpollen = "1")

ausflowervisitingbirds <- ausbirds %>%
  dplyr::full_join(flowervisitingbirds, by = c("species", "family", "nectarpollen"))
rm(ausbirds, flowervisitingbirds)

# fix mispellings and synonymies
ausflowervisitingbirds$species[ausflowervisitingbirds$species == "Glyciphila melanops"] <- "Gliciphila melanops"
ausflowervisitingbirds$species[ausflowervisitingbirds$species == "Meliphaga gracilis"] <- "Microptilotis gracilis"
ausflowervisitingbirds$species[ausflowervisitingbirds$species == "Meliphaga albilineata"] <- "Microptilotis albilineatus"

# add synonym for Nectarinia jugularis to list to match BirdLife data
ausflowervisitingbirds <- ausflowervisitingbirds %>%
  tibble::add_row(species = "Cinnyris jugularis", family = "Nectarinidae")

# export full list in case needed
readr::write_csv(ausflowervisitingbirds, "data_output/aus_flower_visiting_birds.csv")

#* filter ranges shapefile ----

# read in massive shapefile for all bird species ranges across Australia
# Birds of the World by Birdlife International
# and filter down to only flower-visiting taxa

ausbirdranges <- sf::st_read("data_cache/BOTW_ausonly.shp")

ausbirdrangestaxa <- ausbirdranges %>%
  sf::st_drop_geometry() %>%
  dplyr::select(sci_name) %>%
  dplyr::distinct()

# check how much overlap with flower-visiting bird names (taxonomic problems?)
print(paste(sum(ausflowervisitingbirds$species %in% ausbirdrangestaxa$sci_name), 
            "species of flower-visiting birds have matching range maps from BirdLife International"))

#what names don't match? 
mismatches <- ausflowervisitingbirds[!(ausflowervisitingbirds$species %in% ausbirdrangestaxa$sci_name),]
# many appear to be taxonomic mismatches that will be covered by use of 
# alternative names e.g. Lichenostomus, or extinct or rare island taxa
# have individually checked all and patched names as required above
# 101 species of flower-visiting birds across Australia!

# filter range maps down to just these 101 taxa
ausbirdranges <- ausbirdranges %>%
  dplyr::filter(sci_name %in% ausflowervisitingbirds$species)

# export one shapefile for each species' range to use next
ausbirdranges %>%
  group_by(sci_name) %>%
  nest() %>%
  mutate( 
    txt = walk2(.x = data, 
                .y = sci_name, 
                ~st_write(obj = .x, 
                          dsn = paste0("data_cache/ausflvisbirdliferanges/", 
                                       .y, 
                                       ".shp"),
                          append = FALSE)))

rm(ausbirdrangestaxa, ausbirdranges, ausflowervisitingbirds, mismatches)

#* prep raster flower-visiting bird diversity ----

# first read in range maps for Australian flower visiting bird species
# as per above assembled species list
files <- list.files(path = "data_cache/ausflvisbirdliferanges", 
                    pattern = ".shp$", full.names = TRUE)
birdranges <- lapply(files, FUN = terra::vect)
files <- gsub("data_cache/ausflvisbirdliferanges/", "", files)
files <- gsub("\\.shp", "", files)
names(birdranges) <- files
rm(files)
# make sure projections same as Euc ranges raster
for(name in names(birdranges)) {
  birdranges[[name]] <- terra::project(birdranges[[name]], 
                                       crs(rangerast$cell_id))
}
rm(name)

# test plotting a few different bird ranges to check shapefiles
plot(blankrast)
plot(birdranges$`Acanthagenys rufogularis`, add = TRUE) # central Aus
plot(birdranges$`Xanthotis flaviventer`, add = TRUE) # tropical north Qld
# looks good!

# now, extract blank 20x20km raster cell ID for each bird range map
birdrange_cells <- tibble::tibble()
for(name in names(birdranges)) {
  temp <- terra::extract(x = blankrast, y = birdranges[[name]])
  temp$species <- name
  birdrange_cells <- rbind(birdrange_cells, temp)
}
rm(temp, name)
# filter out NAs and remove unnecessary ID column
birdrange_cells <- birdrange_cells %>%
  dplyr::select(-ID) %>%
  dplyr::filter(!(is.na(lyr.1)))

# prepare map figure of flower-visiting bird species richness across Aus
# calculate fl-vis bird richness per grid cell 
birdrichness <- birdrange_cells %>%
  dplyr::group_by(lyr.1) %>%
  dplyr::summarise(richness = n())
summary(birdrichness)

# turn into birdrichness raster
rbirdrichness <- terra::subst(blankrast, from = birdrichness$lyr.1, 
                              to = birdrichness$richness, others = 0)
names(rbirdrichness) <- "richness"
plot(rbirdrichness)
# save raster as output
terra::writeRaster(rbirdrichness, "data_output/rasters/flvisbird_sprichness_aus.tif",
                   overwrite = TRUE)
# then have to convert this back to df to plot with ggplot
rbirdrichness <- as.data.frame(rbirdrichness, xy = TRUE) %>%
  na.omit()
head(rbirdrichness)

# plot using ggplot and viridis colour scale with Aus coastline
pdf("figures/maps/aus_mean_flower-visiting_bird_richness.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = rbirdrichness, aes(x = x, y = y, fill = richness, colour = richness)) +
  scale_fill_gradientn(colours = c("#F2F2F2","#EEBEAC", "#EBB065", "#E7CA24", "#C0DD00", "#74CB01", "#36B700", "#00A600")) +
  scale_colour_gradientn(colours = c("#F2F2F2","#EEBEAC", "#EBB065", "#E7CA24", "#C0DD00", "#74CB01", "#36B700", "#00A600")) +
  geom_sf(data = aus, fill = NA, linewidth = 0.5, colour = "black") +
  theme_void() +
  labs(fill = "Flower-visiting bird\nspecies richness", colour = "Flower-visiting bird\nspecies richness")
dev.off()
rm(birdranges, birdrange_cells)

# now calculate mean richness of fl-vis birds in landcsape per species
# get raster back
# turn into batrichness raster
rbirdrichness <- terra::subst(blankrast, from = birdrichness$lyr.1, 
                              to = birdrichness$richness, others = 0)
names(rbirdrichness) <- "richness"
rm(birdrichness)

# extract birdrichness value for each cleaned eucalypt occurrence
# check it out
plot(rbirdrichness)
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_fvbirdr <- terra::extract(rbirdrichness, euc_occurr)

# and join back to data frame
occurrences$birdrich <- euc_occurr_mean_fvbirdr[, -1]

# now summarise by species!
species_meanbirdrich <- occurrences %>%
  dplyr::group_by(range_names) %>%
  dplyr::summarise(meanbirdrich = mean(birdrich, na.rm = TRUE))

# check it out
summary(species_meanbirdrich$meanbirdrich)
hist(species_meanbirdrich$meanbirdrich)
rm(rbirdrichness, euc_occurr_mean_fvbirdr, euc_occurr, aus, blankrast)

#### FINAL SP MEAN ENV ####

# join all together and rename column
spmean_env <- species_meanAVP %>%
  dplyr::left_join(species_meanMAT, by = "range_names") %>%
  dplyr::left_join(species_meanMAP, by = "range_names") %>%
  dplyr::left_join(species_meanbatpres, by = "range_names") %>%
  dplyr::left_join(species_meanbirdrich, by = "range_names")

# add in median longitude of each eucalypt species
medianlong <- occurrences %>%
  dplyr::group_by(range_names) %>%
  dplyr::summarise(medianlong = median(longitude, na.rm = TRUE))
spmean_env <- spmean_env %>%
  dplyr::left_join(medianlong, by = "range_names")

# export this to use in final data!!!
readr::write_csv(spmean_env, "data_output/euc_species_mean_env.csv")

# export eucalypt herbarium records with associated climate and pollinator data
readr::write_csv(occurrences, "data_output/occurrence_data_with_environment.csv")

rm(species_meanAVP, species_meanMAP, species_meanMAT, species_meanbatpres,
   species_meanbirdrich, medianlong, occurrences)

})

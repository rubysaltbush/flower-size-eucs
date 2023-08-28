# script to get environmental data rasters and calculate species means using
# cleaned eucalypt occurrence data, for: 
#                     1) soil available phosphorus
#                     2) mean annual temperature
#                     3) mean annual precipitation
#                     4) flower-visiting bat species richness and 
#                     5) flower-visiting bird species richness

# resulting species mean file has been cached because original data too large to
# share over GitHub etc, but below script shows steps used to extract data

# MULTIPLE FILES MISSING FROM CURRENT DATA CACHE, COPY ACROSS WEDNESDAY

spmean_env <- cache_RDS("data_output/euc_species_mean_env.csv", 
                        read_function = readr::read_csv,
                        save_function = write_csv, function() {

#### 1 - PHOSPHORUS ####                        
# read in 3 rasters of available phosphorus across Australia (90m resolution)
# from the Soil and Landscape Grid of Australia (SLGA)
# https://esoil.io/TERNLandscapes/Public/Pages/SLGA/index.html
# and take 1 mean of top 30cm, where P most available to plants

# available soil phosphorus #
# read in 3 rasters downloaded from SLGA using terra stack function
# FILES IN DATA CACHE AS TOO LARGE FOR GITHUB
avp <- terra::rast(list.files("data_cache/AVP_EV_N_P_AU_TRN_N_20220826/", 
                              full.names = TRUE))

# check them out
plot(avp)

# take mean of all depths
mean_avp <- terra::app(avp, fun = mean)

# how does this look?
plot(mean_avp)
# generally higher along east coast but a bit variable

# export this raster to use in analysis!
terra::writeRaster(mean_avp, "data_cache/mean_AVP_0to30cm_SLGA.tiff")
# because it's huge have to write to cache, can't sync via GitHub

# now calculate mean AVP for each species by intersecting occurrences
# with mean modelled available phosphorus for Australia at 0-30cm metres depth

# extract available phosphorus value for each cleaned eucalypt occurrence
# first make occurrences spatial
euc_occurr <- terra::vect(occurrences, geom = c("longitude", "latitude"), 
                          crs = crs(mean_avp))
# check it out
plot(euc_occurr)

# now extract!
euc_occurr_mean_avp <- terra::extract(mean_avp, euc_occurr)

# and join back to data frame
occurrences$mean_AVP <- euc_occurr_mean_avp[, -1]

# now summarise by species!
species_meanAVP <- occurrences %>%
  dplyr::group_by(speciesLevelUpdated) %>%
  dplyr::summarise(meanAVP = mean(mean_AVP, na.rm = TRUE))

# check it out
summary(species_meanAVP$meanAVP)
hist(species_meanAVP$meanAVP)

rm(mean_avp, avp, euc_occurr_mean_avp, euc_occurr)

#### 2,3 - TEMPERATURE AND PRECIPITATION ####  

# using occurrence data and provided Mean Annual Temperature and Mean Annual
# Precipitation calculate MAT and MAP per species as well

# for now use Bree's provided MAT and MAP (from CHELSA, checking with Bree which version etc)

# summarise MAT by species!
species_meanMAT <- occurrences %>%
  dplyr::group_by(speciesLevelUpdated) %>%
  dplyr::summarise(meanMAT = mean(MAT, na.rm = TRUE))
hist(species_meanMAT$meanMAT)

# summarise MAP by species!
species_meanMAP <- occurrences %>%
  dplyr::group_by(speciesLevelUpdated) %>%
  dplyr::summarise(meanMAP = mean(MAP, na.rm = TRUE))
hist(species_meanMAP$meanMAP)

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

# turn into batrichness raster
rbatrichness <- terra::subst(blankrast, from = batrichness$lyr.1, 
                             to = batrichness$richness, others = 0)
names(rbatrichness) <- "richness"
plot(rbatrichness)
# save raster as output
terra::writeRaster(rbatrichness, "data_output/rasters/flvisbat_sprichness_aus.tif",
                   overwrite = TRUE)
# then have to convert this back to df to plot with ggplot
rbatrichness <- as.data.frame(rbatrichness, xy = TRUE) %>%
  na.omit()
head(rbatrichness)

# outline of Aus for plotting
# read in shapefile of Aus outline
aus <- terra::vect("data_input/Aus_outline.shp")
# bit slow and over complex, simplify to make faster
aus <- terra::simplifyGeom(aus)
# crop to remove Norfolk and Christmas Islands from outline
aus <- terra::crop(aus, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# change aus outline projection to match raster
aus <- terra::project(aus, crs(rangerast$cell_id))
# convert aus to sf for easy plotting
aus <- sf::st_as_sf(aus)

# plot using ggplot and viridis colour scale with Aus coastline
ggplot() +
  geom_tile(data = rbatrichness, aes(x = x, y = y, fill = richness)) +
  scale_fill_viridis_c() +
  geom_sf(data = aus, fill = NA, linewidth = 0.5, colour = "grey") +
  theme_void() +
  labs(fill = "Flower-visiting bat\nspecies richness")
ggsave("figures/maps/flvis bat species richness Aus.png", width = 11, height = 6, units = "in")
# export as png for now
rm(batranges, batrange_cells)

# now calculate mean richness of bats in landcsape per species
# get raster back
# turn into batrichness raster
rbatrichness <- terra::subst(blankrast, from = batrichness$lyr.1, 
                             to = batrichness$richness, others = 0)
names(rbatrichness) <- "richness"
rm(batrichness)

# extract batrichness value for each cleaned eucalypt occurrence
# first make occurrences spatial
euc_occurr <- terra::vect(occurrences, geom = c("longitude", "latitude"), 
                          crs = "epsg:4326")
# change projection so raster and vector match
euc_occurr <- terra::project(euc_occurr, crs(rbatrichness))
# check it out
plot(rbatrichness)
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_batr <- terra::extract(rbatrichness, euc_occurr)

# and join back to data frame
occurrences$batrich <- euc_occurr_mean_batr[, -1]

# now summarise by species!
species_meanbatrich <- occurrences %>%
  dplyr::group_by(speciesLevelUpdated) %>%
  dplyr::summarise(meanbatrich = mean(batrich, na.rm = TRUE))

# check it out
summary(species_meanbatrich$meanbatrich)
hist(species_meanbatrich$meanbatrich) # more 0s with finer resolution
rm(rbatrichness, euc_occurr_mean_batr)

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

flowervisitingbirds <- data_frame(species = c("Acanthagenys rufogularis", 
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
                                        data_frame(species = "Nectarinia jugularis",
                                                   family = "Nectariniidae"))
flowervisitingbirds <- dplyr::bind_rows(flowervisitingbirds, 
                                        data_frame(species = c("Cyclopsitta diophthalma", 
                                                               "Glossopsitta concinna",
                                                               "Parvipsitta porphyrocephala",
                                                               "Parvipsitta pusilla", 
                                                               "Psitteuteles versicolor",
                                                               "Trichoglossus haematodus", 
                                                               "Trichoglossus chlorolepidotus"),
                                                   family = "Psittacidae"))

flowervisitingbirds <- dplyr::bind_rows(flowervisitingbirds, 
                                        data_frame(species = c("Zosterops tenuirostris", # Norfolk Island
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
  # first filter out non-native birds (not important in evolutionary time)
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
# taxa that don't match look like taxonomic mismatches 
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
# ONLY AVAILABLE FROM CACHED DATA
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
                                       ".shp"))))

rm(ausbirdrangestaxa, ausbirdranges, ausbirds, ausflowervisitingbirds, mismatches)

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
ggplot() +
  geom_tile(data = rbirdrichness, aes(x = x, y = y, fill = richness)) +
  scale_fill_viridis_c() +
  geom_sf(data = aus, fill = NA, linewidth = 0.5, colour = "grey") +
  theme_void() +
  labs(fill = "Flower-visiting bird\nspecies richness")
ggsave("figures/maps/flvis bird species richness Aus.png", width = 11, height = 6, units = "in")
# export as png for now
rm(birdranges, birdrange_cells)

# now calculate mean richness of fl-vis birds in landcsape per species
# get raster back
# turn into batrichness raster
rbirdrichness <- terra::subst(blankrast, from = birdrichness$lyr.1, 
                              to = birdrichness$richness, others = 0)
names(rbirdrichness) <- "richness"
rm(birdrichness)

# extract birdrichness value for each cleaned eucalypt occurrence
# first make occurrences spatial
euc_occurr <- terra::vect(occurrences, geom = c("longitude", "latitude"), 
                          crs = "epsg:4326")
# change projection so raster and vector match
euc_occurr <- terra::project(euc_occurr, crs(rbirdrichness))
# check it out
plot(rbirdrichness)
plot(euc_occurr, add = TRUE)

# now extract!
euc_occurr_mean_fvbirdr <- terra::extract(rbirdrichness, euc_occurr)

# and join back to data frame
occurrences$birdrich <- euc_occurr_mean_fvbirdr[, -1]

# now summarise by species!
species_meanbirdrich <- occurrences %>%
  dplyr::group_by(speciesLevelUpdated) %>%
  dplyr::summarise(meanbirdrich = mean(birdrich, na.rm = TRUE))

# check it out
summary(species_meanbirdrich$meanbirdrich)
hist(species_meanbirdrich$meanbirdrich)
rm(rbirdrichness, euc_occurr_mean_fvbirdr, euc_occurr, aus, blankrast)

#### FINAL SP MEAN ENV ####

# join all together and rename column
spmean_env <- species_meanAVP %>%
  dplyr::left_join(species_meanMAT, by = "speciesLevelUpdated") %>%
  dplyr::left_join(species_meanMAP, by = "speciesLevelUpdated") %>%
  dplyr::left_join(species_meanbatrich, by = "speciesLevelUpdated") %>%
  dplyr::left_join(species_meanbirdrich, by = "speciesLevelUpdated") %>%
  dplyr::rename(range_names = speciesLevelUpdated)

# export this to use in final data!!!
readr::write_csv(spmean_env, "data_output/euc_species_mean_env.csv")

rm(species_meanAVP, species_meanMAP, species_meanMAT, species_meanbatrich,
   species_meanbirdrich, euc_occurr, aus)

})

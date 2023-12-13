# script to extract and clean insect pollinator occurrence records from the 
# Atlas of Living Australia (ALA), intersect them with a grid raster and 
# calculate species richness of insect pollinators for each grid cell

library(galah)

# get records for all possible pollinator insects across Australia from ALA
insectpollrecords <- cache_RDS("data_cache/insectpollinatorrecordsAus.csv", 
                                read_function = readr::read_csv,
                                save_function = write_csv, function() {

# to start with, build list of insect families from Armstrong (1979)
# known to act as pollinators in Australia

insectpolls <- c("Apidae", # all bee families
                 "Colletidae",
                 "Halictidae",
                 "Megachilidae",
                 "Stenotritidae",
                 "Scoliidae", # wasp families mentioned as pollinators in Armstrong (1979)
                 "Tiphiidae",
                 "Vespoidea",
                 "Ichneumonidae",
                 "Pergidae", # main sawfly family mentioned in Armstrong (1979)
                 "Lepidoptera", # all Lepidoptera
                 "Buprestidae", # Coleoptera families from Table 3 in Armstrong (1979)
                 "Cantharidae", # inc. exclusively and commonly anthophilous
                 "Melyridae", # but not occasionally/rarely anthophilous families
                 "Cleridae",
                 "Languriinae", # now subfamily
                 "Mordellidae",
                 "Oedemeridae",
                 "Phalacridae",
                 "Ripiphoridae", # spelling changed
                 "Scraptiidae",
                 "Dermestidae",
                 "Throscidae", # synonym Trixagidae
                 "Anisopodidae", # Diptera families from Table 4 in Armstrong (1979)
                 "Bibionidae",
                 "Cecidomyiidae",
                 "Mycetophilidae",
                 "Scatopsidae",
                 "Sciaridae",
                 "Ceratopogonidae",
                 "Chironomidae",
                 "Culicidae",
                 "Simuliidae",
                 "Simuliidae",
                 "Psychodidae",
                 "Tipulidae",
                 "Lonchopteridae",
                 "Platypezidae",
                 "Phoridae",
                 "Pipunculidae",
                 "Syrphidae",
                 "Conopidae",
                 "Chyromyidae",
                 "Neurochaetidae",
                 "Drosophilidae",
                 "Ephydridae",
                 "Chloropidae",
                 "Chloropidae",
                 "Milichiidae",
                 "Calliphoridae",
                 "Muscidae",
                 "Sarcophagidae",
                 "Tachinidae",
                 "Lauxaniidae",
                 "Pyrgotidae",
                 "Tephritidae",
                 "Apioceridae",
                 "Asilidae",
                 "Bombyliidae",
                 "Mydidae", #misspelled in Armstrong
                 "Therevidae",
                 "Dolichopodidae",
                 "Empididae",
                 "Acroceridae",
                 "Nemestrinidae",
                 "Nemestrinidae",
                 "Pelecorhynchidae",
                 "Rhagionidae",
                 "Stratiomyidae",
                 "Tabanidae"
                 )

# make sure galah getting data from ALA
galah::galah_config(atlas = "ALA")

# check all listed pollinators in ALA taxa (checked as made list but just to be sure)
galah::search_taxa(insectpolls)
# all seem to return a direct match

# check how many records match these names
galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 %in% c("New South Wales", "Victoria", "Queensland",
                           "South Australia", "Western Australia",
                           "Northern Territory", "Australian Capital Territory", 
                           "Tasmania")) %>%
  atlas_counts()
# 2081100 records, that's a few.

# configure galah to have my email for API access
galah_config(email = "stephenseruby@gmail.com", verbose = TRUE)

# download all filtered ALA insect pollinator records
# have to do state by state as too many records at once for galah otherwise
insectpollrecords <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 %in% c("Tasmania", "Victoria")) %>%
  atlas_occurrences()

insectpollrecords2 <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 == "New South Wales") %>%
  atlas_occurrences()

insectpollrecords3 <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 == "Queensland") %>%
  atlas_occurrences()

insectpollrecords4 <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 == "South Australia") %>%
  atlas_occurrences()

insectpollrecords5 <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 == "Western Australia") %>%
  atlas_occurrences()

insectpollrecords6 <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 == "Northern Territory") %>%
  atlas_occurrences()

insectpollrecords7 <- galah_call() %>%
  galah_identify(insectpolls) %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 == "Australian Capital Territory") %>%
  atlas_occurrences()

# applying ALA filter excludes many iffy records, as does restricting records 
# to Australian states

# bind all records together into one df
insectpollrecords <- insectpollrecords %>%
  dplyr::bind_rows(insectpollrecords2,
                   insectpollrecords3,
                   insectpollrecords4,
                   insectpollrecords5,
                   insectpollrecords6,
                   insectpollrecords7)
rm(insectpollrecords2, insectpollrecords3, insectpollrecords4,
   insectpollrecords5, insectpollrecords6, insectpollrecords7, insectpolls)

# write to data cache in case need to reload later
readr::write_csv(insectpollrecords, "data_cache/insectpollinatorrecordsAus.csv")
})

# extract number of pollinator species per 100x100km cell across Aus
# first make pollinator records a terra spatial vector
insectpollspatial <- terra::vect(insectpollrecords, 
                             geom = c("decimalLongitude", "decimalLatitude"),
                             crs = "EPSG:4326")

# then re-project in same crs as euc raster (important!)
insectpollspatial <- terra::project(insectpollspatial, crs(rangerast$cell_id))
plot(rangerast$cell_id)
plot(insectpollspatial, add = TRUE)

# then extract raster cell value for each point!
pollinatormap <- terra::extract(x = rangerast$cell_id, y = insectpollspatial)
rm(insectpollspatial)

# join back to records
insectpollrecords <- insectpollrecords %>%
  dplyr::mutate(ID = row_number()) %>%
  dplyr::left_join(pollinatormap, by = "ID")
rm(pollinatormap)

# NOW, can calculate species richness per cell of raster
insectpollrichness <- insectpollrecords %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(richness = n_distinct(scientificName, na.rm = TRUE))
rm(insectpollrecords)
# make sure "NA" cells (values in ocean) retained as NA/0 species richness
insectpollrichness[is.na(insectpollrichness$cell_id),2] <- NA

# turn into raster
InsectPollinatorRichness <- terra::subst(rangerast$cell_id, 
                               from = insectpollrichness$cell_id, 
                               to = insectpollrichness$richness,
                               others = 0)
# looks like 100 cells have no records in them, across north-central-west Aus
names(InsectPollinatorRichness) <- "richness"
# NA 
plot(InsectPollinatorRichness)
plot(log(InsectPollinatorRichness))

# save raster as output
terra::writeRaster(InsectPollinatorRichness, 
                   "data_output/rasters/insect_pollinator_richness_aus.tif",
                   overwrite = TRUE)

# map figure ----

# read in shapefile of Aus outline
aus <- terra::vect("data_input/Aus_outline.shp")
# bit slow and over complex, simplify to make faster
aus <- terra::simplifyGeom(aus, tolerance = 0.01)
# crop to remove Norfolk and Christmas Islands from outline
aus <- terra::crop(aus, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# change aus outline projection to match raster
aus <- terra::project(aus, crs(rangerast$cell_id))
# convert aus to sf for easy plotting
aus <- sf::st_as_sf(aus)

# convert raster back to df to plot with ggplot
InsectPollinatorRichnessdf <- as.data.frame(InsectPollinatorRichness, xy = TRUE) %>%
  na.omit()
head(InsectPollinatorRichnessdf)
# log transform for better visualisation
InsectPollinatorRichnessdf$richness <- log(InsectPollinatorRichnessdf$richness)

# plot using ggplot and viridis colour scale with Aus coastline
pdf("figures/maps/insectpollinator_species_richness_map.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = InsectPollinatorRichnessdf, aes(x = x, y = y, fill = richness)) +
  scale_fill_viridis_c() + #breaks = c(10, 50, 100, 150)
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "grey") +
  theme_void() +
  labs(fill = "Insect pollinator\nspecies richness (log)")
dev.off()

# can fine tune in illustrator to display raw numbers on scale
rm(InsectPollinatorRichnessdf, insectpollrichness, InsectPollinatorRichness, aus)

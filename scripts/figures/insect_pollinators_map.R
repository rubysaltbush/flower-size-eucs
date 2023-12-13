# script to extract and clean insect pollinator occurrence records from the 
# Atlas of Living Australia (ALA), intersect them with a grid raster and 
# calculate species richness of insect pollinators for each grid cell

library(galah)
library(CoordinateCleaner)

# to start with, build list of insect families from Armstrong (1979)
# known to act as pollinators in Australia

# OR try just getting all records for Apidae in Aus to start with
# (bees are messed as a clade in ALA)

# make sure galah getting data from ALA
galah::galah_config(atlas = "ALA")

# check Apidae in ALA taxa
galah::search_taxa("Apidae")

# check how many records match these names
galah_call() %>%
  galah_identify("Apidae") %>%
  atlas_counts()
# 56870, 10k more than web search on ala.org.au shows

# configure galah to have my email for API access
galah_config(email = "stephenseruby@gmail.com")

# download all filtered ALA Apidae records
apidaerecords <- galah_call() %>%
  galah_identify("Apidae") %>%
  galah_apply_profile(ALA) %>% # filters based on several conditions, excludes presence-only records, eDNA and fossil records, records before 1700, duplicates, uncertain coordinates
  galah_filter(cl22 %in% c("New South Wales", "Victoria", "Queensland",
                           "South Australia", "Western Australia",
                           "Northern Territory", "Australian Capital Territory", 
                           "Tasmania")) %>%
  atlas_occurrences()

# applying ALA filter excludes many iffy records, as does restricting records 
# to Australian states -> 42430 records

# next step - essentially I need to get a raster, count how many species records
# intersect with each cell of raster
# actually thinking about it I'll need a coarse resolution raster for this
# might as well use euc ranges raster and intersect this with apidae records

plot(rangerast$cell_id)

# extract number of species per cell
# first make apidae records a terra spatial vector
apidaespatial <- terra::vect(apidaerecords, 
                             geom = c("decimalLongitude", "decimalLatitude"),
                             crs = "EPSG:4326")
plot(apidaespatial)

# then re-project in same crs as euc raster (important!)
apidaespatial <- terra::project(apidaespatial, crs(rangerast$cell_id))
plot(rangerast$cell_id)
plot(apidaespatial, add = TRUE)

# then extract raster cell value for each point!
apidaemap <- terra::extract(x = rangerast$cell_id, y = apidaespatial)

# join back to records
apidaerecords <- apidaerecords %>%
  dplyr::mutate(ID = row_number()) %>%
  dplyr::left_join(apidaemap, by = "ID")
rm(apidaemap)

# NOW, can calculate species richness per cell of raster
apidaerichness <- apidaerecords %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(richness = n_distinct(scientificName, na.rm = TRUE))

# turn into raster
ApidaeRichness <- terra::subst(rangerast$cell_id, 
                               from = apidaerichness$cell_id, 
                               to = apidaerichness$richness,
                               others = 0)
names(ApidaeRichness) <- "richness"
plot(ApidaeRichness)

# it works!!! now to try for a whole host of insect families
# and then make prettier map

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
ApidaeRichnessdf <- as.data.frame(ApidaeRichness, xy = TRUE) %>%
  na.omit()
head(ApidaeRichnessdf)

# plot using ggplot and viridis colour scale with Aus coastline
#pdf("figures/maps/insectpollinator_species_richness_map.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = ApidaeRichnessdf, aes(x = x, y = y, fill = richness)) +
  scale_fill_viridis_c() + #breaks = c(10, 50, 100, 150)
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "grey") +
  theme_void() +
  labs(fill = "Apidae species richness")
#dev.off()



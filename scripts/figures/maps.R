# script to produce maps of eucalypt traits by their ranges, especially
# mean bud size (log mm2) across Australia as per Figure 1a

### make pretty maps ####
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

#* log bud size ----
# prep log bud size data
logbudsize_mm2 <- euc_traits_nosubsp %>%
  dplyr::select(apc_nosubsp, logbudsize_mm2) %>%
  dplyr::filter(!is.na(logbudsize_mm2))

logbudsize_mm2 <- merge(logbudsize_mm2, rangerast$taxa, by.x = "apc_nosubsp", 
                        by.y = "taxon_name", all.x = FALSE)

logbudsize_mm2 <- logbudsize_mm2 %>%
  merge(rangerast$cell_taxa, by = "taxon_id", all.x = FALSE)

# calculate mean per grid cell
meanlogBudSize <- logbudsize_mm2 %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(mean_bud_size = mean(logbudsize_mm2))
summary(meanlogBudSize)

# match to ranges raster
rlogBudSize <- terra::subst(rangerast$cell_id, from = meanlogBudSize$cell_id, 
                            to = meanlogBudSize$mean_bud_size)
names(rlogBudSize) <- "mean_bud_size"
plot(rlogBudSize)

# save raster as output
terra::writeRaster(rlogBudSize, "data_output/rasters/aus_mean_log_euc_bud_size.tif",
                   overwrite = TRUE)

# convert raster back to df to plot with ggplot
rlogBudSize <- as.data.frame(rlogBudSize, xy = TRUE) %>%
  na.omit()
head(rlogBudSize)

# plot using ggplot and custom colour scale with Aus coastline
pdf("figures/maps/mean_euc_budsize_logmm2_map.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = rlogBudSize, aes(x = x, y = y, fill = mean_bud_size)) +
  scale_fill_gradientn(colours = my_colours$budsz) +
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "darkgrey") +
  theme_void() +
  labs(fill = "Mean eucalypt\nbud size (log mm²)")
dev.off()

rm(rlogBudSize, logbudsize_mm2, meanlogBudSize)

#* flower colourfulness ----
# prep flower colour data
colour_bin <- euc_traits_nosubsp %>%
  dplyr::select(apc_nosubsp, colour_binary) %>%
  dplyr::filter(!is.na(colour_binary))
colour_bin <- merge(colour_bin, rangerast$taxa, by.x = "apc_nosubsp", 
                    by.y = "taxon_name", all.x = FALSE) 
# lose 11 taxa missing range data, total n = 778
colour_bin <- colour_bin %>%
  merge(rangerast$cell_taxa, by = "taxon_id", all.x = FALSE)

# calculate mean per grid cell
meanColour <- colour_bin %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(mean_colour = mean(as.numeric(as.character(colour_binary))))
summary(meanColour)

# match to ranges raster
rColour <- terra::subst(rangerast$cell_id, from = meanColour$cell_id, 
                        to = meanColour$mean_colour)
names(rColour) <- "mean_colour"
plot(rColour)

# save raster as output
terra::writeRaster(rColour, "data_output/rasters/aus_mean_euc_flower_colourfulness.tif",
                   overwrite = TRUE)

# convert raster to df to plot with ggplot
rColour <- as.data.frame(rColour, xy = TRUE) %>%
  na.omit()
head(rColour)

# plot using ggplot and custom colour scale with Aus coastline
pdf("figures/maps/mean_euc_flower_colourfulness_map.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = rColour, aes(x = x, y = y, fill = mean_colour)) +
  scale_fill_gradientn(colours = my_colours$flcol_cont) +
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "darkgrey") +
  theme_void() +
  labs(fill = "Mean eucalypt flower colourfulness\n(0 = white-cream, 1 = colourful)")
dev.off()

rm(rColour, colour_bin, meanColour)

#* species richness ----
# calculate richness of euc species across Australia
richness <- rangerast$cell_taxa %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(richness = n())
summary(richness)
# this includes a few species not in our analysis as no bud size or flower
# colour data available for them, but broad patterns very similar

# turn into raster
rRichness <- terra::subst(rangerast$cell_id, from = richness$cell_id, 
                          to = richness$richness)
names(rRichness) <- "richness"
plot(rRichness)

# save raster as output
terra::writeRaster(rRichness, "data_output/rasters/euc_sp_richness_aus.tif",
                   overwrite = TRUE)
# then have to convert this back to df to plot with ggplot
rRichness <- as.data.frame(rRichness, xy = TRUE) %>%
  na.omit()
head(rRichness)

# plot using ggplot and viridis colour scale with Aus coastline
pdf("figures/maps/euc_species_richness_map.pdf", width = 10, height = 10)
ggplot() +
  geom_tile(data = rRichness, aes(x = x, y = y, fill = richness, colour = richness)) +
  scale_fill_viridis_c(breaks = c(10, 50, 100, 150)) +
  scale_colour_viridis_c(breaks = c(10, 50, 100, 150)) +
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "grey") +
  theme_void() +
  labs(fill = "Eucalypt species richness", colour = "Eucalypt species richness")
dev.off()

rm(rRichness, richness)

rm(aus)

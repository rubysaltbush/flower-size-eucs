# script to produce maps of eucalypt traits by their ranges, especially
# mean bud size (log mm2) across Australia as per Figure 1a

### make pretty maps ####
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

#* log bud size ----
# prep log bud size data
logbudsize_mm2 <- euc_traits_nosubsp %>%
  #dplyr::filter(grepl("^(Angophora|Corymbia|Eucalyptus) ", apc_nosubsp)) %>%
  dplyr::select(apc_nosubsp, logbudsize_mm2) %>%
  dplyr::filter(!is.na(logbudsize_mm2))

logbudsize_mm2 <- merge(logbudsize_mm2, rangerast$taxa, by.x = "apc_nosubsp", by.y = "taxon_name", all.x = FALSE)

logbudsize_mm2 <- logbudsize_mm2 %>%
  merge(rangerast$cell_taxa, by = "taxon_id", all.x = FALSE)

#** mean log bud size ----
meanlogBudSize <- logbudsize_mm2 %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(mean_bud_size = mean(logbudsize_mm2))

summary(meanlogBudSize)

rlogBudSize <- terra::subst(rangerast$cell_id, from = meanlogBudSize$cell_id, 
                            to = meanlogBudSize$mean_bud_size)
names(rlogBudSize) <- "mean_bud_size"
plot(rlogBudSize)
# then have to convert this back to df to plot with ggplot
rlogBudSize_df <- as.data.frame(rlogBudSize, xy = TRUE) %>%
  na.omit()
head(rlogBudSize_df)

# plot using ggplot and viridis colour scale with Aus coastline
ggplot() +
  geom_tile(data = rlogBudSize_df, aes(x = x, y = y, fill = mean_bud_size)) +
  scale_fill_viridis_c(breaks = c(3.1, 3.7, 4.3)) +
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "grey") +
  theme_void() +
  labs(fill = "Mean eucalypt bud size (log mm²)")

# save raster as output
terra::writeRaster(rlogBudSize, "data_output/rasters/aus_mean_log_euc_bud_size.tif",
                   overwrite = TRUE)

#* flower colour ----
# prep flower colour data
colour_bin <- euc_traits_nosubsp %>%
  dplyr::filter(grepl("^(Angophora|Corymbia|Eucalyptus) ", apc_nosubsp)) %>%
  dplyr::select(apc_nosubsp, colour_binary) %>%
  dplyr::filter(!is.na(colour_binary))

colour_bin <- merge(colour_bin, rangerast$taxa, by.x = "apc_nosubsp", 
                    by.y = "taxon_name", all.x = FALSE) 
# lose 11 taxa missing range data, total n = 778

colour_bin <- colour_bin %>%
  merge(rangerast$cell_taxa, by = "taxon_id", all.x = FALSE)

#** mean flower colourfulness ----
meanColour <- colour_bin %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(mean_colour = mean(as.numeric(as.character(colour_binary))))

summary(meanColour)

rColour <- terra::subst(rangerast$cell_id, from = meanColour$cell_id, 
                        to = meanColour$mean_colour)
names(rColour) <- "mean_colour"

# save raster as output
terra::writeRaster(rColour, "data_output/rasters/euc_flower_colourfulness_aus.tif",
                   overwrite = TRUE)
# then convert raster back to df to plot with ggplot
rColour <- as.data.frame(rColour, xy = TRUE) %>%
  na.omit()
head(rColour)

# plot using ggplot and viridis colour scale with Aus coastline
ggplot() +
  geom_tile(data = rColour, aes(x = x, y = y, fill = mean_colour)) +
  scale_fill_viridis_c(breaks = c(0, 0.1, 0.2)) +
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "grey") +
  theme_void() +
  labs(fill = "Mean eucalypt flower colourfulness\n(0 = white-cream, 1 = colourful)")

rm(rColour, colour_bin, meanColour)

#* leaf area ----
# prepare leaf area data #
leafarea_mm2 <- euc_traits_nosubsp %>%
  dplyr::filter(grepl("^(Angophora|Corymbia|Eucalyptus) ", apc_nosubsp)) %>%
  dplyr::select(apc_nosubsp, leafarea_mm2) %>%
  dplyr::filter(!is.na(leafarea_mm2))

leafarea_mm2 <- merge(leafarea_mm2, rangerast$taxa, by.x = "apc_nosubsp", by.y = "taxon_name", all.x = FALSE)
# lose 11 taxa with no range data, this makes sense

leafarea_mm2 <- leafarea_mm2 %>%
  merge(rangerast$cell_taxa, by = "taxon_id", all.x = FALSE)

#** mean leaf area ----
meanleafarea <- leafarea_mm2 %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(mean_leafarea = mean(leafarea_mm2))

summary(meanleafarea)

rmeanleafarea <- terra::subst(rangerast$cell_id, from = meanleafarea$cell_id, 
                              to = meanleafarea$mean_leafarea)
names(rmeanleafarea) <- "mean_leafarea"
# save raster as output
terra::writeRaster(rmeanleafarea, "data_output/rasters/euc_leaf_area_aus_mean.tif",
                   overwrite = TRUE)
# convert raster back to df to plot with ggplot
rmeanleafarea <- as.data.frame(rmeanleafarea, xy = TRUE) %>%
  na.omit()
head(rmeanleafarea)

# plot using ggplot and viridis colour scale with Aus coastline
ggplot() +
  geom_tile(data = rmeanleafarea, aes(x = x, y = y, fill = mean_leafarea)) +
  scale_fill_viridis_c() +
  geom_sf(data = aus, fill = NA, linewidth = 0.75, colour = "grey") +
  theme_void() +
  labs(fill = "Mean eucalypt leaf area (mm²)")
ggsave("figures/maps/mean euc leaf area Aus.png", width = 11, height = 6, units = "in")
# export as png for now

# leaf area quite different to bud size and fruit size patterns, very interesting!!!

rm(rmeanleafarea, meanleafarea, leafarea_mm2)


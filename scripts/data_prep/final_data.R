# read in manually filled in final data script and calculate any/all derived
# columns from this

# all species are aligned to the Australian Plant Census in "apc_nosubsp", 
# names from tree manually matched to these in "tree_names", ditto names from
# range data in "range_names"

euc_traits_nosubsp <- cache_RDS("data_output/euc_traits_nosubsp.csv", 
                                read_function = readr::read_csv,
                                save_function = write_csv, function() {

# eucalypt trait data ----

# assembled by text mining using regular expressions to extract data from 
# descriptions on EUCLID (https://apps.lucidcentral.org/euclid/text/intro/index.html, 
# for bud and fruit dimensions, flower colour, number of buds and most max height) 
# combined with AusTraits data (max height). 

euc_traits_nosubsp <- readr::read_csv("data_input/euc_traits_nosubsp_filled.csv")

# Data have been manually checked with missing values filled in and outlying 
# values corrected. Species names have been matched to the Australian Plant
# Census using APCalign and then by manually cross checking taxa with the APC 
# to align taxonomy of EUCLID, plant occurrences and ranges, and the 
# Thornhill et al. phylogeny.
                                  
# derived columns -----
# calculate bud size and fruit size as mean(min, max)(length x width)
euc_traits_nosubsp <- euc_traits_nosubsp %>%
  dplyr::mutate(budsize_mm2 = 10*(rowMeans(dplyr::select(euc_traits_nosubsp, budlength_min, budlength_max), na.rm = TRUE))*10*(rowMeans(dplyr::select(euc_traits_nosubsp, budwidth_min, budwidth_max), na.rm = TRUE))) %>%
  dplyr::mutate(frtsize_mm2 = 10*(rowMeans(dplyr::select(euc_traits_nosubsp, frtlength_min, frtlength_max), na.rm = TRUE))*10*(rowMeans(dplyr::select(euc_traits_nosubsp, frtwidth_min, frtwidth_max), na.rm = TRUE)))
summary(euc_traits_nosubsp$budsize_mm2)
summary(euc_traits_nosubsp$frtsize_mm2) 
# smallest fruit (Euc. raveretiana) smaller than smallest bud (Euc. acroleuca)
# have checked and this tracks with species descriptions
                                  
# replace NaNs (from missing data) with NA
euc_traits_nosubsp$budsize_mm2[is.nan(euc_traits_nosubsp$budsize_mm2)] <- NA
euc_traits_nosubsp$frtsize_mm2[is.nan(euc_traits_nosubsp$frtsize_mm2)] <- NA
                                  
# create binary colourful/not colourful variable from flower colour
euc_traits_nosubsp$colour_binary <- euc_traits_nosubsp$colours_all
euc_traits_nosubsp$colour_binary <- gsub("white_cream", "0", euc_traits_nosubsp$colour_binary)
euc_traits_nosubsp$colour_binary <- gsub("yellow|green|pink|red|orange", "1", euc_traits_nosubsp$colour_binary)
euc_traits_nosubsp$colour_binary <- gsub("1 1 1", "1", euc_traits_nosubsp$colour_binary)
euc_traits_nosubsp$colour_binary <- gsub("1 1", "1", euc_traits_nosubsp$colour_binary)
euc_traits_nosubsp$colour_binary <- gsub("1 0 1|1 0|0 1", "0.5", euc_traits_nosubsp$colour_binary)
table(euc_traits_nosubsp$colour_binary)
# try classifying 0.5 (mixed white_cream and colourful flowers) as 1s for now
# for logistic regression where need full binary
euc_traits_nosubsp$colour_fullbinary <- gsub("0.5", "1", euc_traits_nosubsp$colour_binary)
table(euc_traits_nosubsp$colour_fullbinary)
                                  
# take mean of buds per umbel max and min
euc_traits_nosubsp$bud_n_mean <- rowMeans(dplyr::select(euc_traits_nosubsp, 
                                                        bud_n_max, bud_n_min), 
                                          na.rm = TRUE)
hist(euc_traits_nosubsp$bud_n_mean)
                                  
# add in leaf area extracted from EUCLID leaf dimensions with 2/3 correction
# leaf area will be supplementary to flower size analyses
leaf_dim <- readr::read_csv("data_input/leaf_dim_nosub.csv") %>%
  dplyr::select(apc_nosubsp, leafarea_mm2)
euc_traits_nosubsp <- euc_traits_nosubsp %>%
  dplyr::left_join(leaf_dim, by = "apc_nosubsp")
rm(leaf_dim)
# confirm that species mean leaf area extracted from EUCLID dimensions
# i.e. (2/3)*mean(length)*mean(width) correlates with field measurements
# of actual scanned leaf area available in AusTraits database
leafarea <- austraits::load_austraits(version = "4.2.0", path = "data_cache/austraits")
leafarea <- leafarea$traits %>%
  dplyr::filter(trait_name == "leaf_area") %>%
  dplyr::filter(stringr::str_detect(taxon_name, "Eucalyptus|Angophora|Corymbia"))
# get rid of subspecies etc
leafarea$taxon_name <- gsub("\\s(?:subsp|var)\\.\\s.+$", "", leafarea$taxon_name, perl = TRUE)
paste("There are", length(unique(leafarea$taxon_name)), "eucalypt taxa with leaf area data in AusTraits")
# 474 without subspecies/variants
table(leafarea$unit) # all values in mm2
table(leafarea$basis_of_record) # majority from field measurements, <50 from literature
# summarise by species
leafarea <- leafarea %>%
  dplyr::group_by(taxon_name) %>%
  dplyr::summarise(meanleafarea_mm2 = mean(as.numeric(value)))
sum(leafarea$taxon_name %in% euc_traits_nosubsp$apc_nosubsp)
# 451 taxa match my eucalypt data taxa
# join leaf area estimated from literature dimensions to austraits data
leafarea <- leafarea %>%
  dplyr::left_join(euc_traits_nosubsp, by = c("taxon_name" = "apc_nosubsp")) %>%
  dplyr::select(taxon_name, meanleafarea_mm2, leafarea_mm2)
# plot against each other
plot(log(meanleafarea_mm2) ~ log(leafarea_mm2), data = leafarea)
# relationship looks pretty linear, check with linear regression
lflm <- lm(log(meanleafarea_mm2) ~ log(leafarea_mm2), data = leafarea)
summary(lflm)
# Call:
#   lm(formula = log(meanleafarea_mm2) ~ log(leafarea_mm2), data = leafarea)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.79667 -0.24570 -0.00985  0.21936  1.40892 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        1.06144    0.21189   5.009 7.86e-07 ***
#   log(leafarea_mm2)  0.86964    0.02907  29.919  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4068 on 449 degrees of freedom
# (23 observations deleted due to missingness)
# Multiple R-squared:  0.666,	Adjusted R-squared:  0.6652 
# F-statistic: 895.1 on 1 and 449 DF,  p-value: < 2.2e-16

# check residuals
car::residualPlot(lflm)
qqnorm(lflm$residuals)
qqline(lflm$residuals)
# look roughly okay?

# export pretty plot to include in supp. mat.
ggplot(leafarea, aes(x = log(meanleafarea_mm2), y = log(leafarea_mm2))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  theme_pubr() +
  xlab("AusTraits mean eucalypt leaf area (log mm²)") +
  ylab("EUCLID mean eucalypt leaf area (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(lflm)$r.squared, 2),
                     "    P = ", format.pval(summary(lflm)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/Fig Sx AusTraits vs EUCLID leaf area.pdf", width = 7, height = 6)
rm(lflm, leafarea)

# add in data on species' mean environment (phosphorus, temperature, precipitation
# flower-visiting bat species richness and flower-visiting bird species richness)
source("scripts/data_prep/prep_envdata.R")
# join onto final data
euc_traits_nosubsp <- euc_traits_nosubsp %>%
  dplyr::left_join(spmean_env, by = "range_names")
rm(spmean_env)
hist(euc_traits_nosubsp$meanbatpres)
# bat presence best to classify as binary (bats or no bats)
# will have 50% cutoff for now
euc_traits_nosubsp$meanbatpres_bin <- ifelse(euc_traits_nosubsp$meanbatpres > 0.5, 1, 0)
                                  
# write data to folder so it can be cached
readr::write_csv(euc_traits_nosubsp, "data_output/euc_traits_nosubsp.csv")

euc_traits_nosubsp
})

euc_traits_nosubsp$colour_binary <- factor(euc_traits_nosubsp$colour_binary)

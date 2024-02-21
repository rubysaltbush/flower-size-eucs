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

# given right skew will log transform bud size for all analyses
euc_traits_nosubsp$logbudsize_mm2 <- log(euc_traits_nosubsp$budsize_mm2)
                                  
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

# add in data on species' mean environment (phosphorus, temperature, precipitation
# flower-visiting bat species richness and flower-visiting bird species richness)
source("scripts/data_prep/prep_envdata.R")
# join onto final data
euc_traits_nosubsp <- euc_traits_nosubsp %>%
  dplyr::left_join(spmean_env, by = "range_names")
rm(spmean_env)
hist(euc_traits_nosubsp$meanbatpres)
# export publishable histogram
ggplot(euc_traits_nosubsp, aes(x = meanbatpres)) +
  geom_histogram() +
  ggpubr::theme_pubr() +
  xlab("Mean flower-visiting bat presence") +
  ylab("Number of eucalypt species")
ggsave("figures/Fig S6 euc species mean bat presence histogram.pdf", width = 7, height = 4)
# bat presence best to classify as binary (bats or no bats)
# will have 50% cutoff for now
euc_traits_nosubsp$meanbatpres_bin <- ifelse(euc_traits_nosubsp$meanbatpres > 0.5, 1, 0)
table(euc_traits_nosubsp$meanbatpres_bin)
# 0   1 
# 365 421 
hist(euc_traits_nosubsp$meanmarspres)
# export publishable histogram
ggplot(euc_traits_nosubsp, aes(x = meanmarspres)) +
  geom_histogram() +
  ggpubr::theme_pubr() +
  xlab("Mean eucalypt flower-visiting marsupial presence") +
  ylab("Number of eucalypt species")
ggsave("figures/Fig S7 euc species mean marsupial presence histogram.pdf", width = 7, height = 4)
# marsupial presence best to classify as binary (gliders or no gliders)
# will have 50% cutoff for now
euc_traits_nosubsp$meanmarspres_bin <- ifelse(euc_traits_nosubsp$meanmarspres > 0.5, 1, 0)
table(euc_traits_nosubsp$meanmarspres_bin)
# 0   1 
# 87 699 

# remove irrelevant columns
euc_traits_nosubsp <- euc_traits_nosubsp %>%
  dplyr::select(apc_nosubsp, tree_names, range_names, subgenus, budsize_mm2, 
                logbudsize_mm2, colours_all, colour_binary, colour_fullbinary,
                frtsize_mm2, medianlong, meanMAT, meanMAP, meanAVP, meanbirdrich,
                meanbatpres, meanbatpres_bin, meanmarspres, meanmarspres_bin)

# write data to folder so it can be cached
readr::write_csv(euc_traits_nosubsp, "data_output/euc_traits_nosubsp.csv")

euc_traits_nosubsp
})

euc_traits_nosubsp$colour_binary <- factor(euc_traits_nosubsp$colour_binary)

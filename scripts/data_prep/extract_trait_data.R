# example of method used to extract eucalypt trait values from EUCLID eucalypt
# species descriptions, original data not provided so this script is an
# example only

# all data were exported from here and manually checked, then matched to 
# their equivalent species in the Australian Plant Census using APCalign
# and manually matched to species in the Thornhill et al. (2019) phylogeny
# and species range and occurrence data; a few species' trait values were then 
# manually added (for taxa missing from EUCLID) to produce the data in
# "data_input/euc_traits_nosubsp_filled.csv" and "data_input/leaf_dim_nosub.csv"

# read in web scraped data courtesy of David Coleman
# DATA NOT PROVIDED, EXAMPLE ONLY
euclid <- readr::read_csv("data_input/Euclid_downloaded.csv")

#### bud size ####

# extract bud dimensions from EUCLID description
buds <- data.frame()
for (n in 1:nrow(euclid)) {
  description <- euclid[n, "description"] # get description
  buds_desc <- str_extract(description, "Mature buds.*?wide[, \\.\\)]") # just Mature buds...wide
  # below regex by Gregory McIntyre, matches different elements in bud description 
  # and returns the useful bits in different groups
  bud_pattern <- "Mature buds.*?(\\d[\\d.]*)(?:[^\\d.](\\d[\\d.]*))?(?:\\([\\d.]+\\))? (\\w+) long.*? (\\d[\\d.]*)(?:[^\\d.](\\d[\\d.]*))?(?:\\([\\d.]+\\))? (\\w+) wide"
  #               Mature buds..  0.9           -       1.2          (3.0)              cm     long .. 1.2         -         3.4           (4.0)              cm     wide
  # now extract bud dimensions using above regex from above bud description
  temp <- as.data.frame(stringr::str_match(buds_desc, bud_pattern)) 
  temp$desc <- buds_desc
  temp <- cbind(euclid[n, c("species", "url", "description")], temp)
  buds <- rbind(buds, temp)
}
rm(n, temp, description, buds_desc, bud_pattern)
names(buds) <- c("species", "url", "description", "budmatched", "budlength_min", "budlength_max", 
                 "budlength_unit", "budwidth_min", "budwidth_max", "budwidth _unit", "buds_desc")
problems <- buds[is.na(buds$budmatched),]
# mostly works, only 21 errors and NAs, can manually check/fix these

# to error check, subtract min from max and check for negative numbers
buds$budlength_max <- as.numeric(buds$budlength_max)
buds$budlength_min <- as.numeric(buds$budlength_min)
buds$budwidth_max <- as.numeric(buds$budwidth_max)
buds$budwidth_min <- as.numeric(buds$budwidth_min)
buds$buddiff_long <- buds$budlength_max - buds$budlength_min # all look good
buds$buddiff_wide <- buds$budwidth_max - buds$budwidth_min # all look good

#### flower colour ####

# extract flower colour
colour <- data.frame()
for (n in 1:nrow(euclid)) {
  description <- euclid[n, "description"] # get description
  flower_desc <- str_extract(description, stringr::regex("Flowers.*?[\\.]", ignore_case = TRUE))
  flower_desc <- gsub("creamy", "x", flower_desc) # get rid of creamy descriptor
  # below regex returns flower colour
  flower_pattern <- "white|yellow|pink|green|purple|red|orange|whitish|lemon|cream"
  #                (colour)
  temp <- data.frame(stringr::str_match_all(flower_desc, flower_pattern))
  names(temp) <- c("c1")
  temp <- temp %>% dplyr::summarise(colours = paste(c1, collapse = " "))
  temp <- cbind(euclid[n, c("species", "url", "description")], temp, flower_desc)
  colour <- rbind(colour, temp)
}
rm(n, temp, description, flower_desc, flower_pattern)
problems <- colour[is.na(colour$flower_desc),]
# 10 missing are those with no flower colour recorded
rm(problems)
colour <- dplyr::distinct(colour)
table(colour$colours)
# 694ish white, 285ish otherwise (though many of these e.g. "white white")

colour$colours_austraits <- gsub("white white|whitish cream|white cream|whitish|cream|white", "white_cream", colour$colours)
table(colour$colours_austraits)

## AUSTRAITS DEFINITION ##
# flower_colour
# label: Flower colour
# description: Flower colour, with six possible outcomes
# type: categorical
# allowable values:
#   blue_purple: blue or purple
#   green: green flower
#   pink: pink flower
#   red_brown: red or brown flower
#   white_cream: white or cream flower
#   yellow_orange: yellow orange flower

# will have to individually determine dominant colour by checking other resources
# and manually align all colours to AusTraits colour categories

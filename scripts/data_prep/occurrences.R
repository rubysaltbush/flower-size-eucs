# test exisitng eucalypt occurrences and/or get occurrences from ALA

occurrences$country <- "AUS"

flags <- CoordinateCleaner::clean_coordinates(x = occurrences,
                                              lon = "longitude",
                                              lat = "latitude",
                                              countries = "country",
                                              species = "speciesLevelUpdated",
                                              tests = c("capitals", 
                                                        "centroids", 
                                                        "equal",
                                                        "gbif", 
                                                        "institutions",
                                                        "outliers", 
                                                        "seas",
                                                        "zeros", 
                                                        "countries"))
# all look pretty alright really, many "sea"  and non-country records are just  
# coastal eucalypt species, country centroids are in Booderee National Park,
# institutions are genuine collections around UNE, and outliers seem totes legit

# how many records per species in current data?
checking <- occurrences %>%
  dplyr::group_by(speciesLevelUpdated) %>%
  dplyr::summarise(count = n())

# euc angularis lowest at 3 records, checks out with what's on AVH and its restricted distribution
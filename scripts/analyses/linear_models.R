# run linear models (multiple regression and glm) to assess whether the  
# distributions of eucalypt flower (bud) size and eucalypt flower colourfulness 
# are shaped more by biotic or abiotic environmental variabless

#### examine data ####

#* response variables ----

# first, main response variable, bud size (mm2)
hist(euc_traits_nosubsp$budsize_mm2)
# extreme left-biased distribution, what about log transformation?
hist(log(euc_traits_nosubsp$budsize_mm2))
# still a bit left-biased but fairly normal

# second response variable is flower colourfulness, binary
plot(euc_traits_nosubsp$colour_binary)
table(euc_traits_nosubsp$colour_binary)
# way more 0s than 1s, and 28 0.5s (exclude? or treat as ordinal?)

#* predictor variables ----

hist(euc_traits_nosubsp$meanMAT)
#roughly normal
hist(euc_traits_nosubsp$meanMAP)
# left-biased
hist(euc_traits_nosubsp$meanAVP)
# left-biased
hist(euc_traits_nosubsp$meanbatrich)
# many many 0s
hist(euc_traits_nosubsp$meanbirdrich)
# normalish

#* check correlation ----
correlation <- cor(euc_traits_nosubsp[,22:26],
                   use = "pairwise.complete.obs")
par(mfrow = c(1, 1))
corrplot::corrplot(correlation, type = "upper", order = "hclust",  
                   col = RColorBrewer::brewer.pal(n = 10, name = "RdYlBu"),
                   tl.cex = 0.8, tl.col = "black")
# as suspected correlation between bird richness and bat richness (0.68)
# and both these and mean soil available phosphorus (~bird=0.62, ~bat=0.46)
correlation
# all correlations are below 0.7 and all are independently interesting so will
# leave in for now but double check later if this is appropriate
rm(correlation)

# The full model formula will look like this:
  
  # Fixed effects for trend: 
  #  budsize_mm2 ~ meanMAT + meanMAP + meanAVP + meanbirdrich + meanbatrich
  
  # Random effects for phylogenetic relatedness
#              + ape::corBrownian(phy = tree_budsz, form = ~spp)

#### identify distributions and link functions ####

# how???


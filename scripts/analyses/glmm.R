# run Generalised Linear Mixed-effects Model to assess whether the distributions 
# of eucalypt flower (bud) size and eucalypt flower colourfulness are shaped
# more by biotic or abiotic environmental variables (or a mixture??)
# and whether this holds true for the evolution of these traits as well

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

# need to check for correlation?
correlation <- cor(euc_traits_nosubsp[,22:26],
                   use = "pairwise.complete.obs")
correlation
par(mfrow = c(1, 1))
corrplot::corrplot(correlation, type = "upper", order = "hclust",  
                   col = RColorBrewer::brewer.pal(n = 10, name = "RdYlBu"),
                   tl.cex = 0.8, tl.col = "black")
# hmm as suspected quite high correlation between bird richness and bat richness (0.68)
# and both of these and mean soil available phosphorus (bird-0.62, bat-0.46)

# does this mean I should remove any of these? hope not, they're all part of 
# the story


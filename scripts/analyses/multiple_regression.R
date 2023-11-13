# run multiple regression models (least squares and glm) to assess whether the  
# distribution of eucalypt flower (bud) size is shaped more by abiotic (temp, 
# precip, and soil phosphorus) or biotic (flower-visiting bird richness and bat 
# presence) environmental variables

# first run model of leaf area against climate and soil variables
# then run model of bud size against climate and soil variables
# then add biotic (pollinator) variables to bud size model
# finally model flower colour against full mix of biotic and abiotic variables

#### examine data ####

#* response variables ----

# response variable for comparison is leaf area
hist(euc_traits_nosubsp$leafarea_mm2)
# right-skewed distribution, log transform
hist(log(euc_traits_nosubsp$leafarea_mm2))
# much better

# main response variable, eucalypt bud size (mm2)
hist(euc_traits_nosubsp$budsize_mm2)
# right-skewed distribution, log transform
hist(log(euc_traits_nosubsp$budsize_mm2))
# fairly normal with log transformation

# second response variable is flower colourfulness, binary
plot(euc_traits_nosubsp$colour_fullbinary)
table(euc_traits_nosubsp$colour_fullbinary)
# 63 colourful (1), 696 white-cream (0) taxa

#* predictor variables ----

hist(euc_traits_nosubsp$meanMAT)
# roughly normal but extra high temps
hist(euc_traits_nosubsp$meanMAP)
# right-skewed
hist(euc_traits_nosubsp$meanAVP)
# slightly right-skewed
plot(euc_traits_nosubsp$meanbatpres_bin)
table(euc_traits_nosubsp$meanbatpres_bin)
# almost equal numbers in bat:no bats categories
hist(euc_traits_nosubsp$meanbirdrich)
# normalish

#* correlation plots ----

# visually check relationships between predictor and response variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(19, 21, 25, 26, 24, 28, 30), 
                columnLabels = c("Bud size (log)",
                                 "Flower colour",
                                 "Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence"))
ggsave("figures/regressions/full pairwise correlation plot.pdf", 
       width = 13, height = 10)
# lots going on - looks like bud size best (-vely )correlated with bat presence, 
# then bird richness, then soil phosphorus, then precipitation
# and flower colour bats then birds then phosphorus then precipitation
# temperature no correlation for both bud size and flower colour

# check for collinearity among predictor variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(25, 26, 24, 28, 30), 
                columnLabels = c("Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence"))
ggsave("figures/regressions/predictors pairwise correlation plot.pdf", width = 9, height = 5.6)
# some correlation between mean bird richness and bat presence (0.69)
# and birds/bats and mean soil available phosphorus (~bird = 0.62, ~bat = 0.54)
# all correlations are below 0.7 and all are independently interesting so will
# leave in model, check VIF after

#### run models ####

# with all predictors scaled so can compare effect sizes meaningfully

multi_reg <- list()

#* leaf area abiotic ----

# to compare with bud size multiple regression, expect that leaf area
# will increase positively with species mean temp, precip and phosphorus
# not including birds or bats as no predicted relationships

# The full model formula will look like this:

#  log(leaf_area) ~ meanMAT + meanMAP + meanAVP

multi_reg$leafarea_abiotic <- lm(log(leafarea_mm2) ~ scale(meanMAT) + 
                                                     scale(meanMAP) + 
                                                     scale(meanAVP), #abiotic
                                            data = euc_traits_nosubsp)
summary(multi_reg$leafarea_abiotic)

# Call:
#   lm(formula = log(leafarea_mm2) ~ scale(meanMAT) + scale(meanMAP) + 
#        scale(meanAVP), data = euc_traits_nosubsp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -3.03964 -0.38082  0.02849  0.37837  2.13782 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.15865    0.02227 321.451  < 2e-16 ***
#   scale(meanMAT)  0.16994    0.02626   6.471 1.72e-10 ***
#   scale(meanMAP)  0.24094    0.02744   8.782  < 2e-16 ***
#   scale(meanAVP)  0.15526    0.03007   5.163 3.09e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6243 on 782 degrees of freedom
# (12 observations deleted due to missingness)
# Multiple R-squared:  0.2549,	Adjusted R-squared:  0.252 
# F-statistic: 89.16 on 3 and 782 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$leafarea_abiotic)
# scale(meanMAT) scale(meanMAP) scale(meanAVP) 
# 1.390463       1.517761       1.821095 
# VIF under 1.83 for all, multicollinearity unlikely

# adjusted R2 decent at 0.252

#* bud size abiotic ----

# to compare with leaf area multiple regression, expect that leaf area
# will increase positively with species mean temp, precip and phosphorus
# not including birds or bats as no predicted relationships

multi_reg$budsize_abiotic <- lm(logbudsize_mm2 ~ scale(meanMAT) + 
                                                 scale(meanMAP) + 
                                                 scale(meanAVP), #abiotic
                                           data = euc_traits_nosubsp)
summary(multi_reg$budsize_abiotic)

# Call:
#   lm(formula = logbudsize_mm2 ~ scale(meanMAT) + scale(meanMAP) + 
#        scale(meanAVP), data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3684 -0.6252 -0.0642  0.4782  3.7170 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.74785    0.03189 117.524  < 2e-16 ***
#   scale(meanMAT) -0.06469    0.03755  -1.723  0.08531 .  
# scale(meanMAP) -0.11271    0.03914  -2.880  0.00409 ** 
#   scale(meanAVP) -0.31159    0.04304  -7.240 1.08e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8906 on 776 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1442,	Adjusted R-squared:  0.1408 
# F-statistic: 43.57 on 3 and 776 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_abiotic)
# scale(meanMAT) scale(meanMAP) scale(meanAVP) 
# 1.394436       1.513810       1.822927 
# VIF under 1.83 for all, multicollinearity unlikely

# adjusted R2 defs lower than for leaf area at 0.14

#* bud size biotic model ----

multi_reg$budsize_biotic <- lm(logbudsize_mm2 ~ scale(meanbirdrich) + 
                               scale(meanbatpres_bin), #biotic
                             data = euc_traits_nosubsp)
summary(multi_reg$budsize_biotic)

# Call:
#   lm(formula = logbudsize_mm2 ~ scale(meanbirdrich) + scale(meanbatpres_bin), 
#      data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.0831 -0.5612 -0.0655  0.4599  3.7358 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.74975    0.03150 119.041  < 2e-16 ***
#   scale(meanbirdrich)    -0.15381    0.04487  -3.428  0.00064 ***
#   scale(meanbatpres_bin) -0.26594    0.04460  -5.963 3.75e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8797 on 777 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.164,	Adjusted R-squared:  0.1618 
# F-statistic:  76.2 on 2 and 777 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_biotic)
# scale(meanbirdrich) scale(meanbatpres_bin) 
# 1.999415               1.999415 
# VIF under 2 for all, multicollinearity unlikely

# some improvement in adjusted R2 with biotic rather than abiotic predictors at 0.16

#* bud size full model ----

multi_reg$budsize_full <- lm(logbudsize_mm2 ~ scale(meanMAT) + 
                                              scale(meanMAP) + 
                                              scale(meanAVP) + #abiotic
                                              scale(meanbirdrich) + 
                                              scale(meanbatpres_bin), #biotic
                                        data = euc_traits_nosubsp)
summary(multi_reg$budsize_full)

# Call:
#   lm(formula = logbudsize_mm2 ~ scale(meanMAT) + scale(meanMAP) + 
#        scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#      data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2854 -0.5713 -0.0620  0.4833  3.6696 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.749201   0.031244 119.999  < 2e-16 ***
#   scale(meanMAT)          0.027142   0.043678   0.621   0.5345    
# scale(meanMAP)         -0.006904   0.043025  -0.160   0.8726    
# scale(meanAVP)         -0.149774   0.050813  -2.948   0.0033 ** 
#   scale(meanbirdrich)    -0.050238   0.054113  -0.928   0.3535    
# scale(meanbatpres_bin) -0.255489   0.062599  -4.081 4.94e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8725 on 774 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1807,	Adjusted R-squared:  0.1754 
# F-statistic: 34.14 on 5 and 774 DF,  p-value: < 2.2e-16

# bat presence/absence explains most variation, also phosphorus but small effect

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_full)
# meanMAT         meanMAP         meanAVP    meanbirdrich meanbatpres_bin 
# 1.966073        1.905858        2.647471        2.956335        4.004342 
# VIF below 4.1 for all, generally recommended threshold is 5-10
# higher VIF for bat presence here suggests some overlap with other predictors

#* AIC/BIC to compare bud size models (Bayesian Information Criterion) ----

# first BIC (penalises models with too many variables, lowest BIC model best)

BIC(multi_reg$budsize_abiotic, multi_reg$budsize_biotic, multi_reg$budsize_full)

#                           df      BIC
# multi_reg$budsize_abiotic  5 2062.106
# multi_reg$budsize_biotic   4 2037.176
# multi_reg$budsize_full     7 2041.373

# biotic model has lowest BIC

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$budsize_abiotic, multi_reg$budsize_biotic, multi_reg$budsize_full)

#                           df      AIC
# multi_reg$budsize_abiotic  5 2038.809
# multi_reg$budsize_biotic   4 2018.539
# multi_reg$budsize_full     7 2008.758

# dif between full and abiotic (~30) larger than dif between full and biotic (~10)
# suggesting biotic variables more important to model

#* flower colourfulness abiotic ----

# one observation missing biotic but not abiotic variable, remove from data
# for these models
tempflcol <- euc_traits_nosubsp %>%
  dplyr::select(apc_nosubsp, colour_fullbinary, meanMAT, meanMAP, meanAVP,
                meanbirdrich, meanbatpres_bin) %>%
  dplyr::filter(complete.cases(.))

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_abiotic <- glm(colour_fullbinary ~ scale(meanMAT) + 
                                 scale(meanMAP) + 
                                 scale(meanAVP),
                               data = tempflcol,
                               family = binomial)
summary(multi_reg$flcolour_abiotic)

# Call:
#   glm(formula = as.numeric(colour_fullbinary) ~ scale(meanMAT) + 
#         scale(meanMAP) + scale(meanAVP), family = binomial(), data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -2.4105     0.1570 -15.350  < 2e-16 ***
#   scale(meanMAT)  -0.0880     0.1532  -0.574  0.56564    
#   scale(meanMAP)  -0.4667     0.1800  -2.593  0.00951 ** 
#   scale(meanAVP)  -0.8042     0.2026  -3.970 7.19e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 499.87  on 774  degrees of freedom
# AIC: 507.87
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_abiotic)
# scale(meanMAT) scale(meanMAP) scale(meanAVP) 
# 1.170342       1.416815       1.411756 
# VIF  under 1.8 for all, multicollinearity unlikely

#* flower colourfulness biotic ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_biotic <- glm(colour_fullbinary ~ scale(meanbirdrich) + 
                                 scale(meanbatpres_bin),
                               data = tempflcol,
                               family = binomial)
summary(multi_reg$flcolour_biotic)

# Call:
#   glm(formula = colour_fullbinary ~ scale(meanbirdrich) + 
#         scale(meanbatpres_bin), family = binomial, data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -2.4785     0.1676 -14.789  < 2e-16 ***
#   scale(meanbirdrich)     -0.4260     0.2117  -2.013   0.0442 *  
#   scale(meanbatpres_bin)  -0.7960     0.1960  -4.061 4.89e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 489.00  on 775  degrees of freedom
# AIC: 495
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_biotic)
# scale(meanbirdrich) scale(meanbatpres_bin) 
# 1.602602               1.602602  
# VIF under 1.65, multicollinearity unlikely

#* flower colourfulness full model ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_full <- glm(colour_fullbinary ~ scale(meanMAT) + 
                                                   scale(meanMAP) + 
                                                   scale(meanAVP) + 
                                                   scale(meanbirdrich) + 
                                                   scale(meanbatpres_bin),
                                          data = tempflcol,
                                          family = binomial())
summary(multi_reg$flcolour_full)

# Call:
#   glm(formula = colour_fullbinary ~ scale(meanMAT) + 
#         scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#       family = binomial, data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.60679    0.19384 -13.448   <2e-16 ***
#   scale(meanMAT)          0.17150    0.19640   0.873   0.3826    
# scale(meanMAP)         -0.06169    0.19368  -0.319   0.7501    
# scale(meanAVP)         -0.37688    0.21890  -1.722   0.0851 .  
# scale(meanbirdrich)    -0.37067    0.25213  -1.470   0.1415    
# scale(meanbatpres_bin) -0.72379    0.30156  -2.400   0.0164 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 480.21  on 772  degrees of freedom
# (20 observations deleted due to missingness)
# AIC: 492.21
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_full)
# meanMAT         meanMAP         meanAVP    meanbirdrich meanbatpres_bin 
# 1.935153        1.881569        1.491282        1.627528        3.734424 
# VIF still under 4 for all, multicollinearity unlikely

#* AIC/BIC to compare flower colourfulness models ----

# first BIC (penalises models with too many variables, lowest BIC model best)

BIC(multi_reg$flcolour_abiotic, multi_reg$flcolour_biotic, multi_reg$flcolour_full)

#                             df      BIC
# multi_reg$flcolour_abiotic  4 526.4958
# multi_reg$flcolour_biotic   3 508.9739
# multi_reg$flcolour_full     6 520.1455

# biotic model has lowest BIC

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$flcolour_abiotic, multi_reg$flcolour_biotic, multi_reg$flcolour_full)

#                             df      AIC
# multi_reg$flcolour_abiotic  4 507.8689
# multi_reg$flcolour_biotic   3 495.0037
# multi_reg$flcolour_full     6 492.2051

# dif between full and abiotic (~15) larger than dif between full and biotic (~3)
# suggesting biotic variables more important to model

rm(tempflcol)

#### with phylogeny ####

# subset data to phylogenetic least squares regression variables
# removing data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, colour_fullbinary, meanAVP, meanMAT, 
                meanMAP, meanbirdrich, meanbatpres_bin) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 62 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2))
tree_pgls <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_pgls$tip.label) # 669 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1] # tree names to row names
pgls_data[,1] <- NULL

#* bud size abiotic PGLS ----

spp <- rownames(pgls_data) # set species names as reference point
multi_reg$budsize_abiotic_PGLS <- nlme::gls(logbudsize_mm2 ~ 
                                                     scale(meanMAT) + 
                                                     scale(meanMAP) + 
                                                     scale(meanAVP),
                                            data = pgls_data,
                                            correlation = 
                                              ape::corBrownian(phy = tree_pgls, 
                                                               form = ~spp),
                                            method = "ML")
summary(multi_reg$budsize_abiotic_PGLS)

# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ scale(meanMAT) + scale(meanMAP) + scale(meanAVP) 
# Data: pgls_data 
# AIC      BIC    logLik
# 2165.808 2188.337 -1077.904
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)     3.680442 2.7142600  1.355965  0.1756
# scale(meanMAT)  0.012478 0.0492099  0.253572  0.7999
# scale(meanMAP) -0.140890 0.0428950 -3.284518  0.0011
# scale(meanAVP)  0.030442 0.0467182  0.651619  0.5149
# 
# Correlation: 
#   (Intr) s(MAT) s(MAP)
# scale(meanMAT) -0.010              
# scale(meanMAP) -0.008  0.092       
# scale(meanAVP) -0.001  0.282 -0.209
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.310820476 -0.082971603 -0.002602373  0.079007903  0.520943997 
# 
# Residual standard error: 7.1574 
# Degrees of freedom: 669 total; 665 residual

#* bud size biotic PGLS ----

multi_reg$budsize_biot_PGLS <- nlme::gls(logbudsize_mm2 ~ 
                                           scale(meanbirdrich) + 
                                           scale(meanbatpres_bin),
                                         data = pgls_data,
                                         correlation = 
                                           ape::corBrownian(phy = tree_pgls, 
                                                            form = ~spp),
                                         method = "ML")
summary(multi_reg$budsize_biot_PGLS)

# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ scale(meanbirdrich) + scale(meanbatpres_bin) 
# Data: pgls_data 
# AIC    BIC    logLik
# 2154.076 2172.1 -1073.038
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)             3.590751 2.6923724  1.333676  0.1828
# scale(meanbirdrich)    -0.155245 0.0406167 -3.822201  0.0001
# scale(meanbatpres_bin)  0.152366 0.0376897  4.042652  0.0001
# 
# Correlation: 
#   (Intr) scl(m)
# scale(meanbirdrich)     0.001       
# scale(meanbatpres_bin) -0.003 -0.470
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -0.32638726 -0.07644885  0.01045039  0.09677704  0.55686787 
# 
# Residual standard error: 7.105529 
# Degrees of freedom: 669 total; 666 residual

#* bud size full PGLS ----

multi_reg$budsize_PGLS <- nlme::gls(logbudsize_mm2 ~ scale(meanMAT) + 
                                                     scale(meanMAP) + 
                                                     scale(meanAVP) + 
                                                     scale(meanbirdrich) + 
                                                     scale(meanbatpres_bin),
                                           data = pgls_data,
                                           correlation = 
                                             ape::corBrownian(phy = tree_pgls, 
                                                              form = ~spp),
                                           method = "ML")
summary(multi_reg$budsize_PGLS)

# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ scale(meanMAT) + scale(meanMAP) + scale(meanAVP) +      scale(meanbirdrich) + scale(meanbatpres_bin) 
# Data: pgls_data 
# AIC      BIC    logLik
# 2147.99 2179.531 -1066.995
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)             3.693240 2.6743845  1.380968  0.1678
# scale(meanMAT)         -0.085780 0.0535892 -1.600694  0.1099
# scale(meanMAP)         -0.134165 0.0423390 -3.168822  0.0016
# scale(meanAVP)          0.022618 0.0469329  0.481927  0.6300
# scale(meanbirdrich)    -0.171424 0.0422779 -4.054705  0.0001
# scale(meanbatpres_bin)  0.170628 0.0415065  4.110879  0.0000
# 
# Correlation: 
#   (Intr) s(MAT) s(MAP) s(AVP) scl(m)
# scale(meanMAT)         -0.009                            
# scale(meanMAP)         -0.008  0.062                     
# scale(meanAVP)         -0.001  0.296 -0.215              
# scale(meanbirdrich)    -0.001  0.254 -0.005 -0.065       
# scale(meanbatpres_bin)  0.001 -0.424  0.053 -0.123 -0.517
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.299615533 -0.087045080 -0.002149824  0.079794330  0.529754487 
# 
# Residual standard error: 7.041633 
# Degrees of freedom: 669 total; 663 residual

#* BIC to compare bud size PGLS ----

BIC(multi_reg$budsize_abiotic_PGLS, multi_reg$budsize_biot_PGLS, multi_reg$budsize_PGLS)

#                                 df      BIC
# multi_reg$budsize_abiotic_PGLS  5 2188.337
# multi_reg$budsize_biot_PGLS     4 2172.100
# multi_reg$budsize_PGLS          7 2179.531

# bigger change from full to abiotic model (~9) than to biotic model (~-7)

#* flower colourfulness full model ----

# using phylolm to run phylogenetic logistic regression by Ives & Garland method
# VERY SLOW/COMPUTATIONALLY INTENSE
multi_reg$flcolour_PGLS <- phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + 
                                                                 scale(meanMAP) + 
                                                                 scale(meanAVP) + 
                                                                 scale(meanbirdrich) + 
                                                                 scale(meanbatpres_bin),
                                             data = pgls_data,
                                             phy = tree_pgls,
                                             method = "logistic_IG10", 
                                             boot = 100)
summary(multi_reg$flcolour_PGLS)

# Warning message:
#  In phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) +  :
#  the estimate of 'alpha' (0.92163414891527) reached the upper bound (0.935504358152755).
#  This may simply reflect a flat likelihood at large alpha values,
#  meaning that the phylogenetic correlation is estimated to be negligible.
#                                                   
# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanMAT) + 
#                       scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#                     data = pgls_data, phy = tree_pgls, method = "logistic_IG10", 
#                     boot = 100)
# AIC     logLik Pen.logLik 
# 416.4     -201.2     -191.2 
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.9216341 
# bootstrap mean: 0.5611803 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.1087595,0.9353957)
# 
# Coefficients:
#   Estimate      StdErr     z.value lowerbootCI upperbootCI
# (Intercept)             -2.4468578   0.2264412 -10.8057104  -2.9739735     -1.9196
# scale(meanMAT)           0.3726886   0.2120531   1.7575250  -0.0180384      0.8957
# scale(meanMAP)          -0.0324701   0.1826080  -0.1778130  -0.4460400      0.3401
# scale(meanAVP)          -0.0056982   0.2243294  -0.0254012  -0.4140431      0.4181
# scale(meanbirdrich)     -0.3877675   0.2311509  -1.6775517  -0.6751863      0.0870
# scale(meanbatpres_bin)  -0.5712757   0.2694750  -2.1199582  -1.2254959     -0.1124
# p.value    
# (Intercept)            < 2e-16 ***
#   scale(meanMAT)         0.07883 .  
# scale(meanMAP)         0.85887    
# scale(meanAVP)         0.97973    
# scale(meanbirdrich)    0.09343 .  
# scale(meanbatpres_bin) 0.03401 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9216341
# Parametric bootstrap results based on 100 fitted replicates

#* flower colourfulness abiotic model ----

# using phylolm to run phylogenetic logistic regression by Ives & Garland method
# VERY SLOW/COMPUTATIONALLY INTENSE
multi_reg$flcolour_abioticPGLS <- phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + 
                                               scale(meanMAP) + 
                                               scale(meanAVP),
                                             data = pgls_data,
                                             phy = tree_pgls,
                                             method = "logistic_IG10", 
                                             boot = 100)
summary(multi_reg$flcolour_abioticPGLS)

# Warning messages:
#   1: In phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) +  :
#   the estimate of 'alpha' (0.935002387515753) reached the upper bound (0.935504358152755).
#   This may simply reflect a flat likelihood at large alpha values,
#   meaning that the phylogenetic correlation is estimated to be negligible.
#   2: In phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) +  :
#   the boundary of the linear predictor has been reached during the optimization procedure.
#   You can increase this bound by increasing 'btol'.
#                                                   
# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanMAT) +
#                       scale(meanMAP) + scale(meanAVP), data = pgls_data, phy = tree_pgls,
#                     method = "logistic_IG10", boot = 100)
# AIC     logLik Pen.logLik
# 424.6     -207.3     -200.3
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.9350024
# bootstrap mean: 0.6983564 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.2185395,0.9351291)
# 
# Coefficients:
#   Estimate     StdErr    z.value lowerbootCI upperbootCI p.value
# (Intercept)     -2.288204   0.203393 -11.250185   -2.793611     -1.8605 < 2e-16 ***
#   scale(meanMAT)   0.217935   0.167753   1.299144   -0.054573      0.5065 0.19389
# scale(meanMAP)  -0.456089   0.177682  -2.566884   -0.955646     -0.1234 0.01026 *
#   scale(meanAVP)  -0.279404   0.204229  -1.368096   -0.598215      0.0614 0.17128
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9350024
# Parametric bootstrap results based on 100 fitted replicates

#* flower colourfulness biotic model ----

# using phylolm to run phylogenetic logistic regression by Ives & Garland method
# VERY SLOW/COMPUTATIONALLY INTENSE
multi_reg$flcolour_bioticPGLS <- phylolm::phyloglm(colour_fullbinary ~ scale(meanbirdrich) + 
                                               scale(meanbatpres_bin),
                                             data = pgls_data,
                                             phy = tree_pgls,
                                             method = "logistic_IG10", 
                                             boot = 100)
summary(multi_reg$flcolour_bioticPGLS)

# Warning messages:
#   1: In phylolm::phyloglm(colour_fullbinary ~ scale(meanbirdrich) + scale(meanbatpres_bin),  :
#   the estimate of 'alpha' (0.921940772659661) reached the upper bound (0.935504358152755).
#   This may simply reflect a flat likelihood at large alpha values,
#   meaning that the phylogenetic correlation is estimated to be negligible.
#   2: In phylolm::phyloglm(colour_fullbinary ~ scale(meanbirdrich) + scale(meanbatpres_bin),  :
#   the boundary of the linear predictor has been reached during the optimization procedure.
#   You can increase this bound by increasing 'btol'.

# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanbirdrich) + 
#                       scale(meanbatpres_bin), data = pgls_data, phy = tree_pgls, 
#                     method = "logistic_IG10", boot = 100)
# AIC     logLik Pen.logLik 
# 418.6     -205.3     -200.1 
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.9219408 
# bootstrap mean: 0.5448469 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.07379715,0.9353375)
# 
# Coefficients:
#   Estimate    StdErr   z.value lowerbootCI upperbootCI p.value    
# (Intercept)             -2.30201   0.20371 -11.30025    -2.71243     -1.8081 < 2e-16 ***
#   scale(meanbirdrich)     -0.48803   0.20315  -2.40228    -0.84913     -0.1235 0.01629 *  
#   scale(meanbatpres_bin)  -0.38649   0.18964  -2.03803    -0.67594     -0.0822 0.04155 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9219408
# Parametric bootstrap results based on 100 fitted replicates

#* AIC to compare flower colourfulness PGLS ----

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

c(multi_reg$flcolour_abioticPGLS$aic, # 424.5613
  multi_reg$flcolour_bioticPGLS$aic,  # 418.5980
  multi_reg$flcolour_PGLS$aic).       # 416.3765

# dif between full model and biotic (~2) smaller than dif between full
# model and abiotic (~8)

#### conclusions ####

# comparing all these models, my interpretations are that: 
# 1) comparing BIC between the full models, models with biotic variables only & 
#    models with abiotic variables only suggests that biotic variables better
#    predict eucalypt bud size and flower colour than abiotic variables. This is
#    true both with and without the phylogenetic relatedness of eucalypt species
#    accounted for.
# 2) in least squares regressions (without phylogeny) abiotic variables
#    explain 25 % of the variation in species mean leaf area, while they only 
#    explain only 14 % of variation in species mean bud size.
# 3) looking at effect sizes




rm(spp, tree_pgls, multi_reg, pgls_data)

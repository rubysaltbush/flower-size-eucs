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
# fewer colourful (1) than white-cream (0) taxa

#* predictor variables ----

hist(euc_traits_nosubsp$meanMAT)
# roughly normal
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

multi_reg <- list()

#* leaf area abiotic ----

# to compare with bud size multiple regression, expect that leaf area
# will increase positively with species mean temp, precip and phosphorus
# not including birds or bats as no predicted relationships

# The full model formula will look like this:

#  log(leaf_area) ~ meanMAT + meanMAP + meanAVP

multi_reg$leafarea_abiotic <- lm(log(leafarea_mm2) ~ 
                                              meanMAT + meanMAP + meanAVP, #abiotic
                                            data = euc_traits_nosubsp)
summary(multi_reg$leafarea_abiotic)

# Call:
#   lm(formula = log(leafarea_mm2) ~ meanMAT + meanMAP + meanAVP, 
#      data = euc_traits_nosubsp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.91555 -0.38677  0.03106  0.38317  2.06176 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.701e+00  1.292e-01  44.130  < 2e-16 ***
#   meanMAT     3.774e-02  5.993e-03   6.298 5.02e-10 ***
#   meanMAP     7.262e-04  7.755e-05   9.364  < 2e-16 ***
#   meanAVP     1.790e-02  3.706e-03   4.829 1.65e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.624 on 782 degrees of freedom
# (12 observations deleted due to missingness)
# Multiple R-squared:  0.2556,	Adjusted R-squared:  0.2528 
# F-statistic: 89.51 on 3 and 782 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$leafarea_abiotic)
# meanMAT  meanMAP  meanAVP 
# 1.408954 1.472960 1.796042 
# VIF under 1.8 for all, multicollinearity unlikely

#* bud size abiotic ----

# to compare with leaf area multiple regression, expect that leaf area
# will increase positively with species mean temp, precip and phosphorus
# not including birds or bats as no predicted relationships

multi_reg$budsize_abiotic <- lm(logbudsize_mm2 ~ 
                                             meanMAT + meanMAP + meanAVP, #abiotic
                                           data = euc_traits_nosubsp)
summary(multi_reg$budsize_abiotic)

# Call:
#   lm(formula = logbudsize_mm2 ~ meanMAT + meanMAP + meanAVP, data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2711 -0.6229 -0.0639  0.4801  3.7359 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.7323995  0.1862765  25.405  < 2e-16 ***
#   meanMAT     -0.0114042  0.0086175  -1.323 0.186101    
# meanMAP     -0.0003881  0.0001113  -3.486 0.000517 ***
#   meanAVP     -0.0348467  0.0053396  -6.526 1.22e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8955 on 776 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1347,	Adjusted R-squared:  0.1313 
# F-statistic: 40.25 on 3 and 776 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_abiotic)
# meanMAT  meanMAP  meanAVP 
# 1.412562 1.469958 1.798264 
# VIF under 1.8 for all, multicollinearity unlikely

#* bud size full model ----

multi_reg$budsize_full <- lm(logbudsize_mm2 ~ 
                                          meanMAT + meanMAP + meanAVP + #abiotic
                                          meanbirdrich + meanbatpres_bin, #biotic
                                        data = euc_traits_nosubsp)
summary(multi_reg$budsize_full)

# Call:
#   lm(formula = logbudsize_mm2 ~ meanMAT + meanMAP + meanAVP + meanbirdrich + 
#        meanbatpres_bin, data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2199 -0.5542 -0.0717  0.4887  3.6779 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.281e+00  2.702e-01  15.841  < 2e-16 ***
#   meanMAT          9.525e-03  9.776e-03   0.974   0.3302    
# meanMAP         -3.509e-05  1.238e-04  -0.283   0.7769    
# meanAVP         -1.388e-02  6.128e-03  -2.265   0.0238 *  
#   meanbirdrich    -7.312e-03  6.367e-03  -1.148   0.2512    
# meanbatpres_bin -5.552e-01  1.228e-01  -4.520 7.16e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8727 on 774 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1803,	Adjusted R-squared:  0.175 
# F-statistic: 34.05 on 5 and 774 DF,  p-value: < 2.2e-16

# bat presence/absence explains most variation, also phosphorus but small effect

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_full)
# meanMAT         meanMAP         meanAVP    meanbirdrich meanbatpres_bin 
# 1.914214        1.913569        2.493547        2.647623        3.836827 
# VIF below 3.85 for all, generally recommended threshold is 5-10

#* flower colourfulness full model ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_full <- glm(as.numeric(colour_fullbinary) ~ 
                                            meanMAT + meanMAP + meanAVP + 
                                            meanbirdrich + meanbatpres_bin,
                                          data = euc_traits_nosubsp,
                                          family = binomial())
summary(multi_reg$flcolour_full)

# Call:
#   glm(formula = colour_fullbinary ~ meanMAT + meanMAP + meanAVP + 
#         meanbirdrich + meanbatpres_bin, family = binomial(), data = euc_traits_nosubsp)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)     -0.9331043  1.2673817  -0.736  0.46158   
# meanMAT          0.0471845  0.0433322   1.089  0.27620   
# meanMAP         -0.0002741  0.0005545  -0.494  0.62114   
# meanAVP         -0.0357412  0.0258664  -1.382  0.16704   
# meanbirdrich    -0.0381501  0.0288910  -1.320  0.18667   
# meanbatpres_bin -1.5677426  0.5838047  -2.685  0.00724 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 481.53  on 772  degrees of freedom
# (20 observations deleted due to missingness)
# AIC: 493.53
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_full)
# meanMAT         meanMAP         meanAVP    meanbirdrich meanbatpres_bin 
# 1.766085        1.871925        1.472729        1.631975        3.502379 
# VIF still under 4 for all, multicollinearity unlikely

#### with phylogeny ####

# subset data to phylogenetic least squares regression variables
# removing data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, colour_fullbinary, meanAVP, meanMAT, 
                meanMAP,meanbirdrich, meanbatpres_bin) %>%
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
spp <- rownames(pgls_data) # set species names as reference point

#* bud size full PGLS ----

multi_reg$budsize_PGLS <- nlme::gls(logbudsize_mm2 ~ meanMAT + 
                                         meanMAP + meanAVP + 
                                         meanbirdrich + meanbatpres_bin, 
                                       correlation = ape::corBrownian(phy = tree_pgls, 
                                                                      form = ~spp),
                                       data = pgls_data, method = "ML")
summary(multi_reg$budsize_PGLS)

# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ meanMAT + meanMAP + meanAVP + meanbirdrich +      meanbatpres_bin 
# Data: pgls_data 
# AIC      BIC    logLik
# 2151.951 2183.491 -1068.975
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)      4.337911 2.7026075  1.605084  0.1090
# meanMAT         -0.013184 0.0125714 -1.048730  0.2947
# meanMAP         -0.000440 0.0001243 -3.537104  0.0004
# meanAVP          0.010079 0.0052163  1.932166  0.0538
# meanbirdrich    -0.016222 0.0051560 -3.146294  0.0017
# meanbatpres_bin  0.294969 0.0824193  3.578875  0.0004
# 
# Correlation: 
#   (Intr) menMAT menMAP menAVP mnbrdr
# meanMAT         -0.109                            
# meanMAP         -0.045  0.080                     
# meanAVP         -0.043  0.299 -0.174              
# meanbirdrich    -0.061  0.253  0.057 -0.083       
# meanbatpres_bin  0.047 -0.419 -0.011 -0.099 -0.506
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.303837451 -0.088099764 -0.001542781  0.083065783  0.532925665 
# 
# Residual standard error: 7.062507 
# Degrees of freedom: 669 total; 663 residual

#* flower colourfulness full model ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_PGLS <- nlme::gls(as.numeric(colour_fullbinary) ~ meanMAT + 
                                       meanMAP + meanAVP + 
                                       meanbirdrich + meanbatpres_bin, 
                                     correlation = ape::corBrownian(phy = tree_pgls, 
                                                                    form = ~spp),
                                     data = pgls_data, method = "ML")
summary(multi_reg$flcolour_PGLS)

# Generalized least squares fit by maximum likelihood
# Model: as.numeric(colour_fullbinary) ~ meanMAT + meanMAP + meanAVP +      meanbirdrich + meanbatpres_bin 
# Data: pgls_data 
# AIC      BIC    logLik
# 1041.021 1072.561 -513.5104
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)      0.3961881 1.1781268  0.336286  0.7368
# meanMAT         -0.0022067 0.0054801 -0.402669  0.6873
# meanMAP         -0.0002070 0.0000542 -3.820388  0.0001
# meanAVP          0.0017863 0.0022739  0.785586  0.4324
# meanbirdrich    -0.0023820 0.0022476 -1.059790  0.2896
# meanbatpres_bin -0.0745808 0.0359284 -2.075817  0.0383
# 
# Correlation: 
#   (Intr) menMAT menMAP menAVP mnbrdr
# meanMAT         -0.109                            
# meanMAP         -0.045  0.080                     
# meanAVP         -0.043  0.299 -0.174              
# meanbirdrich    -0.061  0.253  0.057 -0.083       
# meanbatpres_bin  0.047 -0.419 -0.011 -0.099 -0.506
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.096964960 -0.077419834 -0.029194632 -0.005510027  0.350241160 
# 
# Residual standard error: 3.078704 
# Degrees of freedom: 669 total; 663 residual

#### conclusions ####

# comparing all these models, my interpretations are that: 
# 1) including biotic variables includes model R2 for bud size (.135 with 
# abiotic only to 0.18)
# 2) with full model precipitation becomes a less significant predictor, with
# bud size mostly explained by presence/absence of bats in a eucalypt's 
# environment improves 
# 3) this matches result of full model for flower colourfulness, bat presence
# best predictor
# 4) with phylogenetic relatedness included via PGLS, bat presence still a 
# significant predictor of bud size and flower colourfulness but mean annual
# precipitation also considered a significant predictor of both, plus bird
# richness for bud size - model doesn't seem to be accounting for significant
# covariance in these predictors? will need to ask someone more expert in PGLS

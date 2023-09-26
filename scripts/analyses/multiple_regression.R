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

multiple_regressions <- list()

#* leaf area abiotic ----

# to compare with bud size multiple regression, expect that leaf area
# will increase positively with species mean temp, precip and phosphorus
# not including birds or bats as no predicted relationships

# The full model formula will look like this:

#  log(leaf_area) ~ meanMAT + meanMAP + meanAVP

multiple_regressions$leafarea_abiotic <- lm(log(leafarea_mm2) ~ 
                                              meanMAT + meanMAP + meanAVP, #abiotic
                                            data = euc_traits_nosubsp)
summary(multiple_regressions$leafarea_abiotic)

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
car::vif(multiple_regressions$leafarea_abiotic)
# meanMAT  meanMAP  meanAVP 
# 1.408954 1.472960 1.796042 
# VIF under 1.8 for all, multicollinearity unlikely

#* bud size abiotic ----

# to compare with leaf area multiple regression, expect that leaf area
# will increase positively with species mean temp, precip and phosphorus
# not including birds or bats as no predicted relationships

multiple_regressions$budsize_abiotic <- lm(logbudsize_mm2 ~ 
                                             meanMAT + meanMAP + meanAVP, #abiotic
                                           data = euc_traits_nosubsp)
summary(multiple_regressions$budsize_abiotic)

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
car::vif(multiple_regressions$budsize_abiotic)
# meanMAT  meanMAP  meanAVP 
# 1.412562 1.469958 1.798264 
# VIF under 1.8 for all, multicollinearity unlikely

#* bud size full model ----

multiple_regressions$budsize_full <- lm(logbudsize_mm2 ~ 
                                          meanMAT + meanMAP + meanAVP + #abiotic
                                          meanbirdrich + meanbatpres_bin, #biotic
                                        data = euc_traits_nosubsp)
summary(multiple_regressions$budsize_full)

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
car::vif(multiple_regressions$budsize_full)
# meanMAT         meanMAP         meanAVP    meanbirdrich meanbatpres_bin 
# 1.914214        1.913569        2.493547        2.647623        3.836827 
# VIF below 3.85 for all, generally recommended threshold is 5-10

#* flower colourfulness full model ----

# using glm with binomial distribution for logistic regression
multiple_regressions$flcolour_full <- glm(as.numeric(colour_fullbinary) ~ 
                                            meanMAT + meanMAP + meanAVP + 
                                            meanbirdrich + meanbatpres_bin,
                                          data = euc_traits_nosubsp,
                                          family = binomial())
summary(multiple_regressions$flcolour_full)

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
car::vif(multiple_regressions$flcolour_full)
# meanMAT         meanMAP         meanAVP    meanbirdrich meanbatpres_bin 
# 1.766085        1.871925        1.472729        1.631975        3.502379 
# VIF still under 4 for all, multicollinearity unlikely



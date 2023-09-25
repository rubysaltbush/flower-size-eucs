# run multiple regression models (least squares and glm) to assess whether the  
# distributions of eucalypt flower (bud) size and eucalypt flower colourfulness 
# are shaped more by biotic or abiotic environmental variabless

# redo below with phylogeny included upfront

#### examine data ####

#* response variables ----

# first, main response variable, bud size (mm2)
hist(euc_traits_nosubsp$budsize_mm2)
# extreme right-skewed distribution, what about log transformation?
hist(euc_traits_nosubsp$logbudsize_mm2)
# still a bit right-skewed but fairly normal

# second response variable is flower colourfulness, binary
plot(euc_traits_nosubsp$colour_fullbinary)
table(euc_traits_nosubsp$colour_fullbinary)

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

#* check correlations ----

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

#### run PGLS models ####

#* log bud size mm2 ----

#** full model ----

# The full model formula will look like this:
  
  #  log(budsize_mm2) ~ meanMAT + meanMAP + meanAVP + meanbirdrich + meanbatpres

multiple_regressions <- list()
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

#* flower colourfulness ----

#** full model ----

# The full model formula will look like this:

#  colour_fullbinary ~ meanMAT + meanMAP + meanAVP + meanbirdrich + meanbatpres_bin

# make colour binary numeric
euc_traits_nosubsp$colour_fullbinary <- as.numeric(euc_traits_nosubsp$colour_fullbinary)

# using glm with binomial distribution for logistic regression
multiple_regressions$flcolour_full <- glm(colour_fullbinary ~ 
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

# results of multiple regressions likely need to be displayed in table
# though I could do a scatter plot of 

plot(logbudsize_mm2 ~ meanAVP, data = euc_traits_nosubsp, col = meanbatpres_bin)

# one outlier with high avp, should I remove this???


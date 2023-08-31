# run linear models (multiple regression and glm) to assess whether the  
# distributions of eucalypt flower (bud) size and eucalypt flower colourfulness 
# are shaped more by biotic or abiotic environmental variabless

#### examine data ####

#* response variables ----

# first, main response variable, bud size (mm2)
hist(euc_traits_nosubsp$budsize_mm2)
# extreme right-skewed distribution, what about log transformation?
hist(log(euc_traits_nosubsp$budsize_mm2))
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
                columns = c(17, 20, 24, 25, 23, 26, 28), 
                columnLabels = c("Bud size",
                                 "Flower colour",
                                 "Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence")) + theme_pubr()
# lots going on - looks like bud size best correlated with bat presence, then
# soil phosphorus, then bird richness
# and flower colour bats then birds then phosphorus
# temperature and precipitation low correlation for both bud size and flower colour

# check for collinearity among predictor variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(24, 25, 23, 26, 28), 
                columnLabels = c("Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence")) + theme_pubr()
ggsave("figures/pairwise correlation plot.pdf", width = 9, height = 5.6)
# some correlation between mean bird richness and bat presence (0.69)
# and birds/bats and mean soil available phosphorus (~bird = 0.62, ~bat = 0.54)
# all correlations are below 0.7 and all are independently interesting so will
# leave in model, check VIF after

#### run linear models ####

#* log bud size mm2 ----

#** full model ----

# The full model formula will look like this:
  
  #  log(budsize_mm2) ~ meanMAT + meanMAP + meanAVP + meanbirdrich + meanbatpres

multiple_regressions <- list()
multiple_regressions$budsize_full <- lm(log(budsize_mm2) ~ 
                                          meanMAT + meanMAP + meanAVP + 
                                          meanbirdrich + meanbatpres,
                                        data = euc_traits_nosubsp)
summary(multiple_regressions$budsize_full)

# Call:
#   lm(formula = log(budsize_mm2) ~ meanMAT + meanMAP + meanAVP + 
#        meanbirdrich + meanbatpres, data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1797 -0.5680 -0.0637  0.4630  3.5927 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.2683854  0.2131402  20.026  < 2e-16 ***
#   meanMAT       0.0020393  0.0078031   0.261  0.79389    
# meanMAP       0.0002000  0.0001036   1.930  0.05394 .  
# meanAVP      -0.0150678  0.0051273  -2.939  0.00339 ** 
#   meanbirdrich -0.0043171  0.0060995  -0.708  0.47929    
# meanbatpres  -0.7034861  0.1042282  -6.749 2.91e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.865 on 774 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1947,	Adjusted R-squared:  0.1895 
# F-statistic: 37.43 on 5 and 774 DF,  p-value: < 2.2e-16

# bat presence/absence and soil phosphorus explain most variation, precipitation
# ~maybe some amount but p>0.05

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$budsize_full)
# meanMAT      meanMAP      meanAVP meanbirdrich  meanbatpres 
# 1.112974     1.057996     1.777219     2.473326     2.402253 
# VIF below 2.5 for all variables, no serious multicollinearity 

#** final model ----

# run again, removing non-significant factors from regression
multiple_regressions$budsize_final <- lm(log(budsize_mm2) ~ meanAVP + meanbatpres,
                                         data = euc_traits_nosubsp)
summary(multiple_regressions$budsize_final)

# Call:
#   lm(formula = log(budsize_mm2) ~ meanAVP + meanbatpres, data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1240 -0.5667 -0.0722  0.4583  3.6055 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.367849   0.063897  68.358  < 2e-16 ***
#   meanAVP     -0.017231   0.004671  -3.689 0.000241 ***
#   meanbatpres -0.703526   0.081678  -8.613  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8659 on 777 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:   0.19,	Adjusted R-squared:  0.1879 
# F-statistic: 91.12 on 2 and 777 DF,  p-value: < 2.2e-16

# triple check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$budsize_final)
# meanAVP meanbatpres 
# 1.472267    1.472267 

#* flower colourfulness ----

#** full model ----

# The full model formula will look like this:

#  colour_fullbinary ~ meanMAT + meanMAP + meanAVP + meanbirdrich + meanbatpres

# using glm with binomial distribution for logistic regression
multiple_regressions$flcolour_full <- glm(colour_fullbinary ~ 
                                            meanMAT + meanMAP + meanAVP + 
                                            meanbirdrich + meanbatpres,
                                          data = euc_traits_nosubsp,
                                          family = binomial())
summary(multiple_regressions$flcolour_full)

# Call:
#   glm(formula = colour_fullbinary ~ meanMAT + meanMAP + meanAVP + 
#         meanbirdrich + meanbatpres, family = binomial(), data = euc_traits_nosubsp)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.1082355  0.9251137  -1.198   0.2309    
# meanMAT       0.0325679  0.0306552   1.062   0.2881    
# meanMAP       0.0004911  0.0004031   1.218   0.2230    
# meanAVP      -0.0422988  0.0226345  -1.869   0.0617 .  
# meanbirdrich -0.0316837  0.0267977  -1.182   0.2371    
# meanbatpres  -1.9521996  0.4724624  -4.132  3.6e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 477.74  on 772  degrees of freedom
# (20 observations deleted due to missingness)
# AIC: 489.74
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$flcolour_full)
# meanMAT      meanMAP      meanAVP meanbirdrich  meanbatpres 
# 1.128953     1.079208     1.201261     1.423437     1.854147 
# VIF below 1.9 for all variables, no serious multicollinearity 

#** final model ----

# run again, removing non-significant factors from regression

# using glm with binomial distribution for logistic regression
multiple_regressions$flcolour_final <- glm(colour_fullbinary ~ 
                                          meanbatpres,
                                          data = euc_traits_nosubsp,
                                          family = binomial())
summary(multiple_regressions$flcolour_final)

# Call:
#   glm(formula = colour_fullbinary ~ meanbatpres, family = binomial(), 
#       data = euc_traits_nosubsp)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.1802     0.1309  -9.014  < 2e-16 ***
#   meanbatpres  -2.4600     0.3411  -7.212 5.52e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 565.74  on 778  degrees of freedom
# Residual deviance: 489.59  on 777  degrees of freedom
# (19 observations deleted due to missingness)
# AIC: 493.59
# 
# Number of Fisher Scoring iterations: 6

#### graph results ####

# not sure how to do this? hmm...

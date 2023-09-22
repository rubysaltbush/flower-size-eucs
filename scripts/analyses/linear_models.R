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
                columns = c(17, 20, 24, 25, 23, 27, 28), 
                columnLabels = c("Bud size",
                                 "Flower colour",
                                 "Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence")) + theme_pubr()
ggsave("figures/regressions/full pairwise correlation plot.pdf", 
       width = 13, height = 10)
# lots going on - looks like bud size best correlated with bat presence, then
# soil phosphorus, then bird richness then precipitation
# and flower colour bats then birds then phosphorus then precipitation
# temperature no correlation for both bud size and flower colour

# check for collinearity among predictor variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(24, 25, 23, 27, 28), 
                columnLabels = c("Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence")) + theme_pubr()
ggsave("figures/regressions/predictors pairwise correlation plot.pdf", width = 9, height = 5.6)
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
# -2.2424 -0.5540 -0.0629  0.4557  3.6427 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.842e+00  2.880e-01  13.341  < 2e-16 ***
#   meanMAT       2.357e-02  1.031e-02   2.286   0.0225 *  
#   meanMAP       2.567e-05  1.212e-04   0.212   0.8323    
# meanAVP      -7.612e-03  6.274e-03  -1.213   0.2254    
# meanbirdrich  1.111e-03  6.611e-03   0.168   0.8666    
# meanbatpres  -8.647e-01  1.437e-01  -6.017 2.73e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8642 on 774 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1963,	Adjusted R-squared:  0.1911 
# F-statistic:  37.8 on 5 and 774 DF,  p-value: < 2.2e-16

# bat presence/absence explains most variation, also ???temperature???

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$budsize_full)
# meanMAT      meanMAP      meanAVP meanbirdrich  meanbatpres 
# 2.171117     1.869797     2.666469     2.911299     4.574500 
# VIF below 4.6 for all, generally recommended threshold is 5-10

#** final model ----

# run again, removing non-significant factors from regression and 
# including interaction (as can't think why else temperature would 
# be significant predictor?)
multiple_regressions$budsize_final <- lm(log(budsize_mm2) ~ meanbatpres*meanMAT,
                                         data = euc_traits_nosubsp)
summary(multiple_regressions$budsize_final)

# Call:
#   lm(formula = log(budsize_mm2) ~ meanbatpres * meanMAT, data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2836 -0.5439 -0.0720  0.4474  3.6585 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.99896    0.30929  12.929  < 2e-16 ***
#   meanbatpres         -1.32383    0.36609  -3.616 0.000318 ***
#   meanMAT              0.01217    0.01784   0.682 0.495302    
# meanbatpres:meanMAT  0.02303    0.02076   1.110 0.267547    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8632 on 776 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.196,	Adjusted R-squared:  0.1929 
# F-statistic: 63.06 on 3 and 776 DF,  p-value: < 2.2e-16

# triple check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$budsize_final)
# there are higher-order terms (interactions) in this model
# consider setting type = 'predictor'; see ?vif
# meanbatpres             meanMAT meanbatpres:meanMAT 
# 29.760168            6.515787           39.573433 

# can safely ignore high VIF as this is just due to interaction term in model

#* flower colourfulness ----

#** full model ----

# The full model formula will look like this:

#  colour_fullbinary ~ meanMAT + meanMAP + meanAVP + meanbirdrich + meanbatpres

# make colour binary numeric
euc_traits_nosubsp$colour_fullbinary <- as.numeric(euc_traits_nosubsp$colour_fullbinary)

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
# (Intercept)  -2.674e+00  1.492e+00  -1.792 0.073141 .  
# meanMAT       1.070e-01  5.181e-02   2.065 0.038945 *  
#   meanMAP       2.999e-05  5.446e-04   0.055 0.956088    
# meanAVP      -1.218e-02  2.706e-02  -0.450 0.652469    
# meanbirdrich -1.416e-02  3.105e-02  -0.456 0.648406    
# meanbatpres  -2.643e+00  7.644e-01  -3.458 0.000544 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 475.65  on 772  degrees of freedom
# (20 observations deleted due to missingness)
# AIC: 487.65
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$flcolour_full)
# meanMAT      meanMAP      meanAVP meanbirdrich  meanbatpres 
# 2.386535     1.949238     1.618947     1.774548     4.758462 
# VIF still under 5 for all, multicollinearity unlikely

#** final model ----

# run again, removing non-significant factors from regression

# using glm with binomial distribution for logistic regression
multiple_regressions$flcolour_final <- glm(colour_fullbinary ~ meanbatpres*meanMAT,
                                          data = euc_traits_nosubsp,
                                          family = binomial())
summary(multiple_regressions$flcolour_final)

# Call:
#   glm(formula = colour_fullbinary ~ meanbatpres * meanMAT, family = binomial(), 
#       data = euc_traits_nosubsp)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.61462    0.92214  -1.751 0.079953 .  
# meanbatpres         -8.13605    2.28186  -3.566 0.000363 ***
#   meanMAT              0.02634    0.05318   0.495 0.620433    
# meanbatpres:meanMAT  0.25233    0.10141   2.488 0.012836 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 565.74  on 778  degrees of freedom
# Residual deviance: 472.11  on 775  degrees of freedom
# (19 observations deleted due to missingness)
# AIC: 480.11
# 
# Number of Fisher Scoring iterations: 7

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regressions$flcolour_final)
# there are higher-order terms (interactions) in this model
# consider setting type = 'predictor'; see ?vif
# meanbatpres             meanMAT meanbatpres:meanMAT 
# 43.207886            3.032475           52.402661 
# can safely ignore high VIF as this is just due to interaction term in model

#### graph results ####

# not sure how to do this? hmm...

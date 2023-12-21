# run multiple regression models (least squares and glm) to assess whether the  
# distribution of eucalypt flower (bud) size is shaped more by abiotic (temp, 
# precip, and soil phosphorus) or biotic (flower-visiting bird richness and bat 
# presence) environmental variables

#### examine data ####

#* response variables ----

# main response variable, eucalypt bud size (mm2)
hist(euc_traits_nosubsp$budsize_mm2)
# right-skewed distribution, log transform
hist(log(euc_traits_nosubsp$budsize_mm2))
# fairly normal with log transformation

# second response variable is flower colourfulness, binary
plot(euc_traits_nosubsp$colour_fullbinary)
table(euc_traits_nosubsp$colour_fullbinary)
# 93 colourful (1), 696 white-cream (0) taxa

#* predictor variables ----

hist(euc_traits_nosubsp$meanMAT)
# roughly normal but extra high temps
hist(euc_traits_nosubsp$meanMAP)
# right-skewed
hist(euc_traits_nosubsp$meanAVP)
# slightly right-skewed
plot(euc_traits_nosubsp$meanbatpres_bin)
table(euc_traits_nosubsp$meanbatpres_bin)
# 0   1 
# 367 420
# almost equal numbers in bat:no bats categories
hist(euc_traits_nosubsp$meanbirdrich)
# normalish

#* correlation plots ----

# visually check relationships between predictor and response variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(6, 9, 12, 13, 14, 15, 17), 
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
# then soil phosphorus, then bird richness, then precipitation,
# and flower colour bats then birds then phosphorus then precipitation
# temperature no correlation for both bud size and flower colour

# check for collinearity among predictor variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(12, 13, 14, 15, 17), 
                columnLabels = c("Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence"))
ggsave("figures/regressions/predictors pairwise correlation plot.pdf", width = 9, height = 5.6)
# some correlation between mean bird richness and bat presence (0.73)
# and birds/bats and mean soil available phosphorus (~bird = 0.68, ~bat = 0.56)
# all correlations are below 0.75 and all are independently interesting so will
# leave in model, check VIF after

#### run models ####

# with all predictors scaled so can compare effect sizes meaningfully

multi_reg <- list()

#* bud size abiotic ----

# first run model of bud size as predicted just by abiotic environment

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

# adjusted R2 lowish at 0.14

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$budsize_abiotic)

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
# -2.0941 -0.5682 -0.0645  0.4614  3.7223 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.74881    0.03157 118.745  < 2e-16 ***
#   scale(meanbirdrich)    -0.13098    0.04576  -2.862  0.00432 ** 
#   scale(meanbatpres_bin) -0.27891    0.04587  -6.081 1.87e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8817 on 777 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1602,	Adjusted R-squared:  0.158 
# F-statistic:  74.1 on 2 and 777 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_biotic)
# scale(meanbirdrich) scale(meanbatpres_bin) 
# 2.10535                2.10535 
# VIF under 2.2 for all, multicollinearity unlikely

# some improvement in adjusted R2 with biotic rather than abiotic predictors at 0.16

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$budsize_biotic)
# look quite weird with binary predictor

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
# -2.3145 -0.5766 -0.0655  0.4780  3.6607 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.748926   0.031259 119.930  < 2e-16 ***
#   scale(meanMAT)          0.038201   0.044075   0.867  0.38636    
# scale(meanMAP)         -0.002826   0.042816  -0.066  0.94739    
# scale(meanAVP)         -0.158253   0.050647  -3.125  0.00185 ** 
#   scale(meanbirdrich)    -0.009713   0.055426  -0.175  0.86093    
# scale(meanbatpres_bin) -0.283715   0.062957  -4.506 7.61e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.873 on 774 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1798,	Adjusted R-squared:  0.1745 
# F-statistic: 33.94 on 5 and 774 DF,  p-value: < 2.2e-16

# bat presence/absence explains most variation, also phosphorus but smaller effect

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_full)
# scale(meanMAT)         scale(meanMAP)         scale(meanAVP)    
# 1.999784               1.885292               2.627319              
# scale(meanbirdrich)   scale(meanbatpres_bin) 
# 3.150322              4.045992
# VIF below 4.1 for all, generally recommended threshold is 5-10
# higher VIF for bat presence here suggests some overlap with other predictors

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$budsize_full)

#* AIC/BIC to compare bud size models (Bayesian Information Criterion) ----

# first BIC (penalises models with too many variables, lowest BIC model best)

BIC(multi_reg$budsize_abiotic, multi_reg$budsize_biotic, multi_reg$budsize_full)

#                           df      BIC
# multi_reg$budsize_abiotic  5 2062.106
# multi_reg$budsize_biotic   4 2040.705
# multi_reg$budsize_full     7 2042.211

# biotic model has lowest BIC

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$budsize_abiotic, multi_reg$budsize_biotic, multi_reg$budsize_full)

#                           df      AIC
# multi_reg$budsize_abiotic  5 2038.809
# multi_reg$budsize_biotic   4 2022.068
# multi_reg$budsize_full     7 2009.595

# dif between full and abiotic (~29) larger than dif between full and biotic (~13)
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

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$flcolour_abiotic)

# quite weird with binary response variable!

#* flower colourfulness biotic ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_biotic <- glm(colour_fullbinary ~ scale(meanbirdrich) + 
                                 scale(meanbatpres_bin),
                               data = tempflcol,
                               family = binomial)
summary(multi_reg$flcolour_biotic)

# Call:
#   glm(formula = colour_fullbinary ~ scale(meanbirdrich) + scale(meanbatpres_bin), 
#       family = binomial, data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -2.4944     0.1705 -14.630  < 2e-16 ***
#   scale(meanbirdrich)     -0.5100     0.2264  -2.253 0.024285 *  
#   scale(meanbatpres_bin)  -0.7411     0.1994  -3.717 0.000201 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 487.96  on 775  degrees of freedom
# AIC: 493.96
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_biotic)
# scale(meanbirdrich) scale(meanbatpres_bin) 
# 1.654498               1.654498  
# VIF under 1.66, multicollinearity unlikely

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$flcolour_biotic)

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
#   glm(formula = colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) + 
#         scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#       family = binomial(), data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.62275    0.19741 -13.286   <2e-16 ***
#   scale(meanMAT)          0.15976    0.19729   0.810   0.4181    
# scale(meanMAP)         -0.03623    0.18870  -0.192   0.8477    
# scale(meanAVP)         -0.37183    0.21869  -1.700   0.0891 .  
# scale(meanbirdrich)    -0.41847    0.26986  -1.551   0.1210    
# scale(meanbatpres_bin) -0.70877    0.30239  -2.344   0.0191 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 479.94  on 772  degrees of freedom
# AIC: 491.94
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_full)
# scale(meanMAT)         scale(meanMAP)         scale(meanAVP)    scale(meanbirdrich) 
# 2.005967               1.805250               1.501611               1.703532 
# scale(meanbirdrich)   scale(meanbatpres_bin) 
# 1.703532              3.752646 
# VIF under 4 for all, multicollinearity unlikely

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$flcolour_full)

#* AIC/BIC to compare flower colourfulness models ----

# first BIC (penalises models with too many variables, lowest BIC model best)

BIC(multi_reg$flcolour_abiotic, multi_reg$flcolour_biotic, multi_reg$flcolour_full)

#                             df      BIC
# multi_reg$flcolour_abiotic  4 526.4958
# multi_reg$flcolour_biotic   3 507.9303
# multi_reg$flcolour_full     6 519.8797

# biotic model has lowest BIC

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$flcolour_abiotic, multi_reg$flcolour_biotic, multi_reg$flcolour_full)

#                             df      AIC
# multi_reg$flcolour_abiotic  4 507.8689
# multi_reg$flcolour_biotic   3 493.9602
# multi_reg$flcolour_full     6 491.9394

# dif between full and abiotic (~16) larger than dif between full and biotic (~2)
# suggesting biotic variables more important to model

rm(tempflcol)

#### with phylogeny ####

#* prep data ---- 

# subset data to phylogenetic least squares regression variables
# removing data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2,
                meanAVP, meanMAT, meanMAP, meanbirdrich, meanbatpres_bin) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2))
tree_pgls <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_pgls$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1] # tree names to row names
pgls_data[,1] <- NULL

spp <- rownames(pgls_data) # set species names as reference point

#* bud size abiotic PGLS ----

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
# 2180.942 2203.515 -1085.471
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)     3.679728 2.7082520  1.358710  0.1747
# scale(meanMAT)  0.015339 0.0492438  0.311493  0.7555
# scale(meanMAP) -0.141598 0.0426460 -3.320307  0.0009
# scale(meanAVP)  0.033280 0.0463575  0.717896  0.4731
# 
# Correlation: 
#   (Intr) s(MAT) s(MAP)
# scale(meanMAT) -0.010              
# scale(meanMAP) -0.008  0.093       
# scale(meanAVP) -0.001  0.283 -0.208
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.311732698 -0.082279194 -0.002503113  0.079509622  0.522033930 
# 
# Residual standard error: 7.141769 
# Degrees of freedom: 675 total; 671 residual

# plot residuals to check
plot(multi_reg$budsize_abiotic_PGLS)

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
# AIC      BIC    logLik
# 2165.632 2183.691 -1078.816
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)             3.596566 2.6794971  1.342254    0.18
# scale(meanbirdrich)    -0.174506 0.0406999 -4.287612    0.00
# scale(meanbatpres_bin)  0.156106 0.0369556  4.224138    0.00
# 
# Correlation: 
#   (Intr) scl(m)
# scale(meanbirdrich)     0.001       
# scale(meanbatpres_bin) -0.002 -0.445
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -0.32923390 -0.07747648  0.01085582  0.09754168  0.55337062 
# 
# Residual standard error: 7.071707 
# Degrees of freedom: 675 total; 672 residual

# plot residuals to check
plot(multi_reg$budsize_biot_PGLS)

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
# AIC     BIC    logLik
# 2162.957 2194.56 -1074.478
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)             3.683896 2.6684852  1.380520  0.1679
# scale(meanMAT)         -0.079014 0.0533733 -1.480399  0.1392
# scale(meanMAP)         -0.107635 0.0426566 -2.523283  0.0119
# scale(meanAVP)          0.026316 0.0465785  0.564974  0.5723
# scale(meanbirdrich)    -0.175694 0.0431652 -4.070278  0.0001
# scale(meanbatpres_bin)  0.167580 0.0409878  4.088526  0.0000
# 
# Correlation: 
#   (Intr) s(MAT) s(MAP) s(AVP) scl(m)
# scale(meanMAT)         -0.009                            
# scale(meanMAP)         -0.008  0.025                     
# scale(meanAVP)         -0.001  0.299 -0.200              
# scale(meanbirdrich)     0.000  0.234 -0.162 -0.070       
# scale(meanbatpres_bin)  0.000 -0.416  0.131 -0.123 -0.503
# 
# Standardized residuals:
#   Min            Q1           Med            Q3           Max 
# -0.3053361999 -0.0867856247 -0.0007746207  0.0825956010  0.5330582471 
# 
# Residual standard error: 7.026407 
# Degrees of freedom: 675 total; 669 residual

# plot residuals to check
plot(multi_reg$budsize_PGLS)

#* AIC/BIC to compare bud size PGLS ----

BIC(multi_reg$budsize_abiotic_PGLS, multi_reg$budsize_biot_PGLS, multi_reg$budsize_PGLS)

#                                 df      BIC
# multi_reg$budsize_abiotic_PGLS  5 2203.515
# multi_reg$budsize_biot_PGLS     4 2183.691
# multi_reg$budsize_PGLS          7 2194.560

# biotic model lowest BIC by fair margin

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$budsize_abiotic_PGLS, multi_reg$budsize_biot_PGLS, multi_reg$budsize_PGLS)

#                                 df      AIC
# multi_reg$budsize_abiotic_PGLS  5 2180.942
# multi_reg$budsize_biot_PGLS     4 2165.632
# multi_reg$budsize_PGLS          7 2162.957

# dif from full to abiotic (18) larger than full to biotic (3)

#* flower colourfulness ----

#** prep data ----
# subset data to phylogenetic least squares regression variables
# removing data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, colour_fullbinary,
                meanAVP, meanMAT, meanMAP, meanbirdrich, meanbatpres_bin) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 58 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(colour_fullbinary))
tree_pgls <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_pgls$tip.label) # 673 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1] # tree names to row names
pgls_data[,1] <- NULL

spp <- rownames(pgls_data) # set species names as reference point

#** full model ----
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

# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanMAT) + 
#                       scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#                     data = pgls_data, phy = tree_pgls, method = "logistic_IG10", 
#                     boot = 100)
# AIC     logLik Pen.logLik 
# 425.9     -206.0     -196.0 
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.8577142 
# bootstrap mean: 0.5734456 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.07327956,0.935209)
# 
# Coefficients:
#   Estimate     StdErr    z.value lowerbootCI upperbootCI p.value    
# (Intercept)             -2.449627   0.233220 -10.503514   -2.957906     -1.9577 < 2e-16 ***
# scale(meanMAT)           0.298312   0.210032   1.420318   -0.108877      0.6496 0.15552    
# scale(meanMAP)           0.015514   0.177578   0.087362   -0.215215      0.3629 0.93038    
# scale(meanAVP)           0.014103   0.221847   0.063570   -0.301509      0.3724 0.94931    
# scale(meanbirdrich)     -0.559330   0.243706  -2.295102   -1.052424     -0.1564 0.02173 *  
# scale(meanbatpres_bin)  -0.520705   0.271574  -1.917364   -1.062322      0.0591 0.05519 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.8577142
# Parametric bootstrap results based on 100 fitted replicates

#** abiotic model ----

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

# Warning message:
# In phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) +  :
# the estimate of 'alpha' (0.918925367286166) reached the upper bound (0.935504358062397).
# This may simply reflect a flat likelihood at large alpha values,
# meaning that the phylogenetic correlation is estimated to be negligible.
#                                                   
# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanMAT) + 
#                       scale(meanMAP) + scale(meanAVP), data = pgls_data, phy = tree_pgls, 
#                     method = "logistic_IG10", boot = 100)
# AIC     logLik Pen.logLik 
# 441.4     -215.7     -208.6 
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.9189254 
# bootstrap mean: 0.6577098 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.2372187,0.9352892)
# 
# Coefficients:
#   Estimate    StdErr   z.value lowerbootCI upperbootCI p.value    
# (Intercept)     -2.25586   0.20028 -11.26371    -2.63946     -1.8070 < 2e-16 ***
# scale(meanMAT)   0.22521   0.16744   1.34503    -0.14100      0.5080 0.17862    
# scale(meanMAP)  -0.44486   0.17482  -2.54465    -0.83110     -0.0751 0.01094 *  
# scale(meanAVP)  -0.16041   0.19686  -0.81482    -0.53565      0.1467 0.41517    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9189254
# Parametric bootstrap results based on 100 fitted replicates

#** biotic model ----

# using phylolm to run phylogenetic logistic regression by Ives & Garland method
# VERY SLOW/COMPUTATIONALLY INTENSE
multi_reg$flcolour_bioticPGLS <- phylolm::phyloglm(colour_fullbinary ~ scale(meanbirdrich) + 
                                               scale(meanbatpres_bin),
                                             data = pgls_data,
                                             phy = tree_pgls,
                                             method = "logistic_IG10", 
                                             boot = 100)
summary(multi_reg$flcolour_bioticPGLS)

# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanbirdrich) + 
#                       scale(meanbatpres_bin), data = pgls_data, phy = tree_pgls, 
#                     method = "logistic_IG10", boot = 100)
# AIC     logLik Pen.logLik 
# 423.5     -207.7     -202.7 
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.9150445 
# bootstrap mean: 0.5679544 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.03880339,0.935391)
# 
# Coefficients:
#   Estimate    StdErr   z.value lowerbootCI upperbootCI   p.value    
# (Intercept)             -2.37183   0.21442 -11.06174    -2.90707     -1.9424 < 2.2e-16 ***
#   scale(meanbirdrich)     -0.68401   0.22483  -3.04242    -1.04703     -0.2366  0.002347 ** 
#   scale(meanbatpres_bin)  -0.32624   0.19856  -1.64302    -0.76278      0.0027  0.100378    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9150445
# Parametric bootstrap results based on 100 fitted replicates

#** AIC? ----

c(multi_reg$flcolour_abioticPGLS$aic, #441.3588
  multi_reg$flcolour_bioticPGLS$aic,  #423.4877 
  multi_reg$flcolour_PGLS$aic)        #425.9343

# for some phyloglm, the estimate of alpha has reached the upper bound
# suggesting that "the phylogenetic correlation is estimated to be negligible"
# this suggests I should just use the above glm models for flower 
# colourfulness, and disregard these phyloglm models
# (though in any case the differences in AIC support biotic > abiotic at
# predicting flower colourfulness)

# in any case can't calculate pseudo R2 for these
# remove phyloglm models from list
multi_reg$flcolour_PGLS <- NULL
multi_reg$flcolour_abioticPGLS <- NULL
multi_reg$flcolour_bioticPGLS <- NULL

rm(spp, pgls_data)

### partial R2 ####

# calculate partial/pseudo R2 for all models, using R2 from Ives (2019) as 
# these are made to work with phylogenetic models
r2 <- list()
# non-phylogenetic models
for(name in names(multi_reg[1:6])) {
  r2[[name]] <- rr2::R2(multi_reg[[name]])
}
# phylogenetic models (slow!)
for(name in names(multi_reg[7:9])) {
  r2[[name]] <- rr2::R2(multi_reg[[name]], phy = tree_pgls)
}
# build list into exportable data frame
r2_results <- data.frame(model = character(0),
                         R2_lik = numeric(0),
                         R2_resid = numeric(0),
                         R2_pred = numeric(0))
for(name in names(r2)) {
  r2_results <- tibble::add_row(r2_results, 
                                model = name, 
                                R2_lik = r2[[name]][1],
                                R2_resid = r2[[name]][2], 
                                R2_pred = r2[[name]][3])
}
# add in BIC and AIC from models while I'm here, and n as well
BIC <- c()
AIC <- c()
nobs <- c()
for(name in names(r2)) {
  BIC <- c(BIC, BIC(multi_reg[[name]]))
  AIC <- c(AIC, AIC(multi_reg[[name]]))
  nobs <- c(nobs, nobs(multi_reg[[name]]))
}

r2_results <- tibble::add_column(r2_results, BIC, AIC, nobs)

# export results to csv
readr::write_csv(r2_results, "results/model_summaries.csv")
rm(name, r2, r2_results, AIC, BIC, nobs)

#### conclusions ####

# comparing all these models, my interpretations are that: 
# 1) comparing BIC between the full models, models with biotic variables only & 
#    models with abiotic variables only suggests that biotic variables better
#    predict eucalypt bud size and flower colour than abiotic variables. This is
#    true both with and without the phylogenetic relatedness of eucalypt species
#    accounted for.

# 2) looking at effect sizes, in all models bat presence/absence has the 
#    strongest effect on bud size or flower colour, except in some phylogenetic
#    models where bird richness (highly correlated with bat presence/absence) 
#    has an equal if opposite effect (i.e. more birds = smaller flowers)
#    In OLS regressions soil phosphorus has the strongest effect of any abiotic
#    predictor, while in PGLS MAP has a stronger effect

# RECHECK ABOVE AGAINST UPDATED RESULTS


rm(tree_pgls, multi_reg)

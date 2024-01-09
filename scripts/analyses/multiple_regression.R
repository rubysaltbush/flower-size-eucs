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
table(euc_traits_nosubsp$meanmarspres_bin)
# 0   1 
# 87 699 
# much fewer species in no marsupials category

#* correlation plots ----

# visually check relationships between predictor and response variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(6, 9, 12, 13, 14, 15, 17, 19), 
                columnLabels = c("Bud size (log)",
                                 "Flower colour",
                                 "Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence",
                                 "Mars. presence"))
ggsave("figures/regressions/full pairwise correlation plot.pdf", 
       width = 13, height = 10)
# lots going on - looks like bud size best (-vely )correlated with bat presence, 
# then soil phosphorus, then bird richness, then precipitation,
# and flower colour bats then birds then phosphorus then precipitation
# temperature no correlation for both bud size and flower colour

# check for collinearity among predictor variables
GGally::ggpairs(data = euc_traits_nosubsp, 
                columns = c(12, 13, 14, 15, 17, 19), 
                columnLabels = c("Temperature", 
                                 "Precipitation",
                                 "Phosphorus",
                                 "Bird richness",
                                 "Bat presence",
                                 "Mars. presence"))
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
                               scale(meanbatpres_bin) + scale(meanmarspres_bin), #biotic
                             data = euc_traits_nosubsp)
summary(multi_reg$budsize_biotic)

# Call:
#   lm(formula = logbudsize_mm2 ~ scale(meanbirdrich) + scale(meanbatpres_bin) + 
#        scale(meanmarspres_bin), data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.0995 -0.5690 -0.0709  0.4642  3.7096 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.74853    0.03156 118.757  < 2e-16 ***
#   scale(meanbirdrich)     -0.14198    0.04901  -2.897  0.00387 ** 
#   scale(meanbatpres_bin)  -0.27569    0.04636  -5.947 4.12e-09 ***
#   scale(meanmarspres_bin)  0.02199    0.03423   0.642  0.52080    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8815 on 776 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1615,	Adjusted R-squared:  0.1583 
# F-statistic: 49.84 on 3 and 776 DF,  p-value: < 2.2e-16

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_biotic)
# scale(meanbirdrich)  scale(meanbatpres_bin) scale(meanmarspres_bin) 
# 2.415335                2.151503                1.182782 
# VIF under 2.45 for all, multicollinearity unlikely

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
                                              scale(meanbatpres_bin) +
                                              scale(meanmarspres_bin), #biotic
                                        data = euc_traits_nosubsp)
summary(multi_reg$budsize_full)

# Call:
#   lm(formula = logbudsize_mm2 ~ scale(meanMAT) + scale(meanMAP) + 
#        scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin) + 
#        scale(meanmarspres_bin), data = euc_traits_nosubsp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3283 -0.5717 -0.0630  0.4749  3.6566 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.74859    0.03126 119.933  < 2e-16 ***
# scale(meanMAT)           0.05411    0.04866   1.112  0.26653    
# scale(meanMAP)          -0.01041    0.04724  -0.220  0.82568    
# scale(meanAVP)          -0.14306    0.05504  -2.599  0.00953 ** 
# scale(meanbirdrich)     -0.01747    0.05776  -0.302  0.76242    
# scale(meanbatpres_bin)  -0.28919    0.06349  -4.555 6.08e-06 ***
# scale(meanmarspres_bin)  0.02330    0.04020   0.580  0.56236    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8729 on 773 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1811,	Adjusted R-squared:  0.1747 
# F-statistic: 28.49 on 6 and 773 DF,  p-value: < 2.2e-16

# bat presence/absence explains most variation, also phosphorus but smaller effect

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$budsize_full)
# scale(meanMAT)          scale(meanMAP)          scale(meanAVP)     scale(meanbirdrich) 
# 2.438439                2.295189                3.103977                3.422537 
# scale(meanbatpres_bin) scale(meanmarspres_bin) 
# 4.115949                1.663188 
# VIF below 4.2 for all, generally recommended threshold is 5-10
# higher VIF for bat presence here suggests some overlap with other predictors

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$budsize_full)

#* AIC/BIC to compare bud size models (Bayesian Information Criterion) ----

# first BIC (penalises models with too many variables, lowest BIC model best)

BIC(multi_reg$budsize_abiotic, multi_reg$budsize_biotic, multi_reg$budsize_full)

#                           df      BIC
# multi_reg$budsize_abiotic  5 2062.106
# multi_reg$budsize_biotic   5 2046.096
# multi_reg$budsize_full     8 2047.696

# biotic model has lowest BIC, though only ~1 below full model

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$budsize_abiotic, multi_reg$budsize_biotic, multi_reg$budsize_full)

#                           df      AIC
# multi_reg$budsize_abiotic  5 2038.809
# multi_reg$budsize_biotic   5 2022.799
# multi_reg$budsize_full     8 2010.422

# dif between full and abiotic (~28) larger than dif between full and biotic (~12)
# suggesting biotic variables more important to model

#* flower colourfulness abiotic ----

# one observation missing biotic but not abiotic variable, remove from data
# for these models
tempflcol <- euc_traits_nosubsp %>%
  dplyr::select(apc_nosubsp, colour_fullbinary, meanMAT, meanMAP, meanAVP,
                meanbirdrich, meanbatpres_bin, meanmarspres_bin) %>%
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
                                 scale(meanbatpres_bin) + scale(meanmarspres_bin),
                               data = tempflcol,
                               family = binomial)
summary(multi_reg$flcolour_biotic)

# Call:
#   glm(formula = colour_fullbinary ~ scale(meanbirdrich) + scale(meanbatpres_bin) + 
#         scale(meanmarspres_bin), family = binomial, data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -2.49787    0.17104 -14.604  < 2e-16 ***
#   scale(meanbirdrich)     -0.51861    0.23973  -2.163 0.030516 *  
#   scale(meanbatpres_bin)  -0.74187    0.20187  -3.675 0.000238 ***
#   scale(meanmarspres_bin)  0.01222    0.10491   0.117 0.907245    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 487.52  on 774  degrees of freedom
# AIC: 495.52
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_biotic)
# scale(meanbirdrich)  scale(meanbatpres_bin) scale(meanmarspres_bin) 
# 1.842997                1.696707                1.124948 
# VIF under 1.85, multicollinearity unlikely

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$flcolour_biotic)

#* flower colourfulness full model ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_full <- glm(colour_fullbinary ~ scale(meanMAT) + 
                                                   scale(meanMAP) + 
                                                   scale(meanAVP) + 
                                                   scale(meanbirdrich) + 
                                                   scale(meanbatpres_bin) +
                                                   scale(meanmarspres_bin),
                                          data = tempflcol,
                                          family = binomial())
summary(multi_reg$flcolour_full)

# Call:
#   glm(formula = colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) + 
#         scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin) + 
#         scale(meanmarspres_bin), family = binomial(), data = tempflcol)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -2.64671    0.20400 -12.974   <2e-16 ***
#   scale(meanMAT)           0.29240    0.25287   1.156   0.2476    
# scale(meanMAP)          -0.08524    0.22090  -0.386   0.6996    
# scale(meanAVP)          -0.25440    0.26254  -0.969   0.3325    
# scale(meanbirdrich)     -0.46584    0.28740  -1.621   0.1050    
# scale(meanbatpres_bin)  -0.77444    0.32010  -2.419   0.0155 *  
#   scale(meanmarspres_bin)  0.10765    0.15171   0.710   0.4780    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 561.46  on 777  degrees of freedom
# Residual deviance: 478.96  on 771  degrees of freedom
# AIC: 492.96
# 
# Number of Fisher Scoring iterations: 6

# double check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multi_reg$flcolour_full)
# scale(meanMAT)          scale(meanMAP)          scale(meanAVP)     scale(meanbirdrich) 
# 3.290522                2.441691                2.157751                1.847573 
# scale(meanbatpres_bin) scale(meanmarspres_bin) 
# 4.198234                2.352074 
# VIF under 4.2 for all, multicollinearity unlikely

# plot residuals to check
par(mfrow = c(2, 2))
plot(multi_reg$flcolour_full)

#* AIC/BIC to compare flower colourfulness models ----

# first BIC (penalises models with too many variables, lowest BIC model best)

BIC(multi_reg$flcolour_abiotic, multi_reg$flcolour_biotic, multi_reg$flcolour_full)

#                             df      BIC
# multi_reg$flcolour_abiotic  4 526.4958
# multi_reg$flcolour_biotic   4 514.1427
# multi_reg$flcolour_full     7 525.5610

# biotic model has lowest BIC, by 11

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$flcolour_abiotic, multi_reg$flcolour_biotic, multi_reg$flcolour_full)

#                             df      AIC
# multi_reg$flcolour_abiotic  4 507.8689
# multi_reg$flcolour_biotic   4 495.5158
# multi_reg$flcolour_full     7 492.9639

# dif between full and abiotic (~15) larger than dif between full and biotic (~3)
# suggesting biotic variables more important to model

rm(tempflcol)

#### with phylogeny ####

#* prep data ---- 

# subset data to phylogenetic least squares regression variables
# removing data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2,
                meanAVP, meanMAT, meanMAP, 
                meanbirdrich, meanbatpres_bin, meanmarspres_bin) %>%
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
                                           scale(meanbatpres_bin) +
                                           scale(meanmarspres_bin),
                                         data = pgls_data,
                                         correlation = 
                                           ape::corBrownian(phy = tree_pgls, 
                                                            form = ~spp),
                                         method = "ML")
summary(multi_reg$budsize_biot_PGLS)
# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ scale(meanbirdrich) + scale(meanbatpres_bin) +      scale(meanmarspres_bin) 
# Data: pgls_data 
# AIC      BIC   logLik
# 2167.559 2190.133 -1078.78
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)              3.597244 2.6813483  1.341580  0.1802
# scale(meanbirdrich)     -0.168502 0.0463962 -3.631800  0.0003
# scale(meanbatpres_bin)   0.151106 0.0413527  3.654082  0.0003
# scale(meanmarspres_bin) -0.007764 0.0287387 -0.270175  0.7871
# 
# Correlation: 
#   (Intr) scl(m) scl(mnb_)
# scale(meanbirdrich)      0.001                 
# scale(meanbatpres_bin)  -0.003 -0.564          
# scale(meanmarspres_bin) -0.001 -0.479  0.447   
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -0.32874134 -0.07769112  0.01039178  0.09841640  0.55069055 
# 
# Residual standard error: 7.071323 
# Degrees of freedom: 675 total; 671 residual

# plot residuals to check
plot(multi_reg$budsize_biot_PGLS)

#* bud size full PGLS ----

multi_reg$budsize_PGLS <- nlme::gls(logbudsize_mm2 ~ scale(meanMAT) + 
                                                     scale(meanMAP) + 
                                                     scale(meanAVP) + 
                                                     scale(meanbirdrich) + 
                                                     scale(meanbatpres_bin) +
                                                     scale(meanmarspres_bin),
                                           data = pgls_data,
                                           correlation = 
                                             ape::corBrownian(phy = tree_pgls, 
                                                              form = ~spp),
                                           method = "ML")
summary(multi_reg$budsize_PGLS)
# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ scale(meanMAT) + scale(meanMAP) + scale(meanAVP) +      scale(meanbirdrich) + scale(meanbatpres_bin) + scale(meanmarspres_bin) 
# Data: pgls_data 
# AIC      BIC    logLik
# 2164.75 2200.868 -1074.375
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)              3.683274 2.6700739  1.379465  0.1682
# scale(meanMAT)          -0.069161 0.0576819 -1.199000  0.2310
# scale(meanMAP)          -0.117971 0.0484208 -2.436365  0.0151
# scale(meanAVP)           0.032283 0.0484398  0.666459  0.5053
# scale(meanbirdrich)     -0.184337 0.0472336 -3.902672  0.0001
# scale(meanbatpres_bin)   0.173121 0.0428052  4.044400  0.0001
# scale(meanmarspres_bin)  0.015671 0.0346662  0.452042  0.6514
# 
# Correlation: 
#   (Intr) s(MAT) s(MAP) s(AVP) scl(m) scl(mnb_)
# scale(meanMAT)          -0.009                                      
# scale(meanMAP)          -0.007 -0.158                               
# scale(meanAVP)          -0.001  0.369 -0.298                        
# scale(meanbirdrich)      0.000  0.046  0.060 -0.171                 
# scale(meanbatpres_bin)   0.000 -0.260 -0.024 -0.036 -0.556          
# scale(meanmarspres_bin) -0.001  0.378 -0.472  0.273 -0.405  0.286   
# 
# Standardized residuals:
#   Min            Q1           Med            Q3           Max 
# -0.3069960087 -0.0862511081 -0.0003897475  0.0830043370  0.5363147316 
# 
# Residual standard error: 7.025333 
# Degrees of freedom: 675 total; 668 residual

# plot residuals to check
plot(multi_reg$budsize_PGLS)

#* AIC/BIC to compare bud size PGLS ----

BIC(multi_reg$budsize_abiotic_PGLS, multi_reg$budsize_biot_PGLS, multi_reg$budsize_PGLS)

#                                 df      BIC
# multi_reg$budsize_abiotic_PGLS  5 2203.515
# multi_reg$budsize_biot_PGLS     5 2190.133
# multi_reg$budsize_PGLS          8 2200.868

# biotic model lowest BIC by 10

# now AIC (doesn't penalise additional variables so consider difference between
# full model and biotic/abiotic models - if dropping X variables leads to larger
# change in AIC, then they are likely more significant to model)

AIC(multi_reg$budsize_abiotic_PGLS, multi_reg$budsize_biot_PGLS, multi_reg$budsize_PGLS)

#                                 df      AIC
# multi_reg$budsize_abiotic_PGLS  5 2180.942
# multi_reg$budsize_biot_PGLS     5 2167.559
# multi_reg$budsize_PGLS          8 2164.750

# dif from full to abiotic (16) larger than full to biotic (3)

#* flower colourfulness ----

#** prep data ----
# subset data to phylogenetic least squares regression variables
# removing data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, colour_fullbinary,
                meanAVP, meanMAT, meanMAP, 
                meanbirdrich, meanbatpres_bin, meanmarspres_bin) %>%
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
# VERY SLOW/COMPUTATIONALLY INTENSE (2.5 hours to run)
multi_reg$flcolour_PGLS <- phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + 
                                                                 scale(meanMAP) + 
                                                                 scale(meanAVP) + 
                                                                 scale(meanbirdrich) + 
                                                                 scale(meanbatpres_bin) +
                                                                 scale(meanmarspres_bin),
                                             data = pgls_data,
                                             phy = tree_pgls,
                                             method = "logistic_IG10", 
                                             boot = 100)
summary(multi_reg$flcolour_PGLS)

# Call:
#   phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanMAT) + 
#                       scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin) + 
#                       scale(meanmarspres_bin), data = pgls_data, phy = tree_pgls, 
#                     method = "logistic_IG10", boot = 100)
# AIC     logLik Pen.logLik 
# 427.1     -205.6     -193.6 
# 
# Method: logistic_IG10
# Mean tip height: 58.36226
# Parameter estimate(s):
#   alpha: 0.9328167 
# bootstrap mean: 0.564187 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.1119177,0.9352041)
# 
# Coefficients:
#   Estimate     StdErr    z.value lowerbootCI upperbootCI p.value    
# (Intercept)              -2.458098   0.231972 -10.596537   -2.845717     -1.9628 < 2e-16 ***
# scale(meanMAT)            0.383641   0.244510   1.569023   -0.029643      0.8593 0.11664    
# scale(meanMAP)           -0.080483   0.208953  -0.385174   -0.498080      0.2544 0.70011    
# scale(meanAVP)            0.090151   0.253657   0.355405   -0.360510      0.4837 0.72229    
# scale(meanbirdrich)      -0.651354   0.259449  -2.510531   -1.096116     -0.1136 0.01205 *  
# scale(meanbatpres_bin)   -0.488802   0.264956  -1.844843   -1.025788      0.0863 0.06506 .  
# scale(meanmarspres_bin)   0.111786   0.150295   0.743778   -0.143246      0.4288 0.45701    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9328167
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
                                               scale(meanbatpres_bin) + scale(meanmarspres_bin),
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
# bootstrap mean: 0.6088478 (on log scale, then back transformed)
# so possible downward bias.
# bootstrap 95% CI: (0.09130003,0.9351329)
# 
# Coefficients:
#   Estimate    StdErr   z.value lowerbootCI upperbootCI   p.value    
# (Intercept)             -2.37183   0.21442 -11.06174    -2.73065     -1.9155 < 2.2e-16 ***
#   scale(meanbirdrich)     -0.68401   0.22483  -3.04242    -1.14724     -0.2849  0.002347 ** 
#   scale(meanbatpres_bin)  -0.32624   0.19856  -1.64302    -0.69793      0.0288  0.100378    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Note: Wald-type p-values for coefficients, conditional on alpha=0.9150445
# Parametric bootstrap results based on 100 fitted replicates

#** AIC? ----

c(multi_reg$flcolour_abioticPGLS$aic, #441.3588
  multi_reg$flcolour_bioticPGLS$aic,  #423.4877 
  multi_reg$flcolour_PGLS$aic)        #424.3441

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
#    has a similar effect (more birds = smaller flowers)
#    In OLS regressions soil phosphorus has the strongest effect of any abiotic
#    predictor, while in PGLS MAP has a stronger effect

rm(tree_pgls, multi_reg)

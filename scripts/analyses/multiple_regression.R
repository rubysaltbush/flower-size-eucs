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

#* flower colourfulness full model ----

# using glm with binomial distribution for logistic regression
multi_reg$flcolour_full <- glm(as.numeric(colour_fullbinary) ~ scale(meanMAT) + 
                                                               scale(meanMAP) + 
                                                               scale(meanAVP) + 
                                                               scale(meanbirdrich) + 
                                                               scale(meanbatpres_bin),
                                          data = euc_traits_nosubsp,
                                          family = binomial())
summary(multi_reg$flcolour_full)

# Call:
#   glm(formula = as.numeric(colour_fullbinary) ~ scale(meanMAT) + 
#         scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#       family = binomial(), data = euc_traits_nosubsp)
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

multi_reg$budsize_abiotic_PGLS <- phylolm::phylolm(logbudsize_mm2 ~ scale(meanMAT) + 
                                             scale(meanMAP) + 
                                             scale(meanAVP),
                                           data = pgls_data,
                                           phy = tree_pgls,
                                           model = "BM",
                                           boot = 1000)
summary(multi_reg$budsize_abiotic_PGLS)

# Call:
#   phylolm::phylolm(formula = logbudsize_mm2 ~ scale(meanMAT) + 
#                      scale(meanMAP) + scale(meanAVP), data = pgls_data, phy = tree_pgls, 
#                    model = "BM", boot = 1000)
# 
# AIC logLik 
# 2166  -1078 
# 
# Raw residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2247 -0.5939 -0.0186  0.5655  3.7286 
# 
# Mean tip height: 58.36226
# Parameter estimate(s) using ML:
#   sigma2: 0.8777655 
# 
# Coefficients:
#   Estimate    StdErr   t.value lowerbootCI upperbootCI  p.value   
# (Intercept)     3.680441  2.714260  1.355965   -1.507380      8.6192 0.175571   
# scale(meanMAT)  0.012478  0.049210  0.253572   -0.073031      0.1085 0.799904   
# scale(meanMAP) -0.140890  0.042895 -3.284518   -0.226152     -0.0592 0.001075 **
#   scale(meanAVP)  0.030442  0.046718  0.651619   -0.065790      0.1243 0.514872   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-squared: 0.01649	Adjusted R-squared: 0.01205 
# 
# sigma2: 0.8777655
# bootstrap mean: 0.8710828 (on raw scale)
# 0.8698737 (on log scale, then back transformed)
# bootstrap 95% CI: (0.7834497,0.9635583)
# 
# Parametric bootstrap results based on 1000 fitted replicates

# curious, AVP not sig with updated env data

#* bud size biotic PGLS ----

multi_reg$budsize_biot_PGLS <- phylolm::phylolm(logbudsize_mm2 ~ scale(meanbirdrich) + 
                                             scale(meanbatpres_bin),
                                           data = pgls_data,
                                           phy = tree_pgls,
                                           model = "BM",
                                           boot = 1000)
summary(multi_reg$budsize_biot_PGLS)

# Call:
#   phylolm::phylolm(formula = logbudsize_mm2 ~ scale(meanbirdrich) + 
#                      scale(meanbatpres_bin), data = pgls_data, phy = tree_pgls, 
#                    model = "BM", boot = 1000)
# 
# AIC logLik 
# 2154  -1073 
# 
# Raw residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3192 -0.5432  0.0743  0.6877  3.9568 
# 
# Mean tip height: 58.36226
# Parameter estimate(s) using ML:
#   sigma2: 0.8650889 
# 
# Coefficients:
#   Estimate    StdErr   t.value lowerbootCI upperbootCI   p.value
# (Intercept)             3.590751  2.692372  1.333676   -1.845639      9.0815 0.1827661
# scale(meanbirdrich)    -0.155245  0.040617 -3.822201   -0.231522     -0.0785 0.0001447
# scale(meanbatpres_bin)  0.152366  0.037690  4.042651    0.078735      0.2319 5.903e-05
# 
# (Intercept)               
# scale(meanbirdrich)    ***
#   scale(meanbatpres_bin) ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-squared: 0.03069	Adjusted R-squared: 0.02778 
# 
# sigma2: 0.8650889
# bootstrap mean: 0.8616804 (on raw scale)
# 0.8603878 (on log scale, then back transformed)
# bootstrap 95% CI: (0.7731418,0.9533291)
# 
# Parametric bootstrap results based on 1000 fitted replicates

#* bud size full PGLS ----

multi_reg$budsize_PGLS <- phylolm::phylolm(logbudsize_mm2 ~ scale(meanMAT) + 
                                                     scale(meanMAP) + 
                                                     scale(meanAVP) + 
                                                     scale(meanbirdrich) + 
                                                     scale(meanbatpres_bin),
                                           data = pgls_data,
                                           phy = tree_pgls,
                                           model = "BM",
                                           boot = 1000)
summary(multi_reg$budsize_PGLS)

Call:
  phylolm::phylolm(formula = logbudsize_mm2 ~ scale(meanMAT) + 
                     scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
                   data = pgls_data, phy = tree_pgls, model = "BM", boot = 1000)

# AIC logLik 
# 2148  -1067 
# 
# Raw residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1098 -0.6129 -0.0151  0.5619  3.7303 
# 
# Mean tip height: 58.36226
# Parameter estimate(s) using ML:
#   sigma2: 0.8496004 
# 
# Coefficients:
#   Estimate    StdErr   t.value lowerbootCI upperbootCI   p.value
# (Intercept)             3.693240  2.674384  1.380968   -1.729875      9.2302  0.167754
# scale(meanMAT)         -0.085780  0.053589 -1.600694   -0.190231      0.0119  0.109921
# scale(meanMAP)         -0.134165  0.042339 -3.168822   -0.214217     -0.0504  0.001601
# scale(meanAVP)          0.022618  0.046933  0.481927   -0.069269      0.1137  0.630017
# scale(meanbirdrich)    -0.171424  0.042278 -4.054705   -0.256607     -0.0902 5.616e-05
# scale(meanbatpres_bin)  0.170628  0.041506  4.110879    0.092045      0.2560 4.436e-05
# 
# (Intercept)               
# scale(meanMAT)            
# scale(meanMAP)         ** 
#   scale(meanAVP)            
# scale(meanbirdrich)    ***
#   scale(meanbatpres_bin) ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-squared: 0.04805	Adjusted R-squared: 0.04087 
# 
# sigma2: 0.8496004
# bootstrap mean: 0.8416765 (on raw scale)
# 0.8404355 (on log scale, then back transformed)
# bootstrap 95% CI: (0.7552477,0.9302625)
# 
# Parametric bootstrap results based on 1000 fitted replicates

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

#### dominance analysis ####

dominanceanalysis <- domir::domin(logbudsize_mm2 ~ scale(meanMAT) + 
                                    scale(meanMAP) + scale(meanAVP) + 
                                    scale(meanbirdrich) + scale(meanbatpres_bin),
                                  reg = lm,
                                  fitstat = list(summary, "r.squared"),
                                  data = euc_traits_nosubsp)
dominanceanalysis
# output suggests bats then AVP then birds then MAP then MAT best predictors
# when all considered separately

# compare models with BIC
BIC(multi_reg$budsize_abiotic, multi_reg$budsize_full)

# no BIC for PGLS, AIC?
c(multi_reg$budsize_abiotic_PGLS$aic, multi_reg$budsize_biot_PGLS$aic,
  multi_reg$budsize_PGLS$aic)
# log likelihood?
c(multi_reg$budsize_abiotic_PGLS$logLik, multi_reg$budsize_biot_PGLS$logLik,
  multi_reg$budsize_PGLS$logLik)

#### conclusions ####

# comparing all these models, my interpretations are that: 
# 1) including biotic variables improves model R2 for bud size (.135 with 
# abiotic only to 0.18 with biotic included)
# 2) with full model precipitation becomes a less significant predictor, as
# bud size mostly explained by presence/absence of bats in a eucalypt's 
# environment  
# 3) this matches result of full model for flower colourfulness, bat presence
# best predictor
# 4) with phylogenetic relatedness included via PGLS, bat presence still a 
# significant predictor of bud size and flower colourfulness but mean annual
# precipitation also considered a significant predictor of both, plus bird
# richness for bud size - model doesn't seem to be accounting for significant
# covariance in these predictors? will need to ask someone more expert in PGLS

rm(spp, tree_pgls, multi_reg, pgls_data)

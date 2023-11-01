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
# -2.91555 -0.38677  0.03106  0.38317  2.06176 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.15865    0.02226 321.613  < 2e-16 ***
#   scale(meanMAT)  0.16642    0.02642   6.298 5.02e-10 ***
#   scale(meanMAP)  0.25297    0.02702   9.364  < 2e-16 ***
#   scale(meanAVP)  0.14414    0.02985   4.829 1.65e-06 ***
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
# -2.2711 -0.6229 -0.0639  0.4801  3.7359 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.74738    0.03207 116.864  < 2e-16 ***
#   scale(meanMAT) -0.05028    0.03800  -1.323 0.186101    
# scale(meanMAP) -0.13520    0.03878  -3.486 0.000517 ***
#   scale(meanAVP) -0.28063    0.04300  -6.526 1.22e-10 ***
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
# -2.2199 -0.5542 -0.0717  0.4887  3.6779 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.74910    0.03125 119.967  < 2e-16 ***
#   scale(meanMAT)          0.04200    0.04310   0.974   0.3302    
# scale(meanMAP)         -0.01222    0.04312  -0.283   0.7769    
# scale(meanAVP)         -0.11177    0.04935  -2.265   0.0238 *  
#   scale(meanbirdrich)    -0.05870    0.05111  -1.148   0.2512    
# scale(meanbatpres_bin) -0.27703    0.06129  -4.520 7.16e-06 ***
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
# (Intercept)            -2.57542    0.18633 -13.822  < 2e-16 ***
# scale(meanMAT)          0.20804    0.19106   1.089  0.27620    
# scale(meanMAP)         -0.09547    0.19317  -0.494  0.62114    
# scale(meanAVP)         -0.28784    0.20831  -1.382  0.16704    
# scale(meanbirdrich)    -0.30626    0.23193  -1.320  0.18667    
# scale(meanbatpres_bin) -0.78231    0.29132  -2.685  0.00724 ** 
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

# Call:
#   phylolm::phylolm(formula = logbudsize_mm2 ~ scale(meanMAT) + 
#                      scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#                    data = pgls_data, phy = tree_pgls, model = "BM", boot = 1000)
# 
# AIC logLik 
# 2152  -1069 
# 
# Raw residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1459 -0.6222 -0.0109  0.5867  3.7638 
# 
# Mean tip height: 58.36226
# Parameter estimate(s) using ML:
#   sigma2: 0.8546448 
# 
# Coefficients:
#   Estimate     StdErr    t.value lowerbootCI upperbootCI
# (Intercept)             3.6907998  2.6823198  1.3759731  -1.5222998      8.9625
# scale(meanMAT)         -0.0562272  0.0536145 -1.0487300  -0.1680211      0.0469
# scale(meanMAP)         -0.1529602  0.0432445 -3.5371042  -0.2417397     -0.0597
# scale(meanAVP)          0.0801289  0.0414710  1.9321662   0.0059369      0.1594
# scale(meanbirdrich)    -0.1288310  0.0409469 -3.1462940  -0.2078447     -0.0518
# scale(meanbatpres_bin)  0.1474361  0.0411962  3.5788754   0.0722149      0.2296
# p.value    
# (Intercept)            0.1692946    
# scale(meanMAT)         0.2946845    
# scale(meanMAP)         0.0004328 ***
# scale(meanAVP)         0.0537647 .  
# scale(meanbirdrich)    0.0017276 ** 
# scale(meanbatpres_bin) 0.0003703 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-squared: 0.04239	Adjusted R-squared: 0.03517 
# 
# sigma2: 0.8546448
# bootstrap mean: 0.8493515 (on raw scale)
# 0.8481553 (on log scale, then back transformed)
# bootstrap 95% CI: (0.7634358,0.9361822)
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

# Warning messages:
# 1: In phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) +  :
# the estimate of 'alpha' (0.935315443447423) reached the upper bound (0.935504358152755).
# This may simply reflect a flat likelihood at large alpha values,
# meaning that the phylogenetic correlation is estimated to be negligible.
# 2: In phylolm::phyloglm(colour_fullbinary ~ scale(meanMAT) + scale(meanMAP) +  :
# phyloglm failed to converge.
#                                                   
# > summary(multi_reg$flcolour_PGLS)
#                                                   
#  Call:
#    phylolm::phyloglm(formula = colour_fullbinary ~ scale(meanMAT) + 
#                      scale(meanMAP) + scale(meanAVP) + scale(meanbirdrich) + scale(meanbatpres_bin), 
#                      data = pgls_data, phy = tree_pgls, method = "logistic_IG10", 
#                      boot = 100)
#  AIC     logLik Pen.logLik 
#  416       -201       -191 
#                                                   
#  Method: logistic_IG10
#  Mean tip height: 58.36226
#  Parameter estimate(s): alpha: 0.9353154 
#  bootstrap mean: 0.6773605 (on log scale, then back transformed)
#  so possible downward bias.
#  bootstrap 95% CI: (0.1521449,0.9352338)
#                                                   
#  Coefficients:
#  Estimate     StdErr    z.value lowerbootCI upperbootCI p.value
#  (Intercept)             -2.465102   0.228064 -10.808823   -3.049390     -1.9663 < 2e-16
#  scale(meanMAT)           0.422187   0.214603   1.967290    0.017509      0.8781 0.04915
#  scale(meanMAP)          -0.044699   0.191254  -0.233717   -0.345850      0.2738 0.81521
#  scale(meanAVP)          -0.033402   0.216146  -0.154534   -0.482564      0.3259 0.87719
#  scale(meanbirdrich)     -0.252156   0.231828  -1.087685   -0.646708      0.2108 0.27673
#  scale(meanbatpres_bin)  -0.682025   0.283976  -2.401700   -1.180479     -0.1798 0.01632
#                                                   
#  (Intercept)            ***
#  scale(meanMAT)         *  
#  scale(meanMAP)            
#  scale(meanAVP)            
#  scale(meanbirdrich)       
#  scale(meanbatpres_bin) *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#                                                   
#  Note: Wald-type p-values for coefficients, conditional on alpha=0.9353154
#  Parametric bootstrap results based on 100 fitted replicates

#### dominance analysis ####

dominanceanalysis <- domir::domin(logbudsize_mm2 ~ scale(meanMAT) + 
                                    scale(meanMAP) + scale(meanAVP) + 
                                    scale(meanbirdrich) + scale(meanbatpres_bin),
                                  reg = lm,
                                  fitstat = list(summary, "r.squared"),
                                  data = euc_traits_nosubsp)
dominanceanalysis
# output suggests bats then birds then AVP then MAP then MAT best predictors
# when all considered separately

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

# compare impact of all environmental variables on bud size and flower 
# colourfulness using model averaged standardised regression coefficients
# to determine which predictor variable explains the most variance in 
# these response variables, in this data set

# This is a relevant paper in Methods of Ecology and Evolution https://doi.org/10.1111/2041-210X.12835.Their recommended method is model averaged standardised regression coefficients. You can do this using modavg from AICcmodavg https://rdrr.io/cran/AICcmodavg/man/modavg.html I believe this package is compatible with lme4.

#### prepare data ####

# subset data for modelling
eucvarscaled <- euc_traits_nosubsp %>%
  dplyr::select(apc_nosubsp, logbudsize_mm2, colour_fullbinary,  
                meanMAT, meanMAP, meanAVP, meanbirdrich, meanbatpres_bin)
# scale all predictor variables
eucvarscaled[4:8] <- mapply(FUN = scale, eucvarscaled[4:8])
# make colour binary factor for logistic regression
eucvarscaled$colour_fullbinary <- as.factor(eucvarscaled$colour_fullbinary)

#### bud size model averaging ####

# first create all possible formulas 
paste0.na.omit = function(x){
  paste0(na.omit(x), collapse = " + ")
}

formulas <- apply(data.frame(expand.grid(c("meanMAT", NA), c("meanMAP", NA),
                                        c("meanAVP", NA), c("meanbirdrich", NA),
                                        c("meanbatpres_bin", NA))), 1, 
                 paste0.na.omit)
formulas[length(formulas)] <- "1"
formulas <- paste0("logbudsize_mm2 ~ ", formulas)
formulas

# run all candidate models          
cand_mod <- list()
for(i in 1:length(formulas)){
  cand_mod[[i]] <- lm(as.formula(formulas[[i]]), 
                      data = eucvarscaled)
}

# use modavg to get the model averaged standardised regression coefficients
AICcmodavg::modavg(cand_mod, parm = "meanMAT", second.ord = FALSE)
# Multimodel inference on "meanMAT" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      3.13  0.10     0.03 0.04
# Mod3  6 2006.78      1.15  0.26     0.03 0.04
# Mod5  6 2015.46      9.83  0.00     0.09 0.04
# Mod7  5 2014.17      8.54  0.01     0.10 0.04
# Mod9  6 2007.63      2.00  0.17     0.04 0.04
# Mod11 5 2005.63      0.00  0.46     0.04 0.04
# Mod13 5 2016.04     10.41  0.00     0.13 0.03
# Mod15 4 2014.59      8.96  0.01     0.13 0.03
# Mod17 6 2023.37     17.74  0.00    -0.07 0.04
# Mod19 5 2025.49     19.86  0.00    -0.10 0.03
# Mod21 5 2039.09     33.46  0.00     0.00 0.03
# Mod23 4 2052.64     47.01  0.00    -0.03 0.03
# Mod25 5 2038.81     33.18  0.00    -0.06 0.04
# Mod27 4 2045.10     39.47  0.00    -0.10 0.04
# Mod29 4 2087.79     82.16  0.00     0.08 0.03
# Mod31 3 2153.54    147.91  0.00     0.06 0.03
# 
# Model-averaged estimate: 0.04 
# Unconditional SE: 0.04 
# 95% Unconditional confidence interval: -0.05, 0.12
AICcmodavg::modavg(cand_mod, parm = "meanMAP", second.ord = FALSE)
# Multimodel inference on "meanMAP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      2.13  0.13    -0.01 0.04
# Mod2  6 2007.15      0.52  0.28    -0.01 0.04
# Mod5  6 2015.46      8.83  0.00    -0.04 0.04
# Mod6  5 2019.49     12.86  0.00    -0.04 0.04
# Mod9  6 2007.63      1.00  0.22     0.00 0.04
# Mod10 5 2006.63      0.00  0.36     0.00 0.04
# Mod13 5 2016.04      9.41  0.00    -0.03 0.04
# Mod14 4 2029.37     22.74  0.00    -0.04 0.04
# Mod17 6 2023.37     16.74  0.00    -0.08 0.04
# Mod18 5 2024.82     18.19  0.00    -0.11 0.04
# Mod21 5 2039.09     32.46  0.00    -0.15 0.04
# Mod22 4 2037.09     30.46  0.00    -0.15 0.04
# Mod25 5 2038.81     32.18  0.00    -0.11 0.04
# Mod26 4 2039.79     33.16  0.00    -0.14 0.04
# Mod29 4 2087.79     81.16  0.00    -0.28 0.03
# Mod30 3 2091.49     84.86  0.00    -0.27 0.03
# 
# Model-averaged estimate: 0 
# Unconditional SE: 0.04 
# 95% Unconditional confidence interval: -0.09, 0.08
AICcmodavg::modavg(cand_mod, parm = "meanAVP", second.ord = FALSE)
# Multimodel inference on "meanAVP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      4.13  0.03    -0.15 0.05
# Mod2  6 2007.15      2.52  0.08    -0.17 0.04
# Mod3  6 2006.78      2.15  0.09    -0.15 0.05
# Mod4  5 2005.16      0.53  0.21    -0.17 0.04
# Mod9  6 2007.63      3.00  0.06    -0.16 0.05
# Mod10 5 2006.63      2.00  0.10    -0.19 0.04
# Mod11 5 2005.63      1.00  0.16    -0.16 0.05
# Mod12 4 2004.63      0.00  0.27    -0.19 0.04
# Mod17 6 2023.37     18.74  0.00    -0.21 0.05
# Mod18 5 2024.82     20.19  0.00    -0.17 0.04
# Mod19 5 2025.49     20.86  0.00    -0.25 0.05
# Mod20 4 2031.12     26.48  0.00    -0.20 0.04
# Mod25 5 2038.81     34.18  0.00    -0.31 0.04
# Mod26 4 2039.79     35.16  0.00    -0.27 0.04
# Mod27 4 2045.10     40.47  0.00    -0.38 0.04
# Mod28 3 2051.79     47.16  0.00    -0.34 0.03
# 
# Model-averaged estimate: -0.17 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.26, -0.08
AICcmodavg::modavg(cand_mod, parm = "meanbirdrich", second.ord = FALSE)
# Multimodel inference on "meanbirdrich" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      3.59  0.08    -0.05 0.05
# Mod2  6 2007.15      1.98  0.19    -0.06 0.05
# Mod3  6 2006.78      1.62  0.22    -0.05 0.05
# Mod4  5 2005.16      0.00  0.50    -0.06 0.05
# Mod5  6 2015.46     10.30  0.00    -0.08 0.05
# Mod6  5 2019.49     14.32  0.00    -0.15 0.04
# Mod7  5 2014.17      9.01  0.01    -0.08 0.05
# Mod8  4 2018.54     13.38  0.00    -0.15 0.04
# Mod17 6 2023.37     18.20  0.00    -0.18 0.04
# Mod18 5 2024.82     19.65  0.00    -0.18 0.04
# Mod19 5 2025.49     20.32  0.00    -0.20 0.04
# Mod20 4 2031.12     25.95  0.00    -0.21 0.04
# Mod21 5 2039.09     33.93  0.00    -0.28 0.04
# Mod22 4 2037.09     31.93  0.00    -0.28 0.04
# Mod23 4 2052.64     47.47  0.00    -0.35 0.03
# Mod24 3 2051.44     46.28  0.00    -0.34 0.03
# 
# Model-averaged estimate: -0.06 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.16, 0.04

AICcmodavg::modavg(cand_mod, parm = "meanbatpres_bin", second.ord = FALSE)
# Multimodel inference on "meanbatpres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      4.13  0.03    -0.26 0.06
# Mod2  6 2007.15      2.52  0.08    -0.23 0.05
# Mod3  6 2006.78      2.15  0.09    -0.26 0.06
# Mod4  5 2005.16      0.53  0.20    -0.24 0.04
# Mod5  6 2015.46     10.83  0.00    -0.31 0.06
# Mod6  5 2019.49     14.85  0.00    -0.24 0.05
# Mod7  5 2014.17      9.54  0.00    -0.33 0.05
# Mod8  4 2018.54     13.91  0.00    -0.27 0.04
# Mod9  6 2007.63      3.00  0.06    -0.29 0.05
# Mod10 5 2006.63      2.00  0.10    -0.27 0.04
# Mod11 5 2005.63      1.00  0.16    -0.29 0.04
# Mod12 4 2004.63      0.00  0.27    -0.27 0.04
# Mod13 5 2016.04     11.40  0.00    -0.38 0.04
# Mod14 4 2029.37     24.74  0.00    -0.35 0.04
# Mod15 4 2014.59      9.96  0.00    -0.40 0.03
# Mod16 3 2028.25     23.62  0.00    -0.37 0.03
# 
# Model-averaged estimate: -0.26 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.36, -0.16

# model averaged estimate is strongest for bat presence/absence (-0.26), then 
# for phosphorus (-0.17), close to 0 for all other variables

#### flower colourfulness model averaging ####

# first create all possible formulas 
formulas <- apply(data.frame(expand.grid(c("meanMAT", NA), c("meanMAP", NA),
                                         c("meanAVP", NA), c("meanbirdrich", NA),
                                         c("meanbatpres_bin", NA))), 1, 
                  paste0.na.omit)
formulas[length(formulas)] <- "1"
formulas <- paste0("colour_fullbinary ~ ", formulas)
formulas

# run all candidate models          
cand_mod <- list()
for(i in 1:length(formulas)){
  cand_mod[[i]] <- glm(as.formula(formulas[[i]]), 
                       data = eucvarscaled,
                       family = binomial)
}

# use modavg to get the model averaged standardised regression coefficients
AICcmodavg::modavg(cand_mod, parm = "meanMAT", second.ord = FALSE)
# Multimodel inference on "meanMAT" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 492.21      1.90  0.12     0.17 0.20
# Mod3  5 490.31      0.00  0.31     0.18 0.19
# Mod5  5 495.32      5.02  0.03     0.29 0.17
# Mod7  4 493.84      3.53  0.05     0.34 0.16
# Mod9  5 492.34      2.03  0.11     0.28 0.18
# Mod11 4 490.35      0.04  0.31     0.28 0.18
# Mod13 4 496.62      6.31  0.01     0.41 0.16
# Mod15 3 494.64      4.33  0.04     0.42 0.16
# Mod17 5 496.61      6.31  0.01    -0.16 0.15
# Mod19 4 498.17      7.86  0.01    -0.25 0.13
# Mod21 4 504.30     13.99  0.00    -0.04 0.13
# Mod23 3 516.49     26.18  0.00    -0.05 0.11
# Mod25 4 507.87     17.56  0.00    -0.09 0.15
# Mod27 3 513.13     22.82  0.00    -0.26 0.14
# Mod29 3 528.54     38.23  0.00     0.10 0.14
# Mod31 2 569.43     79.12  0.00     0.06 0.11
# 
# Model-averaged estimate: 0.24 
# Unconditional SE: 0.2 
# 95% Unconditional confidence interval: -0.16, 0.64
AICcmodavg::modavg(cand_mod, parm = "meanMAP", second.ord = FALSE)
# Multimodel inference on "meanMAP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 492.21      1.24  0.19    -0.06 0.19
# Mod2  5 490.97      0.00  0.35    -0.09 0.19
# Mod5  5 495.32      4.36  0.04    -0.13 0.19
# Mod6  4 496.28      5.31  0.02    -0.25 0.17
# Mod9  5 492.34      1.38  0.18     0.01 0.19
# Mod10 4 492.76      1.79  0.14    -0.01 0.20
# Mod13 4 496.62      5.65  0.02    -0.03 0.18
# Mod14 3 501.18     10.22  0.00    -0.16 0.18
# Mod17 5 496.61      5.65  0.02    -0.31 0.17
# Mod18 4 495.85      4.88  0.03    -0.37 0.16
# Mod21 4 504.30     13.33  0.00    -0.52 0.15
# Mod22 3 502.37     11.41  0.00    -0.52 0.15
# Mod25 4 507.87     16.90  0.00    -0.47 0.18
# Mod26 3 506.20     15.24  0.00    -0.50 0.17
# Mod29 3 528.54     37.57  0.00    -0.89 0.15
# Mod30 2 527.09     36.12  0.00    -0.89 0.16
# 
# Model-averaged estimate: -0.07 
# Unconditional SE: 0.21 
# 95% Unconditional confidence interval: -0.48, 0.34
AICcmodavg::modavg(cand_mod, parm = "meanAVP", second.ord = FALSE)
# Multimodel inference on "meanAVP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 492.21      3.00  0.06    -0.38 0.22
# Mod2  5 490.97      1.77  0.11    -0.46 0.20
# Mod3  5 490.31      1.11  0.16    -0.39 0.22
# Mod4  4 489.20      0.00  0.27    -0.49 0.19
# Mod9  5 492.34      3.14  0.06    -0.36 0.21
# Mod10 4 492.76      3.56  0.05    -0.51 0.20
# Mod11 4 490.35      1.15  0.15    -0.36 0.21
# Mod12 3 490.76      1.56  0.12    -0.51 0.19
# Mod17 5 496.61      7.41  0.01    -0.59 0.21
# Mod18 4 495.85      6.65  0.01    -0.52 0.20
# Mod19 4 498.17      8.97  0.00    -0.78 0.19
# Mod20 3 500.03     10.83  0.00    -0.68 0.19
# Mod25 4 507.87     18.67  0.00    -0.80 0.20
# Mod26 3 506.20     17.00  0.00    -0.76 0.19
# Mod27 3 513.13     23.93  0.00    -1.11 0.17
# Mod28 2 515.16     25.96  0.00    -1.03 0.17
# 
# Model-averaged estimate: -0.44 
# Unconditional SE: 0.21 
# 95% Unconditional confidence interval: -0.86, -0.03
AICcmodavg::modavg(cand_mod, parm = "meanbirdrich", second.ord = FALSE)
# Multimodel inference on "meanbirdrich" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 492.21      3.00  0.09    -0.37 0.25
# Mod2  5 490.97      1.77  0.17    -0.45 0.23
# Mod3  5 490.31      1.11  0.23    -0.35 0.24
# Mod4  4 489.20      0.00  0.40    -0.43 0.23
# Mod5  5 495.32      6.12  0.02    -0.42 0.23
# Mod6  4 496.28      7.08  0.01    -0.56 0.21
# Mod7  4 493.84      4.64  0.04    -0.37 0.22
# Mod8  3 496.62      7.42  0.01    -0.48 0.21
# Mod17 5 496.61      7.41  0.01    -0.73 0.20
# Mod18 4 495.85      6.65  0.01    -0.71 0.20
# Mod19 4 498.17      8.97  0.00    -0.78 0.19
# Mod20 3 500.03     10.83  0.00    -0.79 0.20
# Mod21 4 504.30     15.10  0.00    -0.86 0.17
# Mod22 3 502.37     13.17  0.00    -0.85 0.17
# Mod23 3 516.49     27.29  0.00    -0.99 0.15
# Mod24 2 514.70     25.50  0.00    -0.99 0.15
# 
# Model-averaged estimate: -0.42 
# Unconditional SE: 0.24 
# 95% Unconditional confidence interval: -0.9, 0.06
AICcmodavg::modavg(cand_mod, parm = "meanbatpres_bin", second.ord = FALSE)
# Multimodel inference on "meanbatpres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 492.21      3.00  0.06    -0.72 0.30
# Mod2  5 490.97      1.77  0.11    -0.56 0.23
# Mod3  5 490.31      1.11  0.15    -0.78 0.25
# Mod4  4 489.20      0.00  0.26    -0.62 0.19
# Mod5  5 495.32      6.12  0.01    -0.84 0.28
# Mod6  4 496.28      7.08  0.01    -0.58 0.22
# Mod7  4 493.84      4.64  0.03    -0.97 0.21
# Mod8  3 496.62      7.42  0.01    -0.77 0.19
# Mod9  5 492.34      3.14  0.05    -1.00 0.25
# Mod10 4 492.76      3.56  0.04    -0.81 0.21
# Mod11 4 490.35      1.15  0.14    -0.99 0.21
# Mod12 3 490.76      1.56  0.12    -0.82 0.18
# Mod13 4 496.62      7.42  0.01    -1.18 0.22
# Mod14 3 501.18     11.98  0.00    -0.97 0.19
# Mod15 3 494.64      5.44  0.02    -1.20 0.17
# Mod16 2 500.04     10.84  0.00    -1.08 0.15
# 
# Model-averaged estimate: -0.77 
# Unconditional SE: 0.27 
# 95% Unconditional confidence interval: -1.31, -0.24

# model averaged estimate is strongest for bat presence/absence (-0.77), then 
# for phosphorus (-0.44), then bird richness (-0.42), then MAT (0.26), then MAP (-0.07)

rm(cand_mod, formulas, paste0.na.omit, i, eucvarscaled)

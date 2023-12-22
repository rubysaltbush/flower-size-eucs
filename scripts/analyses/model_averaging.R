# compare impact of all environmental variables on bud size and flower 
# colourfulness using model averaged standardised regression coefficients
# to determine which predictor variable explains the most variance in 
# these response variables, in this data set

# This is a relevant paper in Methods of Ecology and Evolution https://doi.org/10.1111/2041-210X.12835. 
# Their recommended method is model averaged standardised regression coefficients. 

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

# set all formulas
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
# Mod1  7 2008.76      3.98  0.07     0.04 0.04
# Mod3  6 2006.76      1.98  0.19     0.04 0.04
# Mod5  6 2016.17     11.39  0.00     0.11 0.04
# Mod7  5 2014.54      9.76  0.00     0.11 0.04
# Mod9  6 2006.78      2.00  0.19     0.04 0.04
# Mod11 5 2004.78      0.00  0.52     0.04 0.04
# Mod13 5 2014.74      9.96  0.00     0.13 0.03
# Mod15 4 2013.12      8.34  0.01     0.13 0.03
# Mod17 6 2027.80     23.02  0.00    -0.07 0.04
# Mod19 5 2029.15     24.37  0.00    -0.10 0.03
# Mod21 5 2046.53     41.75  0.00     0.00 0.03
# Mod23 4 2057.68     52.90  0.00    -0.03 0.03
# Mod25 5 2038.81     34.03  0.00    -0.06 0.04
# Mod27 4 2045.10     40.32  0.00    -0.10 0.04
# Mod29 4 2087.79     83.01  0.00     0.08 0.03
# Mod31 3 2153.54    148.76  0.00     0.06 0.03
# 
# Model-averaged estimate: 0.05 
# Unconditional SE: 0.04 
# 95% Unconditional confidence interval: -0.04, 0.13

AICcmodavg::modavg(cand_mod, parm = "meanMAP", second.ord = FALSE)
# Multimodel inference on "meanMAP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      2.80  0.10     0.00 0.04
# Mod2  6 2007.69      1.72  0.18     0.00 0.04
# Mod5  6 2016.17     10.20  0.00    -0.03 0.04
# Mod6  5 2022.69     16.72  0.00    -0.03 0.04
# Mod9  6 2006.78      0.81  0.28     0.00 0.04
# Mod10 5 2005.97      0.00  0.42     0.01 0.04
# Mod13 5 2014.74      8.78  0.01    -0.03 0.04
# Mod14 4 2028.70     22.74  0.00    -0.04 0.04
# Mod17 6 2027.80     21.83  0.00    -0.07 0.04
# Mod18 5 2029.42     23.45  0.00    -0.10 0.04
# Mod21 5 2046.53     40.57  0.00    -0.14 0.04
# Mod22 4 2044.54     38.58  0.00    -0.14 0.04
# Mod25 5 2038.81     32.84  0.00    -0.11 0.04
# Mod26 4 2039.79     33.82  0.00    -0.14 0.04
# Mod29 4 2087.79     81.83  0.00    -0.28 0.03
# Mod30 3 2091.49     85.53  0.00    -0.27 0.03
# 
# Model-averaged estimate: 0 
# Unconditional SE: 0.04 
# 95% Unconditional confidence interval: -0.08, 0.09

AICcmodavg::modavg(cand_mod, parm = "meanAVP", second.ord = FALSE)
# Multimodel inference on "meanAVP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      4.78  0.03    -0.16 0.05
# Mod2  6 2007.69      3.71  0.05    -0.18 0.04
# Mod3  6 2006.76      2.78  0.08    -0.16 0.05
# Mod4  5 2005.69      1.71  0.13    -0.18 0.04
# Mod9  6 2006.78      2.80  0.08    -0.16 0.05
# Mod10 5 2005.97      1.99  0.12    -0.19 0.04
# Mod11 5 2004.78      0.80  0.21    -0.16 0.05
# Mod12 4 2003.98      0.00  0.31    -0.19 0.04
# Mod17 6 2027.80     23.82  0.00    -0.22 0.05
# Mod18 5 2029.42     25.44  0.00    -0.18 0.04
# Mod19 5 2029.15     25.17  0.00    -0.26 0.05
# Mod20 4 2034.64     30.66  0.00    -0.21 0.04
# Mod25 5 2038.81     34.83  0.00    -0.31 0.04
# Mod26 4 2039.79     35.81  0.00    -0.27 0.04
# Mod27 4 2045.10     41.12  0.00    -0.38 0.04
# Mod28 3 2051.79     47.81  0.00    -0.34 0.03
# 
# Model-averaged estimate: -0.18 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.27, -0.08

AICcmodavg::modavg(cand_mod, parm = "meanbirdrich", second.ord = FALSE)
# Multimodel inference on "meanbirdrich" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      3.07  0.10    -0.01 0.06
# Mod2  6 2007.69      1.99  0.17    -0.03 0.05
# Mod3  6 2006.76      1.07  0.27    -0.01 0.06
# Mod4  5 2005.69      0.00  0.46    -0.03 0.05
# Mod5  6 2016.17     10.48  0.00    -0.04 0.05
# Mod6  5 2022.69     17.00  0.00    -0.13 0.05
# Mod7  5 2014.54      8.84  0.01    -0.04 0.05
# Mod8  4 2021.21     15.52  0.00    -0.13 0.05
# Mod17 6 2027.80     22.10  0.00    -0.16 0.04
# Mod18 5 2029.42     23.72  0.00    -0.16 0.04
# Mod19 5 2029.15     23.46  0.00    -0.18 0.04
# Mod20 4 2034.64     28.95  0.00    -0.19 0.04
# Mod21 5 2046.53     40.84  0.00    -0.26 0.04
# Mod22 4 2044.54     38.85  0.00    -0.26 0.04
# Mod23 4 2057.68     51.99  0.00    -0.34 0.03
# Mod24 3 2056.33     50.64  0.00    -0.33 0.03
# 
# Model-averaged estimate: -0.02 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.13, 0.08

AICcmodavg::modavg(cand_mod, parm = "meanbatpres_bin", second.ord = FALSE)
# Multimodel inference on "meanbatpres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 2008.76      4.78  0.03    -0.29 0.06
# Mod2  6 2007.69      3.71  0.05    -0.26 0.05
# Mod3  6 2006.76      2.78  0.08    -0.29 0.06
# Mod4  5 2005.69      1.71  0.13    -0.26 0.05
# Mod5  6 2016.17     12.19  0.00    -0.35 0.06
# Mod6  5 2022.69     18.71  0.00    -0.26 0.05
# Mod7  5 2014.54     10.56  0.00    -0.37 0.05
# Mod8  4 2021.21     17.23  0.00    -0.28 0.05
# Mod9  6 2006.78      2.80  0.08    -0.30 0.05
# Mod10 5 2005.97      1.99  0.11    -0.27 0.05
# Mod11 5 2004.78      0.80  0.21    -0.30 0.05
# Mod12 4 2003.98      0.00  0.31    -0.27 0.04
# Mod13 5 2014.74     10.77  0.00    -0.38 0.04
# Mod14 4 2028.70     24.72  0.00    -0.35 0.04
# Mod15 4 2013.12      9.14  0.00    -0.40 0.03
# Mod16 3 2027.40     23.42  0.00    -0.38 0.03
# 
# Model-averaged estimate: -0.28 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.37, -0.18

# model averaged estimate is strongest for bat presence/absence (-0.28), then 
# for phosphorus (-0.18), close to 0 for all other variables


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
# Mod1  6 491.47      2.00  0.09     0.19 0.20
# Mod3  5 489.48      0.00  0.23     0.19 0.20
# Mod5  5 492.34      2.86  0.06     0.33 0.18
# Mod7  4 490.47      0.99  0.14     0.36 0.16
# Mod9  5 491.68      2.21  0.08     0.31 0.19
# Mod11 4 489.73      0.26  0.20     0.31 0.18
# Mod13 4 492.47      2.99  0.05     0.44 0.17
# Mod15 3 490.47      0.99  0.14     0.44 0.16
# Mod17 5 495.93      6.45  0.01    -0.18 0.14
# Mod19 4 496.21      6.73  0.01    -0.24 0.13
# Mod21 4 502.09     12.61  0.00    -0.05 0.13
# Mod23 3 511.08     21.61  0.00    -0.05 0.11
# Mod25 4 507.87     18.39  0.00    -0.09 0.15
# Mod27 3 513.13     23.65  0.00    -0.26 0.14
# Mod29 3 528.54     39.06  0.00     0.10 0.14
# Mod31 2 569.43     79.95  0.00     0.06 0.11
# 
# Model-averaged estimate: 0.3 
# Unconditional SE: 0.21 
# 95% Unconditional confidence interval: -0.12, 0.72

AICcmodavg::modavg(cand_mod, parm = "meanMAP", second.ord = FALSE)
# Multimodel inference on "meanMAP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 491.47      1.15  0.16    -0.01 0.19
# Mod2  5 490.32      0.00  0.29    -0.05 0.19
# Mod5  5 492.34      2.01  0.11    -0.07 0.19
# Mod6  4 493.84      3.52  0.05    -0.21 0.17
# Mod9  5 491.68      1.36  0.15     0.04 0.20
# Mod10 4 492.46      2.14  0.10     0.01 0.20
# Mod13 4 492.47      2.15  0.10     0.00 0.19
# Mod14 3 497.70      7.38  0.01    -0.16 0.18
# Mod17 5 495.93      5.60  0.02    -0.25 0.17
# Mod18 4 495.46      5.13  0.02    -0.31 0.16
# Mod21 4 502.09     11.77  0.00    -0.46 0.15
# Mod22 3 500.23      9.91  0.00    -0.46 0.15
# Mod25 4 507.87     17.54  0.00    -0.47 0.18
# Mod26 3 506.20     15.88  0.00    -0.50 0.17
# Mod29 3 528.54     38.21  0.00    -0.89 0.15
# Mod30 2 527.09     36.76  0.00    -0.89 0.16
# 
# Model-averaged estimate: -0.04 
# Unconditional SE: 0.2 
# 95% Unconditional confidence interval: -0.44, 0.36

AICcmodavg::modavg(cand_mod, parm = "meanAVP", second.ord = FALSE)
# Multimodel inference on "meanAVP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 491.47      3.08  0.06    -0.36 0.22
# Mod2  5 490.32      1.93  0.11    -0.45 0.20
# Mod3  5 489.48      1.08  0.17    -0.36 0.22
# Mod4  4 488.39      0.00  0.29    -0.47 0.19
# Mod9  5 491.68      3.29  0.06    -0.35 0.21
# Mod10 4 492.46      4.07  0.04    -0.51 0.20
# Mod11 4 489.73      1.34  0.15    -0.34 0.21
# Mod12 3 490.46      2.07  0.10    -0.51 0.19
# Mod17 5 495.93      7.53  0.01    -0.57 0.21
# Mod18 4 495.46      7.06  0.01    -0.50 0.20
# Mod19 4 496.21      7.82  0.01    -0.72 0.19
# Mod20 3 497.75      9.36  0.00    -0.63 0.19
# Mod25 4 507.87     19.48  0.00    -0.80 0.20
# Mod26 3 506.20     17.81  0.00    -0.76 0.19
# Mod27 3 513.13     24.74  0.00    -1.11 0.17
# Mod28 2 515.16     26.76  0.00    -1.03 0.17
# 
# Model-averaged estimate: -0.42 
# Unconditional SE: 0.21 
# 95% Unconditional confidence interval: -0.84, -0.01

AICcmodavg::modavg(cand_mod, parm = "meanbirdrich", second.ord = FALSE)
# Multimodel inference on "meanbirdrich" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 491.47      3.08  0.07    -0.40 0.27
# Mod2  5 490.32      1.93  0.13    -0.50 0.25
# Mod3  5 489.48      1.08  0.20    -0.40 0.26
# Mod4  4 488.39      0.00  0.35    -0.49 0.24
# Mod5  5 492.34      3.94  0.05    -0.37 0.25
# Mod6  4 493.84      5.45  0.02    -0.55 0.23
# Mod7  4 490.47      2.08  0.12    -0.35 0.25
# Mod8  3 493.53      5.14  0.03    -0.51 0.23
# Mod17 5 495.93      7.53  0.01    -0.80 0.21
# Mod18 4 495.46      7.06  0.01    -0.78 0.22
# Mod19 4 496.21      7.82  0.01    -0.86 0.21
# Mod20 3 497.75      9.36  0.00    -0.89 0.21
# Mod21 4 502.09     13.70  0.00    -0.89 0.19
# Mod22 3 500.23     11.84  0.00    -0.88 0.19
# Mod23 3 511.08     22.69  0.00    -1.05 0.17
# Mod24 2 509.31     20.91  0.00    -1.05 0.17
# 
# Model-averaged estimate: -0.46 
# Unconditional SE: 0.26 
# 95% Unconditional confidence interval: -0.97, 0.06

AICcmodavg::modavg(cand_mod, parm = "meanbatpres_bin", second.ord = FALSE)
# Multimodel inference on "meanbatpres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  6 491.47      3.08  0.05    -0.76 0.31
# Mod2  5 490.32      1.93  0.08    -0.56 0.23
# Mod3  5 489.48      1.08  0.13    -0.77 0.26
# Mod4  4 488.39      0.00  0.22    -0.59 0.19
# Mod5  5 492.34      3.94  0.03    -0.92 0.29
# Mod6  4 493.84      5.45  0.01    -0.59 0.22
# Mod7  4 490.47      2.08  0.08    -0.99 0.23
# Mod8  3 493.53      5.14  0.02    -0.75 0.20
# Mod9  5 491.68      3.29  0.04    -1.05 0.26
# Mod10 4 492.46      4.07  0.03    -0.83 0.21
# Mod11 4 489.73      1.34  0.11    -1.01 0.21
# Mod12 3 490.46      2.07  0.08    -0.82 0.18
# Mod13 4 492.47      4.08  0.03    -1.21 0.23
# Mod14 3 497.70      9.31  0.00    -0.96 0.19
# Mod15 3 490.47      2.08  0.08    -1.21 0.17
# Mod16 2 496.56      8.17  0.00    -1.07 0.15
# 
# Model-averaged estimate: -0.82 
# Unconditional SE: 0.31 
# 95% Unconditional confidence interval: -1.43, -0.22

# model averaged estimate is strongest for bat presence/absence (-0.82), then 
# bird richness (-0.46), then phosphorus (-0.42), then MAT (0.3), then MAP (-0.04)

rm(cand_mod, formulas, paste0.na.omit, i, eucvarscaled)

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
                meanMAT, meanMAP, meanAVP, meanbirdrich, meanbatpres_bin, meanmarspres_bin)
# scale all predictor variables
eucvarscaled[4:9] <- mapply(FUN = scale, eucvarscaled[4:9])
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
                                         c("meanbatpres_bin", NA), 
                                         c("meanmarspres_bin", NA))), 1, 
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
# Mod1  8 2010.42      5.64  0.02     0.05 0.05
# Mod3  7 2008.47      3.69  0.06     0.05 0.05
# Mod5  7 2015.21     10.43  0.00     0.13 0.04
# Mod7  6 2014.51      9.73  0.00     0.13 0.04
# Mod9  7 2008.51      3.73  0.06     0.06 0.05
# Mod11 6 2006.55      1.76  0.15     0.05 0.05
# Mod13 6 2014.40      9.62  0.00     0.15 0.03
# Mod15 5 2013.56      8.78  0.00     0.15 0.03
# Mod17 7 2029.08     24.30  0.00    -0.05 0.04
# Mod19 6 2031.15     26.37  0.00    -0.10 0.04
# Mod21 6 2041.52     36.74  0.00     0.04 0.04
# Mod23 5 2057.46     52.68  0.00    -0.01 0.03
# Mod25 6 2040.69     35.91  0.00    -0.07 0.04
# Mod27 5 2044.28     39.50  0.00    -0.12 0.04
# Mod29 5 2086.47     81.69  0.00     0.11 0.04
# Mod31 4 2153.19    148.41  0.00     0.04 0.04
# Mod33 7 2008.76      3.98  0.05     0.04 0.04
# Mod35 6 2006.76      1.98  0.14     0.04 0.04
# Mod37 6 2016.17     11.39  0.00     0.11 0.04
# Mod39 5 2014.54      9.76  0.00     0.11 0.04
# Mod41 6 2006.78      2.00  0.13     0.04 0.04
# Mod43 5 2004.78      0.00  0.37     0.04 0.04
# Mod45 5 2014.74      9.96  0.00     0.13 0.03
# Mod47 4 2013.12      8.34  0.01     0.13 0.03
# Mod49 6 2027.80     23.02  0.00    -0.07 0.04
# Mod51 5 2029.15     24.37  0.00    -0.10 0.03
# Mod53 5 2046.53     41.75  0.00     0.00 0.03
# Mod55 4 2057.68     52.90  0.00    -0.03 0.03
# Mod57 5 2038.81     34.03  0.00    -0.06 0.04
# Mod59 4 2045.10     40.32  0.00    -0.10 0.04
# Mod61 4 2087.79     83.01  0.00     0.08 0.03
# Mod63 3 2153.54    148.76  0.00     0.06 0.03
# 
# Model-averaged estimate: 0.05 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.04, 0.14

AICcmodavg::modavg(cand_mod, parm = "meanMAP", second.ord = FALSE)
# Multimodel inference on "meanMAP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  8 2010.42      4.46  0.03    -0.01 0.05
# Mod2  7 2009.67      3.70  0.05     0.00 0.05
# Mod5  7 2015.21      9.24  0.00    -0.05 0.04
# Mod6  6 2023.82     17.85  0.00    -0.04 0.05
# Mod9  7 2008.51      2.55  0.08    -0.01 0.05
# Mod10 6 2007.96      1.99  0.11     0.01 0.05
# Mod13 6 2014.40      8.44  0.00    -0.05 0.04
# Mod14 5 2030.66     24.70  0.00    -0.03 0.05
# Mod17 7 2029.08     23.12  0.00    -0.09 0.04
# Mod18 6 2028.56     22.60  0.00    -0.12 0.04
# Mod21 6 2041.52     35.55  0.00    -0.17 0.04
# Mod22 5 2040.64     34.67  0.00    -0.16 0.04
# Mod25 6 2040.69     34.72  0.00    -0.11 0.04
# Mod26 5 2041.48     35.51  0.00    -0.14 0.04
# Mod29 5 2086.47     80.51  0.00    -0.30 0.04
# Mod30 4 2093.04     87.07  0.00    -0.28 0.03
# Mod33 7 2008.76      2.80  0.08     0.00 0.04
# Mod34 6 2007.69      1.72  0.13     0.00 0.04
# Mod37 6 2016.17     10.20  0.00    -0.03 0.04
# Mod38 5 2022.69     16.72  0.00    -0.03 0.04
# Mod41 6 2006.78      0.81  0.20     0.00 0.04
# Mod42 5 2005.97      0.00  0.30     0.01 0.04
# Mod45 5 2014.74      8.78  0.00    -0.03 0.04
# Mod46 4 2028.70     22.74  0.00    -0.04 0.04
# Mod49 6 2027.80     21.83  0.00    -0.07 0.04
# Mod50 5 2029.42     23.45  0.00    -0.10 0.04
# Mod53 5 2046.53     40.57  0.00    -0.14 0.04
# Mod54 4 2044.54     38.58  0.00    -0.14 0.04
# Mod57 5 2038.81     32.84  0.00    -0.11 0.04
# Mod58 4 2039.79     33.82  0.00    -0.14 0.04
# Mod61 4 2087.79     81.83  0.00    -0.28 0.03
# Mod62 3 2091.49     85.53  0.00    -0.27 0.03
# 
# Model-averaged estimate: 0 
# Unconditional SE: 0.04 
# 95% Unconditional confidence interval: -0.09, 0.09

AICcmodavg::modavg(cand_mod, parm = "meanAVP", second.ord = FALSE)
# Multimodel inference on "meanAVP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  8 2010.42      6.44  0.01    -0.14 0.06
# Mod2  7 2009.67      5.69  0.01    -0.18 0.04
# Mod3  7 2008.47      4.49  0.02    -0.15 0.05
# Mod4  6 2007.67      3.69  0.04    -0.18 0.04
# Mod9  7 2008.51      4.53  0.02    -0.15 0.05
# Mod10 6 2007.96      3.98  0.03    -0.19 0.04
# Mod11 6 2006.55      2.57  0.06    -0.15 0.05
# Mod12 5 2005.98      2.00  0.08    -0.19 0.04
# Mod17 7 2029.08     25.10  0.00    -0.21 0.05
# Mod18 6 2028.56     24.59  0.00    -0.17 0.05
# Mod19 6 2031.15     27.17  0.00    -0.26 0.05
# Mod20 5 2035.61     31.63  0.00    -0.21 0.04
# Mod25 6 2040.69     36.71  0.00    -0.32 0.05
# Mod26 5 2041.48     37.50  0.00    -0.27 0.04
# Mod27 5 2044.28     40.30  0.00    -0.38 0.04
# Mod28 4 2053.42     49.45  0.00    -0.34 0.03
# Mod33 7 2008.76      4.78  0.02    -0.16 0.05
# Mod34 6 2007.69      3.71  0.04    -0.18 0.04
# Mod35 6 2006.76      2.78  0.06    -0.16 0.05
# Mod36 5 2005.69      1.71  0.10    -0.18 0.04
# Mod41 6 2006.78      2.80  0.06    -0.16 0.05
# Mod42 5 2005.97      1.99  0.08    -0.19 0.04
# Mod43 5 2004.78      0.80  0.15    -0.16 0.05
# Mod44 4 2003.98      0.00  0.22    -0.19 0.04
# Mod49 6 2027.80     23.82  0.00    -0.22 0.05
# Mod50 5 2029.42     25.44  0.00    -0.18 0.04
# Mod51 5 2029.15     25.17  0.00    -0.26 0.05
# Mod52 4 2034.64     30.66  0.00    -0.21 0.04
# Mod57 5 2038.81     34.83  0.00    -0.31 0.04
# Mod58 4 2039.79     35.81  0.00    -0.27 0.04
# Mod59 4 2045.10     41.12  0.00    -0.38 0.04
# Mod60 3 2051.79     47.81  0.00    -0.34 0.03
# 
# Model-averaged estimate: -0.17 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.27, -0.08

AICcmodavg::modavg(cand_mod, parm = "meanbirdrich", second.ord = FALSE)
# Multimodel inference on "meanbirdrich" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  8 2010.42      4.73  0.03    -0.02 0.06
# Mod2  7 2009.67      3.97  0.04    -0.03 0.06
# Mod3  7 2008.47      2.78  0.08    -0.02 0.06
# Mod4  6 2007.67      1.98  0.12    -0.03 0.06
# Mod5  7 2015.21      9.51  0.00    -0.06 0.06
# Mod6  6 2023.82     18.12  0.00    -0.15 0.05
# Mod7  6 2014.51      8.81  0.00    -0.06 0.06
# Mod8  5 2022.80     17.11  0.00    -0.14 0.05
# Mod17 7 2029.08     23.39  0.00    -0.17 0.05
# Mod18 6 2028.56     22.87  0.00    -0.18 0.05
# Mod19 6 2031.15     25.46  0.00    -0.18 0.05
# Mod20 5 2035.61     29.91  0.00    -0.21 0.05
# Mod21 6 2041.52     35.83  0.00    -0.27 0.04
# Mod22 5 2040.64     34.95  0.00    -0.28 0.04
# Mod23 5 2057.46     51.77  0.00    -0.35 0.03
# Mod24 4 2055.57     49.87  0.00    -0.35 0.03
# Mod33 7 2008.76      3.07  0.07    -0.01 0.06
# Mod34 6 2007.69      1.99  0.12    -0.03 0.05
# Mod35 6 2006.76      1.07  0.19    -0.01 0.06
# Mod36 5 2005.69      0.00  0.33    -0.03 0.05
# Mod37 6 2016.17     10.48  0.00    -0.04 0.05
# Mod38 5 2022.69     17.00  0.00    -0.13 0.05
# Mod39 5 2014.54      8.84  0.00    -0.04 0.05
# Mod40 4 2021.21     15.52  0.00    -0.13 0.05
# Mod49 6 2027.80     22.10  0.00    -0.16 0.04
# Mod50 5 2029.42     23.72  0.00    -0.16 0.04
# Mod51 5 2029.15     23.46  0.00    -0.18 0.04
# Mod52 4 2034.64     28.95  0.00    -0.19 0.04
# Mod53 5 2046.53     40.84  0.00    -0.26 0.04
# Mod54 4 2044.54     38.85  0.00    -0.26 0.04
# Mod55 4 2057.68     51.99  0.00    -0.34 0.03
# Mod56 3 2056.33     50.64  0.00    -0.33 0.03
# 
# Model-averaged estimate: -0.02 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.13, 0.09

AICcmodavg::modavg(cand_mod, parm = "meanbatpres_bin", second.ord = FALSE)
# Multimodel inference on "meanbatpres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  8 2010.42      6.44  0.01    -0.29 0.06
# Mod2  7 2009.67      5.69  0.01    -0.26 0.06
# Mod3  7 2008.47      4.49  0.02    -0.29 0.06
# Mod4  6 2007.67      3.69  0.04    -0.25 0.05
# Mod5  7 2015.21     11.23  0.00    -0.33 0.06
# Mod6  6 2023.82     19.84  0.00    -0.24 0.06
# Mod7  6 2014.51     10.53  0.00    -0.36 0.05
# Mod8  5 2022.80     18.82  0.00    -0.28 0.05
# Mod9  7 2008.51      4.53  0.02    -0.30 0.05
# Mod10 6 2007.96      3.98  0.03    -0.27 0.05
# Mod11 6 2006.55      2.57  0.06    -0.30 0.05
# Mod12 5 2005.98      2.00  0.08    -0.27 0.04
# Mod13 6 2014.40     10.42  0.00    -0.38 0.04
# Mod14 5 2030.66     26.68  0.00    -0.35 0.04
# Mod15 5 2013.56      9.59  0.00    -0.41 0.03
# Mod16 4 2029.19     25.21  0.00    -0.37 0.03
# Mod33 7 2008.76      4.78  0.02    -0.29 0.06
# Mod34 6 2007.69      3.71  0.03    -0.26 0.05
# Mod35 6 2006.76      2.78  0.06    -0.29 0.06
# Mod36 5 2005.69      1.71  0.09    -0.26 0.05
# Mod37 6 2016.17     12.19  0.00    -0.35 0.06
# Mod38 5 2022.69     18.71  0.00    -0.26 0.05
# Mod39 5 2014.54     10.56  0.00    -0.37 0.05
# Mod40 4 2021.21     17.23  0.00    -0.28 0.05
# Mod41 6 2006.78      2.80  0.05    -0.30 0.05
# Mod42 5 2005.97      1.99  0.08    -0.27 0.05
# Mod43 5 2004.78      0.80  0.15    -0.30 0.05
# Mod44 4 2003.98      0.00  0.22    -0.27 0.04
# Mod45 5 2014.74     10.77  0.00    -0.38 0.04
# Mod46 4 2028.70     24.72  0.00    -0.35 0.04
# Mod47 4 2013.12      9.14  0.00    -0.40 0.03
# Mod48 3 2027.40     23.42  0.00    -0.38 0.03
# 
# Model-averaged estimate: -0.28 
# Unconditional SE: 0.05 
# 95% Unconditional confidence interval: -0.38, -0.18

AICcmodavg::modavg(cand_mod, parm = "meanmarspres_bin", second.ord = FALSE)
# Multimodel inference on "meanmarspres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K     AIC Delta_AIC AICWt Estimate   SE
# Mod1  8 2010.42      4.45  0.03     0.02 0.04
# Mod2  7 2009.67      3.69  0.05     0.00 0.04
# Mod3  7 2008.47      2.49  0.08     0.02 0.04
# Mod4  6 2007.67      1.69  0.12     0.01 0.03
# Mod5  7 2015.21      9.23  0.00     0.06 0.04
# Mod6  6 2023.82     17.84  0.00     0.03 0.04
# Mod7  6 2014.51      8.53  0.00     0.05 0.04
# Mod8  5 2022.80     16.82  0.00     0.02 0.03
# Mod9  7 2008.51      2.54  0.08     0.02 0.04
# Mod10 6 2007.96      1.98  0.11     0.00 0.03
# Mod11 6 2006.55      0.57  0.22     0.02 0.04
# Mod12 5 2005.98      0.00  0.29     0.00 0.03
# Mod13 6 2014.40      8.43  0.00     0.06 0.04
# Mod14 5 2030.66     24.69  0.00    -0.01 0.03
# Mod15 5 2013.56      7.59  0.01     0.04 0.03
# Mod16 4 2029.19     23.22  0.00    -0.01 0.03
# Mod17 7 2029.08     23.11  0.00     0.03 0.04
# Mod18 6 2028.56     22.59  0.00     0.06 0.04
# Mod19 6 2031.15     25.18  0.00     0.00 0.04
# Mod20 5 2035.61     29.63  0.00     0.03 0.03
# Mod21 6 2041.52     35.54  0.00     0.10 0.04
# Mod22 5 2040.64     34.66  0.00     0.08 0.03
# Mod23 5 2057.46     51.48  0.00     0.05 0.04
# Mod24 4 2055.57     49.59  0.00     0.06 0.03
# Mod25 6 2040.69     34.71  0.00    -0.01 0.04
# Mod26 5 2041.48     35.50  0.00     0.02 0.03
# Mod27 5 2044.28     38.30  0.00    -0.06 0.03
# Mod28 4 2053.42     47.45  0.00    -0.02 0.03
# Mod29 5 2086.47     80.50  0.00     0.07 0.04
# Mod30 4 2093.04     87.06  0.00     0.02 0.03
# Mod31 4 2153.19    147.21  0.00    -0.06 0.04
# Mod32 3 2152.16    146.18  0.00    -0.07 0.03
# 
# Model-averaged estimate: 0.01 
# Unconditional SE: 0.04 
# 95% Unconditional confidence interval: -0.06, 0.08

# model averaged estimate is strongest for bat presence/absence (-0.28), then 
# for phosphorus (-0.17), close to 0 for all other variables

#### flower colourfulness model averaging ####

# first create all possible formulas 
formulas <- apply(data.frame(expand.grid(c("meanMAT", NA), c("meanMAP", NA),
                                         c("meanAVP", NA), c("meanbirdrich", NA),
                                         c("meanbatpres_bin", NA), 
                                         c("meanmarspres_bin", NA))), 1, 
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
# Mod1  7 492.96      3.49  0.03     0.29 0.25
# Mod3  6 491.11      1.64  0.06     0.28 0.25
# Mod5  6 491.93      2.45  0.04     0.45 0.20
# Mod7  5 490.60      1.12  0.08     0.48 0.19
# Mod9  6 493.63      4.15  0.02     0.35 0.25
# Mod11 5 491.64      2.16  0.05     0.36 0.25
# Mod13 5 493.23      3.75  0.02     0.55 0.20
# Mod15 4 491.30      1.82  0.06     0.56 0.19
# Mod17 6 497.52      8.04  0.00    -0.09 0.19
# Mod19 5 498.10      8.62  0.00    -0.27 0.15
# Mod21 5 499.42      9.95  0.00     0.12 0.15
# Mod23 4 512.28     22.80  0.00    -0.01 0.12
# Mod25 5 509.45     19.97  0.00    -0.17 0.20
# Mod27 4 510.80     21.32  0.00    -0.39 0.16
# Mod29 4 524.25     34.77  0.00     0.23 0.17
# Mod31 3 564.37     74.89  0.00    -0.01 0.12
# Mod33 6 491.47      2.00  0.05     0.19 0.20
# Mod35 5 489.48      0.00  0.15     0.19 0.20
# Mod37 5 492.34      2.86  0.03     0.33 0.18
# Mod39 4 490.47      0.99  0.09     0.36 0.16
# Mod41 5 491.68      2.21  0.05     0.31 0.19
# Mod43 4 489.73      0.26  0.13     0.31 0.18
# Mod45 4 492.47      2.99  0.03     0.44 0.17
# Mod47 3 490.47      0.99  0.09     0.44 0.16
# Mod49 5 495.93      6.45  0.01    -0.18 0.14
# Mod51 4 496.21      6.73  0.01    -0.24 0.13
# Mod53 4 502.09     12.61  0.00    -0.05 0.13
# Mod55 3 511.08     21.61  0.00    -0.05 0.11
# Mod57 4 507.87     18.39  0.00    -0.09 0.15
# Mod59 3 513.13     23.65  0.00    -0.26 0.14
# Mod61 3 528.54     39.06  0.00     0.10 0.14
# Mod63 2 569.43     79.95  0.00     0.06 0.11
# 
# Model-averaged estimate: 0.34 
# Unconditional SE: 0.24 
# 95% Unconditional confidence interval: -0.12, 0.8

AICcmodavg::modavg(cand_mod, parm = "meanMAP", second.ord = FALSE)
# Multimodel inference on "meanMAP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 492.96      2.64  0.05    -0.08 0.22
# Mod2  6 492.32      2.00  0.07    -0.05 0.22
# Mod5  6 491.93      1.60  0.08    -0.16 0.20
# Mod6  5 495.24      4.92  0.02    -0.28 0.19
# Mod9  6 493.63      3.30  0.04     0.03 0.21
# Mod10 5 493.62      3.29  0.04     0.08 0.21
# Mod13 5 493.23      2.90  0.04    -0.05 0.20
# Mod14 4 499.59      9.27  0.00    -0.14 0.19
# Mod17 6 497.52      7.19  0.01    -0.32 0.21
# Mod18 5 495.75      5.43  0.01    -0.38 0.17
# Mod21 5 499.42      9.10  0.00    -0.57 0.17
# Mod22 4 498.02      7.70  0.00    -0.54 0.16
# Mod25 5 509.45     19.12  0.00    -0.38 0.22
# Mod26 4 508.19     17.86  0.00    -0.49 0.18
# Mod29 4 524.25     33.92  0.00    -0.98 0.17
# Mod30 3 524.14     33.81  0.00    -0.94 0.17
# Mod33 6 491.47      1.15  0.10    -0.01 0.19
# Mod34 5 490.32      0.00  0.19    -0.05 0.19
# Mod37 5 492.34      2.01  0.07    -0.07 0.19
# Mod38 4 493.84      3.52  0.03    -0.21 0.17
# Mod41 5 491.68      1.36  0.09     0.04 0.20
# Mod42 4 492.46      2.14  0.06     0.01 0.20
# Mod45 4 492.47      2.15  0.06     0.00 0.19
# Mod46 3 497.70      7.38  0.00    -0.16 0.18
# Mod49 5 495.93      5.60  0.01    -0.25 0.17
# Mod50 4 495.46      5.13  0.01    -0.31 0.16
# Mod53 4 502.09     11.77  0.00    -0.46 0.15
# Mod54 3 500.23      9.91  0.00    -0.46 0.15
# Mod57 4 507.87     17.54  0.00    -0.47 0.18
# Mod58 3 506.20     15.88  0.00    -0.50 0.17
# Mod61 3 528.54     38.21  0.00    -0.89 0.15
# Mod62 2 527.09     36.76  0.00    -0.89 0.16
# 
# Model-averaged estimate: -0.06 
# Unconditional SE: 0.22 
# 95% Unconditional confidence interval: -0.49, 0.37

AICcmodavg::modavg(cand_mod, parm = "meanAVP", second.ord = FALSE)
# Multimodel inference on "meanAVP" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 492.96      4.57  0.02    -0.25 0.26
# Mod2  6 492.32      3.93  0.03    -0.45 0.21
# Mod3  6 491.11      2.72  0.05    -0.29 0.24
# Mod4  5 490.38      1.99  0.08    -0.47 0.19
# Mod9  6 493.63      5.23  0.01    -0.31 0.25
# Mod10 5 493.62      5.22  0.01    -0.55 0.20
# Mod11 5 491.64      3.25  0.04    -0.31 0.24
# Mod12 4 491.75      3.35  0.04    -0.52 0.19
# Mod17 6 497.52      9.12  0.00    -0.48 0.25
# Mod18 5 495.75      7.36  0.01    -0.42 0.21
# Mod19 5 498.10      9.70  0.00    -0.74 0.20
# Mod20 4 499.38     10.98  0.00    -0.61 0.19
# Mod25 5 509.45     21.05  0.00    -0.87 0.23
# Mod26 4 508.19     19.79  0.00    -0.77 0.19
# Mod27 4 510.80     22.41  0.00    -1.15 0.18
# Mod28 3 515.83     27.44  0.00    -1.03 0.17
# Mod33 6 491.47      3.08  0.04    -0.36 0.22
# Mod34 5 490.32      1.93  0.08    -0.45 0.20
# Mod35 5 489.48      1.08  0.12    -0.36 0.22
# Mod36 4 488.39      0.00  0.20    -0.47 0.19
# Mod41 5 491.68      3.29  0.04    -0.35 0.21
# Mod42 4 492.46      4.07  0.03    -0.51 0.20
# Mod43 4 489.73      1.34  0.10    -0.34 0.21
# Mod44 3 490.46      2.07  0.07    -0.51 0.19
# Mod49 5 495.93      7.53  0.00    -0.57 0.21
# Mod50 4 495.46      7.06  0.01    -0.50 0.20
# Mod51 4 496.21      7.82  0.00    -0.72 0.19
# Mod52 3 497.75      9.36  0.00    -0.63 0.19
# Mod57 4 507.87     19.48  0.00    -0.80 0.20
# Mod58 3 506.20     17.81  0.00    -0.76 0.19
# Mod59 3 513.13     24.74  0.00    -1.11 0.17
# Mod60 2 515.16     26.76  0.00    -1.03 0.17
# 
# Model-averaged estimate: -0.42 
# Unconditional SE: 0.22 
# 95% Unconditional confidence interval: -0.85, 0.02

AICcmodavg::modavg(cand_mod, parm = "meanbirdrich", second.ord = FALSE)
# Multimodel inference on "meanbirdrich" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 492.96      4.57  0.02    -0.46 0.29
# Mod2  6 492.32      3.93  0.03    -0.51 0.28
# Mod3  6 491.11      2.72  0.06    -0.43 0.27
# Mod4  5 490.38      1.99  0.08    -0.48 0.26
# Mod5  6 491.93      3.53  0.04    -0.50 0.28
# Mod6  5 495.24      6.85  0.01    -0.65 0.26
# Mod7  5 490.60      2.21  0.08    -0.42 0.26
# Mod8  4 495.52      7.12  0.01    -0.52 0.24
# Mod17 6 497.52      9.12  0.00    -0.86 0.24
# Mod18 5 495.75      7.36  0.01    -0.88 0.23
# Mod19 5 498.10      9.70  0.00    -0.83 0.22
# Mod20 4 499.38     10.98  0.00    -0.94 0.22
# Mod21 5 499.42     11.03  0.00    -1.01 0.21
# Mod22 4 498.02      9.63  0.00    -1.00 0.20
# Mod23 4 512.28     23.88  0.00    -1.10 0.18
# Mod24 3 510.28     21.89  0.00    -1.10 0.18
# Mod33 6 491.47      3.08  0.05    -0.40 0.27
# Mod34 5 490.32      1.93  0.09    -0.50 0.25
# Mod35 5 489.48      1.08  0.13    -0.40 0.26
# Mod36 4 488.39      0.00  0.23    -0.49 0.24
# Mod37 5 492.34      3.94  0.03    -0.37 0.25
# Mod38 4 493.84      5.45  0.01    -0.55 0.23
# Mod39 4 490.47      2.08  0.08    -0.35 0.25
# Mod40 3 493.53      5.14  0.02    -0.51 0.23
# Mod49 5 495.93      7.53  0.01    -0.80 0.21
# Mod50 4 495.46      7.06  0.01    -0.78 0.22
# Mod51 4 496.21      7.82  0.00    -0.86 0.21
# Mod52 3 497.75      9.36  0.00    -0.89 0.21
# Mod53 4 502.09     13.70  0.00    -0.89 0.19
# Mod54 3 500.23     11.84  0.00    -0.88 0.19
# Mod55 3 511.08     22.69  0.00    -1.05 0.17
# Mod56 2 509.31     20.91  0.00    -1.05 0.17
# 
# Model-averaged estimate: -0.47 
# Unconditional SE: 0.27 
# 95% Unconditional confidence interval: -1, 0.07

AICcmodavg::modavg(cand_mod, parm = "meanbatpres_bin", second.ord = FALSE)
# Multimodel inference on "meanbatpres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 492.96      4.57  0.01    -0.77 0.32
# Mod2  6 492.32      3.93  0.02    -0.56 0.25
# Mod3  6 491.11      2.72  0.04    -0.83 0.29
# Mod4  5 490.38      1.99  0.05    -0.60 0.20
# Mod5  6 491.93      3.53  0.03    -0.87 0.31
# Mod6  5 495.24      6.85  0.00    -0.51 0.25
# Mod7  5 490.60      2.21  0.05    -1.04 0.24
# Mod8  4 495.52      7.12  0.00    -0.74 0.20
# Mod9  6 493.63      5.23  0.01    -1.07 0.28
# Mod10 5 493.62      5.22  0.01    -0.85 0.21
# Mod11 5 491.64      3.25  0.03    -1.06 0.25
# Mod12 4 491.75      3.35  0.03    -0.81 0.17
# Mod13 5 493.23      4.83  0.01    -1.24 0.25
# Mod14 4 499.59     11.20  0.00    -0.97 0.20
# Mod15 4 491.30      2.90  0.03    -1.28 0.19
# Mod16 3 498.19      9.80  0.00    -1.06 0.16
# Mod33 6 491.47      3.08  0.03    -0.76 0.31
# Mod34 5 490.32      1.93  0.06    -0.56 0.23
# Mod35 5 489.48      1.08  0.09    -0.77 0.26
# Mod36 4 488.39      0.00  0.15    -0.59 0.19
# Mod37 5 492.34      3.94  0.02    -0.92 0.29
# Mod38 4 493.84      5.45  0.01    -0.59 0.22
# Mod39 4 490.47      2.08  0.05    -0.99 0.23
# Mod40 3 493.53      5.14  0.01    -0.75 0.20
# Mod41 5 491.68      3.29  0.03    -1.05 0.26
# Mod42 4 492.46      4.07  0.02    -0.83 0.21
# Mod43 4 489.73      1.34  0.08    -1.01 0.21
# Mod44 3 490.46      2.07  0.05    -0.82 0.18
# Mod45 4 492.47      4.08  0.02    -1.21 0.23
# Mod46 3 497.70      9.31  0.00    -0.96 0.19
# Mod47 3 490.47      2.08  0.05    -1.21 0.17
# Mod48 2 496.56      8.17  0.00    -1.07 0.15
# 
# Model-averaged estimate: -0.85 
# Unconditional SE: 0.32 
# 95% Unconditional confidence interval: -1.47, -0.23

AICcmodavg::modavg(cand_mod, parm = "meanmarspres_bin", second.ord = FALSE)
# Multimodel inference on "meanmarspres_bin" based on AIC
# 
# AIC table used to obtain model-averaged estimate:
#   
#   K    AIC Delta_AIC AICWt Estimate   SE
# Mod1  7 492.96      2.58  0.04     0.11 0.15
# Mod2  6 492.32      1.94  0.06     0.01 0.13
# Mod3  6 491.11      0.73  0.11     0.08 0.14
# Mod4  5 490.38      0.00  0.16    -0.01 0.11
# Mod5  6 491.93      1.54  0.07     0.19 0.13
# Mod6  5 495.24      4.86  0.01     0.09 0.12
# Mod7  5 490.60      0.21  0.14     0.16 0.12
# Mod8  4 495.52      5.13  0.01     0.01 0.11
# Mod9  6 493.63      3.24  0.03     0.04 0.15
# Mod10 5 493.62      3.23  0.03    -0.10 0.11
# Mod11 5 491.64      1.25  0.08     0.04 0.14
# Mod12 4 491.75      1.36  0.08    -0.09 0.10
# Mod13 5 493.23      2.84  0.04     0.14 0.12
# Mod14 4 499.59      9.21  0.00    -0.04 0.11
# Mod15 4 491.30      0.91  0.10     0.13 0.12
# Mod16 3 498.19      7.81  0.00    -0.06 0.10
# Mod17 6 497.52      7.13  0.00     0.10 0.15
# Mod18 5 495.75      5.37  0.01     0.14 0.11
# Mod19 5 498.10      7.71  0.00    -0.04 0.12
# Mod20 4 499.38      8.99  0.00     0.06 0.11
# Mod21 5 499.42      9.04  0.00     0.26 0.12
# Mod22 4 498.02      7.64  0.00     0.22 0.11
# Mod23 4 512.28     21.89  0.00     0.10 0.11
# Mod24 3 510.28     19.89  0.00     0.10 0.10
# Mod25 5 509.45     19.06  0.00    -0.09 0.14
# Mod26 4 508.19     17.80  0.00    -0.01 0.10
# Mod27 4 510.80     20.42  0.00    -0.24 0.11
# Mod28 3 515.83     25.45  0.00    -0.12 0.10
# Mod29 4 524.25     33.86  0.00     0.16 0.12
# Mod30 3 524.14     33.75  0.00     0.07 0.10
# Mod31 3 564.37     73.98  0.00    -0.18 0.11
# Mod32 2 562.37     71.98  0.00    -0.18 0.10
# 
# Model-averaged estimate: 0.07 
# Unconditional SE: 0.15 
# 95% Unconditional confidence interval: -0.23, 0.36

# model averaged estimate is strongest for bat presence/absence (-0.85), then 
# bird richness (-0.47), then phosphorus (-0.42), then MAT (0.34), 
# then gliders (0.07) then MAP (-0.06)

rm(cand_mod, formulas, paste0.na.omit, i, eucvarscaled)

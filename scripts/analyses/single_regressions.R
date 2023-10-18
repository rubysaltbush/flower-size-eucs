# phylogenetic and least squares regressions to support multiple regressions
# and produce scatter plots for panelling in figures

# bud size and median longitude ----

#* prepare data and tree ----

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, medianlong) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(medianlong)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
tree_budsz <- ape::drop.tip(treeML1, treeML1$tip.label[-match(pgls_data$tree_names, treeML1$tip.label)])
length(tree_budsz$tip.label) # 675 tips remain

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

#* run PGLS ----
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ medianlong, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ medianlong 
# Data: pgls_data 
# AIC      BIC    logLik
# 2181.178 2194.722 -1087.589
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#              Value    Std.Error  t-value   p-value
# (Intercept)  5.578457 2.8090386  1.985895  0.0475
# medianlong  -0.014449 0.0053874 -2.682039  0.0075
# 
# Correlation: 
#   (Intr)
# medianlong -0.26 
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.303111378 -0.069072506  0.006017333  0.084652998  0.542929495 
# 
# Residual standard error: 7.164214 
# Degrees of freedom: 675 total; 673 residual
plot(logbudsize_mm2 ~ medianlong, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, P = 0.008

#* figure ----
# scatter plot with points coloured by flower colour
ggplot(euc_traits_nosubsp, aes(x = medianlong, y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_abline(intercept = coef(pglsModel)[1], slope = coef(pglsModel)[2], colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#DFBF5B", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species median longitude") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = "PGLS p = 0.008")
ggsave("figures/regressions/bud size by median longitude PGLS.pdf", width = 8, height = 5)

# bud size and flower colour ----

#* prepare data and tree ----

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, colour_binary) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 58 tips
tree_budsz <- ape::drop.tip(treeML1, treeML1$tip.label[-match(pgls_data$tree_names, treeML1$tip.label)])
length(tree_budsz$tip.label) # 673 tips remain

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

#* run PGLS ----
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ colour_binary, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ colour_binary 
# Data: pgls_data 
# AIC      BIC   logLik
# 2108.96 2127.007 -1050.48
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#                  Value    Std.Error  t-value  p-value
# (Intercept)      3.552087 2.5854335 1.373885  0.1699
# colour_binary0.5 0.669066 0.1310472 5.105531  0.0000
# colour_binary1   0.764355 0.0967291 7.902017  0.0000
# 
# Correlation: 
#   (Intr) cl_0.5
# colour_binary0.5 -0.001       
# colour_binary1   -0.003  0.183
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -0.32686568 -0.06756832  0.01349127  0.08720121  0.47904527 
# 
# Residual standard error: 6.823389 
# Degrees of freedom: 673 total; 670 residual
plot(logbudsize_mm2 ~ colour_binary, data = pgls_data)
# significant differences between 3 groups, p < 0.001

#* figure ----
# boxplot figure to show bud size vs flower colourfulness
euc_traits_nosubsp %>%
  dplyr::filter(!is.na(colour_binary)) %>%
  ggplot(aes(x = colour_binary, y = logbudsize_mm2)) +
    geom_boxplot(aes(fill = colour_binary)) +
    geom_jitter(color = "black", size = 0.4, alpha = 0.3) +
    theme_pubr(legend = "none") +
    scale_fill_manual(values = c("#faebcd", "light pink", "red")) +
    scale_x_discrete(labels = c("white-cream", "mixed", "colourful")) +
    xlab("Flower colourfulness") +
    ylab("Eucalypt bud size (log mm²)") +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14))
ggsave("figures/regressions/bud size by flower colourfulness.pdf", width = 8, height = 5)

rm(pglsModel, pgls_data, tree_budsz, spp)

# regression plots, leaf area and bud size ----

#* run regressions ----

# source linear regression function to do all the things
source("scripts/functions/do_regression.R")

#make list of all variables to test in regressions and their plot labels
regressions_todo <- list(
  # bud v available phosphorus
  lmbudszavp = list(
    xdata = euc_traits_nosubsp$meanAVP,
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species mean available phosphorus (mg/kg 0-30cm depth)",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs AVP"
  ),
  # leaf v available phosphorus
  lmlfszavp = list(
    xdata = euc_traits_nosubsp$meanAVP,
    ydata = log(euc_traits_nosubsp$leafarea_mm2),
    xlabel = "Species mean available phosphorus (mg/kg 0-30cm depth)",
    ylabel = "Eucalypt leaf area (log mm²)",
    output_path = "figures/regressions/residuals/leafsize vs AVP"
  ),
  # bud v temperature
  lmbudszmat = list(
    xdata = euc_traits_nosubsp$meanMAT,
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species mean annual temperature (ºC)",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs MAT"
  ),
  # leaf v temperature
  lmlfszmat = list(
    xdata = euc_traits_nosubsp$meanMAT,
    ydata = log(euc_traits_nosubsp$leafarea_mm2),
    xlabel = "Species mean annual temperature (ºC)",
    ylabel = "Eucalypt leaf area (log mm²)",
    output_path = "figures/regressions/residuals/leafsize vs MAT"
  ),
  # bud v precipitation
  lmbudszmap = list(
    xdata = log(euc_traits_nosubsp$meanMAP),
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species mean annual precipitation (log mm)",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs MAP"
  ),
  # leaf v precipitation
  lmlfszmap = list(
    xdata = log(euc_traits_nosubsp$meanMAP),
    ydata = log(euc_traits_nosubsp$leafarea_mm2),
    xlabel = "Species mean annual precipitation (log mm)",
    ylabel = "Eucalypt leaf area (log mm²)",
    output_path = "figures/regressions/residuals/leafsize vs MAP"
  ),
  # bud v flower-visiting bat species richness
  lmbudszbat = list(
    xdata = euc_traits_nosubsp$meanbatpres_bin,
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species flower-visiting bat presence",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs bats"
  )
)

#create list to store regression output
regressions <- list()
#create data frame for regression results table
regresults <- data.frame()

#loop over variables, do linear regression and output graphs and results
for (regression_name in names(regressions_todo)) {
  todo <- regressions_todo[[regression_name]]
  regressions[[regression_name]] <- do_regression(
    xdata = todo$xdata,
    ydata = todo$ydata,
    xlabel = todo$xlabel,
    ylabel = todo$ylabel,
    output_path = todo$output_path
  )
  new_row <- broom::glance(regressions[[regression_name]])
  new_row$slope <- regressions[[regression_name]]$coefficients[[2]]
  new_row$regression_name <- regression_name
  regresults <- rbind(regresults, new_row)
}

write_csv(regresults, "results/single_regression_results.csv")
rm(regression_name, todo, regressions_todo, new_row, regresults, do_regression)

# bud size and MAT
ggplot(euc_traits_nosubsp, aes(x = meanMAT, y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean annual temperature (ºC)") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmbudszmat)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmbudszmat)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/budsize vs MAT.pdf", width = 8, height = 5)

# leaf size and MAT
ggplot(euc_traits_nosubsp, aes(x = meanMAT, y = log(leafarea_mm2))) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_smooth(method = "lm", colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean annual temperature (ºC)") +
  ylab("Eucalypt leaf area (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmlfszmat)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmlfszmat)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/leafsize vs MAT.pdf", width = 8, height = 5)

# bud size and MAP
ggplot(euc_traits_nosubsp, aes(x = log(meanMAP), y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_smooth(method = "lm", colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean annual precipitation (log mm)") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmbudszmap)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmbudszmap)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/budsize vs MAP.pdf", width = 8, height = 5)

# leaf size and MAP
ggplot(euc_traits_nosubsp, aes(x = log(meanMAP), y = log(leafarea_mm2))) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_smooth(method = "lm", colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean annual precipitation (log mm)") +
  ylab("Eucalypt leaf area (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmlfszmap)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmlfszmap)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/leafsize vs MAP.pdf", width = 8, height = 5)

# bud size and AVP
ggplot(euc_traits_nosubsp, aes(x = log(meanAVP), y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_smooth(method = "lm", colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean available phosphorus (log mg/kg)") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmbudszavp)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmbudszavp)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/budsize vs AVP.pdf", width = 8, height = 5)

# leaf size and AVP
ggplot(euc_traits_nosubsp, aes(x = log(meanAVP), y = log(leafarea_mm2))) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_smooth(method = "lm", colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean available phosphorus (log mg/kg)") +
  ylab("Eucalypt leaf area (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmlfszavp)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmlfszavp)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/leafsize vs AVP.pdf", width = 8, height = 5)

# bud size and bat richness
# not sure if this better as boxplot or scatter plot with logistic regression line?
# boxplot for now
euc_traits_nosubsp %>%
  dplyr::filter(!is.na(meanbatpres_bin)) %>%
  dplyr::mutate(meanbatpres_bin = factor(meanbatpres_bin)) %>%
ggplot(aes(x = meanbatpres_bin, y = logbudsize_mm2)) +
  geom_boxplot(aes(fill = meanbatpres_bin)) +
  #geom_smooth(method = "lm", colour = "black") + # fix to logistic regression method???
  theme_pubr(legend = "none") + # or display as boxplot???
  geom_jitter(color = "black", size = 0.4, alpha = 0.3) +
  scale_fill_viridis_d(direction = -1, alpha = 0.9) +
  #scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_x_discrete(labels = c("bats absent", "bats present")) +
  xlab("Species mean flower-visiting bat presence") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmbudszbat)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmbudszbat)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/budsize vs bat presence.pdf", width = 8, height = 5)




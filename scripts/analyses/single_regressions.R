# initial least squares regressions of potential predictor variables
# to support multiple regressions and produce scatter plot figures

# run lm ----

# source linear regression function to do all the things
source("scripts/functions/do_regression.R")

#make list of all variables to test in regressions and their plot labels
regressions_todo <- list(
  # bud v available phosphorus
  lmbudszavp = list(
    xdata = log(euc_traits_nosubsp$meanAVP), # log transform to improve residuals
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species mean available phosphorus (log mg/kg 0-30cm depth)",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs AVP"
  ),
  # bud v temperature
  lmbudszmat = list(
    xdata = euc_traits_nosubsp$meanMAT,
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species mean annual temperature (ºC)",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs MAT"
  ),
  # bud v precipitation
  lmbudszmap = list(
    xdata = log(euc_traits_nosubsp$meanMAP),
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species mean annual precipitation (log mm)",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs MAP"
  ),
  # bud v flower-visiting bat presence
  lmbudszbat = list(
    xdata = euc_traits_nosubsp$meanbatpres_bin,
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species flower-visiting bat presence",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs bats"
  ),
  # bud v flower-visiting bird species richness
  lmbudszbird = list(
    xdata = euc_traits_nosubsp$meanbirdrich,
    ydata = euc_traits_nosubsp$logbudsize_mm2,
    xlabel = "Species flower-visiting bird species richness",
    ylabel = "Eucalypt bud size (log mm²)",
    output_path = "figures/regressions/residuals/budsize vs birds"
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

# figures ----

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

# bud size and bird species richness
ggplot(euc_traits_nosubsp, aes(x = meanbirdrich, y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_smooth(method = "lm", colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Species mean flower-visiting bird species richness") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmbudszbird)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmbudszbird)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/budsize vs birdrich.pdf", width = 8, height = 5)

# bud size and bat richness boxplot
euc_traits_nosubsp %>%
  dplyr::filter(!is.na(meanbatpres_bin)) %>%
  dplyr::mutate(meanbatpres_bin = factor(meanbatpres_bin)) %>%
ggplot(aes(x = meanbatpres_bin, y = logbudsize_mm2)) +
  geom_boxplot() +
  theme_pubr(legend = "right") + 
  geom_jitter(aes(colour = colour_binary), size = 0.8, alpha = 0.9) +
  scale_color_manual(values = c("#F0E4BE", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_x_discrete(labels = c("bats absent", "bats present")) +
  xlab("") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(regressions$lmbudszbat)$r.squared, 2),
                     "    P = ", format.pval(summary(regressions$lmbudszbat)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/budsize vs bat presence boxplot.pdf", width = 8, height = 6)

rm(regressions)

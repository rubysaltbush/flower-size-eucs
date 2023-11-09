# phylogenetic regressions to test evolutionary relationships between some traits

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
# AIC     BIC    logLik
# 2180.466 2194.01 -1087.233
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept)  5.686459 2.8088307  2.024493  0.0433
# medianlong  -0.015249 0.0054214 -2.812656  0.0051
# 
# Correlation: 
#   (Intr)
# medianlong -0.262
# 
# Standardized residuals:
#   Min           Q1          Med           Q3          Max 
# -0.302449572 -0.069079320  0.006085422  0.083490638  0.541317026 
# 
# Residual standard error: 7.160438 
# Degrees of freedom: 675 total; 673 residual

plot(logbudsize_mm2 ~ medianlong, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, P = 0.005

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
  labs(title = "PGLS p = 0.005")
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

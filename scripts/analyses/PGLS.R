# phylogenetic regressions to test (evolutionary) relationships between some traits

# list to store all models
pgls_models <- list()

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
pgls_models$medlong <- nlme::gls(logbudsize_mm2 ~ medianlong, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pgls_models$medlong)
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
abline(a = coef(pgls_models$medlong)[1], b = coef(pgls_models$medlong)[2])
# significant, P = 0.005

# calculate pseudo-R2, R2-pred from Ives (2019)
rr2::R2_pred(mod = pgls_models$medlong, phy = tree_budsz)
# 0.3337541

#* figure ----
# scatter plot with points coloured by flower colour
ggplot(euc_traits_nosubsp, aes(x = medianlong, y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_abline(intercept = coef(pgls_models$medlong)[1], 
              slope = coef(pgls_models$medlong)[2], colour = "black") +
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
pgls_models$colbin <- nlme::gls(logbudsize_mm2 ~ colour_binary,
                                correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                                data = pgls_data, method = "ML")
summary(pgls_models$colbin)
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
abline(a = coef(pgls_models$colbin)[1], b = coef(pgls_models$colbin)[2])
# significant differences between 3 groups, p < 0.001

# calculate pseudo-R2, R2-pred from Ives (2019)
rr2::R2_pred(mod = pgls_models$colbin, phy = tree_budsz)
# 0.4339345

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

# bud size and fruit size ----

#* prepare data and tree ----

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, frtsize_mm2) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# join colour on for plotting purposes
colour <- dplyr::select(euc_traits_nosubsp, tree_names, colour_binary)
pgls_data <- pgls_data %>%
  dplyr::left_join(colour, by = "tree_names")
rm(colour)

# drop outgroups and missing data tips from tree
tree_budsz <- ape::drop.tip(treeML1, treeML1$tip.label[-match(pgls_data$tree_names, treeML1$tip.label)])
length(tree_budsz$tip.label) # 678 tips remain

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

#* run PGLS models ----
spp <- rownames(pgls_data) # set species names as reference point

#** fruit size ----
pgls_models$frtsize <- nlme::gls(logbudsize_mm2 ~ log(frtsize_mm2), 
                                 correlation = ape::corBrownian(phy = tree_budsz, 
                                                                form = ~spp),
                                 data = pgls_data, method = "ML")
summary(pgls_models$frtsize)
# Generalized least squares fit by maximum likelihood
# Model: logbudsize_mm2 ~ log(frtsize_mm2) 
# Data: pgls_data 
# AIC     BIC    logLik
# 1263.053 1276.61 -628.5263
# 
# Correlation Structure: corBrownian
# Formula: ~spp 
# Parameter estimate(s):
#   numeric(0)
# 
# Coefficients:
#   Value Std.Error  t-value p-value
# (Intercept)      -0.4660002 1.3756448 -0.33875  0.7349
# log(frtsize_mm2)  0.8728617 0.0196537 44.41203  0.0000
# 
# Correlation: 
#   (Intr)
# log(frtsize_mm2) -0.067
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -0.50176592  0.04764318  0.18489750  0.28123935  0.62643000 
# 
# Residual standard error: 3.625188 
# Degrees of freedom: 678 total; 676 residual

# calculate pseudo-R2, R2-pred from Ives (2019)
rr2::R2_pred(mod = pgls_models$frtsize, phy = tree_budsz)
# 0.8528477

# scatter plot with PGLS line
ggplot(pgls_data, aes(x = log(frtsize_mm2), y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_abline(intercept = coef(pgls_models$frtsize)[1], 
              slope = coef(pgls_models$frtsize)[2], colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#faebcd", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  scale_color_manual(values = c("#DFBF5B", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
  xlab("Eucalypt fruit size (log mm²)") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = "PGLS p < 0.001, R² = 0.85")
ggsave("figures/regressions/bud size by fruit size PGLS.pdf", width = 8, height = 5)

#### conclusions ####

# 1) buds/flowers larger in more westerly eucalypts

# 2) bud size and flower colour evolutionarily correlated, eucalypts with 
#    larger buds tend to have more colourful flowers

# 3) fruit size is strong correlate of bud size, which makes sense given
#    buds become the flowers which become the fruit

rm(spp, pgls_models, pgls_data, tree_budsz)

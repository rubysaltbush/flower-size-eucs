# phylogenetic and least squares regressions to support multiple regressions
# and produce scatter plots for panelling in figures

# unfinished??

# bud size and median longitude ----

## prepare data and tree ##

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

## run PGLS ##
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
#   Value Std.Error   t-value p-value
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

## pretty plots ##
# try plotting scatter plot with points coloured by flower colour
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
ggsave("figures/regressions/bud size by median longitude PGLS.pdf", width = 10, height = 5)

# bud size and flower colour ----

## prepare data and tree ##

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

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ colour_binary, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML") # why ML method? not REML (default)?
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
#   Value Std.Error  t-value p-value
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

# figure here
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
ggsave("figures/regressions/bud size by flower colourfulness.pdf", width = 10, height = 5)

# bud size and species mean available P ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, meanAVP) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(meanAVP)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(meanAVP))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ meanAVP, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
plot(logbudsize_mm2 ~ meanAVP, data = pgls_data)
# NOT significant, P = 0.18

# bud size and species mean bat presence ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, meanbatpres_bin) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(meanbatpres_bin)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(meanbatpres_bin))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ meanbatpres_bin, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
plot(logbudsize_mm2 ~ meanbatpres_bin, data = pgls_data)
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2]) not appropriate for binary...
# significant, P = 0.01

# interesting again! seems bat richness more evolutionarily important than soil P

# flower colour and species mean bat presence ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, colour_fullbinary, meanbatpres_bin) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(colour_fullbinary) & !is.na(meanbatpres_bin)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 57 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(colour_fullbinary)|is.na(meanbatpres_bin))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 674 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(colour_fullbinary ~ meanbatpres_bin, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
plot(colour_fullbinary ~ meanbatpres_bin, data = pgls_data) # agh two binary variables, need better way to visualise
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2]) not appropriate for binary...
# significant, P = 0.003

rm(pglsModel, pgls_data, tree_budsz, spp)

# predictions:  - larger flowers *evolved* more in western than eastern Aus eucs
#               - larger, colourful flowers evolved in correlation
#               - 

# for final analysis can I write function to run and check assumptions for all 
# these PGLS regressions?

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
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(medianlong))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ medianlong, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
plot(logbudsize_mm2 ~ medianlong, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, P = 0.008

## pretty plots ##
# try plotting scatter plot with points coloured by flower colour
ggplot(euc_traits_nosubsp, aes(x = medianlong, y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary, fill = colour_binary), size = 3, shape = 21) +
  geom_abline(intercept = coef(pglsModel)[1], slope = coef(pglsModel)[2], colour = "black") +
  theme_pubr(legend = "right") +
  scale_fill_manual(values = c("#FFFDD0", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "colourful", "NA")) +
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
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(colour_binary)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 58 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(colour_binary))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 673 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ colour_binary, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML") # why ML method? not REML (default)?
summary(pglsModel)
plot(logbudsize_mm2 ~ colour_binary, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant differences between 3 groups, p < 0.001

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

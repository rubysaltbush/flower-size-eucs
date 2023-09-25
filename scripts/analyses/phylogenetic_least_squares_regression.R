# predictions:  - larger flowers *evolved* more in western than eastern Aus eucs
#               - larger, colourful flowers evolved in correlation
#               - larger, few per umbel flowers evolved in correlation

# AND given results of multiple regression
#               - larger, colourful eucalypt flowers evolved in taxa with no 
#                 bats in their environment

# as tree has to match data will have to run several separate analyses
# ultimately might make function to do these but for now just copy-paste

# bud size and median longitude ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, median_long) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(median_long)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(median_long))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ median_long, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ median_long, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, P = 0.008

## pretty plots ##
# try plotting scatter plot with points coloured by flower colour
ggplot(euc_traits_nosubsp, aes(x = median_long, y = logbudsize_mm2)) +
  geom_point(aes(colour = colour_binary), size = 3) +
  geom_abline(intercept = coef(pglsModel)[1], slope = coef(pglsModel)[2], colour = "black") +
  theme_pubr(legend = "right") +
  scale_color_manual(values = c("light grey", "light pink", "red", "black"), name = "Flower colour", labels = c("white-cream", "mixed", "coloured", "NA")) +
  xlab("Species median longitude") +
  ylab("Eucalypt bud size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("PGLS P = 0.008")) # can't seem to extract p-value from model automatically :/
ggsave("figures/regressions/budsize vs median long PGLS.pdf", width = 10, height = 5)

# bud size and plant max height ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, max_height_m) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(max_height_m)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 51 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(max_height_m))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 680 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ log(max_height_m), 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ log(max_height_m), data = pgls_data)
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# not significant, p = 0.18 (no log) or p = 0.06 (logged)

# bud size and range size (eoo) ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, eoo) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(eoo)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(eoo))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ eoo, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ eoo, data = pgls_data)
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# not significant in this as in non-phylogenetic regression, p = 0.43

# bud size and range size (aoo) ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, aoo) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(aoo)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(aoo))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ log(aoo), 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ log(aoo), data = pgls_data)
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# not significant in this unlike non-phylogenetic regression, p = 0.12
# with or without log transformation

# bud size and buds per umbel ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, bud_n_mean) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(bud_n_mean)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 51 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(bud_n_mean))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 680 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ bud_n_mean, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ bud_n_mean, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, p < 0.001

# bud size and fruit size ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, logfrtsize_mm2) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(logfrtsize_mm2)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 53 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(logfrtsize_mm2))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 678 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ logfrtsize_mm2, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ logfrtsize_mm2, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, p < 0.001

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
coef(pglsModel)
plot(logbudsize_mm2 ~ colour_binary, data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant differences between 3 groups, p < 0.001

# longitude and flower colour ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, median_long, colour_binary) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(median_long) & !is.na(colour_binary)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 57 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(median_long)|is.na(colour_binary))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 674 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(median_long ~ colour_binary, 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML") # why ML method? not REML (default)?
summary(pglsModel)
coef(pglsModel)
plot(median_long ~ colour_binary, data = pgls_data)
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# not significant? not totally sure if interpreting right tho

# inflorescence size and leaf area ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, umbelsz_mm2, leafarea_mm2) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(umbelsz_mm2) & !is.na(leafarea_mm2)) %>%
  as.data.frame() #673 obs

# drop outgroups and missing data tips from tree
# lose 51 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(umbelsz_mm2)|is.na(leafarea_mm2))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 680 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(log(umbelsz_mm2) ~ log(leafarea_mm2), 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(log(umbelsz_mm2) ~ log(leafarea_mm2), data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, p < 0.001

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
pglsModel <- nlme::gls(logbudsize_mm2 ~ log(meanAVP), 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ log(meanAVP), data = pgls_data)
#abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# NOT significant, P = 0.12

# interesting! does imply that soil P may just be a spurious correlation, 
# rather than a driving pattern. Curious that med longitude significant
# when this isn't

# bud size and species mean bat richness ----

## prepare data and tree ##

# subset data to variables of interest for phylogenetic least squares regression
# remove data with no match in tree
pgls_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2, meanbatrich) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2) & !is.na(meanbatrich)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 56 tips
to_drop <- as.data.frame(treeML1$tip.label) %>%
  dplyr::rename(tree_names = `treeML1$tip.label`) %>%
  dplyr::left_join(pgls_data, by = "tree_names") %>%
  dplyr::filter(is.na(logbudsize_mm2)|is.na(meanbatrich))
tree_budsz <- ape::drop.tip(treeML1, to_drop$tree_names)
length(tree_budsz$tip.label) # 675 tips remain
rm(to_drop)

rownames(pgls_data) <- pgls_data[,1]
pgls_data[,1] <- NULL

## run PGLS ##
spp <- rownames(pgls_data) # set species names as reference point
pglsModel <- nlme::gls(logbudsize_mm2 ~ log1p(meanbatrich), 
                       correlation = ape::corBrownian(phy = tree_budsz, 
                                                      form = ~spp),
                       data = pgls_data, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(logbudsize_mm2 ~ log1p(meanbatrich), data = pgls_data)
abline(a = coef(pglsModel)[1], b = coef(pglsModel)[2])
# significant, P < 0.001

# interesting again! seems bat richness mroe evolutionarily important than soil P

rm(pglsModel, pgls_data, tree_budsz, spp)
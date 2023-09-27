# script to simulate evolution of eucalypt bud size on phylogeny and 
# render this in figure with flower colour labelled at tips adn clades (and clade
# distributions?) labelled

# produces circular phylogeny (Figure 1b)

#### prep tree and data ####
# remove taxa with no match in tree or bud size data
contMap_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, budsize_mm2, logbudsize_mm2, medianlong, colour_binary) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 51 tips
tree_budsz <- ape::drop.tip(treeML1, treeML1$tip.label[-match(contMap_data$tree_names, treeML1$tip.label)])
length(tree_budsz$tip.label) # 680 tips remain

# change data into a named vector that matches tree
budsz_contdata <- contMap_data$logbudsize_mm2
names(budsz_contdata) <- contMap_data$tree_names

# #### model evolution of bud size??? ####
# 
# # estimate ancestral states w. variances & 95% confidence intervals for each node
# fit <- phytools::fastAnc(tree_budsz, budsz_contdata, vars = TRUE, CI = TRUE)
# # results show estimated ancestral log_budsize_mm2 for each node in tree, plus var and CI
# # build results into data.frame and export
# logbudsize_fastAnc <- data.frame(names(fit$ace), fit$ace, fit$var, fit$CI95[,1], fit$CI95[,2])
# names(logbudsize_fastAnc) <- c("tip_n", "anc_logbudszmm2", "var", "CI95_lower", "CI95_upper")
# #readr::write_csv(logbudsize_fastAnc, "results/fastAnc_tree_logbudsize_mm2.csv")
# rm(logbudsize_fastAnc, fit)

#### visualise phylogeny ####

# visualise evolution of euc bud size (log mm2) on phylogeny using contmap
treecont <- phytools::contMap(tree_budsz, budsz_contdata, plot = FALSE)
treecont
# Object of class "contMap" containing:
# (1) A phylogenetic tree with 680 tips and 518 internal nodes.
# (2) A mapped continuous trait on the range (1.321756, 7.585154).

# re-colour contmap with continous bud size colour scale
treecont <- phytools::setMap(treecont, my_colours$budsz)

# export basic contMap with species labels
pdf(file = "figures/logbudsizemm2_full_phylogeny.pdf", width = 12, height = 50)
plot(treecont, legend = 0.7*max(nodeHeights(tree_budsz)), sig = 1, 
     fsize = c(0.5, 0.7), lwd = 2, outline = FALSE, leg.txt = "Bud size (log mm²)")
dev.off()

# plot contmap with data on flower colour displayed at tips with phytools
# workaround from http://blog.phytools.org/2022/06/how-to-plot-tip-node-labels-without.html

pdf(file = "figures/phylogeny_logbudsize.pdf", width = 10, height = 10)
# plot contMap
phytools::plot.contMap(treecont, type = "fan", legend = FALSE,
                       ftype = "off", lwd = 3, outline = FALSE,
                       xlim = c(-100, 70))

# first label beginning of Neogene boundary at 23 mya
plotrix::draw.circle(0, 0, radius = max(nodeHeights(tree_budsz)) - 23, 
                     col = "#f8f6f7", lty = 0)

# then label eucalyptae crown age at 52 mya
plotrix::draw.circle(0, 0, radius = max(nodeHeights(tree_budsz)) - 52, 
                     col = "#dadada", lty = 0)

# then add text to boundary points
text(x = max(nodeHeights(tree_budsz)) - 23, y = -6, "23 Ma", cex = 1, col = "#636363")
text(x = max(nodeHeights(tree_budsz)) - 52, y = -6, "52 Ma", cex = 1, col = "#636363")

# plot contMap again
par(new = TRUE) # hack to force below to plot on top of above 
phytools::plot.contMap(treecont, type = "fan", legend = FALSE,
                       ftype = "off", lwd = 2, outline = FALSE,
                       xlim = c(-100, 70))

# below adapted from http://blog.phytools.org/2016/08/vertical-legend-in-contmap-style-plots.html
# add bud size legend using phytools function
phytools::add.color.bar(leg = 100,
                        cols = treecont$cols,
                        title = "Eucalypt bud size (log mm²)",
                        lims = NULL,
                        digits = 2,
                        prompt = FALSE,
                        lwd = 12,
                        direction = "upwards",
                        subtitle = "",
                        x = -90,
                        y = -50)

# then add custom tick marks
lines(x = rep(-87.525, 2), y = c(-50, 52)) # draw vertical line
Y <- cbind(seq(-50, 52, length.out = 5), # define x pos for ticks
         seq(-50, 52, length.out = 5))
X <- cbind(rep(-87.525, 5), # define y pos for ticks
         rep(-85.925, 5))
for(i in 1:nrow(Y)) lines(X[i,], Y[i,]) # draw ticks
ticks <- seq(treecont$lims[1], treecont$lims[2], length.out = 5) # get tick values
text(x = X[,2], y = Y[,2], round(ticks, 1), pos = 4, cex = 0.8) # draw tick values
rm(X, Y, i, ticks)

# points coloured by flower colour
# try using tip.labels rather than below
# get tree tip numbers and match data to these for tip plotting
tipn_treebudsz <- as.data.frame(treecont$tree$tip.label)
names(tipn_treebudsz) <- "tree_names"
tipn_treebudsz$tipn <- rownames(tipn_treebudsz)
contMap_data <- tipn_treebudsz %>%
  dplyr::left_join(contMap_data, by = "tree_names")
rm(tipn_treebudsz)
# first prep tip labels for polymorphic tips
coltips <- contMap_data %>%
  dplyr::select(colour_binary, tipn) %>%
  tibble::remove_rownames() %>%
  tibble::column_to_rownames(var = "tipn") %>% # rename rows with tip numbers
  dplyr::select(colour_binary) %>%
  dplyr::filter(!is.na(colour_binary)) %>% # filter out taxa with missing tip data
  as.matrix()
cols <- my_colours$flcol_fill[1:3]
names(cols) <- c(0, 0.5, 1)
ape::tiplabels(tip = as.numeric(rownames(coltips)), 
               pie = as.numeric(coltips),
               piecol = my_colours$flcol_fill, 
               cex = 0.15)

# # assign plot to object
# pp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
# # from pp object will pull out x and y coordinates to plot points
# # set colours and names for flower colour
# fcols <- my_colours$flcol_fill
# names(fcols) <- c("0", "0.5", "1")
# # change flower colour data into a named vector that matches tree and colour names
# fcol_data <- contMap_data$colour_binary
# names(fcol_data) <- contMap_data$tree_names
# # add flower colour points
# points(pp$xx[1:ape::Ntip(tree_budsz)],
#        pp$yy[1:ape::Ntip(tree_budsz)],
#        pch = 15, cex = 0.5,
#        col = fcols[fcol_data[tree_budsz$tip.label]])

# legends
legend(x = "topright", legend = c("white-cream", "mixed", "colourful"), col = cols, 
       bty = "n", cex = 0.8, title = "Flower colour", pch = 15)

dev.off()
rm(pp, budsz_contdata, fcol_data, fcols, tree_budsz, treecont, contMap_data, cols, coltips)
# to do - offset tip labels from circle - will need to rehash Greg's calculations and make a function
#       - clade labels?
#       - label western/eastern Australia clades?


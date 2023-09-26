# script to simulate evolution of eucalypt bud size on phylogeny and 
# render this in figure with flower colour and median longitude matched
# to species

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

# get tip numbers of this tree and match data to these
tipn_treebudsz <- as.data.frame(tree_budsz$tip.label)
names(tipn_treebudsz) <- "tree_names"
tipn_treebudsz$tipn <- rownames(tipn_treebudsz)
contMap_data <- tipn_treebudsz %>%
  dplyr::left_join(contMap_data, by = "tree_names")
rm(tipn_treebudsz)

# change data into a named vector that matches tree
budsz_contdata <- contMap_data$logbudsize_mm2
names(budsz_contdata) <- contMap_data$tree_names

#### model evolution of bud size??? ####

# estimate ancestral states w. variances & 95% confidence intervals for each node
fit <- phytools::fastAnc(tree_budsz, budsz_contdata, vars = TRUE, CI = TRUE)
# results show estimated ancestral log_budsize_mm2 for each node in tree, plus var and CI
# build results into data.frame and export
logbudsize_fastAnc <- data.frame(names(fit$ace), fit$ace, fit$var, fit$CI95[,1], fit$CI95[,2])
names(logbudsize_fastAnc) <- c("tip_n", "anc_logbudszmm2", "var", "CI95_lower", "CI95_upper")
#readr::write_csv(logbudsize_fastAnc, "results/fastAnc_tree_logbudsize_mm2.csv")
rm(logbudsize_fastAnc, fit)

#### visualise phylogeny ####

# visualise evolution of euc bud size (log mm2) on phylogeny using contmap
treecont <- phytools::contMap(tree_budsz, budsz_contdata, plot = FALSE)
treecont
# Object of class "contMap" containing:
# (1) A phylogenetic tree with 680 tips and 518 internal nodes.
# (2) A mapped continuous trait on the range (1.321756, 7.585154).
# export basic contMap with species labels
# first re-colour contmap with continous white to blue scale
treecont <- phytools::setMap(treecont, my_colours$budsz)
#pdf(file = "figures/logbudsize_mm2_contmap.pdf", width = 12, height = 50)
plot(treecont, legend = 0.7*max(nodeHeights(tree_budsz)), sig = 2, 
     fsize = c(0.5, 0.7), lwd = 2, outline = FALSE, leg.txt = "Bud size (log mm²)")
#dev.off()

# plot contmap with data on bud size, flower colour and median longitude displayed at tips
# try phytools workaround from http://blog.phytools.org/2022/06/how-to-plot-tip-node-labels-without.html

pdf(file = "figures/logbudsize_mm2_contmap_fcol.pdf", width = 6, height = 12)
# plot contMap
plot.contMap(treecont, legend = 0.5*max(phytools::nodeHeights(tree_budsz)), sig = 1, 
             ftype = "off", lwd = 2, outline = FALSE, mar = c(5, 0.2, 0.2, 2),
             leg.txt = "Bud size (log mm²)")
# draw time axis on left hand side
axis(1, at = c(60, 40, 20, 0), labels = c(0, 20, 40, 60))
title(xlab = "time (Ma)")
# assign plot to object
pp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
# from pp object pull out x and y coordinates to plot points
# # add bud size points! with viridis colour scale! BELOW NOT WORKING BECAUSE CAN'T MAP COLOURS IN SAME WAY AS ON TREE GAH
# # now create continuous colour ramp to match tree
# colbsz <- colorRampPalette(c(treecont$cols), space = "Lab")
# colbsz <- colbsz(10)[as.numeric(cut(budsz_contdata, breaks = 10))]
# points(pp$xx[1:ape::Ntip(tree_budsz)],
#        pp$yy[1:ape::Ntip(tree_budsz)] - 0.5,
#        pch = 20,
#        col = colbsz[budsz_contdata[tree_budsz$tip.label]])
# coloured by flower colour
# set colours and names for flower colour
fcols <- c("#FFFDD0", "light pink", "red")
names(fcols) <- c("0", "0.5", "1")
# change flower colour data into a named vector that matches tree and colour names
fcol_data <- contMap_data$colour_binary
names(fcol_data) <- contMap_data$tree_names
# add flower colour points
points(pp$xx[1:ape::Ntip(tree_budsz)] + 0.5,
       pp$yy[1:ape::Ntip(tree_budsz)],
       pch = 15, cex = 0.5,
       col = fcols[fcol_data[tree_budsz$tip.label]])
# change median longitude data into a named vector that matches tree names
mlong_data <- contMap_data$median_long
names(mlong_data) <- contMap_data$tree_names
# now create continuous colour ramp to match this (magma from viridis)
collong <- colorRampPalette(c("#f0f921", "#f89540", "#cc4778", "#7e03a8", "#0d0887"), space = "Lab")
collong <- collong(10)[as.numeric(cut(mlong_data, breaks = 10))]
# add median longitude points! with continuous colour scale!
points(pp$xx[1:ape::Ntip(tree_budsz)] + 1.5,
       pp$yy[1:ape::Ntip(tree_budsz)],
       pch = 18, cex = 0.5,
       col = collong[mlong_data[tree_budsz$tip.label]])
# legends
legend(x = 8, y = 680, legend = c("white-cream", "mixed", "colourful"), col = fcols, 
       bty = "n", cex = 0.8, title = "Flower colour", pch = 15)
legend(x = 22, y = 680, legend = c("114º east (western Australia)", "133º east (central Australia)", "153º east (eastern Australia)"), 
       col = c("#f0f921", "#cc4778", "#0d0887"), 
       bty = "n", cex = 0.8, title = "Median longitude", pch = 18)
dev.off()
rm(pp)
# TO DO!!!! ADD POINTS FOR BUDSIZE IN SAME COLOUR AS TREE
# AND ADD CLADE LABELS!
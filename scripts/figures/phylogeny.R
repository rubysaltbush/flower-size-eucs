# script to simulate evolution of eucalypt bud size on phylogeny and 
# render this in figure with labels for tip flower colour and clades

# produces circular phylogeny (Figure 1b)

#### prep tree and data ####
# remove taxa with no match in tree or bud size data
contMap_data <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, subgenus, budsize_mm2, logbudsize_mm2, medianlong, colour_binary) %>%
  dplyr::filter(!is.na(tree_names) & !is.na(logbudsize_mm2)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
# lose 51 tips
tree_budsz <- ape::drop.tip(treeML1, treeML1$tip.label[-match(contMap_data$tree_names, treeML1$tip.label)])
length(tree_budsz$tip.label) # 680 tips remain

# change data into a named vector that matches tree
budsz_contdata <- contMap_data$logbudsize_mm2
names(budsz_contdata) <- contMap_data$tree_names
# and prep flower colour data also
flcol <- as.factor(contMap_data$colour_binary)
names(flcol) <- contMap_data$tree_names
# make sure discrete character is in the order of tree
flcol <- flcol[tree_budsz$tip.label]
# set factor colours
cols <- setNames(c(my_colours$flcol_fill[1], my_colours$flcol_fill[2], my_colours$flcol_fill[3]), c("0", "0.5", "1"))

# add column with species' tip position in tree for plotting
treetips <- data.frame(tree_names = tree_budsz$tip.label, position = c(1:680))
contMap_data <- contMap_data %>%
  dplyr::left_join(treetips, by = "tree_names")
rm(treetips)

# read in selected section and subgenus clade labelling
cladelabs <- readr::read_csv("data_input/clades_forlabelling.csv")

# match to data with tip position
contMap_data <- contMap_data %>%
  dplyr::left_join(cladelabs, by = c("tree_names", "subgenus", "position"))
rm(cladelabs)

#### visualise phylogeny ####

# visualise evolution of euc bud size (log mm2) on phylogeny using contmap
treecont <- phytools::contMap(tree_budsz, budsz_contdata, plot = FALSE)

# re-colour contmap with continous bud size colour scale
treecont <- phytools::setMap(treecont, my_colours$budsz)

# export basic contMap with species labels
pdf(file = "figures/logbudsizemm2_full_phylogeny.pdf", width = 12, height = 50)
plot(treecont, legend = 0.7*max(nodeHeights(tree_budsz)), sig = 1, 
     fsize = c(0.5, 0.7), lwd = 2, outline = FALSE, leg.txt = "Bud size (log mm²)")
dev.off()

# plot contmap with data on flower colour displayed at tips with phytools
# workaround from http://blog.phytools.org/2022/06/how-to-plot-tip-node-labels-without.html

#* circular plot ----
pdf(file = "figures/phylogeny_logbudsize.pdf", width = 12, height = 9)

# plot contMap
phytools::plot.contMap(treecont, type = "fan", legend = FALSE,
                       ftype = "off", lwd = 2.2, outline = FALSE,
                       xlim = c(-100, 80))

# first label beginning of Neogene at 23 mya
plotrix::draw.circle(0, 0, radius = max(nodeHeights(tree_budsz)) - 23, 
                     border = "grey", lty = 3)

# then label eucalyptae crown age at 52 mya
plotrix::draw.circle(0, 0, radius = max(nodeHeights(tree_budsz)) - 52, 
                     border = "grey", lty = 3)

# plot contMap again
par(new = TRUE) # hack to force below to plot on top of above 
phytools::plot.contMap(treecont, type = "fan", legend = FALSE,
                       ftype = "off", lwd = 2.2, outline = FALSE,
                       xlim = c(-100, 80))

# below adapted from http://blog.phytools.org/2016/08/vertical-legend-in-contmap-style-plots.html
# add bud size legend using phytools function
phytools::add.color.bar(leg = 70,
                        cols = treecont$cols,
                        title = "Eucalypt bud size (log mm²)",
                        lims = NULL,
                        digits = 2,
                        prompt = FALSE,
                        lwd = 12,
                        direction = "upwards",
                        subtitle = "",
                        x = -90,
                        y = -10)

# then add custom tick marks
lines(x = rep(-87.525, 2), y = c(-10, 60)) # draw vertical line
Y <- cbind(seq(-10, 60, length.out = 5), # define x pos for ticks
         seq(-10, 60, length.out = 5))
X <- cbind(rep(-87.525, 5), # define y pos for ticks
         rep(-85.925, 5))
for(i in 1:nrow(Y)) lines(X[i,], Y[i,]) # draw ticks
ticks <- seq(treecont$lims[1], treecont$lims[2], length.out = 5) # get tick values
text(x = X[,2], y = Y[,2], round(ticks, 1), pos = 4, cex = 0.8) # draw tick values
rm(X, Y, i, ticks)

# tip points coloured by flower colour
# assign plot to object
pp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
# from pp object will pull out x and y coordinates to plot points

#function below by GM to offset x and y points, huzzah
offset_xx_yy <- function(xx, yy, offset) {
  angle <- atan2(yy, xx)
  data.frame(
    xx = xx + offset * cos(angle),
    yy = yy + offset * sin(angle)
  )
}

xx_yy <- offset_xx_yy(
  xx = pp$xx[1:ape::Ntip(tree_budsz)],
  yy = pp$yy[1:ape::Ntip(tree_budsz)],
  offset = 2
)

# add flower colour points
points(xx_yy$xx,
       xx_yy$yy,
       pch = 15, cex = 1,
       col = cols[flcol[tree_budsz$tip.label]])

rm(xx_yy, offset_xx_yy, pp)

# legend
legend(x = "bottomleft", legend = c("white-cream", "mixed", "colourful"), 
       col = cols, bty = "n", cex = 1.5, title = "Flower colour", pch = 15)

#** clade labelling ----

# source custom arc labelling function
source("scripts/functions/arclabel.R")

# label broad subgenus clades (some exceptions within label areas)
arclabel(text = "Blakella",
         tips = c(1, 34),
         col = "grey",
         ln.offset = 1.14,
         lab.offset = 1.19)
arclabel(text = "Angophora",
         orientation = "perpendicular",
         tips = c(35, 43),
         col = "grey",
         ln.offset = 1.14,
         lab.offset = 1.16)
arclabel(text = "Corymbia",
         tips = c(44, 87),
         col = "grey",
         ln.offset = 1.14,
         lab.offset = 1.19)
arclabel(text = "Eudesmia",
         tips = c(89, 109),
         col = "grey",
         ln.offset = 1.14,
         lab.offset = 1.19)
arclabel(text = "Eucalyptus",
         tips = c(112, 216),
         col = "grey",
         ln.offset = 1.14,
         lab.offset = 1.19)
arclabel(text = "Symphyomyrtus",
         tips = c(219, 680),
         col = "grey",
         ln.offset = 1.14,
         lab.offset = 1.19)

# label sections of Symphyomyrtus
# first calculate tip positions
sect_label <- contMap_data %>%
  dplyr::select(position, sect_label) %>%
  dplyr::group_by(sect_label) %>%
  tidyr::nest() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(mintip = purrr::map(data, ~{min(.x$position)})) %>%
  dplyr::mutate(maxtip = purrr::map(data, ~{max(.x$position)})) %>%
  tidyr::unnest(cols = c(data, mintip, maxtip)) %>%
  dplyr::select(sect_label, mintip, maxtip) %>%
  dplyr::distinct() %>%
  dplyr::filter(!(is.na(sect_label)))

# loop through and draw labels on phylogeny for larger orders
for(i in 1:length(sect_label$sect_label)) {
  arclabel(text = sect_label$sect_label[i],
           tips = c(sect_label$mintip[i], sect_label$maxtip[i]),
           cex = 0.75,
           col = "grey",
           ln.offset = 1.07,
           lab.offset = 1.1)
}
rm(i, sect_label)

dev.off()


rm(arclabel, budsz_contdata, tree_budsz, treecont, contMap_data, 
   cols, flcol)

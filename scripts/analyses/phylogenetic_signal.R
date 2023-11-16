# calculate phylogenetic signal for flower(bud) size and flower colour

### test phylogenetic signal ####

# flower (bud) size
flsize <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, logbudsize_mm2) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
tree_flsize <- ape::drop.tip(treeML1, treeML1$tip.label[-match(flsize$tree_names, treeML1$tip.label)])
length(tree_flsize$tip.label) # 680 tips remain

# make sure data in same order as tree
flsize <- flsize[order(match(flsize$tree_names, tree_flsize$tip.label)),]
flsize <- flsize[,2]
names(flsize) <- tree_flsize$tip.label

phytools::phylosig(tree_flsize, x = flsize, method = "lambda", test = TRUE)
# Phylogenetic signal lambda : 0.806648 
# logL(lambda) : -809.858 
# LR(lambda=0) : 233.671 
# P-value (based on LR test) : 9.43823e-53 

# lambda > 0.8, intermediate to strong phylogenetic signal

phytools::phylosig(tree_flsize, x = flsize, method = "K", test = TRUE)
# Phylogenetic signal K : 0.0206045 
# P-value (based on 1000 randomizations) : 0.001 

# p-value suggests some phylogenetic signal in bud size, though <1 so
# less phylogenetic signal than expected under Brownian motion evolution

rm(tree_flsize, flsize)

# flower colour, binary test using Fritz and Purvis' D in caper
flcol <- euc_traits_nosubsp %>%
  dplyr::select(tree_names, colour_fullbinary) %>%
  dplyr::filter(complete.cases(.)) %>%
  as.data.frame()

# drop outgroups and missing data tips from tree
tree_flcol <- ape::drop.tip(treeML1, treeML1$tip.label[-match(flcol$tree_names, treeML1$tip.label)])
length(tree_flcol$tip.label) # 678 tips remain


# first remove node labels which confuse caper
tree_flcol$node.label <- NULL

# run test for phylogenetic signal
caper::phylo.d(data = flcol, 
               phy = tree_flcol, 
               names.col = tree_names, 
               binvar = colour_fullbinary)

# Calculation of D statistic for the phylogenetic structure of a binary variable
# 
# Data :  data
# Binary variable :  colour_fullbinary
# Counts of states:  0 = 596
# 1 = 82
# Phylogeny :  phy
# Number of permutations :  1000
# 
# Estimated D :  0.4692107
# Probability of E(D) resulting from no (random) phylogenetic structure :  0
# Probability of E(D) resulting from Brownian phylogenetic structure    :  0.001

# D is <1 (so shows some phylogenetic signal) but >0 (so different to Brownian
# motion expectation)

rm(flcol, tree_flcol)

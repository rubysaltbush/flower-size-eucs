# script to define colours as single object so this can be changed universally
# if/when needed

my_colours <- list()

# flower colourfulness
my_colours$flcol_fill <- c("#faebcd", "light pink", "red", "grey")
names(my_colours$flcol_fill) <- c("white-cream", "mixed", "colourful", "NA")

my_colours$flcol_col <- c("#DFBF5B", "light pink", "red", "grey")
names(my_colours$flcol_col) <- c("white-cream", "mixed", "colourful", "NA")

# log bud size (continuous)
my_colours$budsz <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")

# flower colourfulness (continuous)
my_colours$flcol_cont <- c("#faebcd", "light pink")

# leaf area (continuous)
my_colours$leafarea <- c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")

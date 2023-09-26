# script to define colours as single object so this can be changed universally
# if/when needed

my_colours <- list()

# flower colourfulness
my_colours$flcol_fill <- c("#faebcd", "light pink", "red", "black")
names(my_colours$flcol_fill) <- c("white-cream", "mixed", "colourful", "NA")

my_colours$flcol_col <- c("#F0E4BE", "light pink", "red", "black")
names(my_colours$flcol_col) <- c("white-cream", "mixed", "colourful", "NA")

# log bud size (continuous)
my_colours$budsz <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", 
                      "#4292c6", "#2171b5", "#08519c", "#08306b")


# script to import and analyse field-measured data
# to support use of bud size as proxy for flower size

# get data
fieldmeas <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS8nzr1MXw1OsYBVr_Ndqy5Pa1FNlYgII8JbQrb7J9kT2_wsAKwr8Mah5bGDa0Y17CkazRlv4iT26A_/pub?gid=691498141&single=true&output=csv")
names(fieldmeas) <- c("timestamp", "genus", "species", "location", "fllen_cm", "flwid_cm", "budlen_cm", "budwid_cm", "frtlen_cm", "frtwid_cm", "recorder")
# calculate size from length*width
fieldmeas$flsz_mm2 <- 100*fieldmeas$fllen_cm*fieldmeas$flwid_cm
fieldmeas$bdsz_mm2 <- 100*fieldmeas$budlen_cm*fieldmeas$budwid_cm

# source regression function
source("scripts/functions/do_regression.R")

# run regressions

bud_vs_flower <- do_regression(xdata = log(fieldmeas$bdsz_mm2), 
                               ydata = log(fieldmeas$flsz_mm2),
                               xlabel = "Eucalypt bud size (log mm²)", 
                               ylabel = "Eucalypt flower size (log mm²)",
                               output_path = "figures/regressions/field_bud_vs_flower")
summary(bud_vs_flower)
# Call:
#   lm(formula = ydata ~ xdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.8342 -0.2277  0.0656  0.2476  0.6007 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.59282    0.31473   8.238 2.58e-08 ***
#   xdata        0.74270    0.05758  12.898 5.17e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3998 on 23 degrees of freedom
# Multiple R-squared:  0.8785,	Adjusted R-squared:  0.8733 
# F-statistic: 166.4 on 1 and 23 DF,  p-value: 5.167e-12

# export pretty plot to include in supp. mat.
ggplot(fieldmeas, aes(x = log(bdsz_mm2), y = log(flsz_mm2))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  theme_pubr() +
  xlab("Field measured bud size (log mm²)") +
  ylab("Field measured flower size (log mm²)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(bud_vs_flower)$r.squared, 2),
                     "    P = ", format.pval(summary(bud_vs_flower)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/regressions/Fig S1 field measured bud size vs flower size.pdf", width = 7, height = 6)

rm(fieldmeas, do_regression, bud_vs_flower)

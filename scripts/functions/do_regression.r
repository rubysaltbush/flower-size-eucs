# function to perform linear regressions on list of input data, export basic
# scatter plots (with line if significant) of data and qqplots of residuals

do_regression <- function(xdata, ydata, xlabel, ylabel, output_path) {
  # run linear regression
  lm_data <- lm(ydata ~ xdata)
  
  # plot residuals vs fitted to check for normality - have to inspect visually
  pdf(paste(output_path, "residual plot.pdf"), width = 7, height = 7)
  plot(fitted(lm_data), resid(lm_data), 
       xlab = paste("Fitted values of", xlabel, "~", ylabel),
       ylab = paste("Residuals of", xlabel, "~", ylabel))
  abline(0, 0)
  dev.off()
  
  # plot qqplot of residuals for visual inspection also
  pdf(paste(output_path, "residual qqplot.pdf"), width = 7, height = 7)
  qqnorm(lm_data$residuals, main = paste("QQ Plot of", xlabel, "&", ylabel))
  qqline(lm_data$residuals)
  dev.off()
  
  lm_data
}

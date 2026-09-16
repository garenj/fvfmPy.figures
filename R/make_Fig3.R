# Script to reproduce Fig. 3, comparison of fvfmPy to manual measurement
# Garen, Arnold, and Crous 2026

library(tidyverse)

make_Fig3 = function() {

  # Load data
  comparison_data = read.csv("Data/FvFm_method_comparison_data.csv")

  # Get linear regression and best fit values
  z = lm(FvFm_auto ~ FvFm_manual, data = comparison_data)
  int = round(coef(z)[1],3)
  slp = round(coef(z)[2],3)
  rsq = round(summary(z)$r.squared,3)

  # Build plot
  p = ggplot(comparison_data, aes(x = FvFm_manual, y = FvFm_auto)) +
    geom_point(color = "grey40", size = 1) +
    geom_smooth(method = "lm") +
    theme_classic() +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    annotate(
      geom = "text",
      x = 0.55,
      y = -0.2,
      label = paste0("y = ", slp, "x - ",-int,"; r² = ",rsq),
      color = "black",
      size = 3
    ) +
    scale_x_continuous(breaks = c(-0.2,0,0.2,0.4,0.6,0.8)) +
    scale_y_continuous(breaks = c(-0.2,0,0.2,0.4,0.6,0.8)) +
    xlab(expression(italic(F[v]/F[m])*" (manual)")) +
    ylab(expression(italic(F[v]/F[m])*" (automated)"))

  # Output to file
  png("Figures/Fig3_FvFm_comparison_plot.png", width = 3.75, height = 3.5, units = "in", res = 600)
  print(p)
  dev.off()
}

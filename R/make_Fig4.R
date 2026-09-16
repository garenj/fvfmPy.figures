# Script to reproduce Fig. 4, Thermal load sensitivity example
# Garen, Arnold, and Crous 2026

make_Fig4 = function() {

  # Read in data
  dat <- read.csv("Data/tls_example_deidentified.csv", stringsAsFactors = FALSE)

  # Get duration breaks in data
  dur_breaks <- sort(unique(dat$time_min))

  # Standardize FvFm values
  o_dat <- dat |>
    mutate(prop_final = FvFm_final / FvFm_initial)
  o_dat <- o_dat |>
    mutate(prop_clamped = pmin(pmax(prop_final, 1e-4), 1 - 1e-4))
  dat_std <- standardize_data(
    o_dat, temp = "temp_C", duration = "time_min", proportion = "prop_clamped",
    duration_unit = "minutes", random_effects = c("sp_id", "plant_id")
  )

  # Fit 4 parameter logistic model
  fit_sp <- suppressWarnings(fit_4pl(dat_std, ctmax = ~0 + sp_id + (1|plant_id), z = ~0 + sp_id,
                                           up = ~0 + sp_id, family = "beta", t_ref = 60, method = "wald", quiet = TRUE))

  # Get labels for plotting
  tls_labels <- tls(fit_sp, by = "sp_id", method = "wald")$summary |>
    pivot_wider(names_from = quantity, values_from = c(median, lower, upper)) |>
    transmute(group = sp_id,
              label = sprintf("CTmax = %.1f°C\nz = %.2f", median_CTmax, median_z))

  # Build plots
  p1 <-
    plot_survival_curves(fit_sp) +
    scale_x_log10(breaks = dur_breaks) +
    scale_colour_viridis_d(name = "Temperature (°C)",
                           option = "C", end = 0.9) +
    facet_wrap(~group, ncol = 5) +
    labs(title = NULL, subtitle = NULL, caption = NULL,
         x = bquote("Exposure duration (min)"),
         y = bquote("Scaled"~italic(F)[v] / italic(F)[m])) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.spacing = unit(0.1, "cm"),
          legend.justification = c(0,1),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = margin(t = 3, r = 0, b = 3, l = 20, unit = "pt")) #+

  p2 <-
    plot_survival_surface(fit_sp, temps = seq(35, 50, 0.5)) +
    scale_y_log10(breaks = dur_breaks) +
    scale_fill_viridis_c(
      name = bquote("Scaled"~italic(F)[v] / italic(F)[m]),
      option = "D",
      limits = c(0, 1)) +
    facet_wrap(~group, ncol = 5) +
    labs(title = NULL, subtitle = NULL, caption = NULL,
         y = bquote("Exposure duration"~(min))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(0, "lines"),
          legend.justification = c(0,1),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = margin(t = 3, r = 0, b = 3, l = 20, unit = "pt")) #+

  p3 <-
    suppressWarnings(plot_tdt_curve(fit_sp, p = 0.5, temps = seq(40, 52, length.out = 100))) +
    scale_y_log10(breaks = c(1, 5, 15, 30, 60, 120, 240), limits = c(1,300)) +
    scale_x_continuous(breaks = c(40,44,48), limits = c(40,50)) +
    scale_colour_viridis_d(option = "mako", end = 0.8, name = "Species") +
    facet_wrap(~group, ncol = 5) +
    geom_text(data = tls_labels, aes(label = label), x = Inf, y = Inf,
              hjust = 1.05, vjust = 1.4, inherit.aes = FALSE, size = 2.8) +
    labs(title = NULL, subtitle = NULL, caption = NULL,
         y = bquote("Time to"~italic(T)[50]~(min))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(0, "lines"),
          legend.justification = c(0,1),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = margin(t = 3, r = 0, b = 3, l = 20, unit = "pt")) #+

  # Write to file
  png("Figures/Fig4_TLS_example.png", height = 6, width = 8, res = 600, units = "in")
  print(plot_grid(p1,p2,p3, ncol = 1, align = "v",rel_heights = c(1.13,1,1), labels = "auto"))
  dev.off()
}

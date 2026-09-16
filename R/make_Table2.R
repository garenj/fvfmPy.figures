# Script to reproduce Table 2, TLS model outputs
# Garen, Arnold, and Crous 2026

make_Table2 = function() {

  # Read in data
  dat <- read.csv("Data/tls_example_deidentified.csv", stringsAsFactors = FALSE)

  # Standardize FvFm values
  o_dat <- dat |>
    mutate(prop_final = FvFm_final / FvFm_initial)
  o_dat <- o_dat |>
    mutate(prop_clamped = pmin(pmax(prop_final, 1e-4), 1 - 1e-4))
  dat_std <- standardize_data(
    o_dat, temp = "temp_C", duration = "time_min", proportion = "prop_clamped",
    duration_unit = "minutes", random_effects = c("sp_id", "plant_id")
  )

  # Fit 4 Parameter logistic model
  fit_sp_up <- suppressWarnings(fit_4pl(dat_std, ctmax = ~0 + sp_id + (1|plant_id), z = ~0 + sp_id,
                                           up = ~0 + sp_id, family = "beta", t_ref = 60, method = "wald", quiet = TRUE))

  # Write to file
  write.csv(summary(fit_sp_up)$coefficients,"Tables/Table2.csv", row.names = F)
}

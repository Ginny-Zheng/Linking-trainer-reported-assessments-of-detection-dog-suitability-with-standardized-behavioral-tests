scripts <- c("01_estimate_css.R", "02_evaluate_css.R", "03_correlation_networks.R", "04_longitudinal_css.R")
for (script in scripts) {
  message("Running analysis/", script)
  source(file.path("analysis", script), local = new.env(parent = globalenv()))
}
message("CSS analysis complete. See outputs/.")

css_config <- list(
  data_path = file.path("data", "private", "time_variant_and_invariant_data.xlsx"),
  sheet = 1,
  id_column = "DogID",
  outcome_column = "final_disposition",
  outcome_levels = c("Washout", "Sale Quality"),
  positive_class = "Sale Quality",
  ages = c(3, 6, 10, 12),
  css_cutoffs = c(`3` = 2.6, `6` = 2.8, `10` = 3.0, `12` = 3.2),
  network_correlation_cutoff = 0.5,
  network_adjustment = "fdr",
  network_alpha = 0.05,
  formulas = list(
    `3` = c("Physical.Possessiveness.of.Toy", "Independence", "Hunt", "Work/Effort", "Excitability"),
    `6` = c("Physical.Possessiveness.of.Toy", "Independence", "Surfaces", "Work/Effort", "Excitability"),
    `10` = c("Focus.on.Toy/Reward", "Independence", "People", "Vehicles.&.Urban.Clutter", "Excitability"),
    `12` = c("Hunt", "Surfaces", "People", "Work/Effort", "Excitability")
  )
)

age_column <- function(base_name, age) paste0(base_name, ".Time", age)
css_column <- function(age) paste0("CSS.Time", age)
suitability_column <- function(age) paste0("Trainability.Score.Time", age)

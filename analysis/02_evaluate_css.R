source(file.path("R", "config.R")); source(file.path("R", "data_validation.R")); source(file.path("R", "evaluation.R"))
dir.create(file.path("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
data <- readxl::read_excel(file.path("outputs", "data_with_css.xlsx")) |> as.data.frame(check.names = FALSE) |> prepare_outcome()
utils::write.csv(evaluate_all_ages(data), file.path("outputs", "css_outcome_metrics.csv"), row.names = FALSE)
for (age in css_config$ages) {
  ggplot2::ggsave(file.path("outputs", "figures", paste0("css_agreement_", age, "m.png")), plot_css_agreement(data, age), width = 6, height = 5, dpi = 300)
  ggplot2::ggsave(file.path("outputs", "figures", paste0("css_ranked_", age, "m.png")), plot_ranked_css(data, age), width = 7, height = 5, dpi = 300)
}

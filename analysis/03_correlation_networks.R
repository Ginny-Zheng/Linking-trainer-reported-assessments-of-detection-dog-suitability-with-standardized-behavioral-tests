source(file.path("R", "config.R")); source(file.path("R", "data_validation.R")); source(file.path("R", "networks.R"))
dir.create(file.path("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
data <- readxl::read_excel(file.path("outputs", "data_with_css.xlsx")) |> as.data.frame(check.names = FALSE)
for (age in css_config$ages) {
  variables <- c(age_column(unique(unlist(css_config$formulas)), age), css_column(age))
  variables <- intersect(variables, names(data))
  graph <- correlation_network(data, variables)
  title <- sprintf("%d-month Spearman network (|rho| >= %.2f, FDR < %.2f)", age, css_config$network_correlation_cutoff, css_config$network_alpha)
  plot_correlation_network(graph, title, file.path("outputs", "figures", paste0("network_", age, "m.pdf")))
}

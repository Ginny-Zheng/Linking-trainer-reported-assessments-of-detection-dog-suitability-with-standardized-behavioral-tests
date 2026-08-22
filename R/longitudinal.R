css_long_format <- function(data, config = css_config) {
  columns <- setNames(css_column(config$ages), paste0("month_", config$ages))
  frame <- data[, c(config$id_column, config$outcome_column, unname(columns)), drop = FALSE]
  names(frame)[match(unname(columns), names(frame))] <- names(columns)
  tidyr::pivot_longer(frame, dplyr::starts_with("month_"), names_to = "time", values_to = "css") |>
    dplyr::mutate(age_months = as.numeric(sub("month_", "", time)))
}

dog_css_slopes <- function(long_data, config = css_config) {
  long_data |>
    dplyr::filter(stats::complete.cases(css, age_months)) |>
    dplyr::group_by(.data[[config$id_column]], .data[[config$outcome_column]]) |>
    dplyr::summarise(n_timepoints = dplyr::n(), slope = if (dplyr::n() >= 2) stats::coef(stats::lm(css ~ age_months))[2] else NA_real_, .groups = "drop")
}

plot_longitudinal_css <- function(long_data, config = css_config) {
  ggplot2::ggplot(long_data, ggplot2::aes(age_months, css, color = .data[[config$outcome_column]])) +
    ggplot2::geom_smooth(method = "lm", se = TRUE) +
    ggplot2::scale_x_continuous(breaks = config$ages) +
    ggplot2::scale_color_manual(values = c("Washout" = "#2b8cbe", "Sale Quality" = "#f28e2b")) +
    ggplot2::labs(x = "Age (months)", y = "CSS", color = "Final disposition") +
    ggplot2::theme_classic(base_size = 12)
}

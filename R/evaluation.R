classification_metrics <- function(observed, score, cutoff, config = css_config) {
  keep <- stats::complete.cases(observed, score)
  observed <- factor(observed[keep], levels = config$outcome_levels)
  predicted <- factor(ifelse(score[keep] > cutoff, config$positive_class, config$outcome_levels[1]),
                      levels = config$outcome_levels)
  cm <- caret::confusionMatrix(predicted, observed, positive = config$positive_class, mode = "everything")
  data.frame(
    n = length(observed), cutoff = cutoff,
    accuracy = unname(cm$overall["Accuracy"]),
    sensitivity = unname(cm$byClass["Sensitivity"]),
    specificity = unname(cm$byClass["Specificity"]),
    precision = unname(cm$byClass["Pos Pred Value"]),
    f1 = unname(cm$byClass["F1"])
  )
}

evaluate_all_ages <- function(data, config = css_config) {
  dplyr::bind_rows(lapply(config$ages, function(age) {
    result <- classification_metrics(data[[config$outcome_column]], data[[css_column(age)]],
                                     config$css_cutoffs[as.character(age)], config)
    cbind(age_months = age, result)
  }))
}

plot_css_agreement <- function(data, age, config = css_config) {
  frame <- data.frame(observed = data[[suitability_column(age)]], css = data[[css_column(age)]])
  rho <- stats::cor(frame$observed, frame$css, method = "spearman", use = "complete.obs")
  ggplot2::ggplot(frame, ggplot2::aes(observed, css)) +
    ggplot2::geom_point(alpha = 0.65) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#2c7fb8") +
    ggplot2::labs(title = paste0(age, "-month CSS"), subtitle = sprintf("Spearman rho = %.3f", rho),
                  x = "Trainer-reported suitability", y = "Computationally Synthesized Suitability (CSS)") +
    ggplot2::theme_classic(base_size = 12)
}

plot_ranked_css <- function(data, age, config = css_config) {
  frame <- data.frame(css = data[[css_column(age)]], outcome = data[[config$outcome_column]]) |>
    dplyr::filter(!is.na(css), !is.na(outcome)) |>
    dplyr::arrange(css) |>
    dplyr::mutate(rank = dplyr::row_number())
  ggplot2::ggplot(frame, ggplot2::aes(rank, css, color = outcome)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = config$css_cutoffs[as.character(age)], linetype = 2) +
    ggplot2::scale_color_manual(values = c("Washout" = "#2b8cbe", "Sale Quality" = "#f28e2b")) +
    ggplot2::labs(title = paste0(age, "-month CSS and final disposition"),
                  x = "Dogs rank-ordered by CSS", y = "CSS", color = "Final disposition") +
    ggplot2::theme_classic(base_size = 12)
}

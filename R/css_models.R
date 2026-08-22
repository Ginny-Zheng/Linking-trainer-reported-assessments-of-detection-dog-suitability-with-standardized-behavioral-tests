css_formula <- function(age, config = css_config) {
  outcome <- suitability_column(age)
  predictors <- age_column(config$formulas[[as.character(age)]], age)
  quote_name <- function(x) paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`")
  stats::reformulate(quote_name(predictors), response = quote_name(outcome))
}

fit_css_model <- function(data, age, config = css_config) {
  formula <- css_formula(age, config)
  variables <- all.vars(formula)
  complete <- stats::complete.cases(data[, variables, drop = FALSE])
  if (!all(complete)) warning(sum(!complete), " rows excluded from the ", age, "-month CSS model.")
  stats::lm(formula, data = data[complete, , drop = FALSE], na.action = stats::na.exclude)
}

estimate_css <- function(data, config = css_config) {
  models <- setNames(vector("list", length(config$ages)), config$ages)
  for (age in config$ages) {
    model <- fit_css_model(data, age, config)
    data[[css_column(age)]] <- stats::predict(model, newdata = data)
    models[[as.character(age)]] <- model
  }
  list(data = data, models = models)
}

model_coefficients <- function(models) {
  dplyr::bind_rows(lapply(names(models), function(age) {
    values <- summary(models[[age]])$coefficients
    data.frame(age_months = as.integer(age), term = rownames(values),
               estimate = values[, "Estimate"], std_error = values[, "Std. Error"],
               statistic = values[, "t value"], p_value = values[, "Pr(>|t|)"], row.names = NULL)
  }))
}

css_agreement <- function(data, config = css_config) {
  dplyr::bind_rows(lapply(config$ages, function(age) {
    observed <- data[[suitability_column(age)]]
    predicted <- data[[css_column(age)]]
    keep <- stats::complete.cases(observed, predicted)
    data.frame(age_months = age, n = sum(keep),
               pearson = stats::cor(observed[keep], predicted[keep], method = "pearson"),
               spearman = stats::cor(observed[keep], predicted[keep], method = "spearman"))
  }))
}

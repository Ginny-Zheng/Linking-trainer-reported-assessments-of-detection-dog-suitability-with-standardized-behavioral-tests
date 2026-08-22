read_analysis_data <- function(config = css_config) {
  if (!file.exists(config$data_path)) {
    stop("Data file not found: ", config$data_path,
         "\nSee data/README.md. The private data file must not be committed.")
  }
  data <- readxl::read_excel(config$data_path, sheet = config$sheet) |>
    as.data.frame(check.names = FALSE)
  validate_analysis_data(data, config)
  data
}

required_columns <- function(config = css_config) {
  model_columns <- unlist(lapply(config$ages, function(age) {
    c(suitability_column(age), age_column(config$formulas[[as.character(age)]], age))
  }), use.names = FALSE)
  unique(c(config$id_column, config$outcome_column, model_columns))
}

validate_analysis_data <- function(data, config = css_config) {
  missing <- setdiff(required_columns(config), names(data))
  if (length(missing)) stop("Missing required columns:\n", paste(missing, collapse = "\n"))
  if (anyNA(data[[config$id_column]]) || anyDuplicated(data[[config$id_column]])) {
    stop(config$id_column, " must be nonmissing and unique.")
  }
  observed <- unique(stats::na.omit(data[[config$outcome_column]]))
  unexpected <- setdiff(observed, config$outcome_levels)
  if (length(unexpected)) stop("Unexpected outcome values: ", paste(unexpected, collapse = ", "))
  invisible(TRUE)
}

prepare_outcome <- function(data, config = css_config) {
  data[[config$outcome_column]] <- factor(data[[config$outcome_column]], levels = config$outcome_levels)
  if (anyNA(data[[config$outcome_column]])) stop("Outcome contains missing or invalid values.")
  data
}

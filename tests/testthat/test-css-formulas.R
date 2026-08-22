source(file.path("R", "config.R")); source(file.path("R", "css_models.R"))
testthat::test_that("published predictor sets contain five components", {
  testthat::expect_true(all(lengths(css_config$formulas) == 5L))
})
testthat::test_that("12-month formula contains expected terms", {
  terms <- attr(stats::terms(css_formula(12)), "term.labels")
  testthat::expect_setequal(terms, c("Hunt.Time12", "Surfaces.Time12", "People.Time12", "`Work/Effort.Time12`", "Excitability.Time12"))
})

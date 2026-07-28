source(testthat::test_path("..", "..", "R", "utils.R"))
source(testthat::test_path("..", "..", "R", "registry.R"))
source(testthat::test_path("..", "..", "R", "code_generator.R"))

testthat::test_that("key methods generate parseable R", {
  reg <- load_method_registry(testthat::test_path("..", "..", "data", "methods.csv"))
  inp <- list(
    dependent = "gdp_growth",
    independents = c("inflation", "investment"),
    include_intercept = TRUE,
    missing_action = "na.omit",
    subset_expression = "",
    max_lag = 4,
    lag_selection = "AIC",
    frequency = 1,
    deterministic = "const",
    forecast_horizon = 10,
    bootstrap_reps = 100,
    seasonal = FALSE,
    entity_id = "firm",
    time_id = "year",
    panel_effect = "individual",
    instrument_lag_min = 2,
    instrument_lag_max = 4,
    collapse_instruments = TRUE,
    gmm_steps = "twosteps"
  )
  ids <- c("ols", "ardl", "var", "fe", "sys-gmm", "logit", "garch", "did", "spatial-lag", "dea")
  for (id in ids) {
    m <- method_record(reg, id)
    code <- generate_r_code(m, inp)
    testthat::expect_silent(parse(text = code))
  }
})

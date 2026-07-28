execute_generated_code <- function(code, data, allow_custom = FALSE, method_id = "") {
  if (method_id %in% c("custom-r", "custom-script") && !isTRUE(allow_custom)) {
    stop("Custom R execution is disabled. Set ALLOW_CUSTOM_CODE=true only on a trusted private deployment.")
  }
  env <- new.env(parent = globalenv())
  env$data <- data
  env$analysis_data <- data
  started <- Sys.time()
  captured <- capture.output({
    condition_result <- capture_condition(eval(parse(text = code), envir = env))
  }, type = "output")
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  model <- if (exists("model", envir = env, inherits = FALSE)) env$model else NULL
  result <- if (exists("result", envir = env, inherits = FALSE)) env$result else condition_result$value
  list(
    model = model,
    result = result,
    warnings = condition_result$warnings,
    console = captured,
    elapsed = elapsed,
    objects = ls(env, all.names = TRUE)
  )
}

standardize_result <- function(run) {
  coefficient_table <- NULL
  glance_table <- NULL
  plot_data <- NULL
  model <- run$model
  if (!is.null(model) && requireNamespace("broom", quietly = TRUE)) {
    coefficient_table <- tryCatch(broom::tidy(model, conf.int = TRUE), error = function(e) NULL)
    glance_table <- tryCatch(broom::glance(model), error = function(e) NULL)
    plot_data <- tryCatch(broom::augment(model), error = function(e) NULL)
  }
  summary_text <- tryCatch(capture.output(print(run$result)), error = function(e) capture.output(str(run$result)))
  if (!length(summary_text) && !is.null(model)) summary_text <- capture.output(summary(model))
  list(
    summary_text = compact_text(summary_text),
    coefficient_table = coefficient_table,
    glance_table = glance_table,
    plot_data = plot_data,
    warnings = run$warnings,
    console = run$console,
    elapsed = run$elapsed
  )
}

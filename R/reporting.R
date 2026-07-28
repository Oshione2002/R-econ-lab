build_analysis_html <- function(project, method, code, standardized, data_profile = NULL) {
  esc <- htmltools::htmlEscape
  coef_html <- if (!is.null(standardized$coefficient_table)) {
    paste(capture.output(print(knitr::kable(standardized$coefficient_table, format = "html"))), collapse = "\n")
  } else "<p>No standard coefficient table was available.</p>"
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>", esc(project$title), "</title>",
    "<style>body{font-family:Arial,sans-serif;max-width:1050px;margin:40px auto;line-height:1.55;color:#172033}pre{background:#f5f7fb;padding:16px;border-radius:10px;overflow:auto}table{border-collapse:collapse;width:100%}th,td{padding:8px;border:1px solid #d9dfeb;text-align:left}h1,h2{color:#172033}</style></head><body>",
    "<h1>", esc(project$title), "</h1>",
    "<p><strong>Method:</strong> ", esc(method$name[[1]]), "</p>",
    "<p><strong>Generated:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "</p>",
    "<h2>Results</h2><pre>", esc(standardized$summary_text), "</pre>",
    "<h2>Coefficient table</h2>", coef_html,
    "<h2>Warnings</h2><pre>", esc(paste(standardized$warnings, collapse="\n")), "</pre>",
    "<h2>Reproducible R code</h2><pre>", esc(code), "</pre>",
    "<h2>Session information</h2><pre>", esc(paste(capture.output(sessionInfo()), collapse="\n")), "</pre>",
    "</body></html>"
  )
}

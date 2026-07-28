`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x

escape_name <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  paste0("`", gsub("`", "", x, fixed = TRUE), "`")
}

safe_formula <- function(dependent, independents, intercept = TRUE) {
  if (is.null(dependent) || !nzchar(dependent)) stop("Select a dependent variable.")
  rhs <- if (length(independents)) paste(escape_name(independents), collapse = " + ") else "1"
  if (!isTRUE(intercept)) rhs <- paste(rhs, "- 1")
  stats::as.formula(paste(escape_name(dependent), "~", rhs))
}

capture_condition <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

require_package <- function(pkg) {
  if (is.null(pkg) || !nzchar(pkg) || pkg %in% c("base", "stats", "graphics", "utils")) return(invisible(TRUE))
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but is not installed. Run install.packages('%s') locally, or add it to the deployment package profile.", pkg, pkg), call. = FALSE)
  }
  invisible(TRUE)
}

is_numeric_column <- function(x) is.numeric(x) || is.integer(x)

compact_text <- function(x, max_chars = 18000L) {
  x <- paste(x, collapse = "\n")
  if (nchar(x) <= max_chars) x else paste0(substr(x, 1, max_chars), "\n... output truncated ...")
}

app_is_hosted <- function() {
  nzchar(Sys.getenv("CONNECT_CONTENT_GUID", "")) || nzchar(Sys.getenv("SHINY_PORT", ""))
}

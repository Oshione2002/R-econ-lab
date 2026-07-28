read_uploaded_data <- function(fileinfo) {
  req <- function(pkg) require_package(pkg)
  path <- fileinfo$datapath
  ext <- tolower(tools::file_ext(fileinfo$name))
  switch(ext,
    csv = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    txt = utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE),
    xlsx = { req("readxl"); as.data.frame(readxl::read_excel(path), check.names = FALSE) },
    xls = { req("readxl"); as.data.frame(readxl::read_excel(path), check.names = FALSE) },
    sav = { req("haven"); as.data.frame(haven::read_sav(path), check.names = FALSE) },
    dta = { req("haven"); as.data.frame(haven::read_dta(path), check.names = FALSE) },
    sas7bdat = { req("haven"); as.data.frame(haven::read_sas(path), check.names = FALSE) },
    rds = readRDS(path),
    rdata = {
      env <- new.env(parent = emptyenv()); nms <- load(path, envir = env)
      candidates <- Filter(function(nm) is.data.frame(env[[nm]]), nms)
      if (!length(candidates)) stop("The RData file does not contain a data frame.")
      env[[candidates[[1]]]]
    },
    stop("Unsupported file type: .", ext)
  )
}

data_profile <- function(data) {
  data.frame(
    variable = names(data),
    class = vapply(data, function(x) paste(class(x), collapse = "/"), character(1)),
    non_missing = vapply(data, function(x) sum(!is.na(x)), integer(1)),
    missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    unique = vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1)),
    minimum = vapply(data, function(x) if (is_numeric_column(x) && any(!is.na(x))) format(min(x, na.rm = TRUE), digits = 6) else "", character(1)),
    maximum = vapply(data, function(x) if (is_numeric_column(x) && any(!is.na(x))) format(max(x, na.rm = TRUE), digits = 6) else "", character(1)),
    stringsAsFactors = FALSE
  )
}

apply_transformation <- function(data, variable, operation, new_name, lag_n = 1L, lower = 0.01, upper = 0.99) {
  if (!variable %in% names(data)) stop("Select a valid variable.")
  if (!nzchar(new_name)) stop("Enter a name for the transformed variable.")
  x <- data[[variable]]
  if (lag_n >= length(x) && operation %in% c("difference", "lag", "lead", "growth")) stop("Lag/order must be smaller than the number of observations.")
  if (!is_numeric_column(x) && !operation %in% c("factor", "dummy")) stop("This transformation requires a numeric variable.")
  out <- switch(operation,
    log = ifelse(x > 0, log(x), NA_real_),
    log1p = log1p(x),
    difference = c(rep(NA_real_, lag_n), diff(x, lag = lag_n)),
    lag = c(rep(NA, lag_n), head(x, -lag_n)),
    lead = c(tail(x, -lag_n), rep(NA, lag_n)),
    growth = c(rep(NA_real_, lag_n), 100 * (x[(lag_n + 1):length(x)] / x[1:(length(x)-lag_n)] - 1)),
    square = x^2,
    standardize = as.numeric(scale(x)),
    normalize = if (diff(range(x, na.rm = TRUE)) == 0) rep(0, length(x)) else (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)),
    winsorize = {
      qs <- stats::quantile(x, probs = c(lower, upper), na.rm = TRUE, names = FALSE)
      pmin(pmax(x, qs[1]), qs[2])
    },
    factor = as.factor(x),
    dummy = as.integer(as.factor(x)) - 1L,
    stop("Unknown transformation.")
  )
  data[[new_name]] <- out
  data
}

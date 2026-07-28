load_method_registry <- function(path = "data/methods.csv") {
  registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("id", "name", "category", "family", "package", "fn", "status")
  missing <- setdiff(required, names(registry))
  if (length(missing)) stop("Method registry is missing: ", paste(missing, collapse = ", "))
  registry
}

method_record <- function(registry, id) {
  out <- registry[registry$id == id, , drop = FALSE]
  if (!nrow(out)) stop("Unknown method: ", id)
  out[1, , drop = FALSE]
}

method_choices <- function(registry, category = "All", query = "") {
  x <- registry
  if (!identical(category, "All")) x <- x[x$category == category, , drop = FALSE]
  if (nzchar(trimws(query))) {
    q <- tolower(trimws(query))
    hay <- tolower(paste(x$name, x$short, x$package, x$category, x$id))
    x <- x[grepl(q, hay, fixed = TRUE), , drop = FALSE]
  }
  stats::setNames(x$id, paste0(x$name, "  ·  ", x$package))
}

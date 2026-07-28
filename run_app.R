if (!requireNamespace("shiny", quietly = TRUE)) stop("Run source('deploy/install_core.R') first.")
shiny::runApp(".", launch.browser = TRUE)

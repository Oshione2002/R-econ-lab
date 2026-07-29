if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

runtime_files <- c(
  "app.R",
  "DESCRIPTION",
  list.files("R", recursive = TRUE, full.names = TRUE),
  list.files("data", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE)
)
runtime_files <- runtime_files[file.exists(runtime_files)]

rsconnect::writeManifest(
  appDir = ".",
  appFiles = runtime_files,
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)

manifest <- jsonlite::read_json("manifest.json")
if ("rsconnect" %in% names(manifest$packages)) {
  stop("Deployment-only package 'rsconnect' was incorrectly included in manifest.json")
}

message(
  "manifest.json created with ",
  length(manifest$packages),
  " runtime packages; rsconnect is excluded."
)

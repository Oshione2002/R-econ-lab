if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
message("manifest.json created. Commit it to GitHub for Posit Connect Cloud deployment.")

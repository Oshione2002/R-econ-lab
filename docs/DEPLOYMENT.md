# Deployment checklist

1. Install R and run `source("deploy/install_core.R")`.
2. Test locally with `shiny::runApp(".")`.
3. Run `source("deploy/generate_manifest.R")`.
4. Commit `manifest.json`, `app.R`, `R/`, `data/`, `www/`, `DESCRIPTION`, and report/deploy files to GitHub.
5. In Posit Connect Cloud, create a Shiny for R deployment from the public GitHub repository.
6. Choose `app.R` as the primary file.

The free deployment should use the core profile first. Add advanced packages gradually because a manifest containing every specialist package may exceed free build time, system-library availability, or memory.

@echo off
Rscript -e "source('deploy/install_core.R'); shiny::runApp('.', launch.browser=TRUE)"
pause

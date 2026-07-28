#!/usr/bin/env bash
set -e
Rscript -e "source('deploy/install_core.R'); shiny::runApp('.', launch.browser=TRUE)"

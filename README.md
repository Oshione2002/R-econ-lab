# R Econometrics Studio

A GitHub-ready R Shiny application for visual econometric analysis with transparent R code generation.

## Included in this build

- **460 searchable method entries** across **19 categories**.
- Dynamic controls for lags, deterministic terms, panel identifiers, instruments, covariance estimators, causal designs, GARCH distributions, spatial weights, Bayesian sampling and other method families.
- Upload support for CSV, Excel, Stata, SPSS, SAS, RDS and RData files.
- Data profiling and common transformations.
- R code generation for all catalogue entries.
- Specialised adapters for 226 registered methods.
- Package-aware execution: a method runs when its required R package is installed.
- Results, coefficient tables, automatic OLS diagnostics, plots and HTML/project exports.

## Important meaning of “all methods”

R's package ecosystem changes continuously. This project therefore contains a broad method registry plus an extensible adapter system. A method marked **Method-aware adapter** has a method-aware code template. A method marked **Generic R scaffold** is searchable and generates a starting call, but its full package-specific form still requires additional arguments or a new adapter. The app does not falsely label every catalogue item as fully validated.

## Run locally

1. Install R 4.4 or later and RStudio.
2. Open this folder as an RStudio project or set it as the working directory.
3. Run:

```r
source("deploy/install_core.R")
shiny::runApp(".")
```

For the broader optional package set:

```r
source("deploy/install_full.R")
```

Some advanced packages require system libraries or compilers and may not install on every operating system.

## Deploy free through Posit Connect Cloud

1. Run the core or full installation script locally.
2. Generate the dependency manifest:

```r
source("deploy/generate_manifest.R")
```

3. Commit `manifest.json` with the rest of the repository.
4. Push the repository to GitHub.
5. Create a Shiny deployment in Posit Connect Cloud and select `app.R` as the primary file.

## Security

Arbitrary custom-R execution is disabled by default. Enable it only on a trusted private deployment:

```bash
ALLOW_CUSTOM_CODE=true
```

Package installation during a running session is also disabled by default. Install dependencies during deployment instead.

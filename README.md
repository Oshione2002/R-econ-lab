<div align="center">

# R Econometrics Studio

### A visual, reproducible econometrics workspace built with R and Shiny

[![R checks](https://img.shields.io/github/actions/workflow/status/Oshione2002/R-econ-lab/r-check.yml?branch=main&label=R%20checks&logo=r&logoColor=white)](https://github.com/Oshione2002/R-econ-lab/actions)
[![R](https://img.shields.io/badge/R-4.4%2B-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Application-1f6feb?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Methods](https://img.shields.io/badge/methods-460-5b5bd6)](data/methods.csv)
[![Adapters](https://img.shields.io/badge/method--aware%20adapters-226-2ea44f)](docs/METHOD_COVERAGE.md)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](LICENSE)

**Import data, configure econometric models visually, inspect generated R code, run supported analyses, diagnose models, and export reproducible results from one interface.**

[Features](#features) · [Method coverage](#method-coverage) · [Run locally](#run-locally) · [Deploy](#free-cloud-deployment) · [Project structure](#project-structure)

</div>

---

## Overview

R Econometrics Studio is an open-source Shiny application designed to make rigorous econometric analysis more accessible without hiding the underlying R workflow. It combines a guided visual interface with editable R code, package-aware execution, diagnostics, and reproducible exports.

The project currently indexes **460 methods across 19 categories**. Of these, **226 have method-aware code adapters**, **226 provide generic R scaffolds**, and **8 provide diagnostic scaffolds**.

> [!IMPORTANT]
> The catalogue is broader than the set of fully validated, one-click estimators. A method appearing in the registry means it is searchable and connected to an R package or function. Its implementation label shows whether it has a specialised adapter, a diagnostic scaffold, or a generic starting template.

## Features

<table>
<tr>
<td width="50%" valign="top">

### Data workspace

- Import CSV, Excel, Stata, SPSS, SAS, RDS and RData files
- Preview observations and inspect variable types
- Profile missing values and summary statistics
- Apply common transformations before modelling
- Load included macroeconomic and panel examples

</td>
<td width="50%" valign="top">

### Visual model builder

- Select dependent, explanatory and control variables
- Configure lags and variable-specific lag ranges
- Set deterministic terms, frequencies and forecast horizons
- Define panel identifiers, effects and covariance estimators
- Specify instruments, endogenous variables and GMM controls

</td>
</tr>
<tr>
<td width="50%" valign="top">

### Transparent R execution

- Generate editable R code from visual selections
- Check whether required packages are installed
- Run supported estimators in the active R session
- Preserve package-specific advanced arguments
- Download complete analysis scripts

</td>
<td width="50%" valign="top">

### Results and reproducibility

- View model summaries and coefficient tables
- Run supported residual and specification diagnostics
- Export HTML reports and project metadata
- Track the selected method and model configuration
- Reproduce analyses using generated scripts

</td>
</tr>
</table>

## Method coverage

The registry spans the major areas of applied econometrics:

| Area | Examples |
|---|---|
| Classical regression | OLS, WLS, GLS, robust regression, quantile regression |
| Time series | ARIMA, ARDL, ECM, VAR, VECM, structural-break and unit-root tests |
| Panel data | Pooled OLS, fixed effects, random effects, panel ARDL, dynamic panels |
| Instrumental variables | 2SLS, GMM, weak-instrument and overidentification diagnostics |
| Limited dependent variables | Logit, probit, Tobit, count models and selection models |
| Causal inference | DiD, matching, synthetic control, RDD and treatment effects |
| Financial econometrics | ARCH/GARCH families, volatility and risk models |
| Spatial econometrics | Spatial lag, error, Durbin and spatial-panel workflows |
| Bayesian econometrics | Bayesian regression, time-series and hierarchical workflows |
| Efficiency and productivity | DEA, stochastic frontier and productivity analysis |
| Machine learning | Regularisation, forests, boosting and causal ML scaffolds |

See [`docs/METHOD_COVERAGE.md`](docs/METHOD_COVERAGE.md) for implementation labels and [`data/methods.csv`](data/methods.csv) for the complete registry.

### Implementation labels

| Label | Meaning |
|---|---|
| **Method-aware adapter** | Generates method-specific R code and controls. Execution still requires the package, valid data and a correct specification. |
| **Diagnostic scaffold** | Generates a diagnostic workflow around the active model specification. |
| **Generic R scaffold** | Provides a searchable package/function starting point that can be completed with advanced arguments. |

## Interface workflow

```text
Create or load a project
          ↓
Import and inspect data
          ↓
Choose an econometric method
          ↓
Select variables and configure options
          ↓
Review or edit generated R code
          ↓
Run the model and diagnostics
          ↓
Export scripts, results and reports
```

## Run locally

### Requirements

- R 4.4 or later
- RStudio is recommended but not required
- Internet access for the initial package installation

### 1. Clone the repository

```bash
git clone https://github.com/Oshione2002/R-econ-lab.git
cd R-econ-lab
```

### 2. Install the core packages

```r
source("deploy/install_core.R")
```

### 3. Start the application

```r
shiny::runApp(".")
```

Alternatively, run:

```r
source("run_app.R")
```

Windows users can also double-click `START_HERE.bat` after installing R.

### Optional broader package installation

```r
source("deploy/install_full.R")
```

Some advanced packages require system libraries, compilers, or more memory than a basic computer or free cloud container provides.

## Free cloud deployment

The project is structured for deployment from GitHub to **Posit Connect Cloud** as an R Shiny application.

### Generate the deployment manifest

Install the packages you intend to deploy, then run:

```r
source("deploy/generate_manifest.R")
```

Commit the generated `manifest.json`, connect this repository to Posit Connect Cloud, and select `app.R` as the primary file.

Deployment details are documented in [`docs/DEPLOYMENT.md`](docs/DEPLOYMENT.md).

> [!NOTE]
> Free hosting has CPU, memory and active-use limits. Large Bayesian models, extensive bootstrap procedures, high-dimensional GMM, large spatial models and long simulations may need a stronger runtime.

## Project structure

```text
R-econ-lab/
├── app.R                         # Main Shiny application
├── R/
│   ├── code_generator.R          # Method-aware R code adapters
│   ├── data_io.R                 # Import and data utilities
│   ├── execution.R               # Controlled model execution
│   ├── options_ui.R              # Dynamic method options
│   ├── registry.R                # Method registry access
│   ├── reporting.R               # Export and report helpers
│   └── utils.R                   # Shared utilities
├── data/
│   ├── methods.csv               # 460-method catalogue
│   ├── sample_macro.csv          # Example time-series data
│   └── sample_panel.csv          # Example panel data
├── deploy/                       # Installation and manifest scripts
├── docs/                         # Coverage and deployment notes
├── tests/testthat/               # Registry and code-generation tests
├── www/                          # Styles and browser assets
└── .github/workflows/r-check.yml # Automated R checks
```

## Testing

The repository includes automated checks for:

- R source parsing
- Registry integrity
- Unique method identifiers
- Method counts and implementation labels
- Code-generation behaviour

Run the tests locally with:

```r
install.packages("testthat")
testthat::test_dir("tests/testthat")
```

## Security

Arbitrary custom R execution is disabled by default. Enable it only in a trusted private deployment:

```bash
ALLOW_CUSTOM_CODE=true
```

Installing packages during a live user session is also disabled by default. Dependencies should be installed during development or deployment.

## Current status

This repository is an extensible **v0.3 development release**. It provides the platform structure, method registry, dynamic controls, code-generation system and supported execution paths. It does not claim that all 460 registered methods have been independently validated across every package version and data configuration.

## Contributing

Contributions can add:

- New method-aware adapters
- Package-specific option forms
- Reference datasets and expected results
- Diagnostic workflows
- Documentation and interface improvements

When adding a method, update `data/methods.csv`. When adding a specialised workflow, add or extend its adapter in `R/code_generator.R` and include a test.

## License

Released under the [MIT License](LICENSE).

<div align="center">

Built with **R**, **Shiny**, and a reproducibility-first approach to econometric analysis.

</div>

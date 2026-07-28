common_model_ui <- function(ns = identity) {
  tagList(
    selectInput(ns("dependent"), "Dependent variable", choices = NULL),
    selectizeInput(ns("independents"), "Explanatory variables", choices = NULL, multiple = TRUE),
    checkboxInput(ns("include_intercept"), "Include intercept", TRUE),
    selectInput(ns("missing_action"), "Missing values", c("Listwise deletion" = "na.omit", "Fail when missing" = "na.fail", "Keep where supported" = "na.exclude")),
    sliderInput(ns("confidence"), "Confidence level", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
    textInput(ns("subset_expression"), "Optional row filter", placeholder = "Example: year >= 2000 & country == 'Nigeria'")
  )
}

method_specific_ui <- function(method, data_names, numeric_names) {
  family <- method$family[[1]]
  id <- method$id[[1]]
  common_lags <- tagList(
    numericInput("max_lag", "Maximum lag", value = 4, min = 0, max = 40, step = 1),
    selectInput("lag_selection", "Lag selection criterion", c("AIC", "BIC", "HQ", "Fixed")),
    textAreaInput("variable_lags", "Variable-specific lags", rows = 3, placeholder = "gdp_growth=1:2\ninflation=0:4")
  )
  if (family == "foundations") return(tagList(
    selectizeInput("analysis_variables", "Analysis variables", choices = data_names, multiple = TRUE),
    selectInput("cor_method", "Association method", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
    selectInput("group_variable", "Optional grouping variable", choices = c("None" = "", data_names)),
    numericInput("factor_count", "Factors/components/clusters", value = 2, min = 1, max = 20)
  ))
  if (family == "regression") return(tagList(
    selectInput("se_type", "Standard errors", c("Classical" = "classical", "HC0", "HC1", "HC2", "HC3", "HAC")),
    selectInput("weights_var", "Optional weight variable", choices = c("None" = "", numeric_names)),
    numericInput("polynomial_degree", "Polynomial degree", value = 2, min = 1, max = 10),
    sliderInput("alpha", "Elastic-net alpha", 0, 1, 1, 0.05),
    sliderInput("quantile_tau", "Quantile tau", 0.05, 0.95, 0.50, 0.05),
    common_lags
  ))
  if (family == "diagnostics") return(tagList(
    selectInput("diagnostic_model", "Model source", c("Use current specification" = "current", "Use last fitted model" = "last")),
    numericInput("diagnostic_lag", "Diagnostic lag/order", value = 2, min = 1, max = 40),
    selectInput("vcov_type", "Covariance type", c("HC0", "HC1", "HC2", "HC3", "HAC"))
  ))
  if (family %in% c("time_series", "unit_root", "cointegration", "var_systems", "financial")) return(tagList(
    selectInput("time_variable", "Time variable", choices = c("None" = "", data_names)),
    selectInput("frequency", "Frequency", c("Annual" = 1, "Semiannual" = 2, "Quarterly" = 4, "Monthly" = 12, "Weekly" = 52, "Daily" = 365)),
    common_lags,
    selectInput("deterministic", "Deterministic terms", c("Intercept" = "const", "Trend" = "trend", "Intercept and trend" = "both", "None" = "none")),
    numericInput("forecast_horizon", "Forecast/IRF horizon", value = 10, min = 1, max = 200),
    numericInput("bootstrap_reps", "Bootstrap repetitions", value = 500, min = 50, max = 10000, step = 50),
    checkboxInput("seasonal", "Include seasonality where supported", FALSE),
    selectInput("innovation_distribution", "Innovation distribution", c("Normal" = "norm", "Student t" = "std", "GED" = "ged", "Skewed t" = "sstd"))
  ))
  if (family == "panel") return(tagList(
    selectInput("entity_id", "Entity identifier", choices = c("Select" = "", data_names)),
    selectInput("time_id", "Time identifier", choices = c("Select" = "", data_names)),
    selectInput("panel_effect", "Effects", c("Individual" = "individual", "Time" = "time", "Two-way" = "twoways")),
    selectInput("panel_model", "Panel estimator", c("Within/fixed effects" = "within", "Random effects" = "random", "Pooling" = "pooling", "First differences" = "fd", "Between" = "between")),
    selectInput("panel_vcov", "Panel covariance", c("Conventional" = "classical", "Arellano HC1" = "arellano", "Driscoll-Kraay" = "driscoll", "PCSE" = "pcse", "Cluster by entity" = "cluster_group", "Cluster by time" = "cluster_time")),
    common_lags,
    selectizeInput("endogenous_vars", "Endogenous variables", choices = data_names, multiple = TRUE),
    selectizeInput("predetermined_vars", "Predetermined variables", choices = data_names, multiple = TRUE),
    numericInput("instrument_lag_min", "Minimum instrument lag", 2, min = 1, max = 20),
    numericInput("instrument_lag_max", "Maximum instrument lag", 4, min = 2, max = 40),
    checkboxInput("collapse_instruments", "Collapse instruments", TRUE),
    selectInput("gmm_steps", "GMM steps", c("One-step" = "onestep", "Two-step" = "twosteps"))
  ))
  if (family == "iv_gmm") return(tagList(
    selectizeInput("endogenous_vars", "Endogenous regressors", choices = data_names, multiple = TRUE),
    selectizeInput("instruments", "Excluded instruments", choices = data_names, multiple = TRUE),
    selectInput("gmm_weight", "GMM weighting", c("Two-step efficient" = "twoStep", "Identity" = "ident", "Optimal" = "optimal")),
    selectInput("iv_diagnostics", "Weak-instrument diagnostics", c("All available", "First-stage F", "Anderson-Rubin", "Overidentification"))
  ))
  if (family == "limited") return(tagList(
    selectInput("link", "Link function", c("Logit" = "logit", "Probit" = "probit", "Complementary log-log" = "cloglog")),
    selectInput("censor_direction", "Censoring/truncation", c("Left" = "left", "Right" = "right", "Both" = "both")),
    numericInput("left_limit", "Left limit", value = 0),
    numericInput("right_limit", "Right limit", value = 1e12),
    selectInput("reference_level", "Reference outcome level", choices = c("Automatic" = "")),
    checkboxInput("marginal_effects", "Calculate marginal effects where available", TRUE)
  ))
  if (family == "causal") return(tagList(
    selectInput("treatment_var", "Treatment variable", choices = c("Select" = "", data_names)),
    selectInput("time_variable", "Time variable", choices = c("Select" = "", data_names)),
    selectInput("unit_variable", "Unit identifier", choices = c("Select" = "", data_names)),
    selectInput("post_variable", "Post-period indicator", choices = c("None" = "", data_names)),
    selectInput("running_variable", "Running variable", choices = c("None" = "", numeric_names)),
    numericInput("cutoff", "RDD cutoff", value = 0),
    selectInput("matching_method", "Matching method", c("Nearest", "Optimal", "Full", "Genetic", "CEM")),
    numericInput("caliper", "Matching caliper", value = 0.2, min = 0, step = 0.01),
    numericInput("bootstrap_reps", "Bootstrap repetitions", value = 500, min = 50, max = 10000, step = 50)
  ))
  if (family == "spatial") return(tagList(
    selectInput("longitude", "Longitude/X coordinate", choices = c("Select" = "", numeric_names)),
    selectInput("latitude", "Latitude/Y coordinate", choices = c("Select" = "", numeric_names)),
    selectInput("weights_type", "Spatial weights", c("Queen contiguity", "Rook contiguity", "K nearest neighbours", "Distance band")),
    numericInput("neighbors_k", "Number of neighbours", value = 4, min = 1, max = 30),
    numericInput("distance_threshold", "Distance threshold", value = 100, min = 0),
    selectInput("spatial_style", "Weights style", c("Row-standardised" = "W", "Binary" = "B", "Globally standardised" = "C"))
  ))
  if (family == "efficiency") return(tagList(
    selectizeInput("inputs", "Input variables", choices = numeric_names, multiple = TRUE),
    selectizeInput("outputs", "Output variables", choices = numeric_names, multiple = TRUE),
    selectInput("returns_to_scale", "Returns to scale", c("Variable" = "vrs", "Constant" = "crs", "Non-increasing" = "irs", "Non-decreasing" = "drs")),
    selectInput("orientation", "Orientation", c("Input" = "in", "Output" = "out")),
    selectInput("frontier_distribution", "Inefficiency distribution", c("Half-normal" = "hnormal", "Truncated normal" = "tnormal", "Exponential" = "exponential"))
  ))
  if (family == "bayesian") return(tagList(
    numericInput("chains", "MCMC chains", value = 4, min = 1, max = 12),
    numericInput("iterations", "Iterations", value = 2000, min = 500, max = 50000, step = 500),
    numericInput("warmup", "Warm-up iterations", value = 1000, min = 100, max = 25000, step = 100),
    numericInput("seed", "Random seed", value = 1234),
    textInput("prior", "Prior specification", placeholder = "Example: normal(0, 1)"),
    selectInput("bayes_family", "Likelihood", c("Gaussian", "Bernoulli", "Poisson", "Negative binomial", "Student"))
  ))
  if (family == "nonparametric") return(tagList(
    selectInput("kernel", "Kernel", c("Gaussian", "Epanechnikov", "Uniform", "Triangular")),
    selectInput("bandwidth_method", "Bandwidth", c("Automatic", "Cross-validation", "Rule of thumb", "Manual")),
    numericInput("bandwidth", "Manual bandwidth", value = 1, min = 0.0001),
    numericInput("bootstrap_reps", "Resamples", value = 500, min = 50, max = 10000, step = 50)
  ))
  if (family == "structural") return(tagList(
    selectizeInput("grouping_vars", "Grouping/stratification variables", choices = data_names, multiple = TRUE),
    selectInput("model_distribution", "Distribution", c("Gaussian", "Binomial", "Poisson", "Gamma", "Survival")),
    textAreaInput("structural_syntax", "Model-specific syntax", rows = 6, placeholder = "For SEM/CFA, enter lavaan model syntax here."),
    numericInput("poverty_line", "Poverty line / threshold", value = 1),
    numericInput("fgt_alpha", "FGT alpha", value = 0, min = 0, max = 5)
  ))
  if (family == "machine_learning") return(tagList(
    numericInput("train_fraction", "Training fraction", value = 0.8, min = 0.5, max = 0.95, step = 0.05),
    numericInput("folds", "Cross-validation folds", value = 5, min = 2, max = 20),
    numericInput("trees", "Trees/iterations", value = 500, min = 10, max = 10000, step = 10),
    numericInput("max_depth", "Maximum depth", value = 6, min = 1, max = 50),
    numericInput("seed", "Random seed", value = 1234),
    checkboxInput("standardize", "Standardise predictors", TRUE)
  ))
  tagList(
    textAreaInput("custom_code_notes", "Custom-code notes", rows = 7, placeholder = "Document package-specific assumptions or code requirements."),
    checkboxInput("unsafe_custom", "I understand custom R code can access the host R session", FALSE)
  )
}

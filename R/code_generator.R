qname <- function(x) paste(escape_name(x), collapse = ", ")
qstr <- function(x) paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')

base_prelude <- function(method, input, formula_text = NULL) {
  lines <- c(
    sprintf("# %s", method$name[[1]]),
    sprintf("# Method ID: %s | Package: %s", method$id[[1]], method$package[[1]]),
    "analysis_data <- data"
  )
  if (nzchar(input$subset_expression %||% "")) {
    lines <- c(lines, sprintf("analysis_data <- subset(analysis_data, %s)", input$subset_expression))
  }
  lines <- c(lines, sprintf("analysis_data <- %s(analysis_data)", input$missing_action %||% "na.omit"))
  if (!is.null(formula_text)) lines <- c(lines, sprintf("model_formula <- %s", formula_text))
  lines
}

formula_code <- function(input) {
  dep <- input$dependent %||% "y"
  x <- input$independents %||% character()
  rhs <- if (length(x)) paste(escape_name(x), collapse = " + ") else "1"
  if (!isTRUE(input$include_intercept)) rhs <- paste(rhs, "- 1")
  sprintf("stats::as.formula(%s)", qstr(paste(escape_name(dep), "~", rhs)))
}

package_guard <- function(pkg) {
  if (!nzchar(pkg) || pkg %in% c("base", "stats", "graphics", "utils")) character()
  else sprintf("if (!requireNamespace(%s, quietly = TRUE)) stop(%s)", qstr(pkg), qstr(sprintf("Package '%s' is not installed.", pkg)))
}

generic_scaffold <- function(method, input) {
  pkg <- method$package[[1]] %||% "base"
  fn <- method$fn[[1]] %||% ""
  c(
    base_prelude(method, input, formula_code(input)),
    package_guard(pkg),
    "# This method uses a generic scaffold because its package arguments are method-specific.",
    "# Complete or replace the argument list in Advanced mode.",
    sprintf("model <- %s::%s(", pkg, fn),
    "  formula = model_formula,",
    "  data = analysis_data",
    ")",
    "result <- summary(model)"
  )
}

generate_r_code <- function(method, input) {
  id <- method$id[[1]]; fam <- method$family[[1]]; pkg <- method$package[[1]] %||% "base"
  vars <- input$analysis_variables %||% input$independents %||% character()
  fcode <- formula_code(input)
  p <- base_prelude(method, input, fcode)

  if (id == "descriptive") return(paste(c(base_prelude(method, input),
    sprintf("selected <- analysis_data[c(%s)]", paste(qstr(vars), collapse = ", ")),
    "result <- summary(selected)", "model <- NULL"), collapse="\n"))
  if (id == "frequency") return(paste(c(base_prelude(method,input),
    sprintf("result <- lapply(analysis_data[c(%s)], table, useNA = 'ifany')", paste(qstr(vars), collapse=", ")),
    "model <- NULL"), collapse="\n"))
  if (id %in% c("correlation","spearman-correlation","kendall-correlation","covariance")) {
    method_cor <- if (id=="spearman-correlation") "spearman" else if (id=="kendall-correlation") "kendall" else "pearson"
    fun <- if (id=="covariance") "stats::cov" else "stats::cor"
    return(paste(c(base_prelude(method,input),
      sprintf("selected <- analysis_data[c(%s)]", paste(qstr(vars), collapse=", ")),
      sprintf("result <- %s(selected, use = 'pairwise.complete.obs'%s)", fun, if(fun=="stats::cor") paste0(", method = ",qstr(method_cor)) else ""), "model <- NULL"), collapse="\n"))
  }
  if (id %in% c("one-sample-t","independent-t","paired-t","wilcoxon","paired-wilcoxon")) {
    fun <- if (grepl("wilcoxon", id)) "stats::wilcox.test" else "stats::t.test"
    paired <- id %in% c("paired-t","paired-wilcoxon")
    return(paste(c(p, sprintf("model <- %s(model_formula, data = analysis_data, paired = %s)",fun,toupper(as.character(paired))),"result <- model"),collapse="\n"))
  }
  if (id %in% c("anova","ancova")) return(paste(c(p,"model <- stats::aov(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="manova") return(paste(c(p,"# For MANOVA, place cbind(outcome1, outcome2, ...) on the formula left-hand side.","model <- stats::manova(model_formula, data = analysis_data)","result <- summary(model, test = 'Pillai')"),collapse="\n"))
  if (id=="chi-square") return(paste(c(base_prelude(method,input),sprintf("tab <- table(analysis_data[[%s]], analysis_data[[%s]])",qstr(vars[1] %||% "x"),qstr(vars[2] %||% "y")),"model <- stats::chisq.test(tab)","result <- model"),collapse="\n"))
  if (id=="pca") return(paste(c(base_prelude(method,input),sprintf("selected <- analysis_data[c(%s)]",paste(qstr(vars),collapse=", ")),"model <- stats::prcomp(selected, scale. = TRUE)","result <- summary(model)"),collapse="\n"))
  if (id=="factor-analysis") return(paste(c(base_prelude(method,input),sprintf("selected <- analysis_data[c(%s)]",paste(qstr(vars),collapse=", ")),sprintf("model <- stats::factanal(selected, factors = %s)",input$factor_count %||% 2),"result <- model"),collapse="\n"))
  if (id=="reliability") return(paste(c(base_prelude(method,input),package_guard("psych"),sprintf("selected <- analysis_data[c(%s)]",paste(qstr(vars),collapse=", ")),"model <- psych::alpha(selected)","result <- model$total"),collapse="\n"))
  if (id=="kmeans") return(paste(c(base_prelude(method,input),sprintf("selected <- scale(analysis_data[c(%s)])",paste(qstr(vars),collapse=", ")),sprintf("model <- stats::kmeans(selected, centers = %s)",input$factor_count %||% 2),"result <- model"),collapse="\n"))
  if (id=="hierarchical-cluster") return(paste(c(base_prelude(method,input),sprintf("selected <- scale(analysis_data[c(%s)])",paste(qstr(vars),collapse=", ")),"model <- stats::hclust(stats::dist(selected))","result <- model"),collapse="\n"))

  if (id %in% c("ols","linear-probability","polynomial","log-linear","log-log","distributed-lag")) {
    return(paste(c(p,"model <- stats::lm(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  }
  if (id=="wls") return(paste(c(p,sprintf("model <- stats::lm(model_formula, data = analysis_data, weights = analysis_data[[%s]])",qstr(input$weights_var %||% "weights")),"result <- summary(model)"),collapse="\n"))
  if (id=="nls") return(paste(c(base_prelude(method,input),"# Replace start values with values suitable for your nonlinear equation.","model <- stats::nls(model_formula, data = analysis_data, start = list())","result <- summary(model)"),collapse="\n"))
  if (id %in% c("ridge","lasso","elastic-net")) {
    alpha <- if(id=="ridge") 0 else if(id=="lasso") 1 else input$alpha %||% .5
    return(paste(c(base_prelude(method,input),package_guard("glmnet"),sprintf("x <- stats::model.matrix(%s, analysis_data)[, -1, drop = FALSE]",fcode),sprintf("y <- analysis_data[[%s]]",qstr(input$dependent)),sprintf("model <- glmnet::cv.glmnet(x, y, alpha = %s)",alpha),"result <- model"),collapse="\n"))
  }
  if (id %in% c("quantile","median-regression")) return(paste(c(p,package_guard("quantreg"),sprintf("model <- quantreg::rq(model_formula, data = analysis_data, tau = %s)",if(id=="median-regression") .5 else input$quantile_tau %||% .5),"result <- summary(model, se = 'nid')"),collapse="\n"))
  if (id=="robust-regression") return(paste(c(p,package_guard("MASS"),"model <- MASS::rlm(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="spline-regression") return(paste(c(p,"# Add splines::ns(variable, df = 4) or splines::bs(variable, df = 4) to the formula.","model <- stats::lm(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="gam") return(paste(c(p,package_guard("mgcv"),"# Replace selected terms with s(variable) where smooth effects are required.","model <- mgcv::gam(model_formula, data = analysis_data, method = 'REML')","result <- summary(model)"),collapse="\n"))
  if (id %in% c("mixed-effects","generalized-mixed")) return(paste(c(p,package_guard("lme4"),if(id=="mixed-effects") "model <- lme4::lmer(model_formula, data = analysis_data)" else "model <- lme4::glmer(model_formula, data = analysis_data, family = stats::gaussian())","result <- summary(model)"),collapse="\n"))

  # Diagnostics: fit OLS from current specification then test.
  if (fam=="diagnostics") {
    testline <- switch(id,
      vif="car::vif(model)", `breusch-pagan`="lmtest::bptest(model)", `white-test`="lmtest::bptest(model, ~ stats::fitted(model) + I(stats::fitted(model)^2))",
      `goldfeld-quandt`="lmtest::gqtest(model)", `durbin-watson`="lmtest::dwtest(model)", `breusch-godfrey`=sprintf("lmtest::bgtest(model, order = %s)",input$diagnostic_lag %||% 2),
      `ljung-box`=sprintf("stats::Box.test(stats::residuals(model), lag = %s, type = 'Ljung-Box')",input$diagnostic_lag %||% 2),
      `jarque-bera`="tseries::jarque.bera.test(stats::residuals(model))", `shapiro-wilk`="stats::shapiro.test(stats::residuals(model))",
      `ramsey-reset`="lmtest::resettest(model)", `cook-distance`="stats::cooks.distance(model)", leverage="stats::hatvalues(model)", dfbetas="stats::dfbetas(model)",
      `studentized-residuals`="stats::rstudent(model)", `influence-measures`="stats::influence.measures(model)",
      `arch-lm`=sprintf("FinTS::ArchTest(stats::residuals(model), lags = %s)",input$diagnostic_lag %||% 2),
      "summary(model)")
    guards <- unique(c(package_guard(if(id=="vif")"car" else if(id %in% c("jarque-bera"))"tseries" else if(id=="arch-lm")"FinTS" else "lmtest")))
    return(paste(c(p,guards,"model <- stats::lm(model_formula, data = analysis_data)",paste0("result <- ",testline)),collapse="\n"))
  }

  # Time series and stationarity
  if (id=="acf-pacf") return(paste(c(base_prelude(method,input),sprintf("x <- stats::ts(analysis_data[[%s]], frequency = %s)",qstr(input$dependent),input$frequency %||% 1),"model <- list(acf = stats::acf(x, plot = FALSE), pacf = stats::pacf(x, plot = FALSE))","result <- model"),collapse="\n"))
  if (id %in% c("decomposition","stl")) return(paste(c(base_prelude(method,input),sprintf("x <- stats::ts(analysis_data[[%s]], frequency = %s)",qstr(input$dependent),input$frequency %||% 1),if(id=="stl") "model <- stats::stl(x, s.window = 'periodic')" else "model <- stats::decompose(x)","result <- model"),collapse="\n"))
  if (id %in% c("simple-exp-smoothing","holt","holt-winters","ets","tbats","bats","theta","croston","neural-ar")) {
    fn <- switch(id,`simple-exp-smoothing`="ses",holt="holt",`holt-winters`="hw",ets="ets",tbats="tbats",bats="bats",theta="thetaf",croston="croston",`neural-ar`="nnetar")
    return(paste(c(base_prelude(method,input),package_guard("forecast"),sprintf("x <- stats::ts(analysis_data[[%s]], frequency = %s)",qstr(input$dependent),input$frequency %||% 1),sprintf("model <- forecast::%s(x)",fn),sprintf("result <- forecast::forecast(model, h = %s)",input$forecast_horizon %||% 10)),collapse="\n"))
  }
  if (id %in% c("ar","ma","arma","arima","sarima","arimax","dynamic-regression")) {
    xreg <- if(length(input$independents %||% character())) sprintf("as.matrix(analysis_data[c(%s)])",paste(qstr(input$independents),collapse=", ")) else "NULL"
    return(paste(c(base_prelude(method,input),package_guard("forecast"),sprintf("y <- stats::ts(analysis_data[[%s]], frequency = %s)",qstr(input$dependent),input$frequency %||% 1),sprintf("xreg <- %s",xreg),sprintf("model <- forecast::auto.arima(y, xreg = xreg, seasonal = %s, ic = %s, max.p = %s, max.q = %s)",toupper(as.character(input$seasonal %||% FALSE)),qstr(tolower(input$lag_selection %||% "AIC")),input$max_lag %||% 4,input$max_lag %||% 4),sprintf("result <- forecast::forecast(model, xreg = if (is.null(xreg)) NULL else tail(xreg, %s), h = %s)",input$forecast_horizon %||% 10,input$forecast_horizon %||% 10)),collapse="\n"))
  }
  if (id %in% c("rolling-regression","expanding-window")) return(paste(c(p,package_guard("zoo"),sprintf("width <- %s",max(5,input$max_lag %||% 20)),"model <- zoo::rollapplyr(seq_len(nrow(analysis_data)), width = width, FUN = function(idx) stats::coef(stats::lm(model_formula, data = analysis_data[idx, , drop=FALSE])), by.column = FALSE, fill = NA)","result <- model"),collapse="\n"))
  if (id %in% c("adf","pp","kpss")) {
    fn <- switch(id,adf="adf.test",pp="pp.test",kpss="kpss.test")
    return(paste(c(base_prelude(method,input),package_guard("tseries"),sprintf("x <- stats::na.omit(analysis_data[[%s]])",qstr(input$dependent)),sprintf("model <- tseries::%s(x)",fn),"result <- model"),collapse="\n"))
  }
  if (id %in% c("df-gls","zivot-andrews")) {
    fn <- if(id=="df-gls") "ur.ers" else "ur.za"
    return(paste(c(base_prelude(method,input),package_guard("urca"),sprintf("x <- stats::na.omit(analysis_data[[%s]])",qstr(input$dependent)),sprintf("model <- urca::%s(x, model = 'constant', lag.max = %s)",fn,input$max_lag %||% 4),"result <- summary(model)"),collapse="\n"))
  }
  if (id %in% c("breitung-panel-unit","llc","ips","fisher-adf-panel","fisher-pp-panel","hadri","cips")) return(paste(c(base_prelude(method,input),package_guard("plm"),sprintf("pdata <- plm::pdata.frame(analysis_data, index = c(%s, %s))",qstr(input$entity_id),qstr(input$time_id)),sprintf("model <- plm::purtest(pdata[[%s]], test = %s, exo = 'intercept', lags = 'AIC')",qstr(input$dependent),qstr(switch(id,llc="levinlin",ips="ips",hadri="hadri",cips="ips",`fisher-adf-panel`="madwu",`fisher-pp-panel`="Pm",`breitung-panel-unit`="levinlin"))),"result <- summary(model)"),collapse="\n"))

  # Cointegration
  if (id=="engle-granger") return(paste(c(p,"long_run <- stats::lm(model_formula, data = analysis_data)","residuals_eg <- stats::residuals(long_run)",package_guard("tseries"),"model <- tseries::adf.test(residuals_eg)","result <- list(long_run = summary(long_run), residual_unit_root = model)"),collapse="\n"))
  if (id=="phillips-ouliaris") return(paste(c(base_prelude(method,input),package_guard("tseries"),sprintf("x <- as.matrix(analysis_data[c(%s, %s)])",qstr(input$dependent),paste(qstr(input$independents),collapse=", ")),"model <- tseries::po.test(x)","result <- model"),collapse="\n"))
  if (id %in% c("johansen","vecm")) return(paste(c(base_prelude(method,input),package_guard("urca"),sprintf("x <- stats::na.omit(analysis_data[c(%s, %s)])",qstr(input$dependent),paste(qstr(input$independents),collapse=", ")),sprintf("model <- urca::ca.jo(x, type = 'trace', ecdet = %s, K = %s)",qstr(if((input$deterministic %||% "const")=="trend")"trend" else "const"),max(2,input$max_lag %||% 2)),"result <- summary(model)"),collapse="\n"))
  if (id %in% c("ardl","uecm","ecm")) return(paste(c(p,package_guard("ARDL"),sprintf("search <- ARDL::auto_ardl(model_formula, data = analysis_data, max_order = %s, selection = %s)",paste0("rep(",input$max_lag %||% 4,", ",1+length(input$independents %||% character()),")"),qstr(input$lag_selection %||% "AIC")),"model <- search$best_model",if(id=="ardl")"result <- summary(model)" else "model <- ARDL::uecm(model)\nresult <- summary(model)"),collapse="\n"))
  if (id=="dols") return(paste(c(p,package_guard("cointReg"),sprintf("y <- analysis_data[[%s]]",qstr(input$dependent)),sprintf("x <- as.matrix(analysis_data[c(%s)])",paste(qstr(input$independents),collapse=", ")),sprintf("model <- cointReg::cointRegD(y = y, x = x, n.lead = %s, n.lag = %s)",input$max_lag %||% 2,input$max_lag %||% 2),"result <- summary(model)"),collapse="\n"))
  if (id=="fmols") return(paste(c(p,package_guard("cointReg"),sprintf("y <- analysis_data[[%s]]",qstr(input$dependent)),sprintf("x <- as.matrix(analysis_data[c(%s)])",paste(qstr(input$independents),collapse=", ")),"model <- cointReg::cointRegFM(y = y, x = x)","result <- summary(model)"),collapse="\n"))

  # VAR / systems
  if (id %in% c("var","svar","irf","fevd","granger","toda-yamamoto")) {
    selected <- unique(c(input$dependent,input$independents))
    extra <- switch(id,
      svar="model <- vars::SVAR(var_model, estmethod = 'direct')\nresult <- summary(model)",
      irf=sprintf("model <- vars::irf(var_model, n.ahead = %s, boot = TRUE, runs = %s)\nresult <- model",input$forecast_horizon %||% 10,input$bootstrap_reps %||% 500),
      fevd=sprintf("model <- vars::fevd(var_model, n.ahead = %s)\nresult <- model",input$forecast_horizon %||% 10),
      granger=sprintf("model <- vars::causality(var_model, cause = %s)\nresult <- model",qstr((input$independents %||% "")[1])),
      `toda-yamamoto`="# Toda-Yamamoto requires augmenting the selected VAR lag by the maximum integration order.\nmodel <- var_model\nresult <- summary(model)",
      "model <- var_model\nresult <- summary(model)")
    return(paste(c(base_prelude(method,input),package_guard("vars"),sprintf("x <- stats::na.omit(analysis_data[c(%s)])",paste(qstr(selected),collapse=", ")),sprintf("lag_choice <- vars::VARselect(x, lag.max = %s, type = %s)",input$max_lag %||% 4,qstr(input$deterministic %||% "const")),sprintf("var_model <- vars::VAR(x, p = %s, type = %s)",max(1,input$max_lag %||% 1),qstr(input$deterministic %||% "const")),extra),collapse="\n"))
  }
  if (id=="sur") return(paste(c(base_prelude(method,input),package_guard("systemfit"),"# Define a named list of equations for a SUR system.","equations <- list(eq1 = model_formula)","model <- systemfit::systemfit(equations, method = 'SUR', data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="3sls") return(paste(c(base_prelude(method,input),package_guard("systemfit"),"# Define multiple structural equations and a shared instrument formula.","equations <- list(eq1 = model_formula)","model <- systemfit::systemfit(equations, method = '3SLS', data = analysis_data)","result <- summary(model)"),collapse="\n"))

  # Panel
  if (fam=="panel") {
    panel_head <- c(p,package_guard("plm"),sprintf("pdata <- plm::pdata.frame(analysis_data, index = c(%s, %s))",qstr(input$entity_id),qstr(input$time_id)))
    if (id %in% c("pooled-ols","fe","re","first-difference-panel","between-panel","two-way-fe")) {
      model_type <- switch(id,`pooled-ols`="pooling",fe="within",re="random",`first-difference-panel`="fd",`between-panel`="between",`two-way-fe`="within")
      effect <- if(id=="two-way-fe")"twoways" else input$panel_effect %||% "individual"
      return(paste(c(panel_head,sprintf("model <- plm::plm(model_formula, data = pdata, model = %s, effect = %s)",qstr(model_type),qstr(effect)),"result <- summary(model)"),collapse="\n"))
    }
    if (id=="hausman") return(paste(c(panel_head,"fe_model <- plm::plm(model_formula, data = pdata, model = 'within')","re_model <- plm::plm(model_formula, data = pdata, model = 'random')","model <- plm::phtest(fe_model, re_model)","result <- model"),collapse="\n"))
    if (id=="bp-lm-panel") return(paste(c(panel_head,"pooled <- plm::plm(model_formula, data = pdata, model = 'pooling')","model <- plm::plmtest(pooled, type = 'bp')","result <- model"),collapse="\n"))
    if (id=="f-test-fe") return(paste(c(panel_head,"pooled <- plm::plm(model_formula, data = pdata, model = 'pooling')","fixed <- plm::plm(model_formula, data = pdata, model = 'within')","model <- plm::pFtest(fixed, pooled)","result <- model"),collapse="\n"))
    if (id=="mundlak") return(paste(c(panel_head,"# Add entity means of time-varying regressors to implement the correlated random-effects/Mundlak model.","model <- plm::plm(model_formula, data = pdata, model = 'random')","result <- summary(model)"),collapse="\n"))
    if (id %in% c("pcse","driscoll-kraay")) return(paste(c(panel_head,package_guard("lmtest"),"model <- plm::plm(model_formula, data = pdata, model = 'within')",if(id=="driscoll-kraay")"result <- lmtest::coeftest(model, vcov. = plm::vcovSCC(model, type = 'HC1'))" else "result <- lmtest::coeftest(model, vcov. = plm::vcovBK(model, type = 'HC1'))"),collapse="\n"))
    if (id %in% c("dynamic-panel","diff-gmm","sys-gmm")) {
      trans <- if(id=="diff-gmm")"d" else "ld"
      return(paste(c(panel_head,sprintf("# Specify lagged instruments explicitly. Current default uses lags %s:%s of the dependent variable.",input$instrument_lag_min %||% 2,input$instrument_lag_max %||% 4),sprintf("gmm_formula <- stats::as.formula(paste(deparse(model_formula), '| lag(%s, %s:%s)'))",escape_name(input$dependent),input$instrument_lag_min %||% 2,input$instrument_lag_max %||% 4),sprintf("model <- plm::pgmm(gmm_formula, data = pdata, effect = 'individual', model = %s, transformation = %s, collapse = %s)",qstr(if((input$gmm_steps %||% "twosteps")=="onestep")"onestep" else "twosteps"),qstr(trans),toupper(as.character(input$collapse_instruments %||% TRUE))),"result <- summary(model, robust = TRUE)"),collapse="\n"))
    }
    if (id %in% c("panel-ardl-pmg","panel-mg","panel-dfe")) return(paste(c(panel_head,package_guard("plm"),"# Panel ARDL/PMG specifications require variable-specific lags and a sufficiently long time dimension.","model <- plm::pmg(model_formula, data = pdata)","result <- summary(model)"),collapse="\n"))
    if (id %in% c("cce","amg")) return(paste(c(panel_head,"model <- plm::pcce(model_formula, data = pdata, model = 'mg')","result <- summary(model)"),collapse="\n"))
    if (id=="pesaran-cd") return(paste(c(panel_head,"base_model <- plm::plm(model_formula, data = pdata, model = 'within')","model <- plm::pcdtest(base_model, test = 'cd')","result <- model"),collapse="\n"))
  }

  # IV / GMM
  if (id %in% c("iv","2sls","liml","weak-instrument-f","sargan","durbin-wu-hausman","anderson-rubin")) {
    exo <- setdiff(input$independents %||% character(), input$endogenous_vars %||% character())
    rhs1 <- paste(escape_name(input$independents %||% character()),collapse=" + ")
    rhs2 <- paste(escape_name(unique(c(exo,input$instruments %||% character()))),collapse=" + ")
    ivf <- qstr(paste(escape_name(input$dependent),"~",rhs1,"|",rhs2))
    return(paste(c(base_prelude(method,input),package_guard("AER"),sprintf("iv_formula <- stats::as.formula(%s)",ivf),"model <- AER::ivreg(iv_formula, data = analysis_data)","result <- summary(model, diagnostics = TRUE)"),collapse="\n"))
  }
  if (id %in% c("gmm","cue-gmm")) return(paste(c(base_prelude(method,input),package_guard("gmm"),"moment_function <- function(theta, x) { stop('Define moment conditions in Advanced mode.') }","model <- gmm::gmm(moment_function, x = analysis_data, t0 = rep(0, 1))","result <- summary(model)"),collapse="\n"))

  # Limited dependent variables
  if (id %in% c("logit","probit","cloglog","poisson","fractional-logit")) {
    family_code <- if(id=="poisson")"stats::poisson(link='log')" else sprintf("stats::binomial(link=%s)",qstr(if(id=="fractional-logit")"logit" else id))
    return(paste(c(p,sprintf("model <- stats::glm(model_formula, data = analysis_data, family = %s)",family_code),"result <- summary(model)"),collapse="\n"))
  }
  if (id=="negbin") return(paste(c(p,package_guard("MASS"),"model <- MASS::glm.nb(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="multinomial-logit") return(paste(c(p,package_guard("nnet"),"model <- nnet::multinom(model_formula, data = analysis_data, trace = FALSE)","result <- summary(model)"),collapse="\n"))
  if (id %in% c("ordered-logit","ordered-probit")) return(paste(c(p,package_guard("MASS"),sprintf("model <- MASS::polr(model_formula, data = analysis_data, method = %s, Hess = TRUE)",qstr(if(id=="ordered-logit")"logistic" else "probit")),"result <- summary(model)"),collapse="\n"))
  if (id %in% c("zero-inflated-poisson","zero-inflated-negbin")) return(paste(c(p,package_guard("pscl"),sprintf("model <- pscl::zeroinfl(model_formula, data = analysis_data, dist = %s)",qstr(if(id=="zero-inflated-negbin")"negbin" else "poisson")),"result <- summary(model)"),collapse="\n"))
  if (id %in% c("hurdle-poisson","hurdle-negbin")) return(paste(c(p,package_guard("pscl"),sprintf("model <- pscl::hurdle(model_formula, data = analysis_data, dist = %s)",qstr(if(id=="hurdle-negbin")"negbin" else "poisson")),"result <- summary(model)"),collapse="\n"))
  if (id %in% c("tobit","censored-regression")) return(paste(c(p,package_guard("censReg"),sprintf("model <- censReg::censReg(model_formula, left = %s, right = %s, data = analysis_data)",input$left_limit %||% 0,input$right_limit %||% "Inf"),"result <- summary(model)"),collapse="\n"))
  if (id=="truncated-regression") return(paste(c(p,package_guard("truncreg"),sprintf("model <- truncreg::truncreg(model_formula, point = %s, direction = %s, data = analysis_data)",input$left_limit %||% 0,qstr(input$censor_direction %||% "left")),"result <- summary(model)"),collapse="\n"))
  if (id=="beta-regression") return(paste(c(p,package_guard("betareg"),"model <- betareg::betareg(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="heckman") return(paste(c(base_prelude(method,input),package_guard("sampleSelection"),"# Supply outcome and selection equations in Advanced mode.","selection_formula <- model_formula","outcome_formula <- model_formula","model <- sampleSelection::selection(selection_formula, outcome_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))

  # Causal
  if (id %in% c("did","event-study-did","sun-abraham")) {
    return(paste(c(p,package_guard("fixest"),sprintf("model <- fixest::feols(model_formula, data = analysis_data, fixef = c(%s, %s), cluster = %s)",qstr(input$unit_variable),qstr(input$time_variable),qstr(input$unit_variable)),"result <- summary(model)"),collapse="\n"))
  }
  if (id=="staggered-did") return(paste(c(base_prelude(method,input),package_guard("did"),sprintf("model <- did::att_gt(yname = %s, tname = %s, idname = %s, gname = %s, xformla = ~ 1, data = analysis_data)",qstr(input$dependent),qstr(input$time_variable),qstr(input$unit_variable),qstr(input$treatment_var)),"result <- did::aggte(model, type = 'dynamic')"),collapse="\n"))
  if (id %in% c("matching","psm","nearest-neighbor-matching","coarsened-exact")) return(paste(c(p,package_guard("MatchIt"),sprintf("model <- MatchIt::matchit(model_formula, data = analysis_data, method = %s, caliper = %s)",qstr(if(id=="coarsened-exact")"cem" else "nearest"),input$caliper %||% .2),"result <- summary(model)"),collapse="\n"))
  if (id=="entropy-balancing") return(paste(c(base_prelude(method,input),package_guard("WeightIt"),sprintf("model <- WeightIt::weightit(model_formula, data = analysis_data, method = 'ebal')"),"result <- summary(model)"),collapse="\n"))
  if (id=="synthetic") return(paste(c(base_prelude(method,input),package_guard("Synth"),"# Synth requires dataprep() with treatment and control unit identifiers.","model <- NULL","result <- 'Complete Synth::dataprep arguments in Advanced mode.'"),collapse="\n"))
  if (id %in% c("rdd","fuzzy-rdd")) return(paste(c(base_prelude(method,input),package_guard("rdrobust"),sprintf("model <- rdrobust::rdrobust(y = analysis_data[[%s]], x = analysis_data[[%s]], c = %s)",qstr(input$dependent),qstr(input$running_variable),input$cutoff %||% 0),"result <- summary(model)"),collapse="\n"))
  if (id=="causal-impact") return(paste(c(base_prelude(method,input),package_guard("CausalImpact"),"# Provide a pre-period and post-period using time-series row indices.","model <- CausalImpact::CausalImpact(analysis_data, pre.period = c(1, floor(nrow(analysis_data)*0.7)), post.period = c(floor(nrow(analysis_data)*0.7)+1, nrow(analysis_data)))","result <- summary(model)"),collapse="\n"))
  if (id=="causal-forest") return(paste(c(base_prelude(method,input),package_guard("grf"),sprintf("Y <- analysis_data[[%s]]",qstr(input$dependent)),sprintf("W <- analysis_data[[%s]]",qstr(input$treatment_var)),sprintf("X <- stats::model.matrix(~ %s, analysis_data)[, -1, drop=FALSE]",paste(escape_name(input$independents %||% character()),collapse=" + ")),"model <- grf::causal_forest(X, Y, W)","result <- grf::average_treatment_effect(model)"),collapse="\n"))

  # Financial
  if (id %in% c("arch","garch","garch-m","egarch","gjr-garch","tgarch","aparch","figarch")) {
    variance_model <- switch(id,arch="sGARCH",garch="sGARCH",`garch-m`="sGARCH",egarch="eGARCH",`gjr-garch`="gjrGARCH",tgarch="fGARCH",aparch="apARCH",figarch="fiGARCH","sGARCH")
    return(paste(c(base_prelude(method,input),package_guard("rugarch"),sprintf("x <- stats::na.omit(analysis_data[[%s]])",qstr(input$dependent)),sprintf("spec <- rugarch::ugarchspec(variance.model = list(model = %s, garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = %s)",qstr(variance_model),qstr(input$innovation_distribution %||% "norm")),"model <- rugarch::ugarchfit(spec, data = x)","result <- model"),collapse="\n"))
  }
  if (id=="dcc-garch") return(paste(c(base_prelude(method,input),package_guard("rmgarch"),package_guard("rugarch"),sprintf("x <- stats::na.omit(as.matrix(analysis_data[c(%s)]))",paste(qstr(unique(c(input$dependent,input$independents))),collapse=", ")),"uspec <- rugarch::multispec(replicate(ncol(x), rugarch::ugarchspec(), simplify = FALSE))","spec <- rmgarch::dccspec(uspec = uspec, dccOrder = c(1,1))","model <- rmgarch::dccfit(spec, data = x)","result <- model"),collapse="\n"))
  if (id %in% c("value-at-risk","expected-shortfall")) return(paste(c(base_prelude(method,input),sprintf("r <- stats::na.omit(analysis_data[[%s]])",qstr(input$dependent)),"alpha <- 0.05","VaR <- -stats::quantile(r, alpha)","ES <- -mean(r[r <= stats::quantile(r, alpha)])",sprintf("result <- %s",if(id=="value-at-risk")"VaR" else "ES"),"model <- NULL"),collapse="\n"))
  if (id %in% c("market-model","capm","fama-french-3","fama-french-5","apt")) return(paste(c(p,"model <- stats::lm(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))

  # Spatial
  if (id %in% c("spatial-weights","moran-i","geary-c","lisa","spatial-lag","spatial-error","spatial-durbin","sac")) {
    coords <- sprintf("cbind(analysis_data[[%s]], analysis_data[[%s]])",qstr(input$longitude),qstr(input$latitude))
    head <- c(base_prelude(method,input),package_guard("spdep"),sprintf("coords <- %s",coords),sprintf("knn <- spdep::knearneigh(coords, k = %s)",input$neighbors_k %||% 4),"nb <- spdep::knn2nb(knn)",sprintf("listw <- spdep::nb2listw(nb, style = %s)",qstr(input$spatial_style %||% "W")))
    extra <- switch(id,`spatial-weights`="model <- listw\nresult <- model",`moran-i`=sprintf("model <- spdep::moran.test(analysis_data[[%s]], listw)\nresult <- model",qstr(input$dependent)),`geary-c`=sprintf("model <- spdep::geary.test(analysis_data[[%s]], listw)\nresult <- model",qstr(input$dependent)),lisa=sprintf("model <- spdep::localmoran(analysis_data[[%s]], listw)\nresult <- model",qstr(input$dependent)),`spatial-lag`="model <- spatialreg::lagsarlm(model_formula, data = analysis_data, listw = listw)\nresult <- summary(model)",`spatial-error`="model <- spatialreg::errorsarlm(model_formula, data = analysis_data, listw = listw)\nresult <- summary(model)",`spatial-durbin`="model <- spatialreg::lagsarlm(model_formula, data = analysis_data, listw = listw, Durbin = TRUE)\nresult <- summary(model)",sac="model <- spatialreg::sacsarlm(model_formula, data = analysis_data, listw = listw)\nresult <- summary(model)")
    guards <- if(id %in% c("spatial-lag","spatial-error","spatial-durbin","sac")) package_guard("spatialreg") else character()
    return(paste(c(head,guards,extra),collapse="\n"))
  }

  # Efficiency / structural
  if (id %in% c("dea","bootstrap-dea")) return(paste(c(base_prelude(method,input),package_guard("Benchmarking"),sprintf("X <- as.matrix(analysis_data[c(%s)])",paste(qstr(input$inputs %||% character()),collapse=", ")),sprintf("Y <- as.matrix(analysis_data[c(%s)])",paste(qstr(input$outputs %||% character()),collapse=", ")),sprintf("model <- Benchmarking::dea(X, Y, RTS = %s, ORIENTATION = %s)",qstr(toupper(input$returns_to_scale %||% "vrs")),qstr(input$orientation %||% "in")),"result <- model$eff"),collapse="\n"))
  if (id=="malmquist") return(paste(c(base_prelude(method,input),package_guard("Benchmarking"),"# Malmquist requires input/output matrices and a time identifier.",sprintf("X <- as.matrix(analysis_data[c(%s)])",paste(qstr(input$inputs %||% character()),collapse=", ")),sprintf("Y <- as.matrix(analysis_data[c(%s)])",paste(qstr(input$outputs %||% character()),collapse=", ")),sprintf("model <- Benchmarking::malmquist(X, Y, ID = analysis_data[[%s]], TIME = analysis_data[[%s]])",qstr(input$entity_id %||% "id"),qstr(input$time_id %||% "time")),"result <- model"),collapse="\n"))
  if (id %in% c("sfa","cost-frontier","profit-frontier")) return(paste(c(p,package_guard("frontier"),"model <- frontier::sfa(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id %in% c("cox-survival","duration-model")) return(paste(c(base_prelude(method,input),package_guard("survival"),"# Put survival::Surv(time, status) on the formula left-hand side.","model <- survival::coxph(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="parametric-survival") return(paste(c(base_prelude(method,input),package_guard("survival"),"model <- survival::survreg(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id %in% c("sem","cfa","bayesian-sem")) return(paste(c(base_prelude(method,input),package_guard(if(id=="bayesian-sem")"blavaan" else "lavaan"),sprintf("model_syntax <- %s",qstr(input$structural_syntax %||% "")),if(id=="cfa")"model <- lavaan::cfa(model_syntax, data = analysis_data)" else if(id=="bayesian-sem")"model <- blavaan::bsem(model_syntax, data = analysis_data)" else "model <- lavaan::sem(model_syntax, data = analysis_data)","result <- summary(model, fit.measures = TRUE, standardized = TRUE)"),collapse="\n"))
  if (id=="gini") return(paste(c(base_prelude(method,input),package_guard("ineq"),sprintf("model <- ineq::ineq(analysis_data[[%s]], type = 'Gini')",qstr(input$dependent)),"result <- model"),collapse="\n"))
  if (id=="lorenz") return(paste(c(base_prelude(method,input),package_guard("ineq"),sprintf("model <- ineq::Lc(analysis_data[[%s]])",qstr(input$dependent)),"result <- model"),collapse="\n"))
  if (id=="poverty-fgt") return(paste(c(base_prelude(method,input),sprintf("x <- analysis_data[[%s]]",qstr(input$dependent)),sprintf("z <- %s; alpha <- %s",input$poverty_line %||% 1,input$fgt_alpha %||% 0),"gap <- pmax((z - x) / z, 0)","model <- mean(gap^alpha, na.rm = TRUE)","result <- model"),collapse="\n"))
  if (id=="oaxaca") return(paste(c(p,package_guard("oaxaca"),sprintf("model <- oaxaca::oaxaca(model_formula, data = analysis_data, group.weights = 0.5, R = %s)",input$bootstrap_reps %||% 500),"result <- summary(model)"),collapse="\n"))

  # Bayesian / nonparametric / ML
  if (id %in% c("bayes-lm","bayes-glm")) return(paste(c(p,package_guard("brms"),sprintf("model <- brms::brm(model_formula, data = analysis_data, chains = %s, iter = %s, warmup = %s, seed = %s)",input$chains %||% 4,input$iterations %||% 2000,input$warmup %||% 1000,input$seed %||% 1234),"result <- summary(model)"),collapse="\n"))
  if (id=="bayes-model-averaging") return(paste(c(p,package_guard("BMS"),sprintf("x <- analysis_data[c(%s, %s)]",qstr(input$dependent),paste(qstr(input$independents),collapse=", ")),"model <- BMS::bms(x)","result <- summary(model)"),collapse="\n"))
  if (id %in% c("kernel-regression","local-polynomial")) return(paste(c(p,package_guard("np"),"model <- np::npreg(model_formula, data = analysis_data)","result <- summary(model)"),collapse="\n"))
  if (id=="kernel-density") return(paste(c(base_prelude(method,input),sprintf("model <- stats::density(analysis_data[[%s]], na.rm = TRUE)",qstr(input$dependent)),"result <- model"),collapse="\n"))
  if (id %in% c("bootstrap","wild-bootstrap","block-bootstrap")) return(paste(c(p,package_guard("boot"),"statistic <- function(d, idx) stats::coef(stats::lm(model_formula, data = d[idx, , drop = FALSE]))",sprintf("model <- boot::boot(analysis_data, statistic, R = %s)",input$bootstrap_reps %||% 500),"result <- model"),collapse="\n"))
  if (id=="permutation-test") return(paste(c(base_prelude(method,input),package_guard("coin"),"model <- coin::independence_test(model_formula, data = analysis_data)","result <- model"),collapse="\n"))
  if (id %in% c("random-forest","quantile-forest")) return(paste(c(p,package_guard("ranger"),sprintf("model <- ranger::ranger(model_formula, data = analysis_data, num.trees = %s, importance = 'permutation')",input$trees %||% 500),"result <- model"),collapse="\n"))
  if (id=="gradient-boosting") return(paste(c(p,package_guard("xgboost"),sprintf("X <- stats::model.matrix(%s, analysis_data)[, -1, drop=FALSE]",fcode),sprintf("y <- analysis_data[[%s]]",qstr(input$dependent)),sprintf("model <- xgboost::xgboost(data = X, label = y, nrounds = %s, max_depth = %s, verbose = 0)",input$trees %||% 500,input$max_depth %||% 6),"result <- model"),collapse="\n"))
  if (id=="svm") return(paste(c(p,package_guard("e1071"),"model <- e1071::svm(model_formula, data = analysis_data)","result <- model"),collapse="\n"))
  if (id=="knn") return(paste(c(base_prelude(method,input),package_guard("class"),"# KNN requires explicit training and test matrices; complete the split in Advanced mode.","model <- NULL","result <- 'Configure training/test matrices for class::knn().'"),collapse="\n"))
  if (id %in% c("causal-bart","bart")) return(paste(c(base_prelude(method,input),package_guard("dbarts"),sprintf("X <- stats::model.matrix(~ %s, analysis_data)[, -1, drop=FALSE]",paste(escape_name(input$independents %||% character()),collapse=" + ")),sprintf("y <- analysis_data[[%s]]",qstr(input$dependent)),"model <- dbarts::bart(X, y)","result <- model"),collapse="\n"))
  if (id=="generalized-random-forest") return(paste(c(base_prelude(method,input),package_guard("grf"),sprintf("X <- stats::model.matrix(~ %s, analysis_data)[, -1, drop=FALSE]",paste(escape_name(input$independents %||% character()),collapse=" + ")),sprintf("y <- analysis_data[[%s]]",qstr(input$dependent)),"model <- grf::regression_forest(X, y)","result <- model"),collapse="\n"))

  if (id %in% c("custom-r","custom-script")) return(paste(c("# Custom R workspace","analysis_data <- data","# Enter your R code below. Assign the main fitted object to `model` and the displayed result to `result`.","model <- NULL","result <- summary(analysis_data)"),collapse="\n"))

  paste(generic_scaffold(method,input),collapse="\n")
}

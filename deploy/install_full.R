source("deploy/install_core.R")
advanced <- c("ARDL","fixest","did","MatchIt","WeightIt","rdrobust","Synth","CausalImpact","rugarch","rmgarch","spdep","spatialreg","Benchmarking","frontier","lavaan","brms","glmnet","mgcv","lme4","sampleSelection","censReg","truncreg","betareg","ranger","xgboost","grf","dbarts","boot","psych","ineq","oaxaca","systemfit","cointReg","gmm","FinTS","np","coin","BMS","zoo","e1071")
missing <- advanced[!vapply(advanced, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, dependencies = TRUE)
message("Full profile installation attempted. Review any packages that require system libraries or are unavailable for your R version.")

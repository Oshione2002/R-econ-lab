options(repos = c(CRAN = "https://cloud.r-project.org"))
core <- c("shiny","bslib","DT","htmltools","jsonlite","readxl","haven","ggplot2","broom","knitr","lmtest","sandwich","car","plm","tseries","urca","vars","forecast","AER","MASS","quantreg","nnet","pscl","survival","rsconnect")
missing <- core[!vapply(core, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, dependencies = TRUE)
message("Core profile ready.")

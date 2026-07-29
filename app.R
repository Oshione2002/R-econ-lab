source("R/utils.R", local = TRUE)
source("R/registry.R", local = TRUE)
source("R/data_io.R", local = TRUE)
source("R/options_ui.R", local = TRUE)
source("R/code_generator.R", local = TRUE)
source("R/execution.R", local = TRUE)
source("R/reporting.R", local = TRUE)

required_app_packages <- c("shiny", "bslib", "DT", "htmltools", "jsonlite")
missing_app_packages <- required_app_packages[!vapply(required_app_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_app_packages)) stop("Install required app packages: ", paste(missing_app_packages, collapse = ", "))

library(shiny)
library(bslib)

registry <- load_method_registry()
allow_install <- identical(tolower(Sys.getenv("ALLOW_PACKAGE_INSTALL", "false")), "true")
allow_custom <- identical(tolower(Sys.getenv("ALLOW_CUSTOM_CODE", "false")), "true")

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#3157d5",
  secondary = "#667085",
  success = "#12805c",
  danger = "#c7364f",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

status_badge <- function(status) {
  cls <- if (identical(status, "Method-aware adapter")) "badge-ready" else if (identical(status, "Diagnostic scaffold")) "badge-diagnostic" else "badge-scaffold"
  span(class = paste("method-badge", cls), status)
}

ui <- page_navbar(
  title = div(class="brand-wrap", span(class="brand-mark", "R"), div(strong("Econometrics Studio"), tags$small("Complete analysis workflow"))),
  theme = app_theme,
  window_title = "R Econometrics Studio",
  id = "main_nav",
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js")
  ),
  nav_panel("Dashboard", icon = icon("gauge-high"),
    div(class="page-shell",
      div(class="hero-panel",
        div(class="hero-copy",
          div(class="eyebrow", "R-POWERED ECONOMETRICS PLATFORM"),
          h1("Run a complete econometric analysis from data to report."),
          p("Import data, select any registered method, control lags and estimator arguments, inspect generated R code, run supported adapters, diagnose results, and export a reproducible report."),
          div(class="hero-actions", actionButton("open_data", "Start with data", class="btn btn-primary btn-lg"), actionButton("open_methods", "Explore methods", class="btn btn-light btn-lg"))
        ),
        div(class="hero-metrics",
          div(class="metric-card", span(class="metric-value", nrow(registry)), span("registered methods")),
          div(class="metric-card", span(class="metric-value", length(unique(registry$category))), span("econometric areas")),
          div(class="metric-card", span(class="metric-value", sum(registry$status == "Method-aware adapter")), span("code adapters")),
          div(class="metric-card", span(class="metric-value", "100%"), span("visible R code"))
        )
      ),
      layout_columns(
        col_widths = c(4,4,4),
        card(class="feature-card", card_header(icon("database"), " Data Studio"), p("Upload CSV, Excel, Stata, SPSS, SAS and R files. Profile variables and create transformations without changing the original source file.")),
        card(class="feature-card", card_header(icon("sliders"), " Advanced Model Builder"), p("Set dependent variables, regressors, lags, deterministic terms, panel identifiers, instruments, covariance estimators and package-specific controls.")),
        card(class="feature-card", card_header(icon("file-code"), " Reproducible workflow"), p("Every analysis produces editable R syntax, captured warnings, model output, downloadable code and an HTML report with session information."))
      ),
      card(class="workflow-card", card_header("Analysis workflow"),
        div(class="workflow-steps",
          lapply(seq_along(c("Create project","Import and clean","Choose method","Configure options","Run and diagnose","Export report")), function(i) {
            div(class="workflow-step", span(class="step-number", i), span(c("Create project","Import and clean","Choose method","Configure options","Run and diagnose","Export report")[[i]]))
          })
        )
      )
    )
  ),
  nav_panel("Data Studio", icon = icon("table"),
    div(class="page-shell",
      layout_sidebar(
        sidebar = sidebar(width = 350,
          h4("Project"),
          textInput("project_title", "Project title", "Untitled econometric study"),
          textAreaInput("project_topic", "Research topic or objective", rows = 3, placeholder = "Describe the study and intended analysis."),
          hr(),
          h4("Import data"),
          fileInput("data_file", "Upload dataset", accept = c(".csv", ".txt", ".xlsx", ".xls", ".sav", ".dta", ".sas7bdat", ".rds", ".RData", ".rdata")),
          div(class="button-row", actionButton("load_macro", "Macro sample", class="btn btn-outline-primary"), actionButton("load_panel", "Panel sample", class="btn btn-outline-primary")),
          hr(),
          h4("Create transformed variable"),
          selectInput("transform_variable", "Source variable", choices = NULL),
          selectInput("transform_operation", "Transformation", c("Natural log"="log","log(1+x)"="log1p","Difference"="difference","Lag"="lag","Lead"="lead","Growth rate (%)"="growth","Square"="square","Standardise"="standardize","Normalise 0–1"="normalize","Winsorise"="winsorize","Factor"="factor","Dummy coding"="dummy")),
          textInput("transform_name", "New variable name"),
          numericInput("transform_lag", "Lag/order", 1, min = 1, max = 40),
          actionButton("apply_transform", "Apply transformation", class="btn btn-primary w-100")
        ),
        navset_card_tab(
          nav_panel("Data", DT::DTOutput("data_table")),
          nav_panel("Variable profile", DT::DTOutput("profile_table")),
          nav_panel("Summary", verbatimTextOutput("data_summary")),
          nav_panel("Import log", verbatimTextOutput("data_log"))
        )
      )
    )
  ),
  nav_panel("Method Explorer", icon = icon("magnifying-glass-chart"),
    div(class="page-shell",
      layout_sidebar(
        sidebar = sidebar(width=330,
          textInput("method_search", "Search methods", placeholder = "ARDL, GMM, causal, spatial…"),
          selectInput("method_category", "Category", choices = c("All", sort(unique(registry$category)))),
          selectInput("method_level", "Level", choices = c("All", sort(unique(registry$level)))),
          selectInput("method_status", "Implementation", choices = c("All", sort(unique(registry$status)))),
          helpText("A method catalogue entry is not automatically labelled executable. The implementation badge distinguishes tested adapters from generic R scaffolds.")
        ),
        card(
          card_header(div(class="card-title-row", span("Method catalogue"), uiOutput("method_count"))),
          DT::DTOutput("method_table")
        )
      )
    )
  ),
  nav_panel("Model Builder", icon = icon("diagram-project"),
    div(class="page-shell",
      layout_sidebar(
        sidebar = sidebar(width=380, open="always",
          selectizeInput("selected_method", "Econometric method", choices = stats::setNames(registry$id, paste0(registry$name, " · ", registry$package)), options = list(placeholder="Search 460 methods…")),
          uiOutput("method_meta"),
          hr(),
          common_model_ui(),
          hr(),
          h5("Method-specific options"),
          uiOutput("method_options"),
          hr(),
          textAreaInput("additional_arguments", "Additional R arguments or notes", rows=4, placeholder="Advanced package-specific options."),
          div(class="run-row", actionButton("generate_code", "Generate code", class="btn btn-outline-primary"), actionButton("run_model", "Run analysis", class="btn btn-primary"))
        ),
        navset_card_tab(id="builder_tabs",
          nav_panel("Specification", uiOutput("specification_card")),
          nav_panel("Generated R code", div(class="code-toolbar", actionButton("copy_code", "Copy", icon=icon("copy"), class="btn btn-sm btn-light"), downloadButton("download_code", "Download .R", class="btn btn-sm btn-light")), tags$pre(class="code-editor", textOutput("generated_code"))),
          nav_panel("Package status", DT::DTOutput("package_status_table"))
        )
      )
    )
  ),
  nav_panel("Results", icon = icon("square-poll-vertical"),
    div(class="page-shell",
      uiOutput("run_status"),
      layout_columns(
        col_widths = c(3,3,3,3),
        value_box(title="Method", value=textOutput("result_method", inline=TRUE), showcase=icon("flask")),
        value_box(title="Observations", value=textOutput("result_n", inline=TRUE), showcase=icon("list-ol")),
        value_box(title="Elapsed", value=textOutput("result_elapsed", inline=TRUE), showcase=icon("stopwatch")),
        value_box(title="Warnings", value=textOutput("result_warnings_count", inline=TRUE), showcase=icon("triangle-exclamation"))
      ),
      navset_card_tab(
        nav_panel("Summary", verbatimTextOutput("result_summary")),
        nav_panel("Coefficients", DT::DTOutput("coefficient_table")),
        nav_panel("Model metrics", DT::DTOutput("glance_table")),
        nav_panel("Diagnostics", verbatimTextOutput("diagnostic_output")),
        nav_panel("Residual plots", plotOutput("residual_plot", height="520px")),
        nav_panel("Warnings and console", h5("Warnings"), verbatimTextOutput("warning_output"), h5("Console"), verbatimTextOutput("console_output"))
      )
    )
  ),
  nav_panel("Code Studio", icon = icon("code"),
    div(class="page-shell",
      card(
        card_header(div(class="card-title-row", span("Editable R script"), div(actionButton("reset_code", "Reset generated code", class="btn btn-sm btn-light"), actionButton("run_custom_code", "Run code", class="btn btn-sm btn-primary")))),
        textAreaInput("code_editor", NULL, rows = 25, width="100%", placeholder="# Generated R code will appear here."),
        if (!allow_custom) div(class="security-note", icon("lock"), " Custom-code execution is disabled by default on public deployments. Generated method code can still be run from Model Builder.")
      )
    )
  ),
  nav_panel("Report", icon = icon("file-lines"),
    div(class="page-shell",
      layout_columns(
        col_widths=c(7,5),
        card(card_header("Report preview"), uiOutput("report_preview")),
        card(card_header("Export"),
          p("Export the current analysis with results, warnings, reproducible R code and session information."),
          downloadButton("download_report", "Download HTML report", class="btn btn-primary w-100"),
          br(),br(),
          downloadButton("download_project", "Download project JSON", class="btn btn-outline-primary w-100"),
          hr(),
          h5("Deployment readiness"),
          verbatimTextOutput("deployment_status")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    data = NULL,
    original_data = NULL,
    data_name = NULL,
    log = character(),
    generated_code = "",
    run = NULL,
    standardized = NULL,
    method = NULL,
    history = list()
  )

  set_data <- function(data, name) {
    validate(need(is.data.frame(data), "The imported object must be a data frame."))
    state$data <- data
    state$original_data <- data
    state$data_name <- name
    state$log <- c(state$log, sprintf("%s — Loaded %s (%s rows × %s columns)", format(Sys.time(), "%H:%M:%S"), name, nrow(data), ncol(data)))
    update_variable_controls()
  }

  update_variable_controls <- function() {
    req(state$data)
    nms <- names(state$data)
    numeric <- nms[vapply(state$data, is_numeric_column, logical(1))]
    updateSelectInput(session,"transform_variable",choices=nms)
    default_dep <- if (length(numeric)) numeric[[1]] else if (length(nms)) nms[[1]] else ""
    default_x <- head(setdiff(numeric, default_dep), 3)
    updateSelectInput(session,"dependent",choices=c("Select"="",nms), selected=default_dep)
    updateSelectizeInput(session,"independents",choices=nms,selected=default_x,server=TRUE)
    updateSelectizeInput(session,"analysis_variables",choices=nms,selected=head(numeric, 6),server=TRUE)
    updateSelectInput(session,"group_variable",choices=c("None"="",nms))
    updateSelectInput(session,"weights_var",choices=c("None"="",numeric))
    updateSelectInput(session,"time_variable",choices=c("None"="",nms))
    updateSelectInput(session,"entity_id",choices=c("Select"="",nms))
    updateSelectInput(session,"time_id",choices=c("Select"="",nms))
    updateSelectizeInput(session,"endogenous_vars",choices=nms,server=TRUE)
    updateSelectizeInput(session,"predetermined_vars",choices=nms,server=TRUE)
    updateSelectizeInput(session,"instruments",choices=nms,server=TRUE)
    updateSelectInput(session,"treatment_var",choices=c("Select"="",nms))
    updateSelectInput(session,"unit_variable",choices=c("Select"="",nms))
    updateSelectInput(session,"post_variable",choices=c("None"="",nms))
    updateSelectInput(session,"running_variable",choices=c("None"="",numeric))
    updateSelectInput(session,"longitude",choices=c("Select"="",numeric))
    updateSelectInput(session,"latitude",choices=c("Select"="",numeric))
    updateSelectizeInput(session,"inputs",choices=numeric,server=TRUE)
    updateSelectizeInput(session,"outputs",choices=numeric,server=TRUE)
    updateSelectizeInput(session,"grouping_vars",choices=nms,server=TRUE)
  }

  observeEvent(input$open_data, bslib::nav_select("main_nav", "Data Studio", session = session))
  observeEvent(input$open_methods, bslib::nav_select("main_nav", "Method Explorer", session = session))

  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch(set_data(read_uploaded_data(input$data_file), input$data_file$name), error=function(e) showNotification(conditionMessage(e), type="error", duration=NULL))
  })
  observeEvent(input$load_macro, set_data(utils::read.csv("data/sample_macro.csv", check.names=FALSE), "sample_macro.csv"))
  observeEvent(input$load_panel, set_data(utils::read.csv("data/sample_panel.csv", check.names=FALSE), "sample_panel.csv"))

  observeEvent(input$apply_transform, {
    req(state$data)
    tryCatch({
      state$data <- apply_transformation(state$data,input$transform_variable,input$transform_operation,input$transform_name,as.integer(input$transform_lag))
      state$log <- c(state$log,sprintf("%s — Created %s using %s(%s)",format(Sys.time(),"%H:%M:%S"),input$transform_name,input$transform_operation,input$transform_variable))
      update_variable_controls()
      showNotification("Transformation added.",type="message")
    },error=function(e) showNotification(conditionMessage(e),type="error"))
  })

  output$data_table <- DT::renderDT({req(state$data);DT::datatable(state$data,options=list(pageLength=15,scrollX=TRUE),filter="top",rownames=FALSE)})
  output$profile_table <- DT::renderDT({req(state$data);DT::datatable(data_profile(state$data),options=list(pageLength=25,scrollX=TRUE),rownames=FALSE)})
  output$data_summary <- renderPrint({req(state$data);summary(state$data)})
  output$data_log <- renderText(paste(state$log,collapse="\n"))

  filtered_methods <- reactive({
    x <- registry
    if (!identical(input$method_category %||% "All","All")) x <- x[x$category==input$method_category,,drop=FALSE]
    if (!identical(input$method_level %||% "All","All")) x <- x[x$level==input$method_level,,drop=FALSE]
    if (!identical(input$method_status %||% "All","All")) x <- x[x$status==input$method_status,,drop=FALSE]
    q <- tolower(trimws(input$method_search %||% ""))
    if (nzchar(q)) x <- x[grepl(q,tolower(paste(x$name,x$short,x$package,x$category,x$id)),fixed=TRUE),,drop=FALSE]
    x
  })
  output$method_count <- renderUI(span(class="count-chip",sprintf("%s methods",nrow(filtered_methods()))))
  output$method_table <- DT::renderDT({
    x <- filtered_methods()[,c("name","category","package","level","status","short")]
    names(x) <- c("Method","Category","R package","Level","Implementation","Purpose")
    DT::datatable(x,selection="single",filter="top",rownames=FALSE,options=list(pageLength=20,scrollX=TRUE,columnDefs=list(list(width="280px",targets=0),list(width="360px",targets=5))))
  })
  observeEvent(input$method_table_rows_selected,{
    idx <- input$method_table_rows_selected
    if(length(idx)) { id <- filtered_methods()$id[idx]; updateSelectizeInput(session,"selected_method",selected=id); bslib::nav_select("main_nav", "Model Builder", session = session) }
  })

  selected_method_record <- reactive({method_record(registry,input$selected_method %||% registry$id[[1]])})
  output$method_meta <- renderUI({
    m <- selected_method_record()
    div(class="method-meta",div(class="method-meta-head",strong(m$name),status_badge(m$status)),p(m$short),div(class="meta-grid",span(icon("box"),paste("Package:",m$package)),span(icon("layer-group"),m$category),span(icon("graduation-cap"),m$level)))
  })
  output$method_options <- renderUI({
    m<-selected_method_record(); nms<-if(is.null(state$data))character() else names(state$data); nums<-if(is.null(state$data))character() else nms[vapply(state$data,is_numeric_column,logical(1))]
    method_specific_ui(m,nms,nums)
  })

  collect_inputs <- reactive({
    reactiveValuesToList(input)
  })

  output$specification_card <- renderUI({
    m<-selected_method_record();
    div(class="spec-card",
      div(class="spec-header",div(h3(m$name),p(m$short)),status_badge(m$status)),
      div(class="spec-grid",
        div(class="spec-item",small("Dependent variable"),strong(input$dependent %||% "Not selected")),
        div(class="spec-item",small("Regressors"),strong(if(length(input$independents))paste(input$independents,collapse=", ") else "None selected")),
        div(class="spec-item",small("Package"),strong(m$package)),
        div(class="spec-item",small("Function"),strong(m$fn)),
        div(class="spec-item",small("Data"),strong(state$data_name %||% "No dataset")),
        div(class="spec-item",small("Rows"),strong(if(is.null(state$data))0 else nrow(state$data)))
      ),
      hr(), h5("Available diagnostics"), p(m$diagnostics_text)
    )
  })

  create_code <- function() {
    req(state$data)
    m<-selected_method_record(); inp<-collect_inputs()
    code<-generate_r_code(m,inp)
    if(nzchar(input$additional_arguments %||% "")) code<-paste(code,"","# Additional arguments / notes",input$additional_arguments,sep="\n")
    state$generated_code<-code; state$method<-m; updateTextAreaInput(session,"code_editor",value=code)
    code
  }
  observeEvent(input$generate_code,{tryCatch({create_code();bslib::nav_select("builder_tabs", "Generated R code", session = session)},error=function(e)showNotification(conditionMessage(e),type="error"))})
  output$generated_code <- renderText(state$generated_code %||% "Select a dataset and method, then generate code.")
  observeEvent(input$reset_code,{updateTextAreaInput(session,"code_editor",value=state$generated_code)})

  run_code <- function(code, method, custom=FALSE) {
    req(state$data)
    withProgress(message=paste("Running",method$name),value=.2,{
      run <- execute_generated_code(code,state$data,allow_custom=if(custom)allow_custom else TRUE,method_id=if(custom)"custom-script" else method$id)
      incProgress(.5,detail="Formatting output")
      standardized <- standardize_result(run)
      state$run<-run;state$standardized<-standardized;state$method<-method
      state$history<-append(state$history,list(list(time=Sys.time(),method=method$id,code=code,elapsed=run$elapsed)))
      incProgress(.3)
    })
    bslib::nav_select("main_nav", "Results", session = session)
  }
  observeEvent(input$run_model,{
    tryCatch({code<-create_code();run_code(code,selected_method_record())},error=function(e){state$run<-NULL;state$standardized<-NULL;showNotification(conditionMessage(e),type="error",duration=NULL)})
  })
  observeEvent(input$run_custom_code,{
    tryCatch(run_code(input$code_editor,method_record(registry,"custom-script"),custom=TRUE),error=function(e)showNotification(conditionMessage(e),type="error",duration=NULL))
  })

  output$run_status <- renderUI({
    if(is.null(state$standardized)) div(class="empty-state",icon("flask"),h3("No completed analysis"),p("Configure and run a model from Model Builder."))
    else div(class="success-banner",icon("circle-check"),div(strong("Analysis completed"),span(sprintf("%s finished in %.3f seconds.",state$method$name,state$standardized$elapsed))))
  })
  output$result_method <- renderText(if(is.null(state$method))"—" else state$method$name)
  output$result_n <- renderText(if(is.null(state$data))"—" else format(nrow(state$data),big.mark=","))
  output$result_elapsed <- renderText(if(is.null(state$standardized))"—" else sprintf("%.3fs",state$standardized$elapsed))
  output$result_warnings_count <- renderText(if(is.null(state$standardized))"—" else length(state$standardized$warnings))
  output$result_summary <- renderText(if(is.null(state$standardized))"No results." else state$standardized$summary_text)
  output$coefficient_table <- DT::renderDT({req(state$standardized);x<-state$standardized$coefficient_table;validate(need(!is.null(x),"No standard coefficient table is available for this result type."));DT::datatable(x,rownames=FALSE,options=list(scrollX=TRUE,pageLength=20))})
  output$glance_table <- DT::renderDT({req(state$standardized);x<-state$standardized$glance_table;validate(need(!is.null(x),"No model-metric table is available for this result type."));DT::datatable(x,rownames=FALSE,options=list(scrollX=TRUE))})
  output$warning_output <- renderText(if(is.null(state$standardized)||!length(state$standardized$warnings))"No captured warnings." else paste(state$standardized$warnings,collapse="\n"))
  output$console_output <- renderText(if(is.null(state$standardized)||!length(state$standardized$console))"No console output was captured." else paste(state$standardized$console,collapse="\n"))
  output$diagnostic_output <- renderText({
    req(state$run);m<-state$run$model
    if(is.null(m)) return("This result type does not expose a fitted model for automatic residual diagnostics.")
    out<-character()
    if(inherits(m,"lm")){
      out<-c(out,"Residual summary:",capture.output(summary(residuals(m))))
      if(requireNamespace("lmtest",quietly=TRUE)) out<-c(out,"","Breusch-Pagan:",capture.output(lmtest::bptest(m)),"","Durbin-Watson:",capture.output(lmtest::dwtest(m)))
      if(requireNamespace("car",quietly=TRUE)) out<-c(out,"","Variance inflation factors:",capture.output(tryCatch(car::vif(m),error=function(e)e$message)))
    } else out<-c("Automatic diagnostic suite is not defined for this model class.",paste("Class:",paste(class(m),collapse=", ")))
    paste(out,collapse="\n")
  })
  output$residual_plot <- renderPlot({
    req(state$run);m<-state$run$model;validate(need(!is.null(m),"No fitted model is available."))
    if(inherits(m,"lm")){op<-par(mfrow=c(2,2));on.exit(par(op));plot(m)} else if(!is.null(state$standardized$plot_data)&&all(c(".fitted",".resid")%in%names(state$standardized$plot_data))){plot(state$standardized$plot_data$.fitted,state$standardized$plot_data$.resid,xlab="Fitted",ylab="Residuals");abline(h=0,lty=2)} else plot.new()
  })

  output$package_status_table <- DT::renderDT({
    pkgs<-sort(unique(registry$package));pkgs<-pkgs[!pkgs%in%c("base","stats","graphics","utils")]
    x<-data.frame(package=pkgs,installed=vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1)),methods=vapply(pkgs,function(p)sum(registry$package==p),integer(1)))
    DT::datatable(x,rownames=FALSE,options=list(pageLength=25,scrollX=TRUE))
  })

  output$report_preview <- renderUI({
    if(is.null(state$standardized)) div(class="empty-state compact",h4("Nothing to report yet"),p("Run an analysis first."))
    else tagList(h3(input$project_title),p(strong("Method: "),state$method$name),h4("Result excerpt"),tags$pre(class="report-excerpt",substr(state$standardized$summary_text,1,5000)),h4("Reproducible code"),tags$pre(class="report-excerpt",substr(state$generated_code,1,5000)))
  })
  output$deployment_status <- renderText({
    paste(
      paste("Hosted environment:",app_is_hosted()),
      paste("Package installation enabled:",allow_install),
      paste("Custom-code execution enabled:",allow_custom),
      paste("Registered methods:",nrow(registry)),
      paste("Installed registered packages:",sum(vapply(unique(registry$package),function(p)p%in%c("base","stats","graphics","utils")||requireNamespace(p,quietly=TRUE),logical(1)))),
      sep="\n"
    )
  })

  output$download_code <- downloadHandler(filename=function(){paste0(gsub("[^A-Za-z0-9_-]+","_",input$project_title),".R")},content=function(file)writeLines(state$generated_code,file))
  output$download_report <- downloadHandler(filename=function(){paste0(gsub("[^A-Za-z0-9_-]+","_",input$project_title),"_report.html")},content=function(file){req(state$standardized);html<-build_analysis_html(list(title=input$project_title,topic=input$project_topic),state$method,state$generated_code,state$standardized,if(is.null(state$data))NULL else data_profile(state$data));writeLines(html,file)})
  output$download_project <- downloadHandler(filename=function(){paste0(gsub("[^A-Za-z0-9_-]+","_",input$project_title),"_project.json")},content=function(file){jsonlite::write_json(list(project=list(title=input$project_title,topic=input$project_topic),data=list(name=state$data_name,rows=if(is.null(state$data))0 else nrow(state$data),columns=if(is.null(state$data))character() else names(state$data)),method=if(is.null(state$method))NULL else as.list(state$method),code=state$generated_code,history=state$history),file,pretty=TRUE,auto_unbox=TRUE,null="null")})

  observeEvent(input$copy_code, session$sendCustomMessage("copy-code", state$generated_code))

  # Load a useful sample after the browser session is ready.
  session$onFlushed(function() {
    if (is.null(state$data)) set_data(utils::read.csv("data/sample_macro.csv", check.names = FALSE), "sample_macro.csv")
  }, once = TRUE)
}

shinyApp(ui, server)

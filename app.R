# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – Structural Insights, Simplified
# Shiny app for Structural Equation Modeling with mean structures,
# embedded logo, and enhanced comments
# ---------------------------------------------------------------
options(
  shiny.fullstacktrace = TRUE,
  shiny.reactlog       = TRUE
)

# ---- Libraries --------------------------------------------------
library(shiny)          # Shiny framework
library(shinyjs)        # JavaScript integration
library(DT)             # DataTables
library(rhandsontable)  # Handsontable
library(readflex)       # Fast CSV reading
library(lavaan)         # SEM engine
library(DiagrammeR)     # Graph rendering
library(semDiagram)     # Path diagrams
library(ggplot2)        # Plotting
library(reshape2)       # Data reshaping
library(markdown)       # includeMarkdown()

`%||%` <- function(x, y) if (!is.null(x)) x else y
Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")  # allow Japanese variable names

# ---- Helper: approximation equations ---------------------------
lavaan_to_equations <- function(fit, standardized = TRUE, digits = 3) {
  pe <- parameterEstimates(fit, standardized = standardized, remove.def = TRUE)
  build_eq <- function(op) split(pe[pe$op == op, ], pe$lhs[pe$op == op])
  format_lines <- function(lst, sep) {
    unlist(lapply(names(lst), function(lhs) {
      df  <- lst[[lhs]]
      rhs <- paste0(round(df$est, digits), "*", df$rhs)
      paste(lhs, sep, paste(rhs, collapse = " + "))
    }))
  }
  c(format_lines(build_eq("=~"), "=~"),
    format_lines(build_eq("~"),  "~"))
}

# ---- Helper: load data only once -------------------------------
loadDataOnce <- function(fileInput) {
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}

# ================================================================
#                               UI
# ================================================================
ui <- fluidPage(
  useShinyjs(),

  # ---------- global styles ----------
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$style(HTML("
      #app-logo { position: absolute; top: 8px; right: 16px; }
      /* neutral modal look (no special colours) */
      .modal-header    { background: #f8f9fa; }
      .modal-title     { font-weight: bold; }
      /* grey-out read-only rhandsontable cells */
      .htDimmed        { background-color: #d9d9d9 !important; color: #777 !important; }
      .shiny-modal .modal-content { border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
      .shiny-modal .modal-body    { padding: 20px !important; }
      .shiny-modal .modal-footer  { padding: 10px !important; }
    "))
  ),

  # ---------- app logo ----------
  div(id = "app-logo",
      img(src = "logo.png", height = 40,
          title = "Structural Insights, Simplified")),

  title = "Structura",

  # ---------- top-level tabs ----------
  tabsetPanel(
    ## ---- Data Tab ----
    tabPanel("Data",
             h4("Uploaded Data"),
             DTOutput("datatable")
    ),

    ## ---- Filtered Data Tab ----
    tabPanel("Filtered",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             DTOutput("filtered_table"),
             tags$hr(),
             h4("Correlation Heatmap"),
             plotOutput("corr_heatmap", height = "300px")
    ),

    ## ---- Model Tab ----
    tabPanel("Model",
             fluidRow(
               column(width = 7,
                      h4("Measurement Model"),
                      rHandsontableOutput("input_table"),
                      actionButton("add_row", "Add Row", class = "btn btn-primary"),
                      tags$hr(),
                      h4("Structural Model"),
                      rHandsontableOutput("checkbox_matrix"),
                      tags$hr(),
                      h4("lavaan Syntax"),
                      verbatimTextOutput("lavaan_model")
               ),
               column(width = 5,
                      h4("Fit Indices"),
                      DTOutput("fit_indices"),
                      h4("Approximate Equations"),
                      verbatimTextOutput("approx_eq"),
                      tags$hr(),
                      h4("Path Diagram Options"),
                      selectInput("layout_dir", "Layout Direction:",
                                  choices = c("Left → Right" = "LR",
                                              "Top → Bottom" = "TB"),
                                  selected = "LR"),
                      h4("Path Diagram"),
                      div(style = "max-height:45vh; overflow:auto; border:1px solid #ccc;",
                          uiOutput("sem_plot_ui"))
               )
             )
    ),

    ## ---- Details Tab ----
    tabPanel("Details",
             h4("Model Summary"),
             verbatimTextOutput("fit_summary"),
             tags$hr(),
             h4("Parameter Estimates"),
             DTOutput("param_tbl")
    ),

    ## ---- Help Tab ----
    tabPanel("Help",
             includeMarkdown("help.md")
    )
  )
)

# ================================================================
#                            SERVER
# ================================================================
server <- function(input, output, session) {

  # ---------- data-loading modal ----------
  showModal(
    modalDialog(
      title = span(icon("upload"), "Load Data"),
      fileInput("datafile", NULL,
                buttonLabel = "Browse…",
                placeholder  = "Upload CSV",
                accept       = c(".csv", "text/csv", "application/csv")),
      tags$hr(),
      radioButtons("sample_ds", "Or choose a demo dataset:",
                   choices = c("None", "HolzingerSwineford1939",
                               "PoliticalDemocracy", "Demo.growth",
                               "Demo.twolevel", "FacialBurns")),
      easyClose = FALSE,
      footer    = NULL
    )
  )

  # ---------- dataset reactive store ----------
  data <- reactiveVal(NULL)

  observeEvent(input$datafile, {
    data(loadDataOnce(input$datafile))
    updateRadioButtons(session, "sample_ds", selected = "None")
    removeModal()
  })

  observeEvent(input$sample_ds, {
    req(input$sample_ds != "None")
    ds <- switch(input$sample_ds,
                 "HolzingerSwineford1939" = HolzingerSwineford1939,
                 "PoliticalDemocracy"    = PoliticalDemocracy,
                 "Demo.growth"           = Demo.growth,
                 "Demo.twolevel"         = Demo.twolevel,
                 "FacialBurns"           = FacialBurns)
    data(ds)
    removeModal()
  })

  # ---------- raw data table ----------
  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter = "top", editable = FALSE,
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # ---------- log-transform UI ----------
  output$log_transform_ui <- renderUI({
    req(data())
    nums  <- names(data())[sapply(data(), is.numeric)]
    valid <- nums[sapply(data()[nums], function(x) min(x, na.rm = TRUE) > 0)]
    if (!length(valid)) return()
    checkboxGroupInput("log_columns", "Log-transform columns (log10):",
                       choices = valid, inline = TRUE)
  })

  # ---------- processed data ----------
  processed_data <- reactive({
    req(data())

    idx <- as.numeric(unlist(input$datatable_rows_all))
    if (!length(idx)) idx <- seq_len(nrow(data()))
    df <- data()[idx, , drop = FALSE]
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

    # log10 transform
    if (!is.null(input$log_columns)) {
      for (col in input$log_columns) {
        df[[paste0("log_", col)]] <- log10(df[[col]])
        df[[col]] <- NULL
      }
    }

    # one-hot encode low-cardinality character vars
    chars <- names(df)[vapply(df, is.character, logical(1))]
    multi <- chars[vapply(df[chars], function(x) {
      u <- unique(x); length(u) > 1 && length(u) < nrow(df)
    }, logical(1))]
    if (length(multi)) {
      mm <- model.matrix(~ . - 1, data = df[multi])
      df <- cbind(df[setdiff(names(df), multi)],
                  as.data.frame(mm, check.names = TRUE))
    }
    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  # ---------- display-column selector ----------
  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    numeric_orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
    default <- intersect(c(numeric_orig, logs), names(df))
    checkboxGroupInput("display_columns", "Display columns:",
                       choices = names(df), selected = default, inline = TRUE)
  })

  # ---------- filtered data table ----------
  output$filtered_table <- renderDT({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    datatable(df, editable = FALSE, options = list(pageLength = 10))
  })

  # ---------- correlation heat-map ----------
  output$corr_heatmap <- renderPlot({
    df <- processed_data()
    corr_cols <- input$display_columns
    validate(need(length(corr_cols) >= 2,
                  "Select at least two columns to view the heatmap."))
    cm <- cor(df[, corr_cols], use = "pairwise.complete.obs")
    mf <- melt(round(cm, 3))
    ggplot(mf, aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      geom_text(aes(label = sprintf('%.3f', value))) +
      scale_fill_gradient2(midpoint = 0) +
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Correlation")
  })

  # ---------- initialise measurement table ----------
  input_table_data <- reactiveVal(NULL)

  observeEvent(input$display_columns, ignoreNULL = FALSE, {
    inds <- as.character(input$display_columns %||% names(processed_data()))
    init <- data.frame(Latent    = "LatentVariable1",
                       Indicator = "",
                       Operator  = "=~",
                       matrix(FALSE, nrow = 1, ncol = length(inds)),
                       stringsAsFactors = FALSE)
    colnames(init) <- c("Latent", "Indicator", "Operator", inds)
    input_table_data(init)
  })

  # ---------- measurement rhandsontable ----------
  output$input_table <- renderRHandsontable({
    df <- input_table_data(); req(df)
    rh <- rhandsontable(df, rowHeaders = FALSE) %>%
      hot_table(highlightReadOnly = TRUE)
    rh <- hot_col(rh, "Latent")
    rh <- hot_col(rh, "Indicator", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (nm in setdiff(colnames(df), c("Latent", "Indicator", "Operator")))
      rh <- hot_col(rh, nm, type = "checkbox")
    rh
  })

  observeEvent(input$input_table, {
    tbl <- hot_to_r(input$input_table); req(tbl)
    tbl$Latent    <- make.names(tbl$Latent, unique = FALSE)
    convs         <- make.unique(c(names(processed_data()), tbl$Latent))
    tbl$Indicator <- tail(convs, nrow(tbl))
    input_table_data(tbl)
  })

  observeEvent(input$add_row, {
    df <- input_table_data(); req(df)
    new_row            <- df[1, ]
    new_row[,]         <- FALSE
    new_row$Latent     <- ""
    new_row$Operator   <- "=~"
    input_table_data(rbind(df, new_row))
  })

  # ---------- structural-model rhandsontable ----------
  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    deps  <- as.character(input$display_columns %||% names(df))
    convs <- setdiff(na.omit(unique(input_table_data()$Indicator)), "")
    items <- unique(c(deps, convs))
    if (!length(items)) return()
    mat   <- data.frame(Dependent = items, Operator = "~",
                        stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE
    rh <- rhandsontable(mat, rowHeaders = FALSE) %>%
      hot_table(highlightReadOnly = TRUE)
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (col in items) rh <- hot_col(rh, col, type = "checkbox")
    # lock self-predictors
    for (i in seq_along(items))
      rh <- hot_cell(rh, row = i,
                     col = match(items[i], colnames(mat)),
                     readOnly = TRUE)
    rh
  })

  # ---------- build lavaan syntax ----------
  lavaan_model_str <- reactive({
    req(input$input_table, input$checkbox_matrix)
    meas <- hot_to_r(input$input_table)
    mlines <- lapply(seq_len(nrow(meas)), function(i) {
      lt   <- meas$Latent[i]; if (!nzchar(lt)) return(NULL)
      vars <- names(meas)[4:ncol(meas)]
      inds <- vars[as.logical(meas[i, vars])]
      if (!length(inds)) return(NULL)
      paste0(lt, " =~ ", paste(inds, collapse = " + "))
    })
    struc <- hot_to_r(input$checkbox_matrix)
    slines <- lapply(seq_len(nrow(struc)), function(i) {
      dp    <- struc$Dependent[i]; if (!nzchar(dp)) return(NULL)
      preds <- names(struc)[3:ncol(struc)]
      ps    <- preds[as.logical(struc[i, preds])]
      if (!length(ps)) return(NULL)
      paste0(dp, " ~ ", paste(ps, collapse = " + "))
    })
    unlist(c(mlines, slines))
  })

  output$lavaan_model <- renderText({
    ln <- lavaan_model_str()
    if (length(ln) == 0)
      return("Please define measurement and structural equations to view the lavaan syntax.")
    paste(ln, collapse = "\n")
  })

  # ---------- fit the model ----------
  fit_model <- reactive({
    ln <- lavaan_model_str()
    if (length(ln) == 0) return(NULL)
    sem(paste(ln, collapse = "\n"),
        data          = processed_data(),
        fixed.x       = FALSE,
        parser        = "old",     # needed for Japanese variable names
        meanstructure = TRUE)
  })

  # ---------- fit indices ----------
  output$fit_indices <- renderDT({
    ln <- lavaan_model_str()
    if (length(ln) == 0) {
      msg <- data.frame(Message = "Define a model to view fit indices.")
      return(datatable(msg, rownames = FALSE, options = list(dom = 't')))
    }
    fit <- fit_model()
    ms  <- fitMeasures(fit, c("pvalue","srmr","rmsea","aic","bic",
                              "gfi","agfi","nfi","cfi"))
    vals <- round(as.numeric(ms), 3)
    names(vals) <- names(ms)
    thr <- c(pvalue = .05, srmr = .08, rmsea = .06,
             gfi = .90, agfi = .90, nfi = .90, cfi = .90)
    fmt <- function(idx, v) {
      ok <- switch(idx,
                   pvalue = v >= thr["pvalue"],
                   srmr   = v <= thr["srmr"],
                   rmsea  = v <= thr["rmsea"],
                   gfi    = v >= thr["gfi"],
                   agfi   = v >= thr["agfi"],
                   nfi    = v >= thr["nfi"],
                   cfi    = v >= thr["cfi"], TRUE)
      if (is.na(v)) "NA"
      else if (!ok) sprintf('<span style=\"color:red;\">%.3f</span>', v)
      else sprintf('%.3f', v)
    }
    html_vals <- mapply(fmt, names(vals), vals, USE.NAMES = FALSE)
    tbl <- as.data.frame(t(html_vals), stringsAsFactors = FALSE)
    colnames(tbl) <- toupper(names(vals))
    datatable(tbl, escape = FALSE, rownames = FALSE,
              options = list(dom = 't'))
  })

  # ---------- approximate equations ----------
  output$approx_eq <- renderText({
    ln <- lavaan_model_str()
    if (length(ln) == 0)
      return("Define a model to view approximate equations.")
    paste(lavaan_to_equations(fit_model()), collapse = "\n")
  })

  # ---------- details tab ----------
  output$fit_summary <- renderPrint({
    ln <- lavaan_model_str()
    validate(need(length(ln) > 0, "Define a model to view the summary."))
    summary(fit_model(), fit.measures = TRUE)
  })

  output$param_tbl <- renderDT({
    ln <- lavaan_model_str()
    validate(need(length(ln) > 0, "Define a model to view parameter estimates."))
    datatable(parameterEstimates(fit_model()),
              options = list(pageLength = 15))
  })

  # ---------- path diagram ----------
  output$sem_plot_ui <- renderUI({
    ln <- lavaan_model_str()
    if (length(ln) == 0)
      return(div("Define a model to view the path diagram."))
    tryCatch(grVizOutput("sem_plot"),
             error = function(e)
               div(style = "color:red;", paste("Model Error:", e$message)))
  })

  output$sem_plot <- renderGrViz({
    semDiagram(fit_model(),
               standardized = TRUE,
               layout       = input$layout_dir)
  })
}

# ---- Run the application ---------------------------------------
shinyApp(ui, server)

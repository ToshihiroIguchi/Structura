# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – Structural Insights, Simplified
# Shiny app for Structural Equation Modeling with mean structures
# ---------------------------------------------------------------

options(
  shiny.fullstacktrace = TRUE,
  shiny.reactlog       = TRUE,
  shiny.sanitize.errors = TRUE
)

# ---- Libraries --------------------------------------------------

library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(readflex)
library(lavaan)
library(DiagrammeR)
library(semDiagram)
library(ggplot2)
library(reshape2)
library(markdown)

`%||%` <- function(x, y) if (!is.null(x)) x else y
Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")

# ---- Helper: Approximate Equations -----------------------------
#   * Indicator  =  intercept + loading * Latent
#   * Dependent  =  intercept + Σ( slope * Predictor )
#   * 全て Raw (非標準化) 係数で生成
# ----------------------------------------------------------------
lavaan_to_equations <- function(fit, digits = 3) {

  # ---- 係数取得（標準化しない） ------------------------------
  pe <- parameterEstimates(fit, standardized = FALSE, remove.def = TRUE)

  # ---- 数値フォーマッタ --------------------------------------
  format_est <- function(x, digits = 3) {
    sapply(x, function(v) {
      if (is.na(v)) return("NA")
      if (abs(v) < 10^(-digits))
        format(v, digits = digits, scientific = TRUE)
      else
        format(round(v, digits), nsmall = digits)
    })
  }

  # ---- データフレーム分割 ------------------------------------
  meas_df      <- pe[pe$op == "=~",  ]   # 測定方程式
  reg_df       <- pe[pe$op == "~",   ]   # 構造方程式
  intercept_df <- pe[pe$op == "~1",  ]   # 切片

  eq_lines <- character(0)

  # ---------- 1. 測定方程式 ------------
  if (nrow(meas_df)) {
    for (i in seq_len(nrow(meas_df))) {
      ind     <- meas_df$rhs[i]                # Indicator
      lat     <- meas_df$lhs[i]                # Latent
      loading <- format_est(meas_df$est[i], digits)
      int_val <- intercept_df$est[intercept_df$lhs == ind]
      rhs     <- c(if (length(int_val))
        format_est(int_val, digits) else NULL,
        paste0(loading, "*", lat))
      eq_lines <- c(eq_lines,
                    paste(ind, "=", paste(rhs, collapse = " + ")))
    }
  }

  # ---------- 2. 構造方程式 ------------
  if (nrow(reg_df)) {
    reg_split <- split(reg_df, reg_df$lhs)
    for (lhs in names(reg_split)) {
      df  <- reg_split[[lhs]]
      int <- intercept_df$est[intercept_df$lhs == lhs]
      rhs <- paste0(format_est(df$est, digits), "*", df$rhs)
      rhs <- c(if (length(int))
        format_est(int, digits) else NULL,
        rhs)
      eq_lines <- c(eq_lines,
                    paste(lhs, "=", paste(rhs, collapse = " + ")))
    }
  }

  # ---------- 出力 ---------------------
  eq_lines
}

loadDataOnce <- function(fileInput) {
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}

# ================================================================
# UI
# ================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$style(HTML("
#app-logo { position: absolute; top: 8px; right: 16px; }
.modal-header { background: #f8f9fa; }
.modal-title  { font-weight: bold; }
.htDimmed { background-color: #d9d9d9 !important; color: #777 !important; }
.shiny-modal .modal-content { border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
.shiny-modal .modal-body    { padding: 20px !important; }
.shiny-modal .modal-footer  { padding: 10px !important; }
.alert-box { background:#fff3cd;border:1px solid #ffeeba;border-radius:6px;padding:10px;margin-bottom:10px; }
#lavaan_model { white-space: pre; }
#approx_eq    { white-space: pre-wrap; }
"))
  ),
  div(id = "app-logo",
      img(src = "logo.png", height = 40,
          title = "Structural Insights, Simplified")),
  title = "Structura",

  tabsetPanel(

    # ---------------- Data tab -----------------------------------
    tabPanel("Data", h4("Uploaded Data"), DTOutput("datatable")),

    # -------------- Filtered tab ---------------------------------
    tabPanel("Filtered",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             DTOutput("filtered_table"),
             tags$hr(),
             h4("Correlation Heatmap"),
             plotOutput("corr_heatmap", height = "300px")),

    # ---------------- Model tab ----------------------------------
    tabPanel("Model",
             fluidRow(
               # ---------- Left column (inputs) -------------------
               column(width = 7,
                      # ---- Analysis options -----------------------
                      radioButtons("analysis_mode", "Analysis mode:",
                                   choices  = c("Raw (unstandardized)"  = "raw",
                                                "Standardized (scaled)" = "std"),
                                   selected = "std", inline = TRUE),
                      selectInput("missing_method", "Missing Data Handling:",
                                  choices = c(
                                    "Listwise deletion"           = "listwise",
                                    "FIML (ML)"                   = "ml",
                                    "FIML including exogenous x"  = "ml.x",
                                    "Two-stage ML"                = "two.stage",
                                    "Robust two-stage ML"         = "robust.two.stage"
                                  ),
                                  selected = "listwise"),
                      conditionalPanel(
                        condition = "input.analysis_mode == 'raw'",
                        checkboxInput("diagram_std",
                                      "Show standardized coefficients in diagram",
                                      value = TRUE)),
                      # -------------- Run button -------------------
                      actionButton("run_model", "Run / Update Model",
                                   class = "btn btn-success"),
                      tags$hr(),
                      h4("Measurement Model"),
                      rHandsontableOutput("input_table"),
                      actionButton("add_row", "Add Row", class = "btn btn-primary"),
                      tags$hr(),
                      h4("Structural Model"),
                      p("Color intensity indicates R² strength (white: low, red: high). ",
                        "Use as exploratory reference alongside theoretical knowledge.",
                        style = "font-size: 12px; color: #666; margin-bottom: 10px;"),
                      rHandsontableOutput("checkbox_matrix"),
                      tags$hr(),
                      h4("Manual Equations"),
                      textAreaInput("extra_eq",
                                    "Additional lavaan syntax (one formula per line):",
                                    value = "",
                                    placeholder = "y1 ~ x1 + x2\nlatent2 =~ y3 + y4",
                                    rows = 4,
                                    resize = "vertical"),
                      tags$hr(),
                      h4("lavaan Syntax"),
                      verbatimTextOutput("lavaan_model")
               ),

               # ---------- Right column (outputs) -----------------
               column(width = 5,
                      # ---------- Tabset for outputs ---------------
                      tabsetPanel(id = "right_tabs", type = "tabs",

                                  # ----- Diagnostics tab ---------------------
                                  tabPanel("Diagnostics",
                                           div(id = "fit_alert_box",
                                               textOutput("fit_alert"),
                                               class = "alert-box"),
                                           h4("Fit Indices"),
                                           DTOutput("fit_indices")),

                                  # ----- Equations tab -----------------------
                                  tabPanel("Equations",
                                           h4("Approximate Equations"),
                                           verbatimTextOutput("approx_eq")),

                                  # ----- Diagram settings tab ---------------
                                  tabPanel("Diagram Settings",
                                           h4("Path Diagram Options"),
                                           selectInput("layout_style", "Layout & Engine:",
                                                       choices = c(
                                                         "Hierarchical Left → Right (dot)" = "dot_LR",
                                                         "Hierarchical Top → Bottom (dot)" = "dot_TB",
                                                         "Spring model layout (neato)"      = "neato",
                                                         "Force-Directed Placement (fdp)"   = "fdp",
                                                         "Circular layout (circo)"          = "circo",
                                                         "Radial layout (twopi)"            = "twopi"
                                                       ),
                                                       selected = "dot_LR"))
                      ),
                      # ---------- Path diagram --------------------
                      h4("Path Diagram"),
                      div(style = "height:60vh; overflow-y:auto; overflow-x:hidden; border:1px solid #ccc;",
                          uiOutput("sem_plot_ui"))
               )
             )),

    # ---------------- Details tab --------------------------------
    tabPanel("Details",
             h4("Parameter Estimates"),
             DTOutput("param_tbl"),
             tags$hr(),
             h4("Model Summary"),
             verbatimTextOutput("fit_summary")),

    # ---------------- Help tab -----------------------------------
    tabPanel("Help", includeMarkdown("help.md"))
  )
)

# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {

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

  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter = "top", editable = FALSE,
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # ---------- Filtering & preprocessing --------------------------

  output$log_transform_ui <- renderUI({
    req(data())
    nums  <- names(data())[sapply(data(), is.numeric)]
    valid <- nums[sapply(data()[nums], function(x) min(x, na.rm = TRUE) > 0)]
    if (!length(valid)) return()
    checkboxGroupInput("log_columns", "Log-transform columns (log10):",
                       choices = valid, inline = TRUE)
  })

  processed_data <- reactive({
    req(data())
    idx <- as.numeric(unlist(input$datatable_rows_all))
    if (!length(idx)) idx <- seq_len(nrow(data()))
    df <- data()[idx, , drop = FALSE]
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

    # --- log10 transform -----------------------------------------
    if (!is.null(input$log_columns)) {
      col_order <- names(df)
      for (col in input$log_columns) {
        log_col      <- paste0("log_", col)
        df[[log_col]] <- log10(df[[col]])
        pos           <- match(col, col_order)
        col_order[pos] <- log_col
        df[[col]]      <- NULL
      }
      df <- df[, col_order, drop = FALSE]
    }

    # --- one-hot encode ------------------------------------------
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

    # --- standardize if requested --------------------------------
    if (input$analysis_mode == "std") {
      num_cols <- vapply(df, is.numeric, logical(1))
      df[num_cols] <- scale(df[num_cols])
    }
    df
  })

  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    numeric_orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
    default <- intersect(c(numeric_orig, logs), names(df))
    checkboxGroupInput("display_columns", "Display columns:",
                       choices = names(df), selected = default, inline = TRUE)
  })

  output$filtered_table <- renderDT({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    datatable(df, editable = FALSE, options = list(pageLength = 10))
  })

  output$corr_heatmap <- renderPlot({
    req(!is.null(input$display_columns))
    df <- processed_data()
    all_cols <- intersect(input$display_columns, names(df))
    num_cols <- all_cols[sapply(df[, all_cols, drop = FALSE], is.numeric)]
    if (length(num_cols) < 2) return(NULL)
    cm <- cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
    mf <- reshape2::melt(round(cm, 3))
    ggplot(mf, aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      geom_text(aes(label = sprintf('%.3f', value))) +
      scale_fill_gradient2(midpoint = 0) +
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Correlation")
  })

  # ---------- Measurement table ----------------------------------

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

  # ---------- Helper function for R² calculation -----------------
  
  compute_r2_matrix <- function(data, dep_vars, pred_vars) {
    r2_matrix <- matrix(0, nrow = length(dep_vars), ncol = length(pred_vars),
                        dimnames = list(dep_vars, pred_vars))
    
    for (i in seq_along(dep_vars)) {
      dep <- dep_vars[i]
      if (is.numeric(data[[dep]])) {
        for (j in seq_along(pred_vars)) {
          pred <- pred_vars[j]
          if (is.numeric(data[[pred]]) && dep != pred) {
            tryCatch({
              fit <- lm(data[[dep]] ~ data[[pred]])
              r2_matrix[i, j] <- summary(fit)$r.squared
            }, error = function(e) {
              r2_matrix[i, j] <- 0
            })
          }
        }
      }
    }
    r2_matrix
  }

  # ---------- Structural table -----------------------------------

  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    deps <- as.character(input$display_columns %||% names(df))
    meas <- input_table_data(); req(meas)
    vars <- names(meas)[4:ncol(meas)]
    row_has_indicator <- apply(meas[vars], 1, function(x) any(as.logical(x)))
    convs <- setdiff(na.omit(unique(meas$Indicator[row_has_indicator])), "")
    items <- unique(c(deps, convs))
    if (!length(items)) return()
    
    # Compute R² matrix for color coding
    r2_matrix <- compute_r2_matrix(df, items, items)
    
    mat   <- data.frame(Dependent = items, Operator = "~",
                        stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE
    rh <- rhandsontable(mat, rowHeaders = FALSE) %>%
      hot_table(highlightReadOnly = TRUE)
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    
    # Set diagonal cells as readOnly before applying renderers
    for (i in seq_along(items)) {
      diag_col_index <- match(items[i], colnames(mat))
      if (!is.na(diag_col_index)) {
        rh <- hot_cell(rh, row = i, col = diag_col_index, readOnly = TRUE)
      }
    }
    
    # Apply color coding using custom renderer for each checkbox column
    for (col_name in items) {
      # Calculate R² values for this predictor column
      r2_colors <- sapply(seq_along(items), function(row_idx) {
        dep_var <- items[row_idx]
        if (dep_var == col_name) {
          return("#FFFFFF")  # White for diagonal (self-regression)
        }
        
        r2_val <- tryCatch({
          if (dep_var %in% rownames(r2_matrix) && col_name %in% colnames(r2_matrix)) {
            r2_matrix[dep_var, col_name]
          } else {
            0
          }
        }, error = function(e) 0)
        
        # Create gradient: white (R²=0) to red (R²=1)
        red_intensity <- min(1, max(0, r2_val))
        rgb(1, 1 - red_intensity * 0.7, 1 - red_intensity * 0.7)
      })
      
      # Create JavaScript renderer with row-specific colors
      colors_js <- paste0("['", paste(r2_colors, collapse = "','"), "']")
      diag_row <- match(col_name, items) - 1  # 0-indexed for JavaScript
      
      renderer_js <- paste0("
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          var colors = ", colors_js, ";
          if (colors[row]) {
            td.style.backgroundColor = colors[row];
          }
          if (row === ", diag_row, ") {
            cellProperties.readOnly = true;
            td.style.backgroundColor = '#f0f0f0';
            td.style.cursor = 'not-allowed';
            td.classList.add('htDimmed');
          }
        }")
      
      rh <- hot_col(rh, col_name, type = "checkbox", renderer = renderer_js)
    }
    rh
  })

  # ---------- lavaan syntax --------------------------------------

  lavaan_model_str <- reactive({
    req(input$input_table, input$checkbox_matrix)
    meas <- hot_to_r(input$input_table)
    mlines <- lapply(seq_len(nrow(meas)), function(i) {
      lt   <- meas$Latent[i]; if (!nzchar(lt)) return(NULL)
      vars <- names(meas)[4:ncol(meas)]
      inds <- vars[as.logical(meas[i, vars])];
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
    # ----- add manual equations (new) -----------------------------
    extra <- strsplit(input$extra_eq, "\\n")[[1]]
    extra <- trimws(extra)
    extra <- extra[nzchar(extra)]
    unlist(c(mlines, slines, extra))
  })

  output$lavaan_model <- renderText({
    ln <- lavaan_model_str()
    paste(if (length(ln) == 0)
      "Define a model to proceed."
      else
        ln, collapse = "\n")
  })

  # ---------- Fit model safely (eventReactive) -------------------

  fit_model_safe <- eventReactive(input$run_model, {
    ln <- isolate(lavaan_model_str())
    if (length(ln) == 0)
      return(list(ok = FALSE,
                  msg_friendly = "Define a model to proceed.",
                  fit = NULL))
    tryCatch({
      fm <- sem(paste(ln, collapse = "\n"),
                data          = processed_data(),
                missing       = input$missing_method,
                fixed.x       = FALSE,
                parser        = "old",
                meanstructure = (input$analysis_mode == "raw"))
      list(ok = lavInspect(fm, "converged"),
           msg_friendly = if (lavInspect(fm, "converged"))
             "" else
               "Model did not converge. Check for variables with correlation = 1 and remove or combine them.",
           fit = fm)
    }, error = function(e) {
      list(ok = FALSE,
           msg_friendly = "Estimation failed: possible perfect correlation (r = 1). Remove duplicate variables or merge them, then re-run.",
           fit = NULL)
    })
  }, ignoreNULL = FALSE)  # 初回自動実行

  output$fit_alert <- renderText({
    msg <- fit_model_safe()$msg_friendly
    if (nzchar(msg)) {
      shinyjs::show("fit_alert_box")
      msg
    } else {
      shinyjs::hide("fit_alert_box")
      ""
    }
  })

  output$fit_indices <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    fit <- model$fit
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
      else if (!ok) sprintf('<span style="color:red;">%.3f</span>', v)
      else sprintf('%.3f', v)
    }
    html_vals <- mapply(fmt, names(vals), vals, USE.NAMES = FALSE)
    tbl <- as.data.frame(t(html_vals), stringsAsFactors = FALSE)
    colnames(tbl) <- toupper(names(vals))
    datatable(tbl, escape = FALSE, rownames = FALSE,
              options = list(dom = 't'))
  })

  # ----------------- Approximate Equations ----------------------
  output$approx_eq <- renderText({
    if (input$analysis_mode == "std")
      return("— Hidden in Standardized mode —")
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    paste(lavaan_to_equations(model$fit), collapse = "\n")
  })

  output$fit_summary <- renderPrint({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    summary(model$fit, fit.measures = TRUE)
  })

  output$param_tbl <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    datatable(parameterEstimates(model$fit), options = list(pageLength = 15))
  })

  # ----------------- Path diagram UI ----------------------------
  output$sem_plot_ui <- renderUI({
    ln <- lavaan_model_str()
    if (length(ln) == 0)
      return(div("Define a model to view the path diagram."))
    tryCatch(grVizOutput("sem_plot"),
             error = function(e)
               div(style = "color:red;",
                   paste("Model Error:", e$message)))
  })

  output$sem_plot <- renderGrViz({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std

    # ---- parse layout_style into engine / rankdir ---------------
    parts  <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng    <- parts[1]
    rank   <- ifelse(length(parts) == 2, parts[2], "LR")
    # -------------------------------------------------------------

    semDiagram(model$fit,
               standardized = std_for_plot,
               layout       = rank,
               engine       = eng)
  })
}

# ---- Run the application ---------------------------------------

shinyApp(ui, server)

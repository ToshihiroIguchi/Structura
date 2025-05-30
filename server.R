# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – server.R
#   * Entire server-side logic; relies on helpers.R
# ---------------------------------------------------------------

server <- function(input, output, session) {

  # ---------- Initial modal -------------------------------------
  showModal(
    modalDialog(
      title = span(icon("upload"), "Load Data"),
      tabsetPanel(
        tabPanel("Upload File",
                 fileInput("datafile", NULL,
                           buttonLabel = "Browse…",
                           placeholder  = "Upload CSV",
                           accept       = c(".csv", "text/csv", "application/csv"))
        ),
        tabPanel("Sample Dataset",
                 radioButtons("sample_ds", "Or choose a demo dataset:",
                              choices = c("None",
                                          "HolzingerSwineford1939",
                                          "PoliticalDemocracy",
                                          "Demo.growth",
                                          "Demo.twolevel",
                                          "FacialBurns"))
        )
      ),
      easyClose = FALSE,
      footer    = NULL
    )
  )

  # ---------- Reactive data store -------------------------------
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
    data(ds); removeModal()
  })

  # ---------- Data preview --------------------------------------
  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter = "top", editable = FALSE,
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # ---------- Filtering & preprocessing -------------------------
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

    # log10 transform
    if (!is.null(input$log_columns)) {
      col_order <- names(df)
      for (col in input$log_columns) {
        log_col       <- paste0("log_", col)
        df[[log_col]] <- log10(df[[col]])
        pos           <- match(col, col_order)
        col_order[pos] <- log_col
        df[[col]]      <- NULL
      }
      df <- df[, col_order, drop = FALSE]
    }

    # one-hot encode
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

    # standardize numeric columns
    if (input$analysis_mode == "std") {
      num_cols <- vapply(df, is.numeric, logical(1))
      df[num_cols] <- scale(df[num_cols])
    }
    df
  })

  # ---------- Column selector -----------------------------------
  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    numeric_orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
    default <- intersect(c(numeric_orig, logs), names(df))
    checkboxGroupInput("display_columns", "Display columns:",
                       choices = names(df), selected = default, inline = TRUE)
  })

  # ---------- Filtered table ------------------------------------
  output$filtered_table <- renderDT({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    datatable(df, editable = FALSE, options = list(pageLength = 10))
  })

  # ---------- Correlation heatmap -------------------------------
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

  # ---------- Measurement table ---------------------------------
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

  # ---------- Structural table ----------------------------------
  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    deps <- as.character(input$display_columns %||% names(df))
    meas <- input_table_data(); req(meas)
    vars <- names(meas)[4:ncol(meas)]
    row_has_indicator <- apply(meas[vars], 1, function(x) any(as.logical(x)))
    convs <- setdiff(na.omit(unique(meas$Indicator[row_has_indicator])), "")
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
    for (i in seq_along(items))
      rh <- hot_cell(rh, row = i,
                     col = match(items[i], colnames(mat)),
                     readOnly = TRUE)
    rh
  })

  # ---------- lavaan syntax -------------------------------------
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
    extra <- strsplit(input$extra_eq, "\\n")[[1]]
    extra <- trimws(extra); extra <- extra[nzchar(extra)]
    unlist(c(mlines, slines, extra))
  })

  output$lavaan_model <- renderText({
    ln <- lavaan_model_str()
    paste(if (length(ln) == 0) "Define a model to proceed." else ln,
          collapse = "\n")
  })

  # ---------- Fit model safely ----------------------------------
  fit_model_safe <- eventReactive(input$run_model, {
    ln <- isolate(lavaan_model_str())
    if (length(ln) == 0) {
      return(list(ok = FALSE,
                  msg_friendly = "Define a model to proceed.",
                  fit = NULL))
    }
    tryCatch({
      fm <- sem(paste(ln, collapse = "\n"),
                data          = processed_data(),
                missing       = input$missing_method,
                fixed.x       = FALSE,
                parser        = "old",
                meanstructure = (input$analysis_mode == "raw"))
      list(ok = lavInspect(fm, "converged"),
           msg_friendly = if (lavInspect(fm, "converged")) "" else
             "Model did not converge. Check variables with correlation = 1.",
           fit = fm)
    }, error = function(e) {
      list(ok = FALSE,
           msg_friendly = "Estimation failed: possible perfect correlation (r = 1).",
           fit = NULL)
    })
  }, ignoreNULL = FALSE)

  output$fit_alert <- renderText({
    msg <- fit_model_safe()$msg_friendly
    if (nzchar(msg)) shinyjs::show("fit_alert_box") else shinyjs::hide("fit_alert_box")
    msg
  })

  # ---------- Fit indices table ---------------------------------
  output$fit_indices <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    fit <- model$fit
    ms  <- fitMeasures(fit, c("pvalue","srmr","rmsea","aic","bic",
                              "gfi","agfi","nfi","cfi"))
    vals <- round(as.numeric(ms), 3); names(vals) <- names(ms)
    thr <- c(pvalue = .05, srmr = .08, rmsea = .06,
             gfi = .90, agfi = .90, nfi = .90, cfi = .90)
    fmt <- function(idx, v) {
      ok <- switch(idx,
                   pvalue = v >= thr["pvalue"], srmr = v <= thr["srmr"],
                   rmsea  = v <= thr["rmsea"], gfi = v >= thr["gfi"],
                   agfi   = v >= thr["agfi"], nfi = v >= thr["nfi"],
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

  # ---------- Approximate equations -----------------------------
  output$approx_eq <- renderText({
    if (input$analysis_mode == "std")
      return("— Hidden in Standardized mode —")
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    paste(lavaan_to_equations(model$fit), collapse = "\n")
  })

  # ---------- Details tab outputs --------------------------------
  output$fit_summary <- renderPrint({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    summary(model$fit, fit.measures = TRUE)
  })

  output$param_tbl <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    datatable(parameterEstimates(model$fit),
              options = list(pageLength = 15))
  })

  # ---------- Path diagram --------------------------------------
  output$sem_plot_ui <- renderUI({
    if (length(lavaan_model_str()) == 0)
      return(div("Define a model to view the path diagram."))
    tryCatch(grVizOutput("sem_plot"),
             error = function(e) div(style = "color:red;",
                                     paste("Model Error:", e$message)))
  })

  output$sem_plot <- renderGrViz({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std
    parts  <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng    <- parts[1]; rank <- ifelse(length(parts) == 2, parts[2], "LR")
    semDiagram(model$fit, standardized = std_for_plot,
               layout = rank, engine = eng)
  })
}

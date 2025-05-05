# -*- coding: utf-8 -*-
library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(readflex)
library(lavaan)

`%||%` <- function(x, y) if (!is.null(x)) x else y
Sys.setlocale(category = "LC_CTYPE", locale = "ja_JP.UTF-8")

# CSV 読み込みヘルパー -------------------------------------------------
loadDataOnce <- function(fileInput) {
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}

# ─── UI ──────────────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),
  title = "Structura",
  tags$style(HTML(".htDimmed { background-color: #f0f0f0 !important; color: #888 !important; }")),
  tags$script(HTML("$(document).off('dragenter dragover drop');")),
  tabsetPanel(
    tabPanel("Data",
             h4("Uploaded Data"), DTOutput("datatable")),
    tabPanel("Filtered",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             tableOutput("filtered_table")),
    tabPanel("Equations",
             h4("Measurement Model"),
             rHandsontableOutput("input_table"),
             actionButton("add_row", "Add Row"),
             tags$hr(),
             h4("Structural Model"),
             rHandsontableOutput("checkbox_matrix"),
             tags$hr(),
             h4("Lavaan Model"),
             verbatimTextOutput("lavaan_model"),
             h4("Fit Indices"),
             DTOutput("fit_indices"))
  )
)

# ─── Server ──────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # 1. CSV アップロード ------------------------------------------------
  showModal(modalDialog(
    title = "Upload CSV File",
    fileInput("datafile", "Choose CSV File", accept = c(".csv", "text/csv", "application/csv")),
    easyClose = FALSE, footer = NULL
  ))

  data <- reactiveVal(NULL)
  observeEvent(input$datafile, {
    data(loadDataOnce(input$datafile))
    removeModal()
  }, once = TRUE)

  # 2. Data タブ -------------------------------------------------------
  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter = "top",
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # 3. Filtered タブ ---------------------------------------------------
  output$log_transform_ui <- renderUI({
    req(data())
    df   <- data()
    nums <- names(df)[sapply(df, is.numeric)]
    valid <- nums[sapply(df[nums], function(x) min(x, na.rm = TRUE) > 0)]
    if (!length(valid)) return(NULL)
    checkboxGroupInput("log_columns", "Select columns to log-transform:",
                       choices = valid, inline = TRUE)
  })

  processed_data <- reactive({
    req(data())
    df <- data()[ input$datatable_rows_all %||% seq_len(nrow(data())), , drop = FALSE ]
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

    # 対数変換 ---------------------------------------------------------
    if (!is.null(input$log_columns)) {
      for (col in input$log_columns) {
        df[[paste0("log_", col)]] <- log10(df[[col]])
        df[[col]] <- NULL
      }
    }

    # 文字→ダミー化 ----------------------------------------------------
    chars <- names(df)[sapply(df, is.character)]
    multi <- chars[sapply(df[chars], function(x){
      u <- unique(x); length(u) > 1 && length(u) < nrow(df)
    })]
    if (length(multi) > 0) {
      mm <- model.matrix(~ . - 1, data = df[multi])
      df <- cbind(df[setdiff(names(df), multi)], as.data.frame(mm, check.names = TRUE))
    }

    # 列名を R の合成語法に変換
    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    allc  <- names(df)
    orig  <- names(data())[sapply(data(), is.numeric)]
    logs  <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
    default <- intersect(c(orig, logs), allc)
    checkboxGroupInput("display_columns", "Select columns to display:",
                       choices = allc, selected = default, inline = TRUE)
  })

  output$filtered_table <- renderTable({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    df
  }, striped = TRUE, hover = TRUE)

  # 4. Measurement Model 入力 -----------------------------------------
  input_table_data <- reactiveVal(NULL)
  observeEvent(input$display_columns, {
    df <- processed_data(); req(df)
    inds <- input$display_columns %||% names(df)
    init <- data.frame(matrix(ncol = 3 + length(inds), nrow = 1), stringsAsFactors = FALSE)
    colnames(init) <- c("Latent", "Indicator", "Operator", inds)
    init$Latent    <- ""
    init$Indicator <- ""
    init$Operator  <- "=~~"
    for (col in inds) init[[col]] <- FALSE
    input_table_data(init)
  }, ignoreNULL = FALSE)

  output$input_table <- renderRHandsontable({
    df <- input_table_data(); req(df)
    rh <- rhandsontable(df, readOnly = FALSE, rowHeaders = FALSE)
    rh <- hot_col(rh, "Latent",    readOnly = FALSE)
    rh <- hot_col(rh, "Indicator", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (nm in colnames(df)[4:ncol(df)]) rh <- hot_col(rh, nm, type = "checkbox", readOnly = FALSE)
    rh
  })

  observeEvent(input$input_table, {
    tbl <- hot_to_r(input$input_table); req(tbl)
    tbl$Latent <- make.names(tbl$Latent, unique = FALSE)  # 合成語法へ
    bases <- tbl$Latent
    exist <- names(processed_data())
    convs_all <- make.unique(c(exist, bases))
    tbl$Indicator <- tail(convs_all, length(bases))
    input_table_data(tbl)
  })

  observeEvent(input$add_row, {
    df <- input_table_data(); req(df)
    nr <- df[1, , drop = FALSE]
    nr$Latent    <- ""
    nr$Indicator <- ""
    nr$Operator  <- "=~~"
    for (col in setdiff(colnames(nr), c("Latent", "Indicator", "Operator"))) nr[[col]] <- FALSE
    input_table_data(rbind(df, nr))
  })

  # 5. Structural Model 入力 ------------------------------------------
  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    deps  <- input$display_columns %||% names(df)
    convs <- setdiff(na.omit(unique(input_table_data()$Indicator)), "")
    items <- unique(c(deps, convs))
    if (length(items) == 0) return(NULL)

    mat <- data.frame(Dependent = items, Operator = rep("~", length(items)),
                      stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE

    rh <- rhandsontable(mat, readOnly = FALSE, rowHeaders = FALSE)
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (col in items) rh <- hot_col(rh, col, type = "checkbox", readOnly = FALSE)
    for (i in seq_along(items)) rh <- hot_cell(rh, row = i,
                                               col = match(items[i], colnames(mat)),
                                               readOnly = TRUE)
    rh
  })

  # 6. lavaan モデル文字列 --------------------------------------------
  lavaan_model_str <- reactive({
    req(input$input_table, input$checkbox_matrix)

    # 測定式
    meas <- hot_to_r(input$input_table)
    meas_lines <- unlist(lapply(seq_len(nrow(meas)), function(i) {
      latent <- meas$Latent[i]
      if (is.na(latent) || latent == "") return(NULL)
      vars <- names(meas)[4:ncol(meas)]
      inds <- vars[which(as.logical(meas[i, vars]))]
      if (length(inds) == 0) return(NULL)
      paste0(latent, " =~ ", paste(inds, collapse = " + "))
    }))

    # 構造式
    struc <- hot_to_r(input$checkbox_matrix)
    struc_lines <- unlist(lapply(seq_len(nrow(struc)), function(i) {
      dep <- struc$Dependent[i]
      if (is.na(dep) || dep == "") return(NULL)
      preds <- names(struc)[3:ncol(struc)]
      psel  <- preds[which(as.logical(struc[i, preds]))]
      if (length(psel) == 0) return(NULL)
      paste0(dep, " ~ ", paste(psel, collapse = " + "))
    }))

    c(meas_lines, struc_lines)
  })

  output$lavaan_model <- renderText({
    lines <- lavaan_model_str()
    if (length(lines) == 0) return("※ 測定式／構造式を定義してください")
    paste(lines, collapse = "\n")
  })

  # 7. 適合度指標 -------------------------------------------------------
  output$fit_indices <- renderDT({
    lines <- lavaan_model_str()
    if (length(lines) == 0) {
      df_empty <- data.frame(Index = "Error",
                             Value = "モデルが定義されていません",
                             stringsAsFactors = FALSE)
      return(datatable(df_empty, rownames = FALSE, options = list(dom = 't')))
    }

    model_str <- paste(lines, collapse = "\n")

    # ★ 日本語の変数名を正しく解釈させるため必ず旧パーサ(parser = "old")を指定すること！ ★
    fit <- tryCatch(
      lavaan::sem(model_str, data = processed_data(), parser = "old"),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      df_err <- data.frame(Index = "Error", Value = fit$message,
                           stringsAsFactors = FALSE)
      return(datatable(df_err, rownames = FALSE, options = list(dom = 't')))
    }

    ms  <- fitMeasures(fit, c("pvalue", "srmr", "rmsea", "aic", "bic",
                              "gfi", "agfi", "nfi", "cfi"))
    dfm <- data.frame(Index = names(ms), Value = as.numeric(ms),
                      stringsAsFactors = FALSE)

    thr <- c(pvalue = 0.05, srmr = 0.08, rmsea = 0.06,
             gfi = 0.90, agfi = 0.90, nfi = 0.90, cfi = 0.90)

    dfm$Display <- mapply(function(idx, val) {
      ok <- switch(idx,
                   pvalue = val >= thr["pvalue"],
                   srmr   = val <= thr["srmr"],
                   rmsea  = val <= thr["rmsea"],
                   gfi    = val >= thr["gfi"],
                   agfi   = val >= thr["agfi"],
                   nfi    = val >= thr["nfi"],
                   cfi    = val >= thr["cfi"],
                   TRUE)
      ok <- ifelse(is.na(ok), FALSE, ok)
      if (!ok) {
        if (is.na(val)) '<span style="color:gray;">NA</span>'
        else sprintf('<span style="color:red;">%.3f</span>', val)
      } else if (is.na(val)) {
        'NA'
      } else {
        sprintf('%.3f', val)
      }
    }, dfm$Index, dfm$Value, USE.NAMES = FALSE)

    datatable(dfm[, c("Index", "Display")],
              escape = FALSE, rownames = FALSE,
              colnames = c("Index", "Value"),
              options = list(pageLength = nrow(dfm), dom = 't'))
  })
}

shinyApp(ui, server)

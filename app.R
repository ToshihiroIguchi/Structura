# -*- coding: utf-8 -*-
library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(readflex)

# "%||%" operator: return x if not NULL, else y
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

# ロケールの設定（重要）
Sys.setlocale("LC_CTYPE","ja_JP.UTF-8")

# ─── CSV を一度だけ読み込むヘルパティ関数 ─────────────────────────
loadDataOnce <- function(fileInput) {
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}

ui <- fluidPage(
  useShinyjs(),
  title = "Structura",
  tags$style(HTML(
    ".htDimmed { background-color: #f0f0f0 !important; color: #888 !important; }"
  )),
  # rHandsontable によってキャンセルされたドラッグイベントを再有効化
  tags$script(HTML(
    "$(document).off('dragenter dragover drop');"
  )),
  tabsetPanel(
    tabPanel("Data",
             h4("Uploaded Data"),
             DTOutput("datatable")
    ),
    tabPanel("Filtered",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             tableOutput("filtered_table")
    ),
    tabPanel("Equations",
             h4("Measurement Model"),
             rHandsontableOutput("input_table"),
             actionButton("add_row", "Add Row"),
             tags$hr(),
             h4("Structural Model"),
             rHandsontableOutput("checkbox_matrix"),
             tags$hr(),
             h4("Lavaan Model"),
             verbatimTextOutput("lavaan_model")
    )
  )
)

server <- function(input, output, session) {
  # CSV アップロード用モーダル
  showModal(modalDialog(
    title = "Upload CSV File",
    fileInput(
      "datafile",
      "Choose CSV File",
      accept = c(
        ".csv",
        "text/csv",
        "text/comma-separated-values",
        "application/csv"
      )
    ),
    easyClose = FALSE, footer = NULL
  ))
  data <- reactiveVal(NULL)
  observeEvent(input$datafile, {
    data(loadDataOnce(input$datafile))
    removeModal()
  }, once = TRUE)

  # Data タブ
  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter = "top",
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # Filteredタブ 用 UI & 処理
  output$log_transform_ui <- renderUI({
    req(data())
    df <- data()
    nums <- names(df)[sapply(df, is.numeric)]
    valid <- nums[sapply(df[nums], function(x) min(x, na.rm = TRUE) > 0)]
    if (!length(valid)) return(NULL)
    checkboxGroupInput("log_columns", "Select columns to log-transform:",
                       choices = valid, inline = TRUE)
  })
  processed_data <- reactive({
    req(data())
    df <- data()[ input$datatable_rows_all %||% seq_len(nrow(data())), , drop = FALSE]
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
    if (!is.null(input$log_columns)) {
      for (col in input$log_columns) {
        df[[paste0("log_", col)]] <- log10(df[[col]])
        df[[col]] <- NULL
      }
    }
    chars <- names(df)[sapply(df, is.character)]
    multi <- chars[sapply(df[chars], function(x) {
      u <- unique(x); length(u) > 1 && length(u) < nrow(df)
    })]
    if (length(multi) > 0) {
      mm <- model.matrix(~ . -1, data = df[multi])
      df <- cbind(df[setdiff(names(df), multi)], as.data.frame(mm, check.names = TRUE))
    }
    df
  })
  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    allc <- names(df)
    orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
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

  # Measurement Model
  input_table_data <- reactiveVal(NULL)
  observeEvent(input$display_columns, {
    df <- processed_data(); req(df)
    indicators <- input$display_columns %||% names(df)
    init <- data.frame(matrix(ncol = 3 + length(indicators), nrow = 1), stringsAsFactors = FALSE)
    colnames(init) <- c("Latent", "Indicator", "Operator", indicators)
    init$Latent    <- ""
    init$Indicator <- ""
    init$Operator  <- "=~"
    for (col in indicators) init[[col]] <- FALSE
    input_table_data(init)
  }, ignoreNULL = FALSE)
  output$input_table <- renderRHandsontable({
    df <- input_table_data(); req(df)
    rh <- rhandsontable(df, readOnly = FALSE, rowHeaders = FALSE, colHeaders = colnames(df))
    rh <- hot_col(rh, "Latent",    readOnly = FALSE)
    rh <- hot_col(rh, "Indicator", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (nm in colnames(df)[4:ncol(df)]) rh <- hot_col(rh, nm, type = "checkbox", readOnly = FALSE)
    rh
  })
  observeEvent(input$input_table, {
    tbl <- hot_to_r(input$input_table); req(tbl)
    # gsub() による手動置換を廃止し、直接 make.names() で有効な名前に変換
    bases <- make.names(as.character(tbl$Latent), unique = FALSE)
    exist      <- names(processed_data())
    convs_all  <- make.unique(c(exist, bases))
    tbl$Indicator <- tail(convs_all, length(bases))
    input_table_data(tbl)
  })
  observeEvent(input$add_row, {
    df <- input_table_data(); req(df)
    nr <- df[1, , drop = FALSE]
    nr$Latent    <- ""
    nr$Indicator <- ""
    nr$Operator  <- "=~"
    for (col in setdiff(colnames(nr), c("Latent","Indicator","Operator")))
      nr[[col]] <- FALSE
    input_table_data(rbind(df, nr))
  })

  # Structural Model
  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    dependents <- input$display_columns %||% names(df)
    convs <- setdiff(na.omit(unique(input_table_data()$Indicator)), "")
    items <- unique(c(dependents, convs))
    if (length(items) == 0) return(NULL)
    mat <- data.frame(
      Dependent = items,
      Operator  = rep("~", length(items)),
      stringsAsFactors = FALSE
    )
    for (col in items) mat[[col]] <- FALSE

    rh <- rhandsontable(mat, readOnly = FALSE, rowHeaders = FALSE, colHeaders = colnames(mat))
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (col in items) rh <- hot_col(rh, col, type = "checkbox", readOnly = FALSE)
    for (i in seq_along(items)) {
      col_idx <- match(items[i], colnames(mat))
      rh <- hot_cell(rh, row = i, col = col_idx, readOnly = TRUE)
    }
    rh
  })

  # Lavaan モデル文字列を生成して表示
  output$lavaan_model <- renderText({
    req(input$input_table, input$checkbox_matrix)
    # Measurement Equations (測定方程式)
    meas <- hot_to_r(input$input_table)
    meas_lines <- lapply(seq_len(nrow(meas)), function(i) {
      latent <- meas$Latent[i]
      if (latent == "") return(NULL)
      latent_nm <- make.names(latent)
      vars <- names(meas)[4:ncol(meas)]
      inds <- vars[which(as.logical(meas[i, vars]))]
      if (length(inds) == 0) return(NULL)
      paste0(latent_nm, " =~ ", paste(inds, collapse = " + "))
    })
    # Structural Equations (構造方程式)
    struc <- hot_to_r(input$checkbox_matrix)
    struc_lines <- lapply(seq_len(nrow(struc)), function(i) {
      dep <- struc$Dependent[i]
      if (dep == "") return(NULL)
      dep_nm <- make.names(dep)
      preds <- names(struc)[3:ncol(struc)]
      psel <- preds[which(as.logical(struc[i, preds]))]
      if (length(psel) == 0) return(NULL)
      paste0(dep_nm, " ~ ", paste(psel, collapse = " + "))
    })
    # Combine with comments
    lines <- c(
      "# Measurement Equations",
      unlist(meas_lines),
      "",  # 空行
      "# Structural Equations",
      unlist(struc_lines)
    )
    paste(lines, collapse = "\n")
  })
}

shinyApp(ui, server)

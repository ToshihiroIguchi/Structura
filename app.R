# app.R

library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)

# ─── CSV を一度だけ読み込むヘルパー関数 ──────────────────────────
loadDataOnce <- function(fileInput) {
  req(fileInput)
  read.csv(fileInput$datapath, stringsAsFactors = FALSE)
}

ui <- fluidPage(
  useShinyjs(),
  title = "Structura",
  tags$style(HTML(
    ".htDimmed { background-color: #f0f0f0 !important; color: #888 !important; }"
  )),
  tabsetPanel(
    tabPanel("Data",
             h4("Uploaded Data"),
             DTOutput("datatable")
    ),
    tabPanel("Filtered Data",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             tableOutput("filtered_table")
    ),
    tabPanel("Checkbox Matrix",
             h4("Checkbox Matrix"),
             # ── 上の表（input_table） ─────────────────────────
             rHandsontableOutput("input_table"),
             actionButton("add_row", "Add Row"),
             tags$hr(),
             # ── 既存のチェックボックス行列 ────────────────────────
             rHandsontableOutput("checkbox_matrix")
    )
  )
)

server <- function(input, output, session) {
  # CSVアップロード用モーダル
  showModal(modalDialog(
    title    = "Upload CSV File",
    fileInput("datafile", "Choose CSV File", accept = c(".csv")),
    easyClose = FALSE, footer = NULL
  ))

  # データを一度だけ読み込んで保持
  data <- reactiveVal(NULL)
  observeEvent(input$datafile, {
    data(loadDataOnce(input$datafile))
    removeModal()
  }, once = TRUE)

  # Data タブ
  output$datatable <- DT::renderDT({
    req(data())
    DT::datatable(
      data(),
      filter  = "top",
      options = list(pageLength = 30, autoWidth = TRUE),
      rownames = FALSE
    )
  }, server = FALSE)

  # ログ変換 UI
  output$log_transform_ui <- renderUI({
    req(data())
    df       <- data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    valid_cols <- num_cols[
      sapply(df[, num_cols, drop = FALSE], function(x) min(x, na.rm = TRUE) > 0)
    ]
    if (!length(valid_cols)) return(NULL)
    checkboxGroupInput("log_columns", "Select columns to log-transform:",
                       choices = valid_cols, inline = TRUE)
  })

  # 加工済みデータ (ログ変換＋ダミー化)
  processed_data <- reactive({
    req(data())
    idx <- input$datatable_rows_all
    df  <- if (is.null(idx)) data() else data()[idx, , drop = FALSE]
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

    # ログ変換
    if (!is.null(input$log_columns)) {
      for (col in input$log_columns) {
        new <- paste0("log_", col)
        df[[new]] <- log10(df[[col]])
        df[[col]] <- NULL
      }
    }

    # ダミー変数化
    char_cols <- names(df)[sapply(df, is.character)]
    multi <- char_cols[
      sapply(df[, char_cols, drop = FALSE], function(x) {
        u <- unique(x)
        length(u) > 1 && length(u) < nrow(df)
      })
    ]
    if (length(multi) > 0) {
      dummy_mat <- model.matrix(~ . -1, data = df[, multi, drop = FALSE])
      df <- cbind(
        df[, setdiff(names(df), multi), drop = FALSE],
        as.data.frame(dummy_mat, check.names = TRUE)
      )
    }
    df
  })

  # 表示列選択 UI
  output$display_column_ui <- renderUI({
    df       <- processed_data(); req(df)
    all_cols <- names(df)
    orig_n   <- names(data())[sapply(data(), is.numeric)]
    log_n    <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
    default  <- intersect(c(orig_n, log_n), all_cols)
    checkboxGroupInput("display_columns", "Select columns to display:",
                       choices = all_cols, selected = default, inline = TRUE)
  })

  # 非インタラクティブ表
  output$filtered_table <- renderTable({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns)) {
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    }
    df
  }, striped = TRUE, hover = TRUE)

  # ── 上の表（input_table）用リアクティブ値 ────────────────────
  input_table_data <- reactiveVal(NULL)

  # 初期化／列構造の更新
  observe({
    df <- processed_data(); req(df)
    items <- if (!is.null(input$display_columns)) {
      intersect(input$display_columns, names(df))
    } else {
      names(df)
    }

    init_df <- data.frame(
      matrix(ncol = 2 + length(items), nrow = 1),
      stringsAsFactors = FALSE
    )
    # 1列目を文字型に：空文字で初期化
    init_df[, 1] <- ""
    # 空文字ではなくスペースをヘッダに
    colnames(init_df) <- c(" ", "Symbol", items)
    init_df[, "Symbol"] <- "=~"
    for (col in items) init_df[[col]] <- FALSE

    input_table_data(init_df)
  })

  # input_table 出力
  output$input_table <- renderRHandsontable({
    df <- input_table_data(); req(df)
    rhandsontable(
      df,
      readOnly   = FALSE,
      rowHeaders = FALSE,
      colHeaders = colnames(df)
    ) %>%
      hot_col(colnames(df)[1], readOnly = FALSE) %>%
      hot_col("Symbol",            readOnly = TRUE) %>%
      {  # 3列目以降をチェックボックスに
        rh <- .
        for (nm in colnames(df)[3:ncol(df)]) {
          rh <- rh %>% hot_col(nm, type = "checkbox", readOnly = FALSE)
        }
        rh
      }
  })

  # Add Row ボタンで行を追加
  observeEvent(input$add_row, {
    df <- input_table_data(); req(df)
    new_row <- df[1, , drop = FALSE]
    new_row[, 1] <- ""
    for (col in colnames(df)[3:ncol(df)]) new_row[[col]] <- FALSE
    input_table_data(rbind(df, new_row))
  })

  # ── 既存のチェックボックス行列 ───────────────────────────
  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    items <- if (!is.null(input$display_columns)) {
      intersect(input$display_columns, names(df))
    } else {
      names(df)
    }
    n <- length(items); if (n == 0) return(NULL)
    mat <- data.frame(Item = items, Symbol = rep("~", n), stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE

    rhandsontable(mat, readOnly = FALSE, rowHeaders = FALSE, colHeaders = colnames(mat)) %>%
      hot_col("Item",   readOnly = TRUE) %>%
      hot_col("Symbol", readOnly = TRUE) %>%
      {
        rh <- .
        for (col in items) rh <- rh %>% hot_col(col, type = "checkbox", readOnly = FALSE)
        for (i in seq_along(items)) {
          rh <- rh %>% hot_cell(
            row = i,
            col = which(colnames(mat) == items[i]),
            readOnly = TRUE
          )
        }
        rh
      }
  })
}

shinyApp(ui, server)

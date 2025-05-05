
# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – Structural Insights, Simplified
# 共分散構造分析 Shinyアプリ（平均構造対応・ロゴ組込み・コメント強化）
# ---------------------------------------------------------------
options(
  shiny.fullstacktrace = TRUE,   # エラー時にフルスタック
  shiny.reactlog       = TRUE    # reactlog 可視化
)

# ---- ライブラリ --------------------------------------------------
library(shiny)        # Webアプリ基盤
library(shinyjs)      # JS操作
library(DT)           # DataTables
library(rhandsontable) # Handsontable
library(readflex)     # 高速 CSV 読み込み
library(lavaan)       # SEM 本体
library(DiagrammeR)   # グラフ描画
library(semDiagram)   # Path 図
library(ggplot2)      # 相関ヒートマップ
library(reshape2)     # データ整形

`%||%` <- function(x, y) if (!is.null(x)) x else y
Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8") # UTF-8 固定

# ---- 0. 近似式出力関数 ------------------------------------------
lavaan_to_equations <- function(fit, standardized = TRUE, digits = 3){
  pe <- parameterEstimates(fit, standardized = standardized, remove.def = TRUE) #:contentReference[oaicite:9]{index=9}
  build <- function(op){
    eq <- pe[pe$op == op, ]
    split(eq, eq$lhs)
  }
  eq_lines <- function(lst, sep){
    unlist(lapply(names(lst), function(lhs){
      df <- lst[[lhs]]
      rhs <- paste0(round(df$est, digits), "*", df$rhs)
      paste(lhs, sep, paste(rhs, collapse = " + "))
    }))
  }
  c(eq_lines(build("=~"), "=~"), eq_lines(build("~"), "~"))
}

# ---- 1. 読み込みヘルパ -------------------------------------------
loadDataOnce <- function(fileInput){
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}

# ---- 2. UI -------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),

  # ロゴを右上に固定配置
  tags$head(
    tags$link(rel="icon", type="image/png", href="logo.png"),
    tags$style(HTML("
      #app-logo{position:absolute; top:8px; right:16px;}
      .modal-header{background:#1e90ff; color:white;}
      .modal-title{font-weight:bold;}
    "))
  ),
  div(id="app-logo",
      img(src="logo.png", height=40,
          title="Structural Insights, Simplified")
  ),

  title = "Structura",

  tabsetPanel(
    tabPanel("Data",
             h4("Uploaded Data"),
             DTOutput("datatable")
    ),

    tabPanel("Filtered",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             DTOutput("filtered_table"),
             tags$hr(),
             h4("Correlation"),
             plotOutput("corr_plot", height = "400px")
    ),

    tabPanel("Model",          # <-- Equations -> Model
             fluidRow(
               # --- 左列: モデル定義 ---------------------------------------
               column(
                 width = 7,
                 h4("Measurement"),
                 rHandsontableOutput("input_table"),
                 actionButton("add_row", "Add Row", class = "btn btn-primary"),
                 tags$hr(),
                 h4("Structural"),
                 rHandsontableOutput("checkbox_matrix"),
                 tags$hr(),
                 h4("lavaan Syntax"),
                 verbatimTextOutput("lavaan_model")
               ),
               # --- 右列: 結果と図 ----------------------------------------
               column(
                 width = 5,
                 h4("Fit Indices"),
                 DTOutput("fit_indices"),
                 h4("Approx. Equations"),
                 verbatimTextOutput("approx_eq"),
                 tags$details(
                   tags$summary("Full Summary / Estimates"),
                   verbatimTextOutput("fit_summary"),
                   DTOutput("param_tbl")
                 ),
                 h4("Path Diagram"),
                 div(style="max-height:45vh; overflow:auto; border:1px solid #ccc;",
                     uiOutput("sem_plot_ui"))
               )
             )
    )
  )
)

# ---- 3. Server ----------------------------------------------------
server <- function(input, output, session){

  # --- 3-1. CSV or Sample 選択モーダル -----------------------------
  showModal(modalDialog(
    title = span(icon("upload"), " Load Data"),
    tags$style(HTML(".btn-file{background:#28a745; color:white;}")),
    fileInput("datafile", NULL,
              buttonLabel = "Browse...",
              placeholder = "Upload CSV",
              accept = c(".csv","text/csv","application/csv")),
    tags$hr(),
    radioButtons("sample_ds", "Or choose a demo set:",
                 choices = c("None","HolzingerSwineford1939",
                             "PoliticalDemocracy","Demo.growth",
                             "Demo.twolevel","FacialBurns"),
                 inline = FALSE),
    easyClose = FALSE, footer = NULL
  ))

  # --- 3-2. データ読み込みリアクティブ -------------------------------
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

  # --- 3-3. DataTable (read-only) -----------------------------------
  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter="top",
              editable = FALSE,                             # read-only:contentReference[oaicite:10]{index=10}
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # --- 3-4. Filter UI ----------------------------------------------
  output$log_transform_ui <- renderUI({
    req(data())
    nums <- names(data())[sapply(data(), is.numeric)]
    valid <- nums[sapply(data()[nums], function(x) min(x, na.rm = TRUE) > 0)]
    if (!length(valid)) return()
    checkboxGroupInput("log_columns", "Log-transform columns:",
                       choices = valid, inline = TRUE)
  })

  # --- 3-5. 加工データ ----------------------------------------------
  processed_data <- reactive({
    req(data())
    idx <- as.numeric(unlist(input$datatable_rows_all))
    if (!length(idx)) idx <- seq_len(nrow(data()))
    df <- data()[idx, , drop = FALSE]

    # factor -> character
    df[] <- lapply(df, \(x) if (is.factor(x)) as.character(x) else x)

    # log10 transform
    if (!is.null(input$log_columns))
      for (col in input$log_columns){
        df[[paste0("log_", col)]] <- log10(df[[col]])
        df[[col]] <- NULL
      }

    # one-hot encode character variables
    chars  <- names(df)[vapply(df, is.character, logical(1))]
    multi  <- chars[vapply(df[chars],
                           \(x){u<-unique(x); length(u)>1 && length(u)<nrow(df)},
                           logical(1))]
    if (length(multi)){
      mm <- model.matrix(~ . - 1, data = df[multi])
      df <- cbind(df[setdiff(names(df), multi)],
                  as.data.frame(mm, check.names = TRUE))
    }

    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  # --- 3-6. 表示列チェックボックス ---------------------------------
  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    numeric_orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns))
      paste0("log_", input$log_columns) else NULL
    default <- intersect(c(numeric_orig, logs), names(df))
    checkboxGroupInput("display_columns", "Display columns:",
                       choices = names(df), selected = default, inline = TRUE)
  })

  # --- 3-7. フィルタ済みテーブル (10行制限) -------------------------
  output$filtered_table <- renderDT({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    datatable(df, editable = FALSE, options = list(pageLength = 10))  #:contentReference[oaicite:11]{index=11}
  })

  # --- 3-8. 相関ヒートマップ ----------------------------------------
  output$corr_plot <- renderPlot({
    df  <- processed_data();  req(df)
    num <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(num) < 2) return()

    M  <- cor(num, use = "pairwise.complete.obs")
    md <- reshape2::melt(M)

    ggplot(md, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0, limits = c(-1, 1),
        name = "ρ"                       # ← labs() の代わりにここでも可
      ) +
      labs(fill = "ρ")                   # ← 凡例タイトルの正式指定
    theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }, res = 96)


  # ---- 3-9. Measurement Model テーブル -----------------------------
  input_table_data <- reactiveVal(NULL)
  observeEvent(input$display_columns, {
    inds <- as.character(input$display_columns %||% names(processed_data()))
    init <- data.frame(matrix(ncol = 3 + length(inds), nrow = 1),
                       stringsAsFactors = FALSE)
    colnames(init) <- c("Latent", "Indicator", "Operator", inds)
    init$Latent <- ""; init$Indicator <- ""; init$Operator <- "=~"
    for (col in inds) init[[col]] <- FALSE
    input_table_data(init)
  }, ignoreNULL = FALSE)

  output$input_table <- renderRHandsontable({
    df <- input_table_data(); req(df)
    rh <- rhandsontable(df, rowHeaders = FALSE)
    rh <- hot_col(rh, "Latent")
    rh <- hot_col(rh, "Indicator", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (nm in colnames(df)[4:ncol(df)])
      rh <- hot_col(rh, nm, type = "checkbox")
    rh
  })

  observeEvent(input$input_table, {
    tbl <- as.data.frame(hot_to_r(input$input_table)); req(tbl)
    tbl$Latent <- make.names(tbl$Latent, unique = FALSE)
    convs <- make.unique(c(names(processed_data()), tbl$Latent))
    tbl$Indicator <- tail(convs, nrow(tbl))
    input_table_data(tbl)
  })

  observeEvent(input$add_row, {
    df <- input_table_data(); req(df)
    nr <- df[1, , drop = FALSE]
    nr[,] <- ""
    nr$Operator <- "=~"
    for (col in colnames(nr)[4:ncol(nr)]) nr[[col]] <- FALSE
    input_table_data(rbind(df, nr))
  })

  # ---- 3-10. Structural Model テーブル -----------------------------
  output$checkbox_matrix <- renderRHandsontable({
    df <- processed_data(); req(df)
    deps  <- as.character(input$display_columns %||% names(df))
    convs <- setdiff(na.omit(unique(input_table_data()$Indicator)), "")
    items <- unique(c(deps, convs))
    if (!length(items)) return()
    mat <- data.frame(Dependent = items, Operator = "~",
                      stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE
    rh <- rhandsontable(mat, rowHeaders = FALSE)
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (col in items) rh <- hot_col(rh, col, type = "checkbox")
    for (i in seq_along(items))
      rh <- hot_cell(rh, row = i, col = match(items[i], colnames(mat)),
                     readOnly = TRUE)
    rh
  })

  # ---- 3-11. lavaan モデル文字列 -----------------------------------
  lavaan_model_str <- reactive({
    req(input$input_table, input$checkbox_matrix)
    meas <- as.data.frame(hot_to_r(input$input_table))
    mlines <- lapply(seq_len(nrow(meas)), function(i){
      lt <- meas$Latent[i]; if (lt == "") return()
      vars <- names(meas)[4:ncol(meas)]
      inds <- vars[as.logical(meas[i, vars])]
      if (!length(inds)) return()
      paste0(lt, " =~ ", paste(inds, collapse = " + "))
    })
    struc <- as.data.frame(hot_to_r(input$checkbox_matrix))
    slines <- lapply(seq_len(nrow(struc)), function(i){
      dp <- struc$Dependent[i]; if (dp == "") return()
      preds <- names(struc)[3:ncol(struc)]
      ps    <- preds[as.logical(struc[i, preds])]
      if (!length(ps)) return()
      paste0(dp, " ~ ", paste(ps, collapse = " + "))
    })
    unlist(c(mlines, slines))
  })

  output$lavaan_model <- renderText({
    ln <- lavaan_model_str()
    validate(need(length(ln) > 0,
                  "Define measurement and structural equations."))
    paste(ln, collapse = "\n")
  })

  # ---- 3-12. lavaan フィット ---------------------------------------
  fit_model <- reactive({
    ln <- lavaan_model_str(); req(length(ln) > 0)
    sem(paste(ln, collapse = "\n"),
        data = processed_data(),
        parser = "old",
        meanstructure = TRUE)       # 平均構造を推定:contentReference[oaicite:12]{index=12}
  })

  # ---- 3-13. Fit Indices 表 ----------------------------------------
  output$fit_indices <- renderDT({
    fit <- fit_model()
    ms  <- fitMeasures(fit,
                       c("pvalue","srmr","rmsea","aic","bic","gfi","agfi","nfi","cfi"))
    vals <- round(as.numeric(ms), 3); names(vals) <- names(ms)
    thr <- c(pvalue=.05,srmr=.08,rmsea=.06,gfi=.90,agfi=.90,nfi=.90,cfi=.90)
    fmt <- function(idx, v){
      ok <- switch(idx,
                   pvalue = v >= thr["pvalue"],
                   srmr   = v <= thr["srmr"],
                   rmsea  = v <= thr["rmsea"],
                   gfi    = v >= thr["gfi"],
                   agfi   = v >= thr["agfi"],
                   nfi    = v >= thr["nfi"],
                   cfi    = v >= thr["cfi"],
                   TRUE)
      if (is.na(v)) "NA"
      else if (!ok) sprintf('<span style="color:red;">%.3f</span>', v)
      else sprintf('%.3f', v)
    }
    html_vals <- mapply(fmt, names(vals), vals, USE.NAMES = FALSE)
    tbl <- as.data.frame(t(html_vals), stringsAsFactors = FALSE)
    colnames(tbl) <- toupper(names(vals))        # 大文字化
    datatable(tbl, escape = FALSE, rownames = FALSE,
              options = list(dom = 't'))
  })

  # ---- 3-14. 近似式・Summary・Estimates ----------------------------
  output$approx_eq <- renderText({
    paste(lavaan_to_equations(fit_model()), collapse = "\n")
  })
  output$fit_summary <- renderPrint({
    summary(fit_model(), fit.measures = TRUE)
  })
  output$param_tbl <- renderDT({
    datatable(parameterEstimates(fit_model()), options = list(pageLength = 15))
  })

  # ---- 3-15. Path Diagram + Error ----------------------------------
  output$sem_plot_ui <- renderUI({
    tryCatch(
      grVizOutput("sem_plot"),
      error = function(e)
        div(style = "color:red;",
            paste("Model Error:", e$message))
    )
  })
  output$sem_plot <- renderGrViz({
    semDiagram(fit_model(),  standardized = TRUE)
  })
}

# ---- 4. 起動 -------------------------------------------------------
shinyApp(ui, server)

# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura Shiny app – with semDiagram & two-column layout (2025-05-05)
# モデルエラーは "Model Error:" プレフィックス付きで表示
# ---------------------------------------------------------------
options(
  shiny.fullstacktrace = TRUE,   # フルスタックトレースを有効化
  shiny.reactlog       = TRUE    # reactlog 可視化を有効化
)

library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(readflex)
library(lavaan)
library(DiagrammeR)
library(semDiagram)

`%||%` <- function(x, y) if (!is.null(x)) x else y
Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")

# 1. CSV 読み込みヘルパー
loadDataOnce <- function(fileInput) {
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}

# 2. UI 定義
ui <- fluidPage(
  useShinyjs(),
  title = "Structura (with semDiagram)",
  tags$style(HTML(".htDimmed{background:#f0f0f0!important;color:#888!important;}")),
  tags$script(HTML("$(document).off('dragenter dragover drop');")),
  tabsetPanel(
    tabPanel("Data",
             h4("Uploaded Data"), DTOutput("datatable")
    ),
    tabPanel("Filtered",
             h4("Filtered Data"), uiOutput("log_transform_ui"), uiOutput("display_column_ui"), tableOutput("filtered_table")
    ),
    tabPanel("Equations",
             fluidRow(
               column(width=7,
                      h4("Measurement Model"),
                      rHandsontableOutput("input_table"), actionButton("add_row","Add Row"), tags$hr(),
                      h4("Structural Model"), rHandsontableOutput("checkbox_matrix"), tags$hr(),
                      h4("Lavaan Model"), verbatimTextOutput("lavaan_model")
               ),
               column(width=5,
                      h4("Fit Indices"), DTOutput("fit_indices"),
                      h4("Path Diagram"),
                      div(style="max-height:75vh; overflow:auto; border:1px solid #ccc;",
                          uiOutput("sem_plot_ui")
                      )
               )
             )
    ),
    tabPanel("Results",
             h4("Formula")
    )

  )
)

# 3. Server 定義
server <- function(input, output, session) {

  # CSV アップロード
  showModal(modalDialog(
    title="Upload CSV File",
    fileInput("datafile","Choose CSV File",accept=c(".csv","text/csv","application/csv")),
    easyClose=FALSE, footer=NULL
  ))

  data <- reactiveVal(NULL)
  observeEvent(input$datafile, {
    data(loadDataOnce(input$datafile)); removeModal()
  }, once=TRUE)

  # Data タブ
  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter="top", options=list(pageLength=30, autoWidth=TRUE), rownames=FALSE)
  }, server=FALSE)

  # Filtered タブ
  output$log_transform_ui <- renderUI({
    req(data())
    nums <- names(data())[sapply(data(), is.numeric)]
    valid <- nums[sapply(data()[nums], function(x) min(x, na.rm=TRUE)>0)]
    if(!length(valid)) return()
    checkboxGroupInput("log_columns","Select columns to log-transform:",choices=valid, inline=TRUE)
  })

  processed_data <- reactive({
    req(data())
    idx <- as.numeric(unlist(input$datatable_rows_all))
    if(!length(idx)) idx <- seq_len(nrow(data()))
    df <- data()[idx,,drop=FALSE]
    df[] <- lapply(df, function(x) if(is.factor(x)) as.character(x) else x)
    # log transform
    if(!is.null(input$log_columns)) for(col in input$log_columns) { df[[paste0("log_",col)]]<-log10(df[[col]]); df[[col]]<-NULL }
    # dummy encode
    chars <- names(df)[vapply(df,is.character,logical(1))]
    is_multi<-vapply(df[chars],function(x) {u<-unique(x); length(u)>1 && length(u)<nrow(df)},logical(1))
    multi<-chars[is_multi]
    if(length(multi)) { mm<-model.matrix(~.-1,df[multi]); df<-cbind(df[setdiff(names(df),multi)],as.data.frame(mm,check.names=TRUE)) }
    names(df)<-make.names(names(df),unique=TRUE)
    df
  })

  output$display_column_ui <- renderUI({
    df<-processed_data(); req(df)
    allc<-names(df); orig<-names(data())[sapply(data(),is.numeric)]; logs<-if(!is.null(input$log_columns)) paste0("log_",input$log_columns)
    default<-intersect(c(orig,logs),allc)
    checkboxGroupInput("display_columns","Select columns to display:",choices=allc,selected=default,inline=TRUE)
  })

  output$filtered_table <- renderTable({
    df<-processed_data(); req(df)
    if(!is.null(input$display_columns)) { sel<-as.character(input$display_columns); df<-df[,intersect(sel,names(df)),drop=FALSE] }
    df
  },striped=TRUE,hover=TRUE)

  # Measurement model input
  input_table_data <- reactiveVal(NULL)
  observeEvent(input$display_columns,{
    inds<-as.character(input$display_columns%||%names(processed_data()))
    init<-data.frame(matrix(ncol=3+length(inds),nrow=1),stringsAsFactors=FALSE)
    colnames(init)<-c("Latent","Indicator","Operator",inds)
    init$Latent<-""; init$Indicator<-""; init$Operator<-"=~"
    for(col in inds) init[[col]]<-FALSE
    input_table_data(init)
  },ignoreNULL=FALSE)

  output$input_table <- renderRHandsontable({
    df<-input_table_data(); req(df)
    rh<-rhandsontable(df,rowHeaders=FALSE); rh<-hot_col(rh,"Latent",readOnly=FALSE); rh<-hot_col(rh,"Indicator",readOnly=TRUE); rh<-hot_col(rh,"Operator",readOnly=TRUE)
    for(nm in colnames(df)[4:ncol(df)]) rh<-hot_col(rh,nm,type="checkbox")
    rh
  })

  observeEvent(input$input_table,{
    tbl<-as.data.frame(hot_to_r(input$input_table)); req(tbl)
    tbl$Latent<-make.names(tbl$Latent,unique=FALSE)
    convs<-make.unique(c(names(processed_data()),tbl$Latent))
    tbl$Indicator<-tail(convs,nrow(tbl))
    input_table_data(tbl)
  })

  observeEvent(input$add_row,{
    df<-input_table_data(); req(df)
    nr<-df[1,,drop=FALSE]; nr[,]<-
      ""; nr$Operator<-"=~"; for(col in colnames(nr)[4:ncol(nr)]) nr[[col]]<-FALSE
    input_table_data(rbind(df,nr))
  })

  # Structural model input
  output$checkbox_matrix<-renderRHandsontable({
    df<-processed_data(); req(df)
    deps<-as.character(input$display_columns%||%names(df)); convs<-setdiff(na.omit(unique(input_table_data()$Indicator)),"")
    items<-unique(c(deps,convs)); if(!length(items)) return()
    mat<-data.frame(Dependent=items,Operator="~",stringsAsFactors=FALSE)
    for(col in items) mat[[col]]<-FALSE
    rh<-rhandsontable(mat,rowHeaders=FALSE); rh<-hot_col(rh,"Dependent",readOnly=TRUE); rh<-hot_col(rh,"Operator",readOnly=TRUE)
    for(col in items) rh<-hot_col(rh,col,type="checkbox"); for(i in seq_along(items)) rh<-hot_cell(rh,row=i,col=match(items[i],colnames(mat)),readOnly=TRUE)
    rh
  })

  # Lavaan model string
  lavaan_model_str<-reactive({
    req(input$input_table,input$checkbox_matrix)
    meas<-as.data.frame(hot_to_r(input$input_table))
    mlines<-lapply(seq_len(nrow(meas)),function(i){ lt<-meas$Latent[i]; if(lt=="")return(); vars<-names(meas)[4:ncol(meas)]; flag<-as.logical(unlist(meas[i,vars])); inds<-vars[which(flag)]; if(!length(inds))return(); paste0(lt," =~ ",paste(inds,collapse=" + ")) })
    struc<-as.data.frame(hot_to_r(input$checkbox_matrix))
    slines<-lapply(seq_len(nrow(struc)),function(i){ dp<-struc$Dependent[i]; if(dp=="")return(); preds<-names(struc)[3:ncol(struc)]; flag<-as.logical(unlist(struc[i,preds])); ps<-preds[which(flag)]; if(!length(ps))return(); paste0(dp," ~ ",paste(ps,collapse=" + ")) })
    unlist(c(mlines,slines))
  })

  output$lavaan_model<-renderText({
    ln<-lavaan_model_str(); validate(need(length(ln)>0,"Please define measurement and structural equations")); paste(ln,collapse="\n")
  })

  # Fit Indices
  fit_model<-reactive({ ln<-lavaan_model_str(); req(length(ln)>0); sem(paste(ln,collapse="\n"),data=processed_data(),parser="old") })
  output$fit_indices<-renderDT({
    fit<-fit_model(); ms<-fitMeasures(fit,c("pvalue","srmr","rmsea","aic","bic","gfi","agfi","nfi","cfi")); vals<-round(as.numeric(ms),3); names(vals)<-names(ms)
    thr<-c(pvalue=.05,srmr=.08,rmsea=.06,gfi=.90,agfi=.90,nfi=.90,cfi=.90)
    fmt<-function(idx,v){ ok<-switch(idx,pvalue=v>=thr["pvalue"],srmr=v<=thr["srmr"],rmsea=v<=thr["rmsea"],gfi=v>=thr["gfi"],agfi=v>=thr["agfi"],nfi=v>=thr["nfi"],cfi=v>=thr["cfi"],TRUE); if(is.na(v))"NA"else if(!ok) sprintf('<span style="color:red;">%.3f</span>',v) else sprintf('%.3f',v) }
    html_vals<-mapply(fmt,names(vals),vals,USE.NAMES=FALSE)
    tbl<-as.data.frame(t(html_vals),stringsAsFactors=FALSE); colnames(tbl)<-names(vals)
    datatable(tbl,escape=FALSE,rownames=FALSE,options=list(dom='t'))
  })

  # Path Diagram with error handling
  output$sem_plot_ui<-renderUI({
    tryCatch(
      {
        diagram<-semDiagram(fit_model())
        grVizOutput("sem_plot")
      }, error=function(e) {
        div(style="color:red;","Model Error: ", e$message)
      }
    )
  })
  output$sem_plot<-renderGrViz({ semDiagram(fit_model()) })
}

# 4. Run App
shinyApp(ui, server)

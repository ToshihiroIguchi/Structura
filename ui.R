# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – ui.R
#   * Pure UI definition; all logic in server.R
# ---------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo.png")
  ),
  div(id = "app-logo",
      img(src = "logo.png", height = 40,
          title = "Structural Insights, Simplified")),
  title = "Structura",

  tabsetPanel(

    # ---------------- Data tab -----------------------------------
    tabPanel("Data",
             h4("Uploaded Data"),
             DTOutput("datatable")),

    # -------------- Filtered tab ---------------------------------
    tabPanel("Filtered",
             h4("Filtered Data"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             
             # Data Quality Alert Box
             div(id = "data_quality_alert_box",
                 style = "margin-bottom: 10px;",
                 uiOutput("data_quality_alert")),
             
             DTOutput("filtered_table"),
             tags$hr(),
             h4("Correlation Heatmap"),
             plotOutput("corr_heatmap", height = "300px")),

    # ---------------- Model tab ----------------------------------
    tabPanel("Model",
             fluidRow(
               # ---------- Left column (inputs) -------------------
               column(width = 7,
                      radioButtons("analysis_mode", "Analysis mode:",
                                   choices = c("Raw (unstandardized)"  = "raw",
                                               "Standardized (scaled)" = "std"),
                                   selected = "std", inline = TRUE),
                      selectInput("missing_method", "Missing Data Handling:",
                                  choices = c("Listwise deletion"          = "listwise",
                                              "FIML (ML)"                  = "ml",
                                              "FIML including exogenous x" = "ml.x",
                                              "Two-stage ML"               = "two.stage",
                                              "Robust two-stage ML"        = "robust.two.stage"),
                                  selected = "listwise"),
                      conditionalPanel(
                        condition = "input.analysis_mode == 'raw'",
                        checkboxInput("diagram_std",
                                      "Show standardized coefficients in diagram",
                                      value = TRUE)),
                      actionButton("run_model", "Run / Update Model",
                                   class = "btn btn-success"),
                      tags$hr(),
                      h4("Measurement Model"),
                      rHandsontableOutput("input_table"),
                      actionButton("add_row", "Add Row", class = "btn btn-primary"),
                      tags$hr(),
                      h4("Structural Model"),
                      rHandsontableOutput("checkbox_matrix"),
                      tags$hr(),
                      h4("Manual Equations"),
                      textAreaInput("extra_eq",
                                    "Additional lavaan syntax (one formula per line):",
                                    value = "",
                                    placeholder = "y1 ~ x1 + x2\nlatent2 =~ y3 + y4",
                                    rows = 4, resize = "vertical"),
                      tags$hr(),
                      h4("lavaan Syntax"),
                      verbatimTextOutput("lavaan_model")
               ),

               # ---------- Right column (outputs) -----------------
               column(width = 5,
                      tabsetPanel(id = "right_tabs", type = "tabs",
                                  tabPanel("Diagnostics",
                                           div(id = "fit_alert_box",
                                               textOutput("fit_alert"),
                                               class = "alert-box"),
                                           h4("Fit Indices"),
                                           DTOutput("fit_indices")),
                                  tabPanel("Equations",
                                           h4("Approximate Equations"),
                                           verbatimTextOutput("approx_eq")),
                                  tabPanel("Diagram Settings",
                                           h4("Path Diagram Options"),
                                           selectInput("layout_style", "Layout & Engine:",
                                                       choices = c("Hierarchical Left → Right (dot)" = "dot_LR",
                                                                   "Hierarchical Top → Bottom (dot)" = "dot_TB",
                                                                   "Spring model layout (neato)"      = "neato",
                                                                   "Force-Directed Placement (fdp)"   = "fdp",
                                                                   "Circular layout (circo)"          = "circo",
                                                                   "Radial layout (twopi)"            = "twopi"),
                                                       selected = "dot_LR"))
                      ),
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

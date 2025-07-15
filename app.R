# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – app.R (Unified Application)
#   * Complete Shiny application with UI and Server
#   * Modular architecture with clean dependencies
# ---------------------------------------------------------------

# Load dependencies
source("global.R")
source("helpers.R")
source("modules.R")

# ===============================================================
# UI DEFINITION
# ===============================================================

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
             
             h4("Analysis Settings"),
             fluidRow(
               column(6,
                      radioButtons("analysis_mode", "Analysis mode:",
                                   choices = c("Raw (unstandardized)"  = "raw",
                                               "Standardized (scaled)" = "std"),
                                   selected = "std", inline = TRUE)),
               column(6,
                      selectInput("missing_method", "Missing Data Handling:",
                                  choices = c("Listwise deletion"          = "listwise",
                                              "FIML (ML)"                  = "ml",
                                              "FIML including exogenous x" = "ml.x",
                                              "Two-stage ML"               = "two.stage",
                                              "Robust two-stage ML"        = "robust.two.stage"),
                                  selected = "listwise"))
             ),
             
             # Data Quality Alert Box
             div(id = "data_quality_alert_box",
                 style = "margin-bottom: 10px;",
                 uiOutput("data_quality_alert")),
             
             DTOutput("filtered_table"),
             tags$hr(),
             h4("Correlation Heatmap"),
             plotOutput("corr_heatmap", height = "500px")),

    # ---------------- Model tab ----------------------------------
    tabPanel("Model",
             fluidRow(
               # ---------- Left column (inputs) -------------------
               column(width = 7,
                      div(style = "margin-bottom: 15px;",
                          actionButton("run_model", "Run / Update Model",
                                       class = "btn btn-success btn-lg",
                                       style = "font-size: 14px; padding: 8px 20px; font-weight: bold;")),
                      conditionalPanel(
                        condition = "input.analysis_mode == 'raw'",
                        checkboxInput("diagram_std",
                                      "Show standardized coefficients in diagram",
                                      value = TRUE)),
                      h4("Measurement Model"),
                      rHandsontableOutput("input_table"),
                      div(style = "margin-top: 10px; margin-bottom: 10px;",
                          actionButton("add_row", "Add Row", 
                                       class = "btn btn-primary",
                                       style = "margin-left: 0;")),
                      tags$hr(),
                      h4("Structural Model"),
                      rHandsontableOutput("checkbox_matrix"),
                      tags$hr(),
                      h4("Variance & Covariance"),
                      radioButtons("covariance_mode", "Covariance Settings:",
                                   choices = c("Automatic (default)" = "auto",
                                               "Semi-automatic" = "semi",
                                               "Custom settings" = "custom"),
                                   selected = "auto", inline = TRUE),
                      conditionalPanel(
                        condition = "input.covariance_mode == 'semi'",
                        h5("Covariance Options"),
                        checkboxInput("cov_latent_latent", 
                                      "Estimate covariance between different latent variables", 
                                      value = TRUE),
                        checkboxInput("cov_observed_observed", 
                                      "Estimate covariance between different observed variables", 
                                      value = TRUE),
                        checkboxInput("cov_same_variable", 
                                      "Estimate variance of same variables", 
                                      value = TRUE)
                      ),
                      conditionalPanel(
                        condition = "input.covariance_mode == 'custom'",
                        rHandsontableOutput("covariance_matrix")
                      ),
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
                                               class = "alert-box",
                                               style = "display: none;"),
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
             h4("Path Diagram - Enlarged View"),
             div(style = "height: 80vh; overflow: auto; text-align: center; border: 1px solid #ddd; margin-bottom: 20px;",
                 grVizOutput("sem_plot_enlarged", width = "100%", height = "75vh")),
             tags$hr(),
             h4("Parameter Estimates"),
             DTOutput("param_tbl"),
             tags$hr(),
             h4("Model Summary"),
             verbatimTextOutput("fit_summary")),

    # ---------------- Help tab -----------------------------------
    tabPanel("Help", includeMarkdown("help.md"))
  )
)

# ===============================================================
# SERVER DEFINITION
# ===============================================================

server <- function(input, output, session) {
  
  write_log("INFO", "Server function initialized")
  
  # Session cleanup on disconnect
  session$onSessionEnded(function() {
    write_log("INFO", "User session ended")
  })
  
  # Shared reactive values container (replaces global variables)
  shared_values <- reactiveValues(
    fit_model = NULL,
    model_syntax = NULL,
    correlation_cache = NULL,
    processed_data = NULL
  )
  
  # Initialize data module
  data_module <- data_module_server(input, output, session, shared_values)
  
  # Show initial data loading modal
  data_module$show_modal()
  
  # Initialize model module with shared values and data dependency
  model_module <- model_module_server(input, output, session, shared_values, data_module)
  
  # Initialize plot module with shared values dependency
  plot_module <- plot_module_server(input, output, session, shared_values)
  
  # Session cleanup (no longer needed for global variables)
  onSessionEnded(function() {
    # Clean up reactive values
    shared_values$fit_model <- NULL
    shared_values$model_syntax <- NULL
    shared_values$correlation_cache <- NULL
    shared_values$processed_data <- NULL
  })
}

# ===============================================================
# RUN APPLICATION
# ===============================================================

shinyApp(ui = ui, server = server)
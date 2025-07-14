# Test script to verify alert box behavior
library(shiny)
library(shinyjs)

# Simple test app to check conditionalPanel behavior
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .alert-box { 
        background: #fff3cd; 
        border: 1px solid #ffeeba; 
        border-radius: 6px; 
        padding: 10px; 
        margin-bottom: 10px; 
      }
    "))
  ),
  
  h3("Alert Box Test"),
  
  # Test 1: conditionalPanel with empty output
  h4("Test 1: conditionalPanel with empty output"),
  conditionalPanel(
    condition = "output.test_alert != ''",
    div(id = "test_alert_box",
        textOutput("test_alert"),
        class = "alert-box")
  ),
  
  # Test 2: conditionalPanel with null/undefined output  
  h4("Test 2: conditionalPanel with null output"),
  conditionalPanel(
    condition = "output.test_alert != null && output.test_alert != ''",
    div(id = "test_alert_box2",
        textOutput("test_alert2"),
        class = "alert-box")
  ),
  
  # Test 3: Server-side conditional rendering
  h4("Test 3: Server-side conditional rendering"),
  uiOutput("conditional_alert"),
  
  actionButton("toggle", "Toggle Alert"),
  br(), br(),
  verbatimTextOutput("debug_info")
)

server <- function(input, output, session) {
  alert_text <- reactiveVal("")
  
  output$test_alert <- renderText({
    alert_text()
  })
  
  output$test_alert2 <- renderText({
    alert_text()
  })
  
  output$conditional_alert <- renderUI({
    if (nzchar(alert_text())) {
      div(id = "server_alert_box",
          p(alert_text()),
          class = "alert-box")
    } else {
      NULL
    }
  })
  
  output$debug_info <- renderText({
    paste("Alert text:", shQuote(alert_text()),
          "\nLength:", nchar(alert_text()),
          "\nnzchar:", nzchar(alert_text()))
  })
  
  observeEvent(input$toggle, {
    if (nzchar(alert_text())) {
      alert_text("")
    } else {
      alert_text("This is a test alert message!")
    }
  })
}

shinyApp(ui = ui, server = server, options = list(port = 8100, host = "0.0.0.0"))
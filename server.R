# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – server.R (Clean Architecture)
#   * Reactive values-based module communication
#   * No global environment pollution
#   * Dependency injection pattern
# ---------------------------------------------------------------

# Source all server modules
source("server/data_module.R", local = TRUE)
source("server/model_module.R", local = TRUE)
source("server/plot_module.R", local = TRUE)

server <- function(input, output, session) {
  
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
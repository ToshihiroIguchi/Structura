# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – server.R (Modularized)
#   * Main server function using modular architecture
#   * Modules handle specific functionality areas
# ---------------------------------------------------------------

# Source all server modules
source("server/data_module.R", local = TRUE)
source("server/model_module.R", local = TRUE)
source("server/plot_module.R", local = TRUE)

server <- function(input, output, session) {
  
  # Initialize data module
  data_module <- data_module_server(input, output, session)
  
  # Show initial data loading modal
  data_module$show_modal()
  
  # Initialize model module with data dependency
  model_module <- model_module_server(input, output, session, data_module)
  
  # Initialize plot module with dependencies
  plot_module <- plot_module_server(input, output, session, data_module)
  
  # Make model functions available to plot module
  lavaan_model_str <- model_module$lavaan_model_str
  fit_model_safe <- model_module$fit_model_safe
  
  # Global reactive values available to all modules
  observe({
    # Ensure model functions are available in global environment
    # for plot module to access
    assign("lavaan_model_str", lavaan_model_str, envir = globalenv())
    assign("fit_model_safe", fit_model_safe, envir = globalenv())
  })
  
  # Session end cleanup
  onSessionEnded(function() {
    # Clean up global variables
    if (exists("lavaan_model_str", envir = globalenv())) {
      rm("lavaan_model_str", envir = globalenv())
    }
    if (exists("fit_model_safe", envir = globalenv())) {
      rm("fit_model_safe", envir = globalenv())
    }
  })
}
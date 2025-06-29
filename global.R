# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – global.R
#   * Load libraries
#   * Apply environment configuration
#   * Source helper functions
# ---------------------------------------------------------------

# ---- Libraries --------------------------------------------------
library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(readflex)
library(lavaan)
library(DiagrammeR)
library(semDiagram)
library(ggplot2)
library(reshape2)
library(markdown)

# ---- Configuration System --------------------------------------
source("config/app_config.R", local = TRUE)
app_config <- apply_shiny_config()

# ---- Global helpers --------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Error logging setup --------------------------------------
log_file <- file.path(getwd(), "structura_app.log")
log_connection <- file(log_file, open = "a")

write_log <- function(level, message, details = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, message)
  if (!is.null(details)) {
    log_entry <- paste(log_entry, "\nDetails:", details)
  }
  cat(log_entry, "\n", file = log_connection, append = TRUE)
  flush(log_connection)
  cat(log_entry, "\n", file = stderr())
}

options(shiny.error = function() {
  error_details <- if (exists("geterrmessage")) geterrmessage() else "Unknown error"
  traceback_info <- capture.output(traceback())
  traceback_text <- if (length(traceback_info) > 0) paste(traceback_info, collapse = "\n") else "No traceback available"
  
  write_log("ERROR", "Application crashed", paste("Error:", error_details, "\nTraceback:\n", traceback_text))
})

options(warn = 1)
options(shiny.trace = TRUE)

write_log("INFO", "Application starting up")

# ---- Cleanup on exit ------------------------------------------
reg.finalizer(.GlobalEnv, function(e) {
  tryCatch({
    if (exists("log_connection") && inherits(log_connection, "connection") && isOpen(log_connection)) {
      write_log("INFO", "Application shutting down")
      close(log_connection)
    }
  }, error = function(err) {
    # Silent error handling for cleanup
  })
}, onexit = TRUE)

# ---- Load shared functions -------------------------------------
source("helpers.R", local = TRUE)
source("utils/error_handler.R", local = TRUE)

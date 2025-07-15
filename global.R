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

# ===============================================================
# APPLICATION CONFIGURATION
# ===============================================================
# Environment-specific settings for development, production, and testing

get_app_config <- function(env = NULL) {
  
  # Auto-detect environment if not specified
  if (is.null(env)) {
    env <- Sys.getenv("SHINY_ENV", "development")
    
    # Additional environment detection
    if (Sys.getenv("R_CONFIG_ACTIVE") != "") {
      env <- Sys.getenv("R_CONFIG_ACTIVE")
    }
    
    # Docker/container detection
    if (file.exists("/.dockerenv")) {
      env <- "production"
    }
  }
  
  # Base configuration
  base_config <- list(
    app_name = "Structura",
    version = "1.0.0",
    locale = "en_US.UTF-8"
  )
  
  # Environment-specific configurations
  configs <- list(
    development = list(
      shiny.fullstacktrace = TRUE,
      shiny.reactlog = TRUE,
      shiny.sanitize.errors = FALSE,
      shiny.autoreload = TRUE,
      locale = "ja_JP.UTF-8",
      log_level = "DEBUG",
      host = "0.0.0.0",
      port = 8100
    ),
    
    production = list(
      shiny.fullstacktrace = FALSE,
      shiny.reactlog = FALSE,
      shiny.sanitize.errors = TRUE,
      shiny.autoreload = FALSE,
      locale = "ja_JP.UTF-8",
      log_level = "INFO",
      host = "0.0.0.0",
      port = 3838
    ),
    
    testing = list(
      shiny.fullstacktrace = TRUE,
      shiny.reactlog = FALSE,
      shiny.sanitize.errors = FALSE,
      shiny.autoreload = FALSE,
      locale = "ja_JP.UTF-8",
      log_level = "ERROR",
      host = "127.0.0.1",
      port = 8888
    )
  )
  
  # Get environment config or fallback to development
  env_config <- configs[[env]] %||% configs[["development"]]
  
  # Merge base and environment configs
  final_config <- c(base_config, env_config)
  final_config$environment <- env
  
  return(final_config)
}

# Apply configuration to Shiny options
apply_shiny_config <- function(config = NULL) {
  if (is.null(config)) {
    config <- get_app_config()
  }
  
  # Apply Shiny-specific options
  options(
    shiny.fullstacktrace = config$shiny.fullstacktrace,
    shiny.reactlog = config$shiny.reactlog,
    shiny.sanitize.errors = config$shiny.sanitize.errors,
    shiny.maxRequestSize = 50 * 1024^2  # 50MB file upload limit
  )
  
  # Apply autoreload if available
  if (!is.null(config$shiny.autoreload) && config$shiny.autoreload) {
    options(shiny.autoreload = TRUE)
  }
  
  # Set locale if specified
  if (!is.null(config$locale)) {
    tryCatch({
      Sys.setlocale("LC_CTYPE", config$locale)
    }, warning = function(w) {
      message("Warning: Could not set locale to ", config$locale)
    })
  }
  
  return(config)
}

# ---- Load shared functions -------------------------------------
source("helpers.R", local = TRUE)

# ---- Apply configuration --------------------------------------
app_config <- apply_shiny_config()

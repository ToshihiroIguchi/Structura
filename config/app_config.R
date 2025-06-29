# Application Configuration System
# Provides environment-specific settings for development, production, and testing

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
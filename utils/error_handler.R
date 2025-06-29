# Common Error Handling Utilities
# Provides centralized error handling for the Structura application

safe_execute <- function(expr, error_msg = "Operation failed", notify = TRUE, fallback = NULL, log_error = FALSE) {
  tryCatch({
    expr
  }, error = function(e) {
    # Determine error type and create appropriate message
    full_message <- if (is.character(error_msg)) {
      paste(error_msg, ":", e$message)
    } else {
      e$message
    }
    
    # Log error if requested
    if (log_error) {
      cat(sprintf("[ERROR %s] %s\n", Sys.time(), full_message), file = stderr())
    }
    
    # Show notification if Shiny context available
    if (notify && exists("showNotification")) {
      tryCatch({
        showNotification(
          full_message,
          type = "error",
          duration = 5
        )
      }, error = function(notification_error) {
        # Fallback if showNotification fails
        warning(paste("Notification failed:", notification_error$message))
      })
    }
    
    # Return fallback value or re-throw error
    if (!is.null(fallback)) {
      return(fallback)
    } else {
      stop(e)
    }
  }, warning = function(w) {
    # Handle warnings without stopping execution
    if (notify && exists("showNotification")) {
      tryCatch({
        showNotification(
          paste("Warning:", w$message),
          type = "warning",
          duration = 3
        )
      }, error = function(notification_error) {
        warning(paste("Warning notification failed:", notification_error$message))
      })
    }
    
    if (log_error) {
      cat(sprintf("[WARNING %s] %s\n", Sys.time(), w$message), file = stderr())
    }
    
    # Continue with original result
    suppressWarnings(expr)
  })
}
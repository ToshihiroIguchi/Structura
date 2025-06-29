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

# Validate lavaan syntax input for security and correctness
validate_lavaan_syntax <- function(syntax_text) {
  if (is.null(syntax_text) || !is.character(syntax_text) || length(syntax_text) != 1) {
    return(list(valid = FALSE, message = "Invalid input format"))
  }
  
  # Remove leading/trailing whitespace
  cleaned_text <- trimws(syntax_text)
  
  # Return valid if empty (no equations to add)
  if (nchar(cleaned_text) == 0) {
    return(list(valid = TRUE, message = ""))
  }
  
  # Split into individual lines
  lines <- strsplit(cleaned_text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]  # Remove empty lines
  
  # Security checks - prevent dangerous patterns
  dangerous_patterns <- c(
    "system\\s*\\(",     # system() calls
    "eval\\s*\\(",       # eval() calls  
    "source\\s*\\(",     # source() calls
    "library\\s*\\(",    # library() calls
    "require\\s*\\(",    # require() calls
    "setwd\\s*\\(",      # setwd() calls
    "file\\.",           # file operations
    "unlink\\s*\\(",     # file deletion
    "write\\.",          # write operations
    "save\\s*\\(",       # save operations
    "load\\s*\\(",       # load operations
    "rm\\s*\\(",         # object removal
    "remove\\s*\\(",     # object removal
    "get\\s*\\(",        # environment access
    "assign\\s*\\(",     # environment assignment
    "<<-",               # global assignment
    "->>"                # reverse global assignment
  )
  
  for (pattern in dangerous_patterns) {
    if (any(grepl(pattern, lines, ignore.case = TRUE))) {
      return(list(valid = FALSE, message = "Invalid syntax: potentially unsafe operations detected"))
    }
  }
  
  # Basic lavaan syntax validation
  valid_operators <- c("=~", "~", "~~", "~1", ":=", "<", ">", "==")
  
  for (line in lines) {
    # Skip comments
    if (grepl("^\\s*#", line)) next
    
    # Check if line contains at least one valid lavaan operator
    has_valid_operator <- any(sapply(valid_operators, function(op) grepl(paste0("\\", op), line, fixed = TRUE)))
    
    if (!has_valid_operator) {
      return(list(valid = FALSE, message = paste("Invalid syntax on line:", line)))
    }
    
    # Check for basic variable name patterns (alphanumeric, dots, underscores)
    # Remove operators and check remaining tokens
    temp_line <- line
    for (op in valid_operators) {
      temp_line <- gsub(paste0("\\", op), " ", temp_line, fixed = TRUE)
    }
    
    # Extract potential variable names
    tokens <- unlist(strsplit(temp_line, "[\\s\\+\\*\\-\\(\\)\\[\\]\\{\\}\\,\\;]+"))
    tokens <- tokens[nzchar(tokens)]
    
    # Check each token is a valid variable name or number
    for (token in tokens) {
      if (!grepl("^[a-zA-Z][a-zA-Z0-9\\._]*$|^[0-9\\.]+$", token)) {
        return(list(valid = FALSE, message = paste("Invalid variable name:", token)))
      }
    }
  }
  
  return(list(valid = TRUE, message = "Syntax appears valid"))
}
# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – helpers.R
#   * Utility functions used by both ui.R and server.R
# ---------------------------------------------------------------

# ---- lavaan_to_equations ---------------------------------------
#   * Indicator  =  intercept + loading * Latent
#   * Dependent  =  intercept + Σ( slope * Predictor )
#   * 全て Raw (非標準化) 係数で生成
lavaan_to_equations <- function(fit, digits = 3) {

  # 係数取得（標準化しない）
  pe <- parameterEstimates(fit, standardized = FALSE, remove.def = TRUE)

  # 数値フォーマッタ
  format_est <- function(x, digits = 3) {
    sapply(x, function(v) {
      if (is.na(v)) return("NA")
      if (abs(v) < 10^(-digits))
        format(v, digits = digits, scientific = TRUE)
      else
        format(round(v, digits), nsmall = digits)
    })
  }

  # データフレーム分割
  meas_df      <- pe[pe$op == "=~",  ]   # 測定方程式
  reg_df       <- pe[pe$op == "~",   ]   # 構造方程式
  intercept_df <- pe[pe$op == "~1",  ]   # 切片

  eq_lines <- character(0)

  # 1. 測定方程式
  if (nrow(meas_df)) {
    for (i in seq_len(nrow(meas_df))) {
      ind     <- meas_df$rhs[i]
      lat     <- meas_df$lhs[i]
      loading <- format_est(meas_df$est[i], digits)
      int_val <- intercept_df$est[intercept_df$lhs == ind]
      rhs     <- c(if (length(int_val)) format_est(int_val, digits) else NULL,
                   paste0(loading, "*", lat))
      eq_lines <- c(eq_lines, paste(ind, "=", paste(rhs, collapse = " + ")))
    }
  }

  # 2. 構造方程式
  if (nrow(reg_df)) {
    reg_split <- split(reg_df, reg_df$lhs)
    for (lhs in names(reg_split)) {
      df  <- reg_split[[lhs]]
      int <- intercept_df$est[intercept_df$lhs == lhs]
      rhs <- paste0(format_est(df$est, digits), "*", df$rhs)
      rhs <- c(if (length(int)) format_est(int, digits) else NULL, rhs)
      eq_lines <- c(eq_lines, paste(lhs, "=", paste(rhs, collapse = " + ")))
    }
  }

  eq_lines
}

# ---- File loader ------------------------------------------------
loadDataOnce <- function(fileInput) {
  req(fileInput)
  
  # File validation and quality checks
  validateUploadedFile(fileInput)
  
  # Load data with error handling
  data <- safe_execute({
    readflex(fileInput$datapath, stringsAsFactors = FALSE)
  }, error_msg = "Failed to read file", fallback = NULL)
  
  if (is.null(data)) {
    stop("File could not be loaded")
  }
  
  # Post-load data quality validation
  validateDataQuality(data, fileInput$name)
  
  return(data)
}

# File upload validation function
validateUploadedFile <- function(fileInput) {
  # Check if fileInput has required structure
  if (is.null(fileInput) || !is.list(fileInput) || is.null(fileInput$name)) {
    stop("Invalid file input structure")
  }
  
  # Check file extension
  ext <- tolower(tools::file_ext(fileInput$name))
  if (length(ext) == 0 || ext == "") {
    stop("File has no extension. Please upload files with .csv, .txt, .tsv, or .data extension")
  }
  
  allowed_extensions <- c("csv", "txt", "tsv", "data")
  
  if (!ext %in% allowed_extensions) {
    stop(paste("Invalid file type. Allowed formats:", paste(allowed_extensions, collapse = ", ")))
  }
  
  # Check file size (additional check beyond Shiny default)
  file_size <- file.info(fileInput$datapath)$size
  max_size <- 50 * 1024 * 1024  # 50MB limit
  
  if (is.na(file_size) || file_size > max_size) {
    stop(paste("File too large. Maximum size allowed:", round(max_size / 1024 / 1024, 1), "MB"))
  }
  
  if (file_size == 0) {
    stop("File is empty")
  }
  
  # Basic file content preview check
  tryCatch({
    # Read first few lines to validate structure
    con <- file(fileInput$datapath, "r")
    first_lines <- readLines(con, n = 3, warn = FALSE)
    close(con)
    
    if (length(first_lines) == 0) {
      stop("File appears to be empty or unreadable")
    }
    
    # Check for potential binary content
    if (any(grepl("[\\x01-\\x08\\x0E-\\x1F\\x7F]", first_lines, perl = TRUE))) {
      stop("File appears to contain binary data. Please upload a text-based CSV file")
    }
    
  }, error = function(e) {
    if (grepl("binary data", e$message)) {
      stop(e$message)
    }
    stop("Unable to validate file content. Please ensure file is a valid CSV format")
  })
}

# Data quality validation function
validateDataQuality <- function(data, filename) {
  # Check if data.frame was successfully created
  if (!is.data.frame(data)) {
    stop("File does not contain valid tabular data")
  }
  
  # Check dimensions
  if (nrow(data) == 0) {
    stop("File contains no data rows")
  }
  
  if (ncol(data) == 0) {
    stop("File contains no columns")
  }
  
  # Reasonable limits for SEM analysis
  if (nrow(data) > 50000) {
    stop(paste("Dataset too large for analysis. Maximum 50,000 rows allowed. Your file has", nrow(data), "rows"))
  }
  
  if (ncol(data) > 200) {
    stop(paste("Too many variables for analysis. Maximum 200 columns allowed. Your file has", ncol(data), "columns"))
  }
  
  # Check for reasonable sample size for SEM
  if (nrow(data) < 3) {
    stop("Insufficient sample size. Minimum 3 observations required for analysis")
  }
  
  # Check for at least some numeric data
  numeric_cols <- sapply(data, function(x) is.numeric(x) || (is.character(x) && all(grepl("^-?[0-9]*\\.?[0-9]+$", x[!is.na(x) & x != ""], perl = TRUE))))
  
  if (sum(numeric_cols) < 2) {
    stop("File must contain at least 2 numeric variables for structural equation modeling")
  }
  
  # Memory estimation check
  estimated_memory <- object.size(data)
  max_memory <- 100 * 1024 * 1024  # 100MB
  
  if (estimated_memory > max_memory) {
    stop(paste("Dataset requires too much memory:", round(as.numeric(estimated_memory) / 1024 / 1024, 1), "MB. Maximum allowed:", round(max_memory / 1024 / 1024, 1), "MB"))
  }
  
  # Check for excessive missing data
  missing_proportion <- sum(is.na(data)) / (nrow(data) * ncol(data))
  if (missing_proportion > 0.5) {
    warning(paste("High proportion of missing data detected:", round(missing_proportion * 100, 1), "%. This may affect analysis quality"))
  }
  
  # Advanced data quality checks for numeric variables
  numeric_data <- data[sapply(data, is.numeric)]
  if (ncol(numeric_data) >= 2) {
    
    # Check for zero variance variables
    zero_variance_vars <- names(numeric_data)[sapply(numeric_data, function(x) {
      clean_x <- x[!is.na(x)]
      if (length(clean_x) < 2) return(TRUE)
      var(clean_x, na.rm = TRUE) == 0 || sd(clean_x, na.rm = TRUE) < 1e-10
    })]
    
    if (length(zero_variance_vars) > 0) {
      warning(paste("Variables with zero or near-zero variance detected:", paste(zero_variance_vars, collapse = ", "), ". These variables cannot be used in correlation analysis"))
    }
    
    # Check for perfect multicollinearity (correlation = 1.0)
    if (ncol(numeric_data) >= 2 && nrow(numeric_data) >= 3) {
      tryCatch({
        correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
        
        # Find perfect correlations (excluding diagonal)
        perfect_correlations <- which(abs(correlation_matrix) >= 0.999 & correlation_matrix != 1, arr.ind = TRUE)
        
        if (nrow(perfect_correlations) > 0) {
          perfect_pairs <- character(0)
          processed_pairs <- character(0)
          
          for (i in seq_len(nrow(perfect_correlations))) {
            row_idx <- perfect_correlations[i, 1]
            col_idx <- perfect_correlations[i, 2]
            var1 <- rownames(correlation_matrix)[row_idx]
            var2 <- colnames(correlation_matrix)[col_idx]
            
            # Avoid duplicate pairs (A-B and B-A)
            pair_key <- paste(sort(c(var1, var2)), collapse = "-")
            if (!pair_key %in% processed_pairs) {
              correlation_value <- round(correlation_matrix[row_idx, col_idx], 4)
              perfect_pairs <- c(perfect_pairs, paste0(var1, " & ", var2, " (r=", correlation_value, ")"))
              processed_pairs <- c(processed_pairs, pair_key)
            }
          }
          
          if (length(perfect_pairs) > 0) {
            warning(paste("Perfect or near-perfect correlations detected:", paste(perfect_pairs, collapse = "; "), ". This may cause model identification problems"))
          }
        }
        
        # Check for high correlations (>0.95) as warning
        high_correlations <- which(abs(correlation_matrix) >= 0.95 & abs(correlation_matrix) < 0.999 & correlation_matrix != 1, arr.ind = TRUE)
        
        if (nrow(high_correlations) > 0) {
          high_pairs <- character(0)
          processed_pairs <- character(0)
          
          for (i in seq_len(nrow(high_correlations))) {
            row_idx <- high_correlations[i, 1]
            col_idx <- high_correlations[i, 2]
            var1 <- rownames(correlation_matrix)[row_idx]
            var2 <- colnames(correlation_matrix)[col_idx]
            
            pair_key <- paste(sort(c(var1, var2)), collapse = "-")
            if (!pair_key %in% processed_pairs) {
              correlation_value <- round(correlation_matrix[row_idx, col_idx], 3)
              high_pairs <- c(high_pairs, paste0(var1, " & ", var2, " (r=", correlation_value, ")"))
              processed_pairs <- c(processed_pairs, pair_key)
            }
          }
          
          if (length(high_pairs) > 0 && length(high_pairs) <= 5) {
            warning(paste("High correlations detected:", paste(high_pairs, collapse = "; "), ". Monitor for potential multicollinearity issues"))
          } else if (length(high_pairs) > 5) {
            warning(paste("Multiple high correlations detected (", length(high_pairs), " pairs). Consider reviewing variable selection for multicollinearity"))
          }
        }
        
      }, error = function(e) {
        # Silent error handling for correlation calculation
      })
    }
  }
  
  # Check for duplicate column names
  if (any(duplicated(names(data)))) {
    duplicate_names <- names(data)[duplicated(names(data))]
    stop(paste("Duplicate column names detected:", paste(duplicate_names, collapse = ", ")))
  }
  
  # Check for problematic column names
  problematic_names <- names(data)[grepl("^[0-9]|[^a-zA-Z0-9._]", names(data))]
  if (length(problematic_names) > 0) {
    warning(paste("Some column names may cause issues in analysis:", paste(head(problematic_names, 3), collapse = ", "), ". Consider renaming columns to start with letters and use only letters, numbers, dots, and underscores"))
  }
}

# ---- Identification Problem Detection -------------------------
# Generate user-friendly identification problem messages
generate_identification_message <- function(model_syntax, data, original_error) {
  
  # Extract factors from model syntax
  factors <- extract_factors_from_syntax(model_syntax)
  
  # Check for common identification issues
  issues <- list()
  
  # Issue 1: Scale setting check
  for (factor in factors) {
    if (!has_scale_setting(factor, model_syntax)) {
      first_indicator <- get_first_indicator(factor, model_syntax)
      issues <- append(issues, sprintf(
        "Factor '%s' needs scale setting. Add: '%s =~ 1*%s + other_variables'", 
        factor, factor, first_indicator
      ))
    }
  }
  
  # Issue 2: Insufficient indicators check  
  for (factor in factors) {
    indicator_count <- count_factor_indicators(factor, model_syntax)
    if (indicator_count < 3) {
      issues <- append(issues, sprintf(
        "Factor '%s' has only %d indicators (minimum 3 recommended for identification)", 
        factor, indicator_count
      ))
    }
  }
  
  # Issue 3: Degrees of freedom check
  if (!is.null(data)) {
    df_check <- estimate_degrees_of_freedom(model_syntax, data)
    if (df_check$df < 0) {
      issues <- append(issues, sprintf(
        "Model is over-parameterized: %d free parameters vs %d available moments (df = %d)",
        df_check$n_params, df_check$n_moments, df_check$df
      ))
    }
  }
  
  # Generate user-friendly message
  if (length(issues) > 0) {
    base_msg <- "IDENTIFICATION ISSUE DETECTED:\n\n"
    detailed_issues <- paste("• ", issues, collapse = "\n")
    solution_msg <- "\n\nMost common solution: Fix the first loading of each factor to 1.0"
    
    return(paste0(base_msg, detailed_issues, solution_msg))
  } else {
    return("Model identification problem detected. Check factor loadings and model complexity.")
  }
}

# Extract factor names from lavaan syntax
extract_factors_from_syntax <- function(model_syntax) {
  if (length(model_syntax) == 0) return(character(0))
  
  # Combine all syntax lines
  full_syntax <- paste(model_syntax, collapse = "\n")
  
  # Find measurement model lines (contains =~)
  measurement_lines <- unlist(strsplit(full_syntax, "\n"))
  measurement_lines <- measurement_lines[grepl("=~", measurement_lines)]
  
  # Extract factor names (left side of =~)
  factors <- character(0)
  for (line in measurement_lines) {
    if (grepl("=~", line)) {
      factor_name <- trimws(sub("=~.*", "", line))
      factors <- c(factors, factor_name)
    }
  }
  
  return(unique(factors))
}

# Check if factor has scale setting (1* constraint)
has_scale_setting <- function(factor, model_syntax) {
  if (length(model_syntax) == 0) return(FALSE)
  
  full_syntax <- paste(model_syntax, collapse = "\n")
  
  # Look for pattern: factor =~ 1*variable or factor =~ variable*1
  pattern <- sprintf("%s\\s*=~.*1\\*\\w+|%s\\s*=~.*\\w+\\*1", factor, factor)
  
  return(grepl(pattern, full_syntax))
}

# Get first indicator of a factor
get_first_indicator <- function(factor, model_syntax) {
  if (length(model_syntax) == 0) return("var1")
  
  full_syntax <- paste(model_syntax, collapse = "\n")
  
  # Find the measurement line for this factor
  lines <- unlist(strsplit(full_syntax, "\n"))
  factor_line <- lines[grepl(sprintf("%s\\s*=~", factor), lines)][1]
  
  if (is.na(factor_line)) return("var1")
  
  # Extract indicators (right side of =~)
  indicators_part <- sub(".*=~\\s*", "", factor_line)
  indicators_part <- gsub("1\\*", "", indicators_part)  # Remove existing constraints
  
  # Split by + and get first indicator
  indicators <- unlist(strsplit(indicators_part, "\\+"))
  first_indicator <- trimws(indicators[1])
  
  # Clean up variable name
  first_indicator <- gsub("[^a-zA-Z0-9_\\.]", "", first_indicator)
  
  return(if (nchar(first_indicator) > 0) first_indicator else "var1")
}

# Count indicators for a factor
count_factor_indicators <- function(factor, model_syntax) {
  if (length(model_syntax) == 0) return(0)
  
  full_syntax <- paste(model_syntax, collapse = "\n")
  
  # Find the measurement line for this factor
  lines <- unlist(strsplit(full_syntax, "\n"))
  factor_line <- lines[grepl(sprintf("%s\\s*=~", factor), lines)][1]
  
  if (is.na(factor_line)) return(0)
  
  # Extract and count indicators
  indicators_part <- sub(".*=~\\s*", "", factor_line)
  indicators <- unlist(strsplit(indicators_part, "\\+"))
  indicators <- trimws(indicators)
  indicators <- indicators[nchar(indicators) > 0]
  
  return(length(indicators))
}

# Estimate degrees of freedom
estimate_degrees_of_freedom <- function(model_syntax, data) {
  if (is.null(data) || length(model_syntax) == 0) {
    return(list(df = NA, n_params = NA, n_moments = NA))
  }
  
  n_vars <- ncol(data)
  n_moments <- n_vars * (n_vars + 1) / 2
  
  # Rough parameter counting
  factors <- extract_factors_from_syntax(model_syntax)
  n_factors <- length(factors)
  
  # Estimate parameters:
  # - Factor loadings (minus constraints)
  # - Error variances (one per observed variable)  
  # - Factor variances
  # - Factor covariances
  
  total_loadings <- sum(sapply(factors, function(f) count_factor_indicators(f, model_syntax)))
  fixed_loadings <- sum(sapply(factors, function(f) has_scale_setting(f, model_syntax)))
  
  free_loadings <- total_loadings - fixed_loadings
  error_variances <- n_vars
  factor_variances <- n_factors
  factor_covariances <- n_factors * (n_factors - 1) / 2
  
  n_params <- free_loadings + error_variances + factor_variances + factor_covariances
  df <- n_moments - n_params
  
  return(list(
    df = df,
    n_params = n_params,
    n_moments = n_moments
  ))
}

# ---- Covariance Matrix Validation -----------------------------
# Validate covariance matrix structure and settings
validate_covariance_matrix <- function(cov_data) {
  if (is.null(cov_data) || !is.data.frame(cov_data)) {
    return(FALSE)
  }
  
  # Check required columns
  if (!"Variable" %in% names(cov_data)) {
    return(FALSE)
  }
  
  # Check if matrix is square
  var_cols <- names(cov_data)[2:ncol(cov_data)]  # Skip "Variable" column only
  if (length(var_cols) != nrow(cov_data)) {
    return(FALSE)
  }
  
  # Check if variable names match column names
  if (!all(cov_data$Variable == var_cols)) {
    return(FALSE)
  }
  
  # Check valid settings
  valid_settings <- c("auto", "fix", "0", "")  # Include empty string for lower triangle
  for (i in seq_len(nrow(cov_data))) {
    for (j in seq_along(var_cols)) {
      setting <- cov_data[i, var_cols[j]]
      if (!setting %in% valid_settings) {
        return(FALSE)
      }
    }
  }
  
  # Check diagonal consistency (should be variance-related)
  for (i in seq_len(nrow(cov_data))) {
    diagonal_setting <- cov_data[i, var_cols[i]]
    if (!diagonal_setting %in% c("auto", "fix", "0")) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

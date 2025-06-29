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

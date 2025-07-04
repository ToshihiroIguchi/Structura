# Model Management Module
# Handles SEM model specification, fitting, and results

model_module_server <- function(input, output, session, shared_values, data_module) {
  
  # Measurement model table data
  input_table_data <- reactiveVal(NULL)
  
  # Initialize measurement table when columns change
  observeEvent(input$display_columns, ignoreNULL = FALSE, {
    inds <- as.character(input$display_columns %||% names(data_module$processed_data()))
    
    if (length(inds) > 0) {
      init <- data.frame(
        Latent = "LatentVariable1",
        Indicator = "",
        Operator = "=~",
        matrix(FALSE, nrow = 1, ncol = length(inds)),
        stringsAsFactors = FALSE
      )
      colnames(init) <- c("Latent", "Indicator", "Operator", inds)
      input_table_data(init)
    }
  })
  
  # Render measurement model table
  output$input_table <- renderRHandsontable({
    df <- input_table_data()
    req(df)
    
    tryCatch({
      rh <- rhandsontable(df, rowHeaders = FALSE) %>%
        hot_table(highlightReadOnly = TRUE)
      rh <- hot_col(rh, "Latent")
      rh <- hot_col(rh, "Indicator", readOnly = TRUE)
      rh <- hot_col(rh, "Operator", readOnly = TRUE)
      
      for (nm in setdiff(colnames(df), c("Latent", "Indicator", "Operator"))) {
        rh <- hot_col(rh, nm, type = "checkbox")
      }
      rh
    }, error = function(e) {
      showNotification(
        paste("Measurement table error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Update measurement table
  observeEvent(input$input_table, {
    tbl <- hot_to_r(input$input_table)
    req(tbl)
    
    tryCatch({
      tbl$Latent <- make.names(tbl$Latent, unique = FALSE)
      convs <- make.unique(c(names(data_module$processed_data()), tbl$Latent))
      tbl$Indicator <- tail(convs, nrow(tbl))
      input_table_data(tbl)
    }, error = function(e) {
      showNotification(
        paste("Table update error:", e$message),
        type = "warning",
        duration = 3
      )
    })
  })
  
  # Add row to measurement table
  observeEvent(input$add_row, {
    df <- input_table_data()
    req(df)
    
    new_row <- df[1, ]
    new_row[, ] <- FALSE
    new_row$Latent <- ""
    new_row$Operator <- "=~"
    input_table_data(rbind(df, new_row))
  })
  
  # Covariance matrix data
  covariance_table_data <- reactiveVal(NULL)
  
  # Initialize covariance matrix when variables change
  observeEvent(c(input$display_columns, input_table_data(), input$covariance_mode), ignoreNULL = FALSE, {
    if (!is.null(input$covariance_mode) && input$covariance_mode == "custom") {
      df <- data_module$processed_data()
      meas <- input_table_data()
      
      if (!is.null(df) && !is.null(meas)) {
        # Get all variables (observed + latent)
        deps <- as.character(input$display_columns %||% names(df))
        vars <- names(meas)[4:ncol(meas)]
        row_has_indicator <- apply(meas[vars], 1, function(x) any(as.logical(x)))
        latent_vars <- setdiff(na.omit(unique(meas$Indicator[row_has_indicator])), "")
        all_vars <- unique(c(deps, latent_vars))
        
        if (length(all_vars) > 0) {
          # Create true symmetric matrix structure (square matrix)
          n_vars <- length(all_vars)
          
          # Initialize matrix with variable names as both rows and columns
          cov_mat <- matrix("auto", nrow = n_vars, ncol = n_vars)
          rownames(cov_mat) <- all_vars
          colnames(cov_mat) <- all_vars
          
          # Set diagonal to auto estimation (default for variance)
          diag(cov_mat) <- "auto"
          
          # Set lower triangle to empty (avoid confusion)
          for (i in seq_len(n_vars)) {
            for (j in seq_len(n_vars)) {
              if (i > j) {
                cov_mat[i, j] <- ""
              }
            }
          }
          
          # Convert to data frame with row names as first column
          cov_df <- data.frame(
            Variable = all_vars,
            stringsAsFactors = FALSE
          )
          
          # Add each variable as a column
          for (var in all_vars) {
            cov_df[[var]] <- cov_mat[, var]
          }
          
          covariance_table_data(cov_df)
        }
      }
    }
  })
  
  # Render covariance matrix
  output$covariance_matrix <- renderRHandsontable({
    if (is.null(input$covariance_mode) || input$covariance_mode != "custom") return(NULL)
    
    df <- covariance_table_data()
    req(df)
    
    tryCatch({
      vars <- names(df)[2:ncol(df)]  # Skip "Variable" column, no "Operator" column
      
      rh <- rhandsontable(df, rowHeaders = FALSE) %>%
        hot_table(highlightReadOnly = TRUE)
      rh <- hot_col(rh, "Variable", readOnly = TRUE)
      
      # Configure each variable column as dropdown with symmetric behavior
      for (i in seq_along(vars)) {
        col_name <- vars[i]
        
        # Create custom renderer for symmetric matrix visualization
        renderer_code <- sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            
            // Color coding for matrix structure
            if (row === col - 1) {
              // Diagonal - variance (light blue)
              Handsontable.renderers.DropdownRenderer.apply(this, arguments);
              td.style.backgroundColor = '#e6f3ff';
            } else if (row < col - 1) {
              // Upper triangle - covariance (light green)
              Handsontable.renderers.DropdownRenderer.apply(this, arguments);
              td.style.backgroundColor = '#f0fff0';
            } else {
              // Lower triangle - empty and read-only (very light gray)
              Handsontable.renderers.TextRenderer.apply(this, arguments);
              td.style.backgroundColor = '#fafafa';
              td.style.border = '1px solid #e0e0e0';
              cellProperties.readOnly = true;
              td.innerHTML = '';  // Force empty content
            }
          }")
        
        rh <- hot_col(rh, col_name, type = "dropdown", 
                      source = c("auto", "0", "fix"),
                      strict = TRUE,
                      renderer = renderer_code)
      }
      
      rh
    }, error = function(e) {
      showNotification(
        paste("Covariance matrix error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Update covariance matrix
  observeEvent(input$covariance_matrix, {
    if (is.null(input$covariance_mode) || input$covariance_mode != "custom") return()
    
    tbl <- hot_to_r(input$covariance_matrix)
    req(tbl)
    
    tryCatch({
      # Keep lower triangle empty for clarity
      vars <- names(tbl)[2:ncol(tbl)]  # Skip "Variable" column
      for (i in seq_along(vars)) {
        for (j in seq_along(vars)) {
          if (i > j) {
            # Keep lower triangle empty (don't mirror)
            tbl[i, vars[j]] <- ""
          }
        }
      }
      covariance_table_data(tbl)
    }, error = function(e) {
      showNotification(
        paste("Covariance matrix update error:", e$message),
        type = "warning",
        duration = 3
      )
    })
  })
  
  # Structural model matrix
  output$checkbox_matrix <- renderRHandsontable({
    df <- data_module$processed_data()
    req(df)
    
    tryCatch({
      deps <- as.character(input$display_columns %||% names(df))
      meas <- input_table_data()
      req(meas)
      
      vars <- names(meas)[4:ncol(meas)]
      row_has_indicator <- apply(meas[vars], 1, function(x) any(as.logical(x)))
      convs <- setdiff(na.omit(unique(meas$Indicator[row_has_indicator])), "")
      items <- unique(c(deps, convs))
      
      if (!length(items)) return()
      
      mat <- data.frame(
        Dependent = items, 
        Operator = "~",
        stringsAsFactors = FALSE
      )
      for (col in items) mat[[col]] <- FALSE
      
      rh <- rhandsontable(mat, rowHeaders = FALSE) %>%
        hot_table(highlightReadOnly = TRUE)
      rh <- hot_col(rh, "Dependent", readOnly = TRUE)
      rh <- hot_col(rh, "Operator", readOnly = TRUE)
      
      # Calculate correlation matrix first for coloring
      cor_matrix <- NULL
      r_squared <- NULL
      
      tryCatch({
        available_items <- intersect(items, names(df))
        if (length(available_items) >= 2) {
          numeric_subset <- df[available_items]
          numeric_mask <- sapply(numeric_subset, function(x) is.numeric(x) && !all(is.na(x)))
          numeric_vars <- names(numeric_subset)[numeric_mask]
          
          if (length(numeric_vars) >= 2) {
            cor_data <- df[numeric_vars]
            if (nrow(cor_data) >= 3) {
              cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
              if (!any(is.na(cor_matrix)) && is.matrix(cor_matrix)) {
                r_squared <- cor_matrix^2
              }
            }
          }
        }
      }, error = function(e) {
        # Silent error handling
      })
      
      # Apply column renderers with color coding
      for (j in seq_along(items)) {
        col_name <- items[j]
        col_index <- match(col_name, names(mat))
        
        if (!is.na(col_index)) {
          # Create renderer with color logic
          renderer_code <- "function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
            
            var colors = {"
          
          # Add color mapping for each row
          for (i in seq_along(items)) {
            row_name <- items[i]
            
            if (i == j) {
              # Diagonal - gray
              color <- "#f0f0f0"
            } else if (!is.null(r_squared) && 
                      row_name %in% rownames(r_squared) && 
                      col_name %in% colnames(r_squared)) {
              # Color based on R-squared
              r2_value <- r_squared[row_name, col_name]
              
              if (!is.na(r2_value) && is.finite(r2_value)) {
                # Create seamless white to red gradient based on R-squared value
                # R^2 = 0: white (#ffffff)
                # R^2 = 1: red (#ff0000)
                # Formula: RGB(255, 255*(1-R²), 255*(1-R²))
                
                # Ensure R² is between 0 and 1
                r2_clamped <- max(0, min(1, r2_value))
                
                # Calculate RGB components
                red_component <- 255
                green_component <- round(255 * (1 - r2_clamped))
                blue_component <- round(255 * (1 - r2_clamped))
                
                color <- sprintf("#%02x%02x%02x", red_component, green_component, blue_component)
              } else {
                color <- "#ffffff"
              }
            } else {
              # Default white
              color <- "#ffffff"
            }
            
            renderer_code <- paste0(renderer_code, sprintf('"%d": "%s"', i - 1, color))
            if (i < length(items)) {
              renderer_code <- paste0(renderer_code, ", ")
            }
          }
          
          renderer_code <- paste0(renderer_code, "};
            
            if (colors[row] !== undefined) {
              td.style.backgroundColor = colors[row];
            }
            
            // Make diagonal read-only
            if (colors[row] === '#f0f0f0') {
              cellProperties.readOnly = true;
            }
          }")
          
          rh <- hot_col(rh, col = col_index, type = "checkbox", 
                        renderer = renderer_code)
        }
      }
      
      rh
    }, error = function(e) {
      showNotification(
        paste("Structural matrix error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Generate lavaan model syntax
  lavaan_model_str <- reactive({
    req(input$input_table, input$checkbox_matrix)
    
    tryCatch({
      meas <- hot_to_r(input$input_table)
      
      # Measurement model lines
      mlines <- lapply(seq_len(nrow(meas)), function(i) {
        lt <- meas$Latent[i]
        if (!nzchar(lt)) return(NULL)
        vars <- names(meas)[4:ncol(meas)]
        inds <- vars[as.logical(meas[i, vars])]
        if (!length(inds)) return(NULL)
        paste0(lt, " =~ ", paste(inds, collapse = " + "))
      })
      
      # Structural model lines
      struc <- hot_to_r(input$checkbox_matrix)
      slines <- lapply(seq_len(nrow(struc)), function(i) {
        dp <- struc$Dependent[i]
        if (!nzchar(dp)) return(NULL)
        preds <- names(struc)[3:ncol(struc)]
        ps <- preds[as.logical(struc[i, preds])]
        if (!length(ps)) return(NULL)
        paste0(dp, " ~ ", paste(ps, collapse = " + "))
      })
      
      # Covariance model lines
      clines <- NULL
      if (!is.null(input$covariance_mode) && input$covariance_mode == "custom" && !is.null(input$covariance_matrix)) {
        cov_data <- hot_to_r(input$covariance_matrix)
        if (!is.null(cov_data)) {
          # Validate covariance matrix structure
          if (!validate_covariance_matrix(cov_data)) {
            showNotification(
              "Invalid covariance matrix structure detected",
              type = "warning",
              duration = 3
            )
            return(character(0))
          }
          clines <- lapply(seq_len(nrow(cov_data)), function(i) {
            var1 <- cov_data$Variable[i]
            vars <- names(cov_data)[2:ncol(cov_data)]  # Skip "Variable" column
            
            # Process each variable pair
            covariance_lines <- character(0)
            for (j in seq_along(vars)) {
              var2 <- vars[j]
              setting <- cov_data[i, var2]
              
              # Skip empty cells (lower triangle)
              if (is.na(setting) || setting == "" || is.null(setting)) {
                next
              }
              
              if (i == j) {
                # Diagonal - variance
                if (setting == "fix") {
                  covariance_lines <- c(covariance_lines, paste0(var1, " ~~ 1.0*", var1))
                } else if (setting == "0") {
                  covariance_lines <- c(covariance_lines, paste0(var1, " ~~ 0*", var1))
                }
                # "auto" is handled automatically by lavaan (no explicit specification needed)
              } else if (i < j) {
                # Upper triangle - covariance
                if (setting == "fix") {
                  covariance_lines <- c(covariance_lines, paste0(var1, " ~~ 1.0*", var2))
                } else if (setting == "0") {
                  covariance_lines <- c(covariance_lines, paste0(var1, " ~~ 0*", var2))
                }
                # "auto" means free estimation (no explicit specification needed)
              }
            }
            
            return(covariance_lines)
          })
          clines <- unlist(clines)
        }
      }
      
      # Additional equations with validation
      extra <- if (!is.null(input$extra_eq)) {
        # Validate manual equations for security and syntax
        validation_result <- validate_lavaan_syntax(input$extra_eq)
        
        if (!validation_result$valid) {
          showNotification(
            paste("Manual equations error:", validation_result$message),
            type = "error",
            duration = 5
          )
          return(character(0))
        }
        
        extra_lines <- strsplit(input$extra_eq, "\\n")[[1]]
        extra_lines <- trimws(extra_lines)
        extra_lines[nzchar(extra_lines)]
      } else {
        NULL
      }
      
      model_syntax <- unlist(c(mlines, slines, clines, extra))
      
      # Update shared values
      shared_values$model_syntax <- model_syntax
      model_syntax
      
    }, error = function(e) {
      showNotification(
        paste("Model syntax error:", e$message),
        type = "error",
        duration = 3
      )
      character(0)
    })
  })
  
  # Display lavaan syntax
  output$lavaan_model <- renderText({
    ln <- lavaan_model_str()
    paste(
      if (length(ln) == 0) "Define a model to proceed." else ln,
      collapse = "\n"
    )
  })
  
  # Fit model safely with enhanced error handling
  fit_model_safe <- eventReactive(input$run_model, {
    ln <- isolate(lavaan_model_str())
    
    write_log("INFO", "SEM model fitting initiated")
    
    if (length(ln) == 0) {
      write_log("WARNING", "Model fitting attempted with no equations")
      return(list(
        ok = FALSE,
        msg_friendly = "Define a model to proceed.",
        fit = NULL
      ))
    }
    
    tryCatch({
      processed_df <- data_module$processed_data()
      req(processed_df)
      
      write_log("INFO", "Model fitting starting", paste("Equations:", length(ln), "Data rows:", nrow(processed_df)))
      
      fm <- sem(
        paste(ln, collapse = "\n"),
        data = processed_df,
        missing = input$missing_method %||% "listwise",
        fixed.x = FALSE,
        parser = "old",
        meanstructure = (!is.null(input$analysis_mode) && input$analysis_mode == "raw")
      )
      
      converged <- lavInspect(fm, "converged")
      
      if (converged) {
        write_log("INFO", "SEM model fitted successfully")
      } else {
        write_log("WARNING", "SEM model did not converge")
      }
      
      fit_result <- list(
        ok = converged,
        msg_friendly = if (converged) {
          ""
        } else {
          "Model did not converge. Check variables with correlation = 1."
        },
        fit = fm
      )
      
      # Update shared values
      shared_values$fit_model <- fit_result
      fit_result
      
    }, error = function(e) {
      write_log("ERROR", "SEM model fitting failed", e$message)
      error_msg <- if (grepl("covariance matrix", e$message, ignore.case = TRUE)) {
        "Model estimation failed: Check for perfect correlations or insufficient data."
      } else if (grepl("identification|not identified|rank deficient", e$message, ignore.case = TRUE)) {
        generate_identification_message(lavaan_model_str(), data_module$processed_data(), e$message)
      } else {
        paste("Estimation failed:", e$message)
      }
      
      error_result <- list(
        ok = FALSE,
        msg_friendly = error_msg,
        fit = NULL
      )
      
      # Update shared values
      shared_values$fit_model <- error_result
      error_result
    })
  }, ignoreNULL = FALSE)
  
  # Model fit alert
  output$fit_alert <- renderText({
    model <- fit_model_safe()
    msg <- model$msg_friendly
    
    if (nzchar(msg)) {
      shinyjs::show("fit_alert_box")
    } else {
      shinyjs::hide("fit_alert_box")
    }
    msg
  })
  
  # Fit indices table
  output$fit_indices <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    
    tryCatch({
      fit <- model$fit
      ms <- fitMeasures(fit, c("pvalue", "srmr", "rmsea", "aic", "bic",
                               "gfi", "agfi", "nfi", "cfi"))
      vals <- round(as.numeric(ms), 3)
      names(vals) <- names(ms)
      
      # Thresholds for good fit
      thr <- c(pvalue = .05, srmr = .08, rmsea = .06,
               gfi = .90, agfi = .90, nfi = .90, cfi = .90)
      
      fmt <- function(idx, v) {
        ok <- switch(idx,
                     pvalue = v >= thr["pvalue"], 
                     srmr = v <= thr["srmr"],
                     rmsea = v <= thr["rmsea"], 
                     gfi = v >= thr["gfi"],
                     agfi = v >= thr["agfi"], 
                     nfi = v >= thr["nfi"],
                     cfi = v >= thr["cfi"], 
                     TRUE)
        
        if (is.na(v)) {
          "NA"
        } else if (!ok) {
          sprintf('<span style="color:red;">%.3f</span>', v)
        } else {
          sprintf('%.3f', v)
        }
      }
      
      html_vals <- mapply(fmt, names(vals), vals, USE.NAMES = FALSE)
      tbl <- as.data.frame(t(html_vals), stringsAsFactors = FALSE)
      colnames(tbl) <- toupper(names(vals))
      
      datatable(tbl, 
                escape = FALSE, 
                rownames = FALSE,
                options = list(dom = 't'))
                
    }, error = function(e) {
      showNotification(
        paste("Fit indices error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Approximate equations
  output$approx_eq <- renderText({
    if (!is.null(input$analysis_mode) && input$analysis_mode == "std") {
      return("— Hidden in Standardized mode —")
    }
    
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    
    tryCatch({
      paste(lavaan_to_equations(model$fit), collapse = "\n")
    }, error = function(e) {
      paste("Equation generation error:", e$message)
    })
  })
  
  # Model summary
  output$fit_summary <- renderPrint({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    
    tryCatch({
      summary(model$fit, fit.measures = TRUE)
    }, error = function(e) {
      cat("Summary generation error:", e$message)
    })
  })
  
  # Parameter estimates table
  output$param_tbl <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    
    tryCatch({
      param_est <- parameterEstimates(model$fit)
      datatable(param_est, options = list(pageLength = 15))
    }, error = function(e) {
      showNotification(
        paste("Parameter table error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Return values for other modules
  return(list(
    lavaan_model_str = lavaan_model_str,
    fit_model_safe = fit_model_safe,
    input_table_data = input_table_data
  ))
}
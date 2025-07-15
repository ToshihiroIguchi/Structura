# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – modules.R (Unified Server Modules)
#   * All server-side logic consolidated
#   * Data, Model, and Plot modules integrated
# ---------------------------------------------------------------

# ===============================================================
# DATA MODULE
# ===============================================================
# Handles data loading, preprocessing, and display

data_module_server <- function(input, output, session, shared_values) {
  
  # Helper function to format numeric columns for display
  format_numeric_for_display <- function(df) {
    formatted_df <- df
    for (i in seq_along(formatted_df)) {
      if (is.numeric(formatted_df[[i]])) {
        formatted_df[[i]] <- sapply(formatted_df[[i]], function(x) {
          if (is.na(x)) {
            return(NA_character_)
          } else if (abs(x) >= 1000 || (abs(x) < 0.001 && x != 0)) {
            return(sprintf("%.3e", x))
          } else {
            return(sprintf("%.3f", x))
          }
        })
      }
    }
    return(formatted_df)
  }
  
  # Reactive data store
  data <- reactiveVal(NULL)
  
  # Initial modal for data loading
  show_data_modal <- function() {
    showModal(
      modalDialog(
        title = span(icon("upload"), "Load Data"),
        tabsetPanel(
          tabPanel("Upload File",
                   fileInput("datafile", NULL,
                             buttonLabel = "Browse…",
                             placeholder = "Upload CSV",
                             accept = c(".csv", "text/csv", "application/csv"))
          ),
          tabPanel("Sample Dataset",
                   radioButtons("sample_ds", "Or choose a demo dataset:",
                                choices = c("None",
                                            "HolzingerSwineford1939",
                                            "PoliticalDemocracy",
                                            "Demo.growth",
                                            "Demo.twolevel",
                                            "FacialBurns"))
          )
        ),
        easyClose = FALSE,
        footer = NULL
      )
    )
  }
  
  # File upload observer
  observeEvent(input$datafile, {
    req(input$datafile)
    
    removeModal()
    updateRadioButtons(session, "sample_ds", selected = "None")
    
    tryCatch({
      loaded_data <- loadDataOnce(input$datafile)
      data(loaded_data)
      shared_values$processed_data <- loaded_data
      
      showNotification(
        paste("Successfully loaded:", nrow(loaded_data), "rows,", ncol(loaded_data), "columns"),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading file:", e$message),
        type = "error",
        duration = 5
      )
      show_data_modal()
    })
  })
  
  # Sample dataset observer
  observeEvent(input$sample_ds, {
    if (input$sample_ds != "None") {
      removeModal()
      
      tryCatch({
        sample_data <- switch(input$sample_ds,
                              "HolzingerSwineford1939" = lavaan::HolzingerSwineford1939,
                              "PoliticalDemocracy" = lavaan::PoliticalDemocracy,
                              "Demo.growth" = lavaan::Demo.growth,
                              "Demo.twolevel" = lavaan::Demo.twolevel,
                              "FacialBurns" = {
                                if (requireNamespace("semPlot", quietly = TRUE)) {
                                  semPlot::FacialBurns
                                } else {
                                  stop("Package 'semPlot' required for FacialBurns dataset")
                                }
                              })
        
        data(sample_data)
        shared_values$processed_data <- sample_data
        
        showNotification(
          paste("Loaded sample dataset:", input$sample_ds),
          type = "message",
          duration = 3
        )
      }, error = function(e) {
        showNotification(
          paste("Error loading sample dataset:", e$message),
          type = "error",
          duration = 5
        )
        show_data_modal()
      })
    }
  })
  
  # Data table output
  output$datatable <- DT::renderDataTable({
    req(data())
    format_numeric_for_display(data())
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'Bfrtip',
    columnDefs = list(
      list(className = 'dt-center', targets = '_all')
    ),
    buttons = list(
      list(extend = 'copy', text = 'Copy to Clipboard'),
      list(extend = 'csv', text = 'Download CSV'),
      list(extend = 'excel', text = 'Download Excel')
    )
  ), extensions = 'Buttons')
  
  # Log transform UI
  output$log_transform_ui <- renderUI({
    req(data())
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_vars) > 0) {
      checkboxGroupInput("log_transform_vars",
                         "Apply log transformation to:",
                         choices = numeric_vars,
                         inline = TRUE)
    }
  })
  
  # Display column UI
  output$display_column_ui <- renderUI({
    req(data())
    df <- data()
    
    # Get processed data to show actual column names after one-hot encoding
    processed_df <- tryCatch({
      # Simulate the one-hot encoding process to get column names
      temp_df <- df
      categorical_cols <- names(temp_df)[sapply(temp_df, function(x) is.character(x) || is.factor(x))]
      
      if (length(categorical_cols) > 0) {
        for (col in categorical_cols) {
          if (is.character(temp_df[[col]])) {
            temp_df[[col]] <- as.factor(temp_df[[col]])
          }
          levels_to_encode <- levels(temp_df[[col]])
          for (level in levels_to_encode) {
            new_col_name <- paste0(col, "_", make.names(level))
            temp_df[[new_col_name]] <- as.numeric(temp_df[[col]] == level)
          }
          temp_df[[col]] <- NULL
        }
      }
      temp_df
    }, error = function(e) df)
    
    # Identify original numeric columns (not one-hot encoded)
    original_numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    # Identify one-hot encoded columns
    one_hot_cols <- setdiff(names(processed_df), original_numeric_cols)
    
    # Select only original numeric columns by default
    default_selected <- original_numeric_cols
    
    checkboxGroupInput("display_columns",
                       "Display columns:",
                       choices = names(processed_df),
                       selected = default_selected,
                       inline = TRUE)
  })
  
  # Processed data reactive
  processed_data <- reactive({
    req(data())
    df <- data()
    
    # One-hot encoding for categorical variables
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    if (length(categorical_cols) > 0) {
      # Create one-hot encoded variables
      for (col in categorical_cols) {
        # Convert to factor if character
        if (is.character(df[[col]])) {
          df[[col]] <- as.factor(df[[col]])
        }
        
        # Get unique levels (excluding NA)
        levels_to_encode <- levels(df[[col]])
        
        # Create dummy variables for each level
        for (level in levels_to_encode) {
          new_col_name <- paste0(col, "_", make.names(level))
          df[[new_col_name]] <- as.numeric(df[[col]] == level)
        }
        
        # Remove original categorical column
        df[[col]] <- NULL
      }
    }
    
    # Apply log transformations
    if (!is.null(input$log_transform_vars) && length(input$log_transform_vars) > 0) {
      for (var in input$log_transform_vars) {
        if (var %in% names(df) && is.numeric(df[[var]])) {
          # Handle non-positive values
          min_val <- min(df[[var]], na.rm = TRUE)
          if (min_val <= 0) {
            df[[var]] <- log(df[[var]] - min_val + 1)
          } else {
            df[[var]] <- log(df[[var]])
          }
        }
      }
    }
    
    # Apply standardization if analysis mode is "scale" or similar
    if (!is.null(input$analysis_mode) && input$analysis_mode %in% c("scale", "standardized", "std")) {
      numeric_cols <- sapply(df, is.numeric)
      df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
        if (sd(x, na.rm = TRUE) > 0) {
          scale(x)[, 1]
        } else {
          x
        }
      })
    }
    
    # Filter displayed columns
    if (!is.null(input$display_columns) && length(input$display_columns) > 0) {
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    }
    
    shared_values$processed_data <- df
    return(df)
  })
  
  # Data quality assessment
  data_quality_report <- reactive({
    req(processed_data())
    df <- processed_data()
    
    # Only assess quality for displayed columns
    if (!is.null(input$display_columns) && length(input$display_columns) > 0) {
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    }
    
    tryCatch({
      numeric_data <- df[sapply(df, is.numeric)]
      
      if (ncol(numeric_data) < 2) {
        return(list(
          has_issues = FALSE,
          message = "No quality issues detected.",
          details = list()
        ))
      }
      
      issues <- list()
      
      # Check for missing data
      missing_prop <- sum(is.na(df)) / (nrow(df) * ncol(df))
      if (missing_prop > 0.1) {
        issues <- append(issues, sprintf("High missing data: %.1f%%", missing_prop * 100))
      }
      
      # Check for zero variance
      zero_var_cols <- names(numeric_data)[sapply(numeric_data, function(x) {
        clean_x <- x[!is.na(x)]
        if (length(clean_x) < 2) return(TRUE)
        var(clean_x, na.rm = TRUE) == 0 || sd(clean_x, na.rm = TRUE) < 1e-10
      })]
      
      if (length(zero_var_cols) > 0) {
        issues <- append(issues, paste("Zero variance:", paste(zero_var_cols, collapse = ", ")))
      }
      
      # Check correlations
      if (ncol(numeric_data) >= 2 && nrow(numeric_data) >= 3) {
        cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
        
        # Perfect correlations
        perfect_cors <- which(abs(cor_matrix) >= 0.999 & cor_matrix != 1, arr.ind = TRUE)
        if (nrow(perfect_cors) > 0) {
          perfect_pairs <- character(0)
          for (i in seq_len(nrow(perfect_cors))) {
            row_idx <- perfect_cors[i, 1]
            col_idx <- perfect_cors[i, 2]
            if (row_idx < col_idx) {  # Avoid duplicates
              var1 <- rownames(cor_matrix)[row_idx]
              var2 <- colnames(cor_matrix)[col_idx]
              r_val <- round(cor_matrix[row_idx, col_idx], 3)
              perfect_pairs <- c(perfect_pairs, paste0(var1, " & ", var2, " (r=", r_val, ")"))
            }
          }
          if (length(perfect_pairs) > 0) {
            issues <- append(issues, paste("Perfect correlations:", paste(perfect_pairs, collapse = "; ")))
          }
        }
      }
      
      return(list(
        has_issues = length(issues) > 0,
        message = if (length(issues) > 0) "Data quality issues detected:" else "No quality issues detected.",
        details = issues
      ))
      
    }, error = function(e) {
      return(list(
        has_issues = TRUE,
        message = "Error in data quality assessment",
        details = list(e$message)
      ))
    })
  })
  
  # Data quality alert output
  output$data_quality_alert <- renderUI({
    report <- data_quality_report()
    
    if (report$has_issues) {
      alert_content <- div(
        h5(icon("exclamation-triangle"), report$message),
        if (length(report$details) > 0) {
          tags$ul(
            lapply(report$details, function(detail) tags$li(detail))
          )
        }
      )
      
      alert_style <- if (report$has_issues) "alert alert-warning" else "alert alert-info"
      
      div(
        class = alert_style,
        style = "margin-bottom: 15px;",
        alert_content
      )
    }
  })
  
  # Filtered table output
  output$filtered_table <- DT::renderDataTable({
    req(processed_data())
    format_numeric_for_display(processed_data())
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = list(
      list(extend = 'copy', text = 'Copy to Clipboard'),
      list(extend = 'csv', text = 'Download CSV'),
      list(extend = 'excel', text = 'Download Excel')
    )
  ), extensions = 'Buttons')
  
  # Correlation heatmap
  output$corr_heatmap <- renderPlot({
    req(processed_data())
    df <- processed_data()
    numeric_data <- df[sapply(df, is.numeric)]
    
    if (ncol(numeric_data) < 2) {
      plot.new()
      text(0.5, 0.5, "Need at least 2 numeric variables for correlation", cex = 1.2)
      return()
    }
    
    tryCatch({
      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
      
      # Cache correlation for other modules
      shared_values$correlation_cache <- cor_matrix
      
      # Convert to long format for ggplot
      cor_long <- reshape2::melt(cor_matrix)
      names(cor_long) <- c("Var1", "Var2", "value")
      
      ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1, 1), space = "Lab",
                             name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        coord_fixed() +
        geom_text(aes(label = sprintf("%.2f", value)), size = 3)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating correlation plot:", e$message), cex = 1)
    })
  })
  
  return(list(
    show_modal = show_data_modal,
    processed_data = processed_data
  ))
}

# ===============================================================
# MODEL MODULE  
# ===============================================================
# Handles SEM model building, estimation, and results

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
        # Get only variables that are actually used in the model (same logic as Variables Overview)
        all_vars <- character(0)
        
        # Step 1: Get observed variables actually used in measurement model
        obs_vars_in_measurement <- character(0)
        for (i in seq_len(nrow(meas))) {
          if (nzchar(meas$Latent[i])) {
            vars <- names(meas)[4:ncol(meas)]
            selected_vars <- vars[as.logical(meas[i, vars])]
            obs_vars_in_measurement <- c(obs_vars_in_measurement, selected_vars)
          }
        }
        
        # Step 2: Get latent variables from measurement model
        latent_vars <- character(0)
        for (i in seq_len(nrow(meas))) {
          latent_var <- meas$Latent[i]
          if (nzchar(latent_var)) {
            vars <- names(meas)[4:ncol(meas)]
            if (any(as.logical(meas[i, vars]))) {
              latent_vars <- c(latent_vars, latent_var)
            }
          }
        }
        
        # Step 3: Get variables actually used in structural model
        vars_in_structural <- character(0)
        if (!is.null(input$checkbox_matrix)) {
          struc <- hot_to_r(input$checkbox_matrix)
          if (!is.null(struc)) {
            for (i in seq_len(nrow(struc))) {
              dep_var <- struc$Dependent[i]
              if (nzchar(dep_var)) {
                predictors <- names(struc)[3:ncol(struc)]
                selected_preds <- predictors[as.logical(struc[i, predictors])]
                if (length(selected_preds) > 0) {
                  vars_in_structural <- c(vars_in_structural, dep_var, selected_preds)
                }
              }
            }
          }
        }
        
        # Step 4: Combine only actually used variables
        all_vars <- unique(c(obs_vars_in_measurement, latent_vars, vars_in_structural))
        
        if (length(all_vars) > 0) {
          # Create true symmetric matrix structure (square matrix)
          n_vars <- length(all_vars)
          
          # Initialize matrix with variable names as both rows and columns
          cov_mat <- matrix(TRUE, nrow = n_vars, ncol = n_vars)
          rownames(cov_mat) <- all_vars
          colnames(cov_mat) <- all_vars
          
          # Set diagonal to TRUE (default for variance estimation)
          diag(cov_mat) <- TRUE
          
          # Set lower triangle to FALSE (avoid confusion)
          for (i in seq_len(n_vars)) {
            for (j in seq_len(n_vars)) {
              if (i > j) {
                cov_mat[i, j] <- FALSE
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
              Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              td.style.backgroundColor = '#e6f3ff';
            } else if (row < col - 1) {
              // Upper triangle - covariance (light green)
              Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
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
        
        rh <- hot_col(rh, col_name, type = "checkbox",
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
      # Keep lower triangle FALSE for clarity
      vars <- names(tbl)[2:ncol(tbl)]  # Skip "Variable" column
      for (i in seq_along(vars)) {
        for (j in seq_along(vars)) {
          if (i > j) {
            # Keep lower triangle FALSE (don't mirror)
            tbl[i, vars[j]] <- FALSE
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
      if (!is.null(input$covariance_mode) && input$covariance_mode == "semi") {
        # Semi-automatic mode: use only variables from Variables Overview
        df <- data_module$processed_data()
        meas <- input_table_data()
        
        if (!is.null(df) && !is.null(meas)) {
          # Get only variables that are actually used in the model
          all_model_vars <- character(0)
          
          # Step 1: Get observed variables actually used in measurement model
          obs_vars_in_measurement <- character(0)
          for (i in seq_len(nrow(meas))) {
            if (nzchar(meas$Latent[i])) {
              vars <- names(meas)[4:ncol(meas)]
              selected_vars <- vars[as.logical(meas[i, vars])]
              obs_vars_in_measurement <- c(obs_vars_in_measurement, selected_vars)
            }
          }
          
          # Step 2: Get latent variables from measurement model
          latent_vars <- character(0)
          for (i in seq_len(nrow(meas))) {
            latent_var <- meas$Latent[i]
            if (nzchar(latent_var)) {
              vars <- names(meas)[4:ncol(meas)]
              if (any(as.logical(meas[i, vars]))) {
                latent_vars <- c(latent_vars, latent_var)
              }
            }
          }
          
          # Step 3: Get variables actually used in structural model
          vars_in_structural <- character(0)
          if (!is.null(input$checkbox_matrix)) {
            struc <- hot_to_r(input$checkbox_matrix)
            if (!is.null(struc)) {
              for (i in seq_len(nrow(struc))) {
                dep_var <- struc$Dependent[i]
                if (nzchar(dep_var)) {
                  predictors <- names(struc)[3:ncol(struc)]
                  selected_preds <- predictors[as.logical(struc[i, predictors])]
                  if (length(selected_preds) > 0) {
                    vars_in_structural <- c(vars_in_structural, dep_var, selected_preds)
                  }
                }
              }
            }
          }
          
          # Step 4: Combine only actually used variables
          all_model_vars <- unique(c(obs_vars_in_measurement, latent_vars, vars_in_structural))
          
          # Generate covariance constraints based on user selections
          cov_constraints <- character(0)
          
          for (i in seq_along(all_model_vars)) {
            for (j in seq_along(all_model_vars)) {
              if (i < j) {  # Upper triangle only
                var1 <- all_model_vars[i]
                var2 <- all_model_vars[j]
                
                # Check if both variables are latent
                var1_is_latent <- var1 %in% latent_vars
                var2_is_latent <- var2 %in% latent_vars
                
                if (var1_is_latent && var2_is_latent) {
                  # Covariance between different latent variables
                  if (!is.null(input$cov_latent_latent) && !input$cov_latent_latent) {
                    cov_constraints <- c(cov_constraints, paste0(var1, " ~~ 0*", var2))
                  }
                } else if (!var1_is_latent && !var2_is_latent) {
                  # Covariance between different observed variables
                  if (is.null(input$cov_observed_observed) || !input$cov_observed_observed) {
                    cov_constraints <- c(cov_constraints, paste0(var1, " ~~ 0*", var2))
                  }
                }
              } else if (i == j) {
                # Same variable (variance)
                var1 <- all_model_vars[i]
                if (!is.null(input$cov_same_variable) && !input$cov_same_variable) {
                  cov_constraints <- c(cov_constraints, paste0(var1, " ~~ 0*", var1))
                }
              }
            }
          }
          
          clines <- cov_constraints
        }
      } else if (!is.null(input$covariance_mode) && input$covariance_mode == "custom" && !is.null(input$covariance_matrix)) {
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
              
              # Handle checkbox settings
              if (is.na(setting)) {
                next
              }
              
              checkbox_value <- as.logical(setting)
              
              if (i == j) {
                # Diagonal - variance
                if (!checkbox_value) {
                  # Unchecked means zero variance
                  covariance_lines <- c(covariance_lines, paste0(var1, " ~~ 0*", var1))
                }
                # Checked (TRUE) is handled automatically by lavaan (free estimation)
              } else if (i < j) {
                # Upper triangle - covariance
                if (!checkbox_value) {
                  # Unchecked means zero covariance (independence)
                  covariance_lines <- c(covariance_lines, paste0(var1, " ~~ 0*", var2))
                }
                # Checked (TRUE) means free estimation (no explicit specification needed)
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
                extensions = 'Buttons',
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(extend = 'copy', text = 'Copy to Clipboard'),
                    list(extend = 'csv', text = 'Download CSV'),
                    list(extend = 'excel', text = 'Download Excel')
                  )
                ))
                
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
      
      # Format numeric columns to 3 decimal places
      numeric_cols <- sapply(param_est, is.numeric)
      param_est[numeric_cols] <- lapply(param_est[numeric_cols], function(x) {
        round(x, 3)
      })
      
      datatable(param_est, 
                extensions = 'Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Bfrtip',
                  buttons = list(
                    list(extend = 'copy', text = 'Copy to Clipboard'),
                    list(extend = 'csv', text = 'Download CSV'),
                    list(extend = 'excel', text = 'Download Excel')
                  )
                ))
    }, error = function(e) {
      showNotification(
        paste("Parameter table error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Variables overview tables
  output$measurement_variables_table <- renderDT({
    meas <- input_table_data()
    req(meas)
    
    tryCatch({
      # Extract measurement model variables
      measurement_vars <- data.frame(
        Latent_Variable = character(0),
        Observed_Variables = character(0),
        Count = numeric(0),
        stringsAsFactors = FALSE
      )
      
      for (i in seq_len(nrow(meas))) {
        latent_var <- meas$Latent[i]
        if (nzchar(latent_var)) {
          vars <- names(meas)[4:ncol(meas)]
          selected_vars <- vars[as.logical(meas[i, vars])]
          
          if (length(selected_vars) > 0) {
            measurement_vars <- rbind(measurement_vars, data.frame(
              Latent_Variable = latent_var,
              Observed_Variables = paste(selected_vars, collapse = ", "),
              Count = length(selected_vars),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      datatable(measurement_vars, 
                extensions = 'Buttons',
                options = list(
                  pageLength = 10,
                  dom = 'Bfrtip',
                  buttons = list(
                    list(extend = 'copy', text = 'Copy to Clipboard'),
                    list(extend = 'csv', text = 'Download CSV'),
                    list(extend = 'excel', text = 'Download Excel')
                  )
                ))
    }, error = function(e) {
      showNotification(
        paste("Measurement variables table error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  output$structural_variables_table <- renderDT({
    req(input$checkbox_matrix)
    
    tryCatch({
      struc <- hot_to_r(input$checkbox_matrix)
      req(struc)
      
      # Extract structural model variables
      structural_vars <- data.frame(
        Dependent_Variable = character(0),
        Independent_Variables = character(0),
        Count = numeric(0),
        stringsAsFactors = FALSE
      )
      
      for (i in seq_len(nrow(struc))) {
        dep_var <- struc$Dependent[i]
        if (nzchar(dep_var)) {
          predictors <- names(struc)[3:ncol(struc)]
          selected_preds <- predictors[as.logical(struc[i, predictors])]
          
          if (length(selected_preds) > 0) {
            structural_vars <- rbind(structural_vars, data.frame(
              Dependent_Variable = dep_var,
              Independent_Variables = paste(selected_preds, collapse = ", "),
              Count = length(selected_preds),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      datatable(structural_vars, 
                extensions = 'Buttons',
                options = list(
                  pageLength = 10,
                  dom = 'Bfrtip',
                  buttons = list(
                    list(extend = 'copy', text = 'Copy to Clipboard'),
                    list(extend = 'csv', text = 'Download CSV'),
                    list(extend = 'excel', text = 'Download Excel')
                  )
                ))
    }, error = function(e) {
      showNotification(
        paste("Structural variables table error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  output$all_variables_summary <- renderDT({
    df <- data_module$processed_data()
    meas <- input_table_data()
    req(df, meas)
    
    tryCatch({
      # Get all variables used in the model
      all_vars <- data.frame(
        Variable_Name = character(0),
        Variable_Type = character(0),
        Used_In = character(0),
        Data_Type = character(0),
        Missing_Values = numeric(0),
        stringsAsFactors = FALSE
      )
      
      # Get observed variables from data
      observed_vars <- names(df)
      
      # Get latent variables from measurement model
      latent_vars <- character(0)
      if (!is.null(meas)) {
        for (i in seq_len(nrow(meas))) {
          latent_var <- meas$Latent[i]
          if (nzchar(latent_var)) {
            vars <- names(meas)[4:ncol(meas)]
            if (any(as.logical(meas[i, vars]))) {
              latent_vars <- c(latent_vars, latent_var)
            }
          }
        }
      }
      
      # Process observed variables
      for (var in observed_vars) {
        used_in <- character(0)
        
        # Check if used in measurement model
        if (!is.null(meas)) {
          for (i in seq_len(nrow(meas))) {
            vars <- names(meas)[4:ncol(meas)]
            if (var %in% vars && as.logical(meas[i, var])) {
              used_in <- c(used_in, "Measurement Model")
              break
            }
          }
        }
        
        # Check if used in structural model
        if (!is.null(input$checkbox_matrix)) {
          struc <- hot_to_r(input$checkbox_matrix)
          if (!is.null(struc)) {
            for (i in seq_len(nrow(struc))) {
              if (struc$Dependent[i] == var) {
                used_in <- c(used_in, "Structural Model (Dependent)")
              }
              predictors <- names(struc)[3:ncol(struc)]
              if (var %in% predictors && as.logical(struc[i, var])) {
                used_in <- c(used_in, "Structural Model (Independent)")
              }
            }
          }
        }
        
        # Calculate missing values
        missing_count <- sum(is.na(df[[var]]))
        
        all_vars <- rbind(all_vars, data.frame(
          Variable_Name = var,
          Variable_Type = "Observed",
          Used_In = if (length(used_in) > 0) paste(unique(used_in), collapse = ", ") else "Not Used",
          Data_Type = class(df[[var]])[1],
          Missing_Values = missing_count,
          stringsAsFactors = FALSE
        ))
      }
      
      # Process latent variables
      for (var in latent_vars) {
        used_in <- "Measurement Model"
        
        # Check if used in structural model
        if (!is.null(input$checkbox_matrix)) {
          struc <- hot_to_r(input$checkbox_matrix)
          if (!is.null(struc)) {
            for (i in seq_len(nrow(struc))) {
              if (struc$Dependent[i] == var) {
                used_in <- paste(used_in, "Structural Model (Dependent)", sep = ", ")
              }
              predictors <- names(struc)[3:ncol(struc)]
              if (var %in% predictors && as.logical(struc[i, var])) {
                used_in <- paste(used_in, "Structural Model (Independent)", sep = ", ")
              }
            }
          }
        }
        
        all_vars <- rbind(all_vars, data.frame(
          Variable_Name = var,
          Variable_Type = "Latent",
          Used_In = used_in,
          Data_Type = "Latent Factor",
          Missing_Values = 0,
          stringsAsFactors = FALSE
        ))
      }
      
      datatable(all_vars, 
                extensions = 'Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Bfrtip',
                  buttons = list(
                    list(extend = 'copy', text = 'Copy to Clipboard'),
                    list(extend = 'csv', text = 'Download CSV'),
                    list(extend = 'excel', text = 'Download Excel')
                  )
                ))
    }, error = function(e) {
      showNotification(
        paste("All variables summary error:", e$message),
        type = "error",
        duration = 3
      )
      NULL
    })
  })
  
  # Enlarged Path Diagram for Details tab
  output$sem_plot_enlarged <- renderGrViz({
    model <- fit_model_safe()
    req(model)
    validate(need(model$ok, model$msg_friendly))
    
    tryCatch({
      std_for_plot <- if (!is.null(input$analysis_mode) && input$analysis_mode == "std") {
        TRUE
      } else {
        !is.null(input$diagram_std) && input$diagram_std
      }
      
      layout_parts <- if (!is.null(input$layout_style)) {
        strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
      } else {
        c("dot", "LR")
      }
      
      eng <- layout_parts[1]
      rank <- ifelse(length(layout_parts) == 2, layout_parts[2], "LR")
      
      semDiagram(model$fit, 
                 standardized = std_for_plot,
                 layout = rank, 
                 engine = eng)
                 
    }, error = function(e) {
      # Return empty diagram with error message
      DiagrammeR::grViz("
        digraph {
          node [shape=box]
          'Enlarged View' [label='Path diagram will appear here after running the model']
        }
      ")
    })
  })
  
  # Return values for other modules
  return(list(
    lavaan_model_str = lavaan_model_str,
    fit_model_safe = fit_model_safe,
    input_table_data = input_table_data
  ))
}

# ===============================================================
# PLOT MODULE
# ===============================================================  
# Handles plot generation and visualization

plot_module_server <- function(input, output, session, shared_values) {
  
  # Correlation heatmap with caching
  correlation_cache <- reactiveVal(NULL)
  
  # Cache correlation matrix when data changes
  observe({
    df <- shared_values$processed_data
    if (!is.null(df) && !is.null(input$display_columns)) {
      all_cols <- intersect(input$display_columns, names(df))
      num_cols <- all_cols[sapply(df[, all_cols, drop = FALSE], is.numeric)]
      
      if (length(num_cols) >= 2) {
        tryCatch({
          cm <- cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
          cache_data <- list(matrix = cm, columns = num_cols)
          correlation_cache(cache_data)
          shared_values$correlation_cache <- cache_data
        }, error = function(e) {
          correlation_cache(NULL)
          shared_values$correlation_cache <- NULL
        })
      } else {
        correlation_cache(NULL)
        shared_values$correlation_cache <- NULL
      }
    }
  })
  
  # Render correlation heatmap with ggplot2
  output$corr_heatmap <- renderPlot({
    cache_data <- correlation_cache()
    
    # Show message when no data available
    if (is.null(cache_data)) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Select at least 2 numeric variables to display correlation heatmap",
                 size = 5, color = "gray50") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      tryCatch({
        cm <- cache_data$matrix
        n <- nrow(cm)
        
        # Remove upper triangle and diagonal (keep only lower triangle)
        cm_display <- cm
        cm_display[upper.tri(cm_display, diag = TRUE)] <- NA
        
        # Convert matrix to long format for ggplot2 using reshape2
        melted_data <- melt(cm_display, na.rm = TRUE)
        colnames(melted_data) <- c("Var1", "Var2", "value")
        
        # Create ggplot heatmap
        p <- ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
          geom_tile(color = "white", size = 0.5) +
          scale_fill_gradient2(
            low = "blue", 
            mid = "white", 
            high = "red",
            midpoint = 0,
            limits = c(-1, 1),
            name = "Correlation"
          ) +
          geom_text(aes(label = sprintf("%.3f", value)), 
                   color = ifelse(abs(melted_data$value) > 0.5, "white", "black"),
                   size = 3) +
          labs(title = "Correlation Matrix",
               x = "", y = "") +
          scale_y_discrete(limits = rev(levels(melted_data$Var1))) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, size = 14),
            legend.position = "right",
            aspect.ratio = 0.8
          )
        
        print(p)
        
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Unable to generate correlation plot.\nPlease ensure you have selected numeric variables with valid data.",
                   size = 4, color = "red") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      })
    }
  })
  
  # SEM path diagram UI
  output$sem_plot_ui <- renderUI({
    if (is.null(shared_values$model_syntax) || 
        length(shared_values$model_syntax) == 0) {
      return(div("Define a model to view the path diagram."))
    }
    
    tryCatch({
      grVizOutput("sem_plot")
    }, error = function(e) {
      div(style = "color:red;",
          paste("Model Error:", e$message))
    })
  })
  
  # SEM path diagram
  output$sem_plot <- renderGrViz({
    model <- shared_values$fit_model
    req(model)
    validate(need(model$ok, model$msg_friendly))
    
    tryCatch({
      std_for_plot <- if (!is.null(input$analysis_mode) && input$analysis_mode == "std") {
        TRUE
      } else {
        !is.null(input$diagram_std) && input$diagram_std
      }
      
      layout_parts <- if (!is.null(input$layout_style)) {
        strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
      } else {
        c("dot", "LR")
      }
      
      eng <- layout_parts[1]
      rank <- ifelse(length(layout_parts) == 2, layout_parts[2], "LR")
      
      semDiagram(model$fit, 
                 standardized = std_for_plot,
                 layout = rank, 
                 engine = eng)
                 
    }, error = function(e) {
      # Return empty diagram with error message
      DiagrammeR::grViz("
        digraph {
          node [shape=box]
          Error [label='Diagram generation failed']
        }
      ")
    })
  })
  
  return(list(
    correlation_cache = correlation_cache
  ))
}
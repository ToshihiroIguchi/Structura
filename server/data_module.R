# Data Management Module
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
  
  # File upload handler
  observeEvent(input$datafile, {
    tryCatch({
      write_log("INFO", "File upload initiated", paste("Filename:", input$datafile$name))
      data(loadDataOnce(input$datafile))
      updateRadioButtons(session, "sample_ds", selected = "None")
      removeModal()
      write_log("INFO", "File upload completed successfully")
    }, error = function(e) {
      write_log("ERROR", "File upload failed", e$message)
      showNotification(
        paste("File loading failed:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Sample dataset handler
  observeEvent(input$sample_ds, {
    req(input$sample_ds != "None")
    ds <- switch(input$sample_ds,
                 "HolzingerSwineford1939" = HolzingerSwineford1939,
                 "PoliticalDemocracy" = PoliticalDemocracy,
                 "Demo.growth" = Demo.growth,
                 "Demo.twolevel" = Demo.twolevel,
                 "FacialBurns" = FacialBurns)
    data(ds)
    removeModal()
  })
  
  # Data preview table
  output$datatable <- renderDT({
    req(data())
    
    datatable(data(), 
              filter = "top", 
              editable = TRUE,
              extensions = 'Buttons',
              options = list(
                pageLength = 30, 
                autoWidth = FALSE,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend = 'copy', text = 'Copy to Clipboard'),
                  list(extend = 'csv', text = 'Download CSV'),
                  list(extend = 'excel', text = 'Download Excel')
                ),
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              ),
              rownames = FALSE)
  }, server = TRUE)
  
  # Log transform UI
  output$log_transform_ui <- renderUI({
    req(data())
    nums <- names(data())[sapply(data(), is.numeric)]
    valid <- nums[sapply(data()[nums], function(x) {
      tryCatch({
        min_val <- min(x, na.rm = TRUE)
        !is.infinite(min_val) && min_val > 0
      }, error = function(e) FALSE)
    })]
    
    if (!length(valid)) return()
    checkboxGroupInput("log_columns", "Log-transform columns (log10):",
                       choices = valid, inline = TRUE)
  })
  
  # Data preprocessing
  processed_data <- reactive({
    req(data())
    
    tryCatch({
      idx <- as.numeric(unlist(input$datatable_rows_all))
      if (!length(idx)) idx <- seq_len(nrow(data()))
      df <- data()[idx, , drop = FALSE]
      df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
      
      # Log10 transform
      if (!is.null(input$log_columns) && length(input$log_columns) > 0) {
        col_order <- names(df)
        for (col in input$log_columns) {
          if (col %in% names(df)) {
            log_col <- paste0("log_", col)
            df[[log_col]] <- log10(df[[col]])
            pos <- match(col, col_order)
            col_order[pos] <- log_col
            df[[col]] <- NULL
          }
        }
        df <- df[, col_order, drop = FALSE]
      }
      
      # One-hot encode
      chars <- names(df)[vapply(df, is.character, logical(1))]
      multi <- chars[vapply(df[chars], function(x) {
        u <- unique(x)
        length(u) > 1 && length(u) < nrow(df)
      }, logical(1))]
      
      if (length(multi)) {
        mm <- model.matrix(~ . - 1, data = df[multi])
        df <- cbind(df[setdiff(names(df), multi)],
                    as.data.frame(mm, check.names = TRUE))
      }
      names(df) <- make.names(names(df), unique = TRUE)
      
      # Standardize numeric columns
      if (!is.null(input$analysis_mode) && input$analysis_mode == "std") {
        num_cols <- vapply(df, is.numeric, logical(1))
        if (any(num_cols)) {
          df[num_cols] <- scale(df[num_cols])
        }
      }
      
      # Update shared values
      shared_values$processed_data <- df
      df
      
    }, error = function(e) {
      showNotification(
        paste("Data processing failed:", e$message),
        type = "error",
        duration = 5
      )
      data()
    })
  })
  
  # Real-time data quality assessment
  data_quality_report <- reactive({
    req(processed_data())
    df <- processed_data()
    
    tryCatch({
      numeric_data <- df[sapply(df, is.numeric)]
      
      quality_issues <- list(
        zero_variance = character(0),
        perfect_correlations = character(0),
        high_correlations = character(0),
        dummy_variables = character(0),
        summary = "No significant data quality issues detected"
      )
      
      if (ncol(numeric_data) >= 2) {
        # Check zero variance
        zero_variance_vars <- names(numeric_data)[sapply(numeric_data, function(x) {
          clean_x <- x[!is.na(x)]
          if (length(clean_x) < 2) return(TRUE)
          var(clean_x, na.rm = TRUE) == 0 || sd(clean_x, na.rm = TRUE) < 1e-10
        })]
        
        quality_issues$zero_variance <- zero_variance_vars
        
        # Check correlations if sufficient data
        if (ncol(numeric_data) >= 2 && nrow(numeric_data) >= 3) {
          correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
          
          # Detect dummy variable groups (simplified and safe approach)
          detect_dummy_groups <- function(var_names) {
            groups <- list()
            
            # Safe method: Check for specific known patterns
            # Pattern 1: Variables starting with "school" (model.matrix output)
            school_vars <- var_names[grepl("^school", var_names, ignore.case = TRUE)]
            if (length(school_vars) >= 2) {
              groups[["school"]] <- school_vars
            }
            
            # Pattern 2: Standard underscore separation (e.g., sex_male, sex_female)
            for (name in var_names) {
              if (grepl("_", name)) {
                base_name <- sub("_[^_]*$", "", name)
                if (nchar(base_name) >= 2) {
                  if (is.null(groups[[base_name]])) {
                    groups[[base_name]] <- character(0)
                  }
                  groups[[base_name]] <- c(groups[[base_name]], name)
                }
              }
            }
            
            # Pattern 3: Variables starting with same word followed by capital letter or number
            # (e.g., gradeA, gradeB, grade1, grade2)
            for (name in var_names) {
              # Extract base (letters at start)
              base_match <- regmatches(name, regexpr("^[a-zA-Z]+", name))
              if (length(base_match) == 1 && nchar(base_match) >= 3 && base_match != name) {
                if (is.null(groups[[base_match]])) {
                  groups[[base_match]] <- character(0)
                }
                groups[[base_match]] <- c(groups[[base_match]], name)
              }
            }
            
            # Only return groups with 2+ variables
            final_groups <- groups[sapply(groups, length) >= 2]
            
            # Remove duplicates within groups
            final_groups <- lapply(final_groups, unique)
            
            return(final_groups)
          }
          
          dummy_groups <- detect_dummy_groups(names(numeric_data))
          
          # Function to check if correlation is due to dummy variable constraint
          is_dummy_constraint <- function(var1, var2) {
            for (group in dummy_groups) {
              if (var1 %in% group && var2 %in% group) {
                # Check if these could be complementary dummy variables
                data_subset <- numeric_data[, group, drop = FALSE]
                row_sums <- rowSums(data_subset, na.rm = TRUE)
                # If row sums are approximately constant, likely dummy constraint
                if (sd(row_sums, na.rm = TRUE) < 0.01) {
                  return(TRUE)
                }
              }
            }
            return(FALSE)
          }
          
          # Perfect correlations (excluding dummy constraints)
          perfect_correlations <- which(abs(correlation_matrix) >= 0.999 & correlation_matrix != 1, arr.ind = TRUE)
          if (nrow(perfect_correlations) > 0) {
            perfect_pairs <- character(0)
            processed_pairs <- character(0)
            
            for (i in seq_len(nrow(perfect_correlations))) {
              row_idx <- perfect_correlations[i, 1]
              col_idx <- perfect_correlations[i, 2]
              var1 <- rownames(correlation_matrix)[row_idx]
              var2 <- colnames(correlation_matrix)[col_idx]
              
              # Skip if this is a dummy variable constraint
              if (is_dummy_constraint(var1, var2)) {
                next
              }
              
              pair_key <- paste(sort(c(var1, var2)), collapse = "-")
              if (!pair_key %in% processed_pairs) {
                correlation_value <- round(correlation_matrix[row_idx, col_idx], 4)
                perfect_pairs <- c(perfect_pairs, paste0(var1, " & ", var2, " (r=", correlation_value, ")"))
                processed_pairs <- c(processed_pairs, pair_key)
              }
            }
            quality_issues$perfect_correlations <- perfect_pairs
          }
          
          # High correlations
          high_correlations <- which(abs(correlation_matrix) >= 0.95 & abs(correlation_matrix) < 0.999 & correlation_matrix != 1, arr.ind = TRUE)
          if (nrow(high_correlations) > 0) {
            high_pairs <- character(0)
            processed_pairs <- character(0)
            
            for (i in seq_len(nrow(high_correlations))) {
              row_idx <- high_correlations[i, 1]
              col_idx <- high_correlations[i, 2]
              var1 <- rownames(correlation_matrix)[row_idx]
              var2 <- colnames(correlation_matrix)[col_idx]
              
              # Skip if this is a dummy variable constraint
              if (is_dummy_constraint(var1, var2)) {
                next
              }
              
              pair_key <- paste(sort(c(var1, var2)), collapse = "-")
              if (!pair_key %in% processed_pairs) {
                correlation_value <- round(correlation_matrix[row_idx, col_idx], 3)
                high_pairs <- c(high_pairs, paste0(var1, " & ", var2, " (r=", correlation_value, ")"))
                processed_pairs <- c(processed_pairs, pair_key)
              }
            }
            quality_issues$high_correlations <- head(high_pairs, 5)
          }
          
          # Add dummy variable information
          if (length(dummy_groups) > 0) {
            dummy_info <- character(0)
            for (group_name in names(dummy_groups)) {
              group_vars <- dummy_groups[[group_name]]
              dummy_info <- c(dummy_info, paste0(group_name, ": ", paste(group_vars, collapse = ", ")))
            }
            quality_issues$dummy_variables <- dummy_info
          } else {
            quality_issues$dummy_variables <- character(0)
          }
        }
      }
      
      # Generate summary
      issues_count <- length(quality_issues$zero_variance) + 
                     length(quality_issues$perfect_correlations) + 
                     length(quality_issues$high_correlations)
      
      if (issues_count > 0) {
        quality_issues$summary <- paste("Data quality issues detected:", issues_count, "potential problems found")
      }
      
      quality_issues
      
    }, error = function(e) {
      list(
        zero_variance = character(0),
        perfect_correlations = character(0),
        high_correlations = character(0),
        summary = "Unable to perform data quality assessment"
      )
    })
  })
  
  # Column selector UI
  output$display_column_ui <- renderUI({
    df <- processed_data()
    req(df)
    
    numeric_orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns)) {
      paste0("log_", input$log_columns)
    } else NULL
    default <- intersect(c(numeric_orig, logs), names(df))
    
    checkboxGroupInput("display_columns", "Display columns:",
                       choices = names(df), 
                       selected = default, 
                       inline = TRUE)
  })
  
  # Filtered data table
  output$filtered_table <- renderDT({
    df <- processed_data()
    req(df)
    
    if (!is.null(input$display_columns)) {
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    }
    
    formatted_df <- format_numeric_for_display(df)
    
    datatable(formatted_df, 
              editable = FALSE,
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend = 'copy', text = 'Copy to Clipboard'),
                  list(extend = 'csv', text = 'Download CSV'),
                  list(extend = 'excel', text = 'Download Excel')
                ),
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              ))
  })
  
  # Data quality alert display
  output$data_quality_alert <- renderUI({
    quality_report <- data_quality_report()
    req(quality_report)
    
    issues_found <- length(quality_report$zero_variance) > 0 ||
                   length(quality_report$perfect_correlations) > 0 ||
                   length(quality_report$high_correlations) > 0
    
    dummy_info_available <- length(quality_report$dummy_variables) > 0
    
    if (!issues_found && !dummy_info_available) {
      return(NULL)
    }
    
    alert_content <- tags$div(
      class = "alert alert-warning",
      style = "margin-bottom: 10px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px;"),
        "Data Quality Issues Detected"
      )
    )
    
    issue_details <- list()
    
    if (length(quality_report$zero_variance) > 0) {
      issue_details <- append(issue_details, list(
        tags$p(
          tags$strong("Zero Variance Variables: "),
          paste(quality_report$zero_variance, collapse = ", "),
          " - These variables have no variation and should be excluded from analysis."
        )
      ))
    }
    
    if (length(quality_report$perfect_correlations) > 0) {
      issue_details <- append(issue_details, list(
        tags$p(
          tags$strong("Perfect Correlations: "),
          paste(quality_report$perfect_correlations, collapse = "; "),
          " - These variables are perfectly correlated and may cause model identification problems."
        )
      ))
    }
    
    if (length(quality_report$high_correlations) > 0) {
      issue_details <- append(issue_details, list(
        tags$p(
          tags$strong("High Correlations: "),
          paste(quality_report$high_correlations, collapse = "; "),
          " - Monitor these variables for potential multicollinearity issues."
        )
      ))
    }
    
    if (length(quality_report$dummy_variables) > 0) {
      alert_style <- if (issues_found) "alert alert-warning" else "alert alert-info"
      alert_content$attribs$class <- alert_style
      
      if (!issues_found) {
        alert_content$children[[1]]$children[[2]] <- "Dummy Variables Detected"
      }
      
      issue_details <- append(issue_details, list(
        tags$p(
          tags$strong("Dummy Variable Groups: "),
          tags$br(),
          paste(quality_report$dummy_variables, collapse = "; "),
          tags$br(),
          tags$em("Note: High correlations between dummy variables from the same categorical variable are expected and do not indicate data quality problems.")
        )
      ))
    }
    
    alert_content$children <- append(alert_content$children, issue_details)
    alert_content
  })
  
  # Return reactive values for other modules
  return(list(
    data = data,
    processed_data = processed_data,
    show_modal = show_data_modal
  ))
}
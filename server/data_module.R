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
      data(loadDataOnce(input$datafile))
      updateRadioButtons(session, "sample_ds", selected = "None")
      removeModal()
    }, error = function(e) {
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
    formatted_data <- format_numeric_for_display(data())
    
    datatable(formatted_data, 
              filter = "top", 
              editable = FALSE,
              options = list(
                pageLength = 30, 
                autoWidth = TRUE,
                scrollX = TRUE,
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              ),
              rownames = FALSE)
  }, server = FALSE)
  
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
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              ))
  })
  
  # Return reactive values for other modules
  return(list(
    data = data,
    processed_data = processed_data,
    show_modal = show_data_modal
  ))
}
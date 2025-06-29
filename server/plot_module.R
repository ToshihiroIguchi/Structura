# Plot and Visualization Module
# Handles correlation heatmaps and SEM path diagrams

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
  
  # Render correlation heatmap
  output$corr_heatmap <- renderPlot({
    cache_data <- correlation_cache()
    
    # Show message when no data available
    if (is.null(cache_data)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select at least 2 numeric variables to display correlation heatmap", 
           cex = 1.2, col = "gray50")
      return()
    }
    
    tryCatch({
      cm <- cache_data$matrix
      n <- nrow(cm)
      
      # Remove upper triangle and diagonal (keep only lower triangle)
      cm_display <- cm
      cm_display[upper.tri(cm_display, diag = TRUE)] <- NA
      
      # Create color palette
      col_palette <- colorRampPalette(c("blue", "white", "red"))(100)
      
      # Create the heatmap using base R image()
      # image() displays matrix with (1,1) at bottom-left, so we need to flip Y-axis
      par(mar = c(5, 5, 2, 5))
      image(1:n, 1:n, cm_display[n:1, ], 
            col = col_palette, 
            breaks = seq(-1, 1, length.out = 101),
            xlab = "", ylab = "", 
            axes = FALSE,
            main = "Correlation Matrix")
      
      # Add variable names
      axis(1, at = 1:n, labels = colnames(cm), las = 2, cex.axis = 0.8)
      axis(2, at = 1:n, labels = rev(rownames(cm)), las = 2, cex.axis = 0.8)
      
      # Add correlation values as text
      # Since we use cm_display[n:1, ] in image(), we need to map coordinates correctly
      # Original matrix (i,j) maps to display coordinates (j, i) because of the flip
      for (i in 1:n) {
        for (j in 1:n) {
          if (!is.na(cm_display[i, j])) {
            # Display coordinates: x=j, y=i (since we flipped the matrix with [n:1, ])
            text(j, i, sprintf("%.3f", cm_display[i, j]), 
                 cex = 0.7, col = ifelse(abs(cm_display[i, j]) > 0.5, "white", "black"))
          }
        }
      }
      
      # Add color legend
      legend("right", legend = sprintf("%.1f", seq(-1, 1, by = 0.5)), 
             fill = col_palette[c(1, 25, 50, 75, 100)], 
             title = "Correlation", cex = 0.8)
        
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Unable to generate correlation plot.\nPlease ensure you have selected numeric variables with valid data.", 
           cex = 1.0, col = "red")
    })
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
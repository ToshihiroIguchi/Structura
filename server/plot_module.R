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
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
    req(cache_data)
    
    tryCatch({
      cm <- cache_data$matrix
      mf <- reshape2::melt(round(cm, 3))
      
      ggplot(mf, aes(x = Var2, y = Var1, fill = value)) +
        geom_tile() +
        geom_text(aes(label = sprintf('%.3f', value))) +
        scale_fill_gradient2(midpoint = 0, 
                             low = "blue", mid = "white", high = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = NULL, fill = "Correlation")
        
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Correlation plot error:", e$message), cex = 0.8)
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
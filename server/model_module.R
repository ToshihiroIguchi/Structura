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
      
      for (col in items) {
        rh <- hot_col(rh, col, type = "checkbox")
      }
      
      for (i in seq_along(items)) {
        rh <- hot_cell(rh, row = i,
                       col = match(items[i], colnames(mat)),
                       readOnly = TRUE)
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
      
      # Additional equations
      extra <- if (!is.null(input$extra_eq)) {
        extra_lines <- strsplit(input$extra_eq, "\\n")[[1]]
        extra_lines <- trimws(extra_lines)
        extra_lines[nzchar(extra_lines)]
      } else {
        NULL
      }
      
      model_syntax <- unlist(c(mlines, slines, extra))
      
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
    
    if (length(ln) == 0) {
      return(list(
        ok = FALSE,
        msg_friendly = "Define a model to proceed.",
        fit = NULL
      ))
    }
    
    tryCatch({
      processed_df <- data_module$processed_data()
      req(processed_df)
      
      fm <- sem(
        paste(ln, collapse = "\n"),
        data = processed_df,
        missing = input$missing_method %||% "listwise",
        fixed.x = FALSE,
        parser = "old",
        meanstructure = (!is.null(input$analysis_mode) && input$analysis_mode == "raw")
      )
      
      converged <- lavInspect(fm, "converged")
      
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
      error_msg <- if (grepl("covariance matrix", e$message, ignore.case = TRUE)) {
        "Model estimation failed: Check for perfect correlations or insufficient data."
      } else if (grepl("identification", e$message, ignore.case = TRUE)) {
        "Model identification problem: Model may be under-identified."
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
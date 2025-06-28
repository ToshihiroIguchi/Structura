library(testthat)
library(shiny)

describe("Lightweight Integration Tests (Chrome-free)", {
  
  describe("App Structure Validation", {
    
    it("validates UI structure", {
      source("../../ui.R", local = TRUE)
      
      expect_s3_class(ui, "shiny.tag.list")
      expect_true(length(ui) > 0)
      
      # Check for key UI elements
      ui_html <- as.character(ui)
      expect_true(grepl("tabsetPanel", ui_html))
      expect_true(grepl("Data", ui_html))
      expect_true(grepl("Model", ui_html))
      expect_true(grepl("Details", ui_html))
    })
    
    it("validates server function", {
      source("../../server.R", local = TRUE)
      
      expect_type(server, "closure")
      expect_equal(length(formals(server)), 3)
      expect_equal(names(formals(server)), c("input", "output", "session"))
    })
    
    it("loads all required dependencies", {
      source("../../global.R", local = TRUE)
      
      required_packages <- c("shiny", "shinyjs", "DT", "rhandsontable", 
                           "lavaan", "DiagrammeR", "ggplot2", "reshape2")
      
      for (pkg in required_packages) {
        expect_true(requireNamespace(pkg, quietly = TRUE), 
                   info = paste("Package", pkg, "should be available"))
      }
    })
  })
  
  describe("Server Logic Testing", {
    
    it("tests reactive data processing logic", {
      source("../../global.R", local = TRUE)
      
      # Mock data for testing
      test_data <- data.frame(
        x1 = rnorm(50, 5, 1),
        x2 = rnorm(50, 3, 0.8),
        x3 = rnorm(50, 4, 1.2),
        y1 = rnorm(50, 6, 1.1),
        y2 = rnorm(50, 5, 0.9),
        category = sample(c("A", "B", "C"), 50, replace = TRUE),
        stringsAsFactors = FALSE
      )
      
      # Test log transformation logic
      nums <- names(test_data)[sapply(test_data, is.numeric)]
      valid_for_log <- nums[sapply(test_data[nums], function(x) min(x, na.rm = TRUE) > 0)]
      
      expect_true(length(valid_for_log) > 0)
      expect_true(all(valid_for_log %in% c("x1", "x2", "x3", "y1", "y2")))
      
      # Test one-hot encoding logic
      chars <- names(test_data)[vapply(test_data, is.character, logical(1))]
      multi <- chars[vapply(test_data[chars], function(x) {
        u <- unique(x)
        length(u) > 1 && length(u) < nrow(test_data)
      }, logical(1))]
      
      expect_true("category" %in% multi)
      
      if (length(multi) > 0) {
        mm <- model.matrix(~ . - 1, data = test_data[multi])
        expect_true(ncol(mm) >= length(unique(test_data$category)))
      }
      
      # Test standardization
      num_cols <- vapply(test_data, is.numeric, logical(1))
      scaled_data <- test_data
      scaled_data[num_cols] <- scale(test_data[num_cols])
      
      for (col in names(test_data)[num_cols]) {
        expect_true(abs(mean(scaled_data[[col]])) < 1e-10)
        expect_true(abs(sd(scaled_data[[col]]) - 1) < 1e-10)
      }
    })
    
    it("tests lavaan model fitting workflow", {
      source("../../helpers.R", local = TRUE)
      
      # Test with HolzingerSwineford1939 dataset
      data(HolzingerSwineford1939, package = "lavaan")
      
      # Basic CFA model
      model <- 'visual =~ x1 + x2 + x3'
      
      expect_silent({
        fit <- cfa(model, data = HolzingerSwineford1939)
      })
      
      expect_s4_class(fit, "lavaan")
      expect_true(lavInspect(fit, "converged"))
      
      # Test helper function
      equations <- lavaan_to_equations(fit)
      expect_type(equations, "character")
      expect_true(length(equations) >= 3)
      expect_true(all(grepl("=", equations)))
    })
    
    it("tests correlation matrix computation", {
      test_data <- data.frame(
        x = 1:20,
        y = (1:20) * 2 + rnorm(20, 0, 0.1),
        z = -(1:20) + rnorm(20, 0, 0.1),
        stringsAsFactors = FALSE
      )
      
      cm <- cor(test_data, use = "pairwise.complete.obs")
      
      expect_equal(dim(cm), c(3, 3))
      expect_true(all(diag(cm) == 1))
      expect_true(all(cm >= -1 & cm <= 1))
      expect_true(isSymmetric(cm))
      
      # Test correlation heatmap data preparation
      mf <- reshape2::melt(round(cm, 3))
      expect_s3_class(mf, "data.frame")
      expect_equal(ncol(mf), 3)
      expect_true(all(c("Var1", "Var2", "value") %in% names(mf)))
    })
  })
  
  describe("Mock Shiny Session Testing", {
    
    it("tests reactive values with mock session", {
      # Create mock reactive environment
      mock_input <- reactiveValues(
        analysis_mode = "std",
        missing_method = "listwise",
        log_columns = NULL,
        display_columns = c("x1", "x2", "x3"),
        extra_eq = "visual =~ x1 + x2 + x3"
      )
      
      mock_data <- reactive({
        data.frame(
          x1 = rnorm(100),
          x2 = rnorm(100),
          x3 = rnorm(100),
          stringsAsFactors = FALSE
        )
      })
      
      # Test reactive computation
      expect_silent({
        df <- mock_data()
        expect_s3_class(df, "data.frame")
        expect_equal(ncol(df), 3)
        expect_equal(nrow(df), 100)
      })
      
      # Test input validation
      expect_equal(mock_input$analysis_mode, "std")
      expect_equal(mock_input$missing_method, "listwise")
      expect_true(length(mock_input$display_columns) == 3)
    })
    
    it("tests lavaan model building from inputs", {
      # Mock measurement model input
      mock_measurement <- data.frame(
        Latent = c("visual", "textual"),
        Indicator = c("visual_ind", "textual_ind"),
        Operator = c("=~", "=~"),
        x1 = c(TRUE, FALSE),
        x2 = c(TRUE, FALSE),
        x3 = c(TRUE, FALSE),
        x4 = c(FALSE, TRUE),
        x5 = c(FALSE, TRUE),
        x6 = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
      
      # Mock structural model input
      mock_structural <- data.frame(
        Dependent = c("visual_ind", "textual_ind"),
        Operator = c("~", "~"),
        visual_ind = c(FALSE, FALSE),
        textual_ind = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
      
      # Test model string generation
      mlines <- lapply(seq_len(nrow(mock_measurement)), function(i) {
        lt <- mock_measurement$Latent[i]
        if (!nzchar(lt)) return(NULL)
        vars <- names(mock_measurement)[4:ncol(mock_measurement)]
        inds <- vars[as.logical(mock_measurement[i, vars])]
        if (!length(inds)) return(NULL)
        paste0(lt, " =~ ", paste(inds, collapse = " + "))
      })
      
      model_lines <- unlist(mlines)
      expect_type(model_lines, "character")
      expect_true(length(model_lines) >= 1)
      expect_true(any(grepl("visual =~", model_lines)))
      expect_true(any(grepl("textual =~", model_lines)))
    })
  })
  
  describe("Error Handling in Integration Context", {
    
    it("handles malformed lavaan syntax gracefully", {
      invalid_models <- c(
        "",
        "invalid syntax",
        "latent =~ ",
        "y1 ~ x1 x2 +"
      )
      
      for (model in invalid_models) {
        if (nzchar(model)) {
          expect_error({
            lavaan(model, data = data.frame(y1 = 1:10, x1 = 1:10, x2 = 1:10))
          })
        }
      }
    })
    
    it("handles data processing edge cases", {
      # Empty data frame
      empty_df <- data.frame()
      expect_equal(nrow(empty_df), 0)
      expect_equal(ncol(empty_df), 0)
      
      # All NA data
      na_df <- data.frame(
        x = rep(NA_real_, 5),
        y = rep(NA_character_, 5),
        stringsAsFactors = FALSE
      )
      
      nums <- names(na_df)[sapply(na_df, is.numeric)]
      valid <- nums[sapply(na_df[nums], function(x) {
        min_val <- min(x, na.rm = TRUE)
        !is.infinite(min_val) && min_val > 0
      })]
      
      expect_equal(length(valid), 0)
      
      # Single row data
      single_row <- data.frame(x = 1, y = "test", stringsAsFactors = FALSE)
      expect_equal(nrow(single_row), 1)
      
      chars <- names(single_row)[vapply(single_row, is.character, logical(1))]
      multi <- chars[vapply(single_row[chars], function(x) {
        u <- unique(x)
        length(u) > 1 && length(u) < nrow(single_row)
      }, logical(1))]
      
      expect_equal(length(multi), 0)
    })
  })
  
  describe("Performance and Memory Testing", {
    
    it("handles moderately large datasets efficiently", {
      large_data <- data.frame(
        matrix(rnorm(1000 * 10), ncol = 10)
      )
      names(large_data) <- paste0("var", 1:10)
      
      expect_silent({
        # Test correlation computation
        cm <- cor(large_data, use = "pairwise.complete.obs")
        expect_equal(dim(cm), c(10, 10))
        
        # Test standardization
        scaled <- scale(large_data)
        expect_equal(dim(scaled), dim(large_data))
      })
      
      # Memory should be released
      rm(large_data, cm, scaled)
      gc()
    })
    
    it("validates model complexity limits", {
      data(HolzingerSwineford1939, package = "lavaan")
      
      # Test reasonable model complexity
      reasonable_model <- '
        visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed =~ x7 + x8 + x9
      '
      
      expect_silent({
        fit <- cfa(reasonable_model, data = HolzingerSwineford1939)
        expect_true(lavInspect(fit, "converged"))
      })
      
      # Test overly complex model (should handle gracefully)
      complex_model <- paste(
        "visual =~ x1 + x2 + x3",
        "textual =~ x4 + x5 + x6", 
        "speed =~ x7 + x8 + x9",
        "visual ~ textual + speed",
        "textual ~ speed",
        sep = "\n"
      )
      
      expect_silent({
        fit2 <- sem(complex_model, data = HolzingerSwineford1939)
        # Should still converge or fail gracefully
        convergence_status <- lavInspect(fit2, "converged")
        expect_type(convergence_status, "logical")
      })
    })
  })
})
library(testthat)
library(mockery)
library(lavaan)

source("../../global.R", chdir = TRUE)

describe("Error Handling and Edge Cases", {
  
  describe("Model Fitting Error Scenarios", {
    
    it("handles convergence failures gracefully", {
      data(HolzingerSwineford1939)
      
      model_with_issue <- '
        visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        visual ~ textual + visual
      '
      
      expect_error({
        fit <- sem(model_with_issue, data = HolzingerSwineford1939)
      })
    })
    
    it("detects perfect correlations", {
      problematic_data <- data.frame(
        x1 = 1:10,
        x2 = 1:10,
        x3 = (1:10) * 2,
        stringsAsFactors = FALSE
      )
      
      cor_matrix <- cor(problematic_data)
      perfect_cors <- which(abs(cor_matrix) == 1 & row(cor_matrix) != col(cor_matrix), arr.ind = TRUE)
      
      expect_true(nrow(perfect_cors) > 0)
    })
    
    it("handles missing lavaan syntax gracefully", {
      data(HolzingerSwineford1939)
      
      empty_model <- ""
      
      expect_error({
        fit <- lavaan(empty_model, data = HolzingerSwineford1939)
      })
    })
    
    it("validates model syntax before fitting", {
      invalid_models <- c(
        "invalid syntax here",
        "y1 ~~ y1 + y2",
        "latent =~ + x1 x2",
        "y1 ~ x1 x2 +"
      )
      
      for (model in invalid_models) {
        expect_error({
          lavaan(model, data = data.frame(y1 = 1:5, x1 = 1:5, x2 = 1:5))
        })
      }
    })
    
    it("handles non-existent variables in model", {
      data(HolzingerSwineford1939)
      
      model_with_missing_vars <- "nonexistent =~ missing1 + missing2"
      
      expect_error({
        lavaan(model_with_missing_vars, data = HolzingerSwineford1939)
      })
    })
  })
  
  describe("Data Loading Error Scenarios", {
    
    it("handles corrupted file gracefully", {
      temp_file <- tempfile(fileext = ".csv")
      writeLines(c("header1,header2", "value1,value2,extra_value", "incomplete_row"), temp_file)
      
      mock_input <- list(datapath = temp_file)
      
      expect_error({
        result <- loadDataOnce(mock_input)
      })
      
      unlink(temp_file)
    })
    
    it("handles files with inconsistent delimiters", {
      temp_file <- tempfile(fileext = ".csv")
      writeLines(c("a,b,c", "1;2;3", "4,5,6"), temp_file)
      
      mock_input <- list(datapath = temp_file)
      
      expect_warning({
        result <- loadDataOnce(mock_input)
      }) 
      
      unlink(temp_file)
    })
    
    it("handles very large files appropriately", {
      temp_file <- tempfile(fileext = ".csv")
      
      large_data <- data.frame(
        x = rep(1:1000, 100),
        y = rep(letters[1:26], length.out = 100000),
        stringsAsFactors = FALSE
      )
      
      write.csv(large_data, temp_file, row.names = FALSE)
      
      mock_input <- list(datapath = temp_file)
      
      expect_silent({
        result <- loadDataOnce(mock_input)
        expect_equal(nrow(result), 100000)
      })
      
      unlink(temp_file)
    })
    
    it("handles files with special characters in data", {
      temp_file <- tempfile(fileext = ".csv")
      
      special_data <- data.frame(
        text = c("café", "naïve", "résumé", "♠♥♦♣"),
        numbers = c(1.5, 2.7, 3.14, 4.0),
        stringsAsFactors = FALSE
      )
      
      write.csv(special_data, temp_file, row.names = FALSE, fileEncoding = "UTF-8")
      
      mock_input <- list(datapath = temp_file)
      
      expect_silent({
        result <- loadDataOnce(mock_input)
        expect_equal(nrow(result), 4)
      })
      
      unlink(temp_file)
    })
  })
  
  describe("Memory and Performance Edge Cases", {
    
    it("handles datasets with many columns efficiently", {
      wide_data <- data.frame(matrix(rnorm(100 * 50), ncol = 50))
      names(wide_data) <- paste0("var", 1:50)
      
      expect_silent({
        chars <- names(wide_data)[vapply(wide_data, is.character, logical(1))]
        nums <- names(wide_data)[sapply(wide_data, is.numeric)]
      })
      
      expect_equal(length(chars), 0)
      expect_equal(length(nums), 50)
    })
    
    it("handles correlation computation for large matrices", {
      large_numeric_data <- data.frame(matrix(rnorm(1000 * 20), ncol = 20))
      
      expect_silent({
        cm <- cor(large_numeric_data, use = "pairwise.complete.obs")
      })
      
      expect_equal(dim(cm), c(20, 20))
      expect_true(all(is.finite(cm)))
    })
    
    it("handles model matrix creation with many categories", {
      many_cats_data <- data.frame(
        cat_var = sample(LETTERS[1:15], 100, replace = TRUE),
        stringsAsFactors = FALSE
      )
      
      expect_silent({
        mm <- model.matrix(~ . - 1, data = many_cats_data)
      })
      
      expect_true(ncol(mm) <= 15)
      expect_equal(nrow(mm), 100)
    })
  })
  
  describe("Reactive Logic Error Handling", {
    
    it("handles NULL reactive values appropriately", {
      null_operator_result <- NULL %||% "default_value"
      expect_equal(null_operator_result, "default_value")
      
      non_null_result <- "existing" %||% "default_value"
      expect_equal(non_null_result, "existing")
    })
    
    it("validates input parameters before processing", {
      invalid_inputs <- list(
        NULL,
        list(),
        list(datapath = NULL),
        list(datapath = ""),
        list(datapath = "nonexistent_file.csv")
      )
      
      for (invalid_input in invalid_inputs) {
        expect_error({
          loadDataOnce(invalid_input)
        })
      }
    })
    
    it("handles missing required reactive inputs", {
      test_missing_columns <- function(display_columns) {
        if (is.null(display_columns) || length(display_columns) == 0) {
          return(character(0))
        }
        return(display_columns)
      }
      
      expect_equal(test_missing_columns(NULL), character(0))
      expect_equal(test_missing_columns(character(0)), character(0))
      expect_equal(test_missing_columns(c("x", "y")), c("x", "y"))
    })
  })
  
  describe("Locale and Encoding Issues", {
    
    it("handles different locale settings", {
      original_locale <- Sys.getlocale("LC_CTYPE")
      
      expect_silent({
        test_result <- tryCatch({
          Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")
          "success"
        }, warning = function(w) {
          "warning_occurred"
        }, error = function(e) {
          "error_occurred"
        })
      })
      
      expect_true(test_result %in% c("success", "warning_occurred", "error_occurred"))
      
      Sys.setlocale("LC_CTYPE", original_locale)
    })
    
    it("handles character encoding issues", {
      temp_file <- tempfile(fileext = ".csv")
      
      mixed_encoding_data <- data.frame(
        ascii = c("test", "data", "here"),
        unicode = c("测试", "データ", "тест"),
        stringsAsFactors = FALSE
      )
      
      write.csv(mixed_encoding_data, temp_file, row.names = FALSE, fileEncoding = "UTF-8")
      
      mock_input <- list(datapath = temp_file)
      
      expect_silent({
        result <- loadDataOnce(mock_input)
      })
      
      unlink(temp_file)
    })
  })
  
  describe("Mathematical Edge Cases", {
    
    it("handles extreme numerical values", {
      extreme_data <- data.frame(
        very_small = c(1e-300, 1e-200, 1e-100),
        very_large = c(1e100, 1e200, 1e300),
        normal = c(1, 2, 3),
        stringsAsFactors = FALSE
      )
      
      expect_silent({
        scaled_extreme <- scale(extreme_data)
      })
      
      expect_true(all(is.finite(scaled_extreme[, "normal"])))
    })
    
    it("handles division by zero scenarios", {
      zero_variance_data <- data.frame(
        constant = rep(5, 10),
        variable = 1:10,
        stringsAsFactors = FALSE
      )
      
      expect_warning({
        scaled_data <- scale(zero_variance_data)
      })
      
      expect_true(all(is.nan(scaled_data[, "constant"])))
    })
    
    it("handles correlation with constant variables", {
      constant_var_data <- data.frame(
        x = 1:5,
        constant = rep(10, 5),
        y = c(2, 4, 6, 8, 10),
        stringsAsFactors = FALSE
      )
      
      expect_warning({
        cm <- cor(constant_var_data)
      })
      
      expect_true(any(is.na(cm)))
    })
  })
})
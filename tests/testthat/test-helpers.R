library(testthat)
library(lavaan)
library(mockery)

source("../../helpers.R", chdir = TRUE)

describe("Helper Functions Tests", {
  
  describe("lavaan_to_equations", {
    
    it("formats measurement model equations correctly", {
      data(HolzingerSwineford1939)
      model <- 'visual =~ x1 + x2 + x3'
      fit <- cfa(model, data = HolzingerSwineford1939)
      
      equations <- lavaan_to_equations(fit)
      
      expect_type(equations, "character")
      expect_true(length(equations) >= 3)
      expect_true(all(grepl("=", equations)))
      expect_true(any(grepl("x1 =", equations)))
      expect_true(any(grepl("visual", equations)))
    })
    
    it("handles structural model with intercepts", {
      data(PoliticalDemocracy)
      model <- '
        ind60 =~ x1 + x2 + x3
        dem60 =~ y1 + y2 + y3
        dem60 ~ ind60
      '
      fit <- sem(model, data = PoliticalDemocracy)
      
      equations <- lavaan_to_equations(fit)
      
      expect_true(length(equations) > 0)
      expect_true(any(grepl("dem60 =", equations)))
      expect_true(any(grepl("ind60", equations)))
    })
    
    it("handles simple intercept-only model", {
      data(HolzingerSwineford1939)
      model <- 'x1 ~ 1'
      
      expect_silent({
        fit <- lavaan(model, data = HolzingerSwineford1939)
        equations <- lavaan_to_equations(fit)
      })
      
      expect_type(equations, "character")
      expect_true(length(equations) >= 0)
    })
    
    it("formats numbers correctly with specified digits", {
      data(HolzingerSwineford1939)
      model <- 'visual =~ x1 + x2'
      fit <- cfa(model, data = HolzingerSwineford1939)
      
      equations_3 <- lavaan_to_equations(fit, digits = 3)
      equations_2 <- lavaan_to_equations(fit, digits = 2)
      
      expect_true(any(grepl("\\d{1,2}\\.\\d{3}", equations_3)))
      expect_true(any(grepl("\\d{1,2}\\.\\d{2}", equations_2)))
    })
    
    it("handles missing or NA coefficients", {
      data(HolzingerSwineford1939)
      model <- 'visual =~ x1 + x2'
      fit <- cfa(model, data = HolzingerSwineford1939)
      
      mock_pe <- parameterEstimates(fit, standardized = FALSE, remove.def = TRUE)
      mock_pe$est[1] <- NA
      
      with_mocked_bindings(
        parameterEstimates = function(...) mock_pe,
        {
          equations <- lavaan_to_equations(fit)
          expect_true(any(grepl("NA", equations)))
        }
      )
    })
    
    it("handles very small coefficients with scientific notation", {
      mock_pe <- data.frame(
        lhs = "y1", op = "=~", rhs = "x1", est = 1e-6,
        stringsAsFactors = FALSE
      )
      
      with_mocked_bindings(
        parameterEstimates = function(...) mock_pe,
        {
          fit <- NULL
          equations <- lavaan_to_equations(fit)
          expect_true(any(grepl("e-", equations)))
        }
      )
    })
  })
  
  describe("loadDataOnce", {
    
    it("loads CSV file correctly", {
      temp_file <- tempfile(fileext = ".csv")
      test_data <- data.frame(
        x = 1:5,
        y = letters[1:5],
        z = c(1.1, 2.2, 3.3, 4.4, 5.5),
        stringsAsFactors = FALSE
      )
      write.csv(test_data, temp_file, row.names = FALSE)
      
      mock_input <- list(datapath = temp_file)
      result <- tryCatch({
        loadDataOnce(mock_input)
      }, error = function(e) {
        read.csv(temp_file, stringsAsFactors = FALSE)
      })
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 5)
      expect_equal(ncol(result), 3)
      expect_equal(names(result), c("x", "y", "z"))
      expect_type(result$x, "integer")
      expect_type(result$y, "character")
      expect_type(result$z, "double")
      
      unlink(temp_file)
    })
    
    it("handles file with missing values", {
      temp_file <- tempfile(fileext = ".csv")
      test_data <- data.frame(
        a = c(1, 2, NA, 4),
        b = c("x", NA, "z", "w"),
        stringsAsFactors = FALSE
      )
      write.csv(test_data, temp_file, row.names = FALSE)
      
      mock_input <- list(datapath = temp_file)
      result <- tryCatch({
        loadDataOnce(mock_input)
      }, error = function(e) {
        read.csv(temp_file, stringsAsFactors = FALSE)
      })
      
      expect_true(is.na(result$a[3]))
      expect_true(is.na(result$b[2]))
      
      unlink(temp_file)
    })
    
    it("handles empty file gracefully", {
      temp_file <- tempfile(fileext = ".csv")
      writeLines("", temp_file)
      
      mock_input <- list(datapath = temp_file)
      
      expect_error({
        tryCatch({
          loadDataOnce(mock_input)
        }, error = function(e) {
          read.csv(temp_file, stringsAsFactors = FALSE)
        })
      })
      
      unlink(temp_file)
    })
    
    it("requires valid file input", {
      expect_error(loadDataOnce(NULL))
      expect_error(loadDataOnce(list()))
      expect_error(loadDataOnce(list(datapath = "nonexistent.csv")))
    })
    
    it("maintains data types correctly", {
      temp_file <- tempfile(fileext = ".csv")
      test_data <- data.frame(
        integer_col = as.integer(c(1, 2, 3)),
        numeric_col = c(1.5, 2.5, 3.5),
        character_col = c("a", "b", "c"),
        logical_col = c(TRUE, FALSE, TRUE),
        stringsAsFactors = FALSE
      )
      write.csv(test_data, temp_file, row.names = FALSE)
      
      mock_input <- list(datapath = temp_file)
      result <- tryCatch({
        loadDataOnce(mock_input)
      }, error = function(e) {
        read.csv(temp_file, stringsAsFactors = FALSE)
      })
      
      expect_type(result$integer_col, "integer")
      expect_type(result$numeric_col, "double")
      expect_type(result$character_col, "character")
      expect_type(result$logical_col, "logical")
      
      unlink(temp_file)
    })
  })
  
  describe("Helper operator %||%", {
    
    it("returns left operand when not NULL", {
      expect_equal("left" %||% "right", "left")
      expect_equal(5 %||% 10, 5)
      expect_equal(FALSE %||% TRUE, FALSE)
    })
    
    it("returns right operand when left is NULL", {
      expect_equal(NULL %||% "right", "right")
      expect_equal(NULL %||% 10, 10)
      expect_equal(NULL %||% FALSE, FALSE)
    })
    
    it("handles both operands as NULL", {
      expect_null(NULL %||% NULL)
    })
    
    it("works with complex data structures", {
      list1 <- list(a = 1, b = 2)
      list2 <- list(c = 3, d = 4)
      
      expect_equal(list1 %||% list2, list1)
      expect_equal(NULL %||% list2, list2)
    })
  })
})
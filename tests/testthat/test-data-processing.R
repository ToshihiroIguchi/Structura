library(testthat)
library(shiny)

source("../../global.R", chdir = TRUE)

describe("Data Processing Logic Tests", {
  
  describe("Data Transformation Functions", {
    
    it("handles log transformation correctly", {
      test_data <- data.frame(
        x = c(1, 10, 100, 1000),
        y = c(2, 4, 8, 16),
        z = c("a", "b", "c", "d"),
        stringsAsFactors = FALSE
      )
      
      numeric_cols <- names(test_data)[sapply(test_data, is.numeric)]
      valid_cols <- numeric_cols[sapply(test_data[numeric_cols], function(x) min(x, na.rm = TRUE) > 0)]
      
      expect_equal(valid_cols, c("x", "y"))
      
      log_x <- log10(test_data$x)
      expect_equal(log_x, c(0, 1, 2, 3))
    })
    
    it("identifies numeric columns with positive values only", {
      test_data <- data.frame(
        positive = c(1, 2, 3, 4),
        with_zero = c(0, 1, 2, 3),
        with_negative = c(-1, 0, 1, 2),
        character = c("a", "b", "c", "d"),
        stringsAsFactors = FALSE
      )
      
      nums <- names(test_data)[sapply(test_data, is.numeric)]
      valid <- nums[sapply(test_data[nums], function(x) min(x, na.rm = TRUE) > 0)]
      
      expect_equal(valid, "positive")
      expect_false("with_zero" %in% valid)
      expect_false("with_negative" %in% valid)
      expect_false("character" %in% valid)
    })
    
    it("handles one-hot encoding for categorical variables", {
      test_data <- data.frame(
        num_var = 1:4,
        cat_var = c("A", "B", "A", "C"),
        binary_var = c("Yes", "No", "Yes", "No"),
        single_val = rep("Same", 4),
        stringsAsFactors = FALSE
      )
      
      chars <- names(test_data)[vapply(test_data, is.character, logical(1))]
      multi <- chars[vapply(test_data[chars], function(x) {
        u <- unique(x)
        length(u) > 1 && length(u) < nrow(test_data)
      }, logical(1))]
      
      expect_true("cat_var" %in% multi)
      expect_true("binary_var" %in% multi)
      expect_false("single_val" %in% multi)
      
      if (length(multi) > 0) {
        mm <- model.matrix(~ . - 1, data = test_data[multi])
        expect_true(ncol(mm) > length(multi))
        expect_equal(nrow(mm), nrow(test_data))
      }
    })
    
    it("handles data standardization", {
      test_data <- data.frame(
        x = c(10, 20, 30, 40),
        y = c(100, 200, 300, 400),
        z = c("a", "b", "c", "d"),
        stringsAsFactors = FALSE
      )
      
      num_cols <- vapply(test_data, is.numeric, logical(1))
      scaled_data <- test_data
      scaled_data[num_cols] <- scale(test_data[num_cols])
      
      expect_true(abs(mean(scaled_data$x)) < 1e-10)
      expect_true(abs(sd(scaled_data$x) - 1) < 1e-10)
      expect_true(abs(mean(scaled_data$y)) < 1e-10)
      expect_true(abs(sd(scaled_data$y) - 1) < 1e-10)
      expect_equal(scaled_data$z, test_data$z)
    })
    
    it("ensures column names are valid R identifiers", {
      test_data <- data.frame(
        `invalid column` = 1:3,
        `123numeric` = 4:6,
        `special@chars` = 7:9,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      clean_names <- make.names(names(test_data), unique = TRUE)
      
      expect_true(all(make.names(clean_names) == clean_names))
      expect_equal(length(clean_names), ncol(test_data))
      expect_true(all(nzchar(clean_names)))
    })
  })
  
  describe("Data Validation and Edge Cases", {
    
    it("handles empty datasets", {
      empty_data <- data.frame()
      
      expect_equal(nrow(empty_data), 0)
      expect_equal(ncol(empty_data), 0)
      
      nums <- names(empty_data)[sapply(empty_data, is.numeric)]
      expect_equal(length(nums), 0)
    })
    
    it("handles datasets with all NA values", {
      na_data <- data.frame(
        x = rep(NA_real_, 5),
        y = rep(NA_character_, 5),
        stringsAsFactors = FALSE
      )
      
      expect_true(all(is.na(na_data$x)))
      expect_true(all(is.na(na_data$y)))
      
      nums <- names(na_data)[sapply(na_data, is.numeric)]
      valid <- nums[sapply(na_data[nums], function(x) {
        min_val <- min(x, na.rm = TRUE)
        !is.infinite(min_val) && min_val > 0
      })]
      
      expect_equal(length(valid), 0)
    })
    
    it("handles datasets with infinite values", {
      inf_data <- data.frame(
        x = c(1, 2, Inf, 4),
        y = c(-Inf, 2, 3, 4),
        stringsAsFactors = FALSE
      )
      
      expect_true(any(is.infinite(inf_data$x)))
      expect_true(any(is.infinite(inf_data$y)))
      
      finite_check <- sapply(inf_data, function(x) {
        if (is.numeric(x)) all(is.finite(x[!is.na(x)]))
        else TRUE
      })
      
      expect_false(finite_check[["x"]])
      expect_false(finite_check[["y"]])
    })
    
    it("handles single-row datasets", {
      single_row <- data.frame(
        x = 5,
        y = "test",
        stringsAsFactors = FALSE
      )
      
      expect_equal(nrow(single_row), 1)
      
      chars <- names(single_row)[vapply(single_row, is.character, logical(1))]
      multi <- chars[vapply(single_row[chars], function(x) {
        u <- unique(x)
        length(u) > 1 && length(u) < nrow(single_row)
      }, logical(1))]
      
      expect_equal(length(multi), 0)
    })
    
    it("handles datasets with duplicate column names", {
      dup_data <- data.frame(
        x = 1:3,
        x = 4:6,
        x = 7:9,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      unique_names <- make.names(names(dup_data), unique = TRUE)
      
      expect_equal(length(unique_names), 3)
      expect_equal(length(unique(unique_names)), 3)
      expect_true(all(grepl("^x", unique_names)))
    })
  })
  
  describe("Correlation Matrix Computation", {
    
    it("computes correlation matrix for numeric data", {
      test_data <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = c(2, 4, 6, 8, 10),
        z = c(5, 4, 3, 2, 1),
        stringsAsFactors = FALSE
      )
      
      num_cols <- names(test_data)[sapply(test_data, is.numeric)]
      expect_equal(length(num_cols), 3)
      
      if (length(num_cols) >= 2) {
        cm <- cor(test_data[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
        
        expect_equal(dim(cm), c(3, 3))
        expect_true(all(diag(cm) == 1))
        expect_true(all(cm >= -1 & cm <= 1))
        expect_true(isSymmetric(cm))
      }
    })
    
    it("handles correlation with missing values", {
      test_data <- data.frame(
        x = c(1, 2, NA, 4, 5),
        y = c(2, NA, 6, 8, 10),
        z = c(NA, 4, 3, 2, 1),
        stringsAsFactors = FALSE
      )
      
      cm <- cor(test_data, use = "pairwise.complete.obs")
      
      expect_false(any(is.na(cm)))
      expect_true(all(diag(cm) == 1))
    })
    
    it("handles case with insufficient numeric columns", {
      test_data <- data.frame(
        x = 1:5,
        y = letters[1:5],
        z = c("A", "B", "C", "D", "E"),
        stringsAsFactors = FALSE
      )
      
      num_cols <- names(test_data)[sapply(test_data, is.numeric)]
      
      expect_equal(length(num_cols), 1)
      expect_true(length(num_cols) < 2)
    })
  })
})
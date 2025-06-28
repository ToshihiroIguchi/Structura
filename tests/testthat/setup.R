# Test setup configuration
library(testthat)
library(shiny)

# Global test options
options(
  shiny.testmode = TRUE,
  shiny.fullstacktrace = TRUE,
  warn = 1
)

# Create test data directory if it doesn't exist
test_data_dir <- file.path("tests", "testdata")
if (!dir.exists(test_data_dir)) {
  dir.create(test_data_dir, recursive = TRUE, showWarnings = FALSE)
}

# Helper function to create standardized test datasets
create_test_dataset <- function(name, n_rows = 100, seed = 123) {
  set.seed(seed)
  
  datasets <- list(
    "basic_numeric" = data.frame(
      x1 = rnorm(n_rows, 5, 1),
      x2 = rnorm(n_rows, 3, 0.8),
      x3 = rnorm(n_rows, 4, 1.2),
      y1 = rnorm(n_rows, 6, 1.1),
      y2 = rnorm(n_rows, 5, 0.9),
      stringsAsFactors = FALSE
    ),
    
    "mixed_types" = data.frame(
      numeric_var = rnorm(n_rows),
      integer_var = sample(1:10, n_rows, replace = TRUE),
      character_var = sample(LETTERS[1:5], n_rows, replace = TRUE),
      logical_var = sample(c(TRUE, FALSE), n_rows, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    
    "with_missing" = data.frame(
      x = c(rnorm(n_rows - 10), rep(NA, 10)),
      y = c(rep(NA, 5), rnorm(n_rows - 5)),
      z = sample(c("A", "B", "C", NA), n_rows, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    
    "sem_suitable" = data.frame(
      x1 = rnorm(n_rows, 0, 1),
      x2 = rnorm(n_rows, 0, 1),
      x3 = rnorm(n_rows, 0, 1),
      y1 = rnorm(n_rows, 0, 1),
      y2 = rnorm(n_rows, 0, 1),
      y3 = rnorm(n_rows, 0, 1),
      stringsAsFactors = FALSE
    )
  )
  
  if (name %in% names(datasets)) {
    return(datasets[[name]])
  } else {
    return(datasets[["basic_numeric"]])
  }
}

# Helper function to create mock lavaan fit objects for testing
create_mock_lavaan_fit <- function() {
  data(HolzingerSwineford1939, package = "lavaan")
  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  return(fit)
}

# Helper function to clean up test files
cleanup_test_files <- function() {
  test_files <- list.files(test_data_dir, pattern = "test_.*\\.csv$", full.names = TRUE)
  unlink(test_files)
}

# Register cleanup function to run after tests
withr::defer(cleanup_test_files(), testthat::teardown_env())

# Suppress specific warnings during testing
suppressWarnings({
  # Load required libraries silently
  library(lavaan, warn.conflicts = FALSE, quietly = TRUE)
  library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
  if (requireNamespace("shinytest2", quietly = TRUE)) {
    library(shinytest2, warn.conflicts = FALSE, quietly = TRUE)
  }
})
#' Unit Tests for Data Preprocessing Functions
#'
#' This script contains unit tests for the data preprocessing functions
#' in the TruthScan project.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Load required libraries
library(testthat)
library(here)

# Source the data preprocessing script
source(here::here("R", "data_preprocessing.R"))

# Test normalize_search_results function
test_that("normalize_search_results works correctly with min-max method", {
  # Test with normal data
  test_data <- c(100, 200, 300, 400, 500)
  result <- normalize_search_results(test_data, method = "min-max")
  
  # Check that result is between 0 and 1
  expect_true(all(result >= 0 & result <= 1))
  
  # Check that min value is 0 and max value is 1
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  
  # Check specific values
  expect_equal(result[1], 0)
  expect_equal(result[5], 1)
  expect_equal(result[3], 0.5)
})

test_that("normalize_search_results works correctly with z-score method", {
  # Test with normal data
  test_data <- c(100, 200, 300, 400, 500)
  result <- normalize_search_results(test_data, method = "z-score")
  
  # Check that result is between 0 and 1
  expect_true(all(result >= 0 & result <= 1))
  
  # Check that min value is 0 and max value is 1
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
})

test_that("normalize_search_results handles edge cases", {
  # Test with all same values
  test_data <- c(100, 100, 100)
  result <- normalize_search_results(test_data, method = "min-max")
  expect_equal(result, c(0.5, 0.5, 0.5))
  
  # Test with NA values
  test_data <- c(100, NA, 300)
  result <- normalize_search_results(test_data, method = "min-max")
  expect_equal(result, c(0, 0, 1))
  
  # Test with empty vector
  test_data <- numeric(0)
  expect_error(normalize_search_results(test_data, method = "min-max"))
  
  # Test with invalid method
  test_data <- c(100, 200, 300)
  expect_error(normalize_search_results(test_data, method = "invalid"))
})

# Test preprocess_statement function (if it exists)
if (exists("preprocess_statement")) {
  test_that("preprocess_statement correctly processes a statement", {
    # Test with a simple statement
    test_statement <- "This is a test statement."
    result <- preprocess_statement(test_statement)
    
    # Check that result is a character vector
    expect_is(result, "character")
    
    # Check that stopwords are removed
    expect_false("is" %in% result)
    expect_false("a" %in% result)
    
    # Check that stemming is applied
    expect_true("test" %in% result)
    expect_true("statement" %in% result)
  })
}

# Run the tests
test_results <- test_file(here::here("tests", "test_preprocessing.R"))
print(test_results)

#' Run Tests for TruthScan
#'
#' This script runs all the tests for the TruthScan project.
#' It uses the testthat package to run the tests.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Check if required packages are installed
required_packages <- c("testthat", "here")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("The following required packages are missing: ", 
       paste(missing_packages, collapse = ", "), 
       ". Please run install_dependencies.R first.")
}

# Load required packages
library(testthat)
library(here)

# Set up logging
if (requireNamespace("logger", quietly = TRUE)) {
  library(logger)
  library(glue)
  log_threshold(INFO)
  log_formatter(logger::formatter_glue)
  log_info("Starting tests")
}

# Run all tests in the tests directory
message("Running tests...")
test_results <- test_dir(here("tests"), reporter = "summary")

# Print test results
message("Test results:")
print(test_results)

# Check if all tests passed
if (all(test_results$failed == 0)) {
  message("All tests passed!")
} else {
  warning("Some tests failed. Please check the test results.")
}

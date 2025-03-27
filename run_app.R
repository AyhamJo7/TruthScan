#' Run TruthScan Shiny Application
#'
#' This script runs the TruthScan Shiny application.
#' It checks for required packages and launches the app.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Check if required packages are installed
required_packages <- c("shiny", "here")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("The following required packages are missing: ", 
       paste(missing_packages, collapse = ", "), 
       ". Please run install_dependencies.R first.")
}

# Load required packages
library(shiny)
library(here)

# Set up logging
if (requireNamespace("logger", quietly = TRUE)) {
  library(logger)
  log_threshold(INFO)
  log_formatter(logger::formatter_glue)
  log_info("Starting TruthScan application")
}

# Check if models exist
models_exist <- FALSE
if (file.exists(here("models", "binary_rf_model.rds")) ||
    file.exists(here("models", "binary_xgb_model.rds")) ||
    file.exists(here("models", "binary_nn_model.rds"))) {
  models_exist <- TRUE
}

if (!models_exist) {
  message("No pre-trained models found. The app will run in demo mode with simulated predictions.")
  message("To train models, run the model training scripts in the R directory.")
}

# Run the Shiny app
message("Starting TruthScan application...")
shiny::runApp(here("app"), launch.browser = TRUE)

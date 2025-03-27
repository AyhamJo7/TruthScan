#' Train All Models for TruthScan
#'
#' This script trains all the machine learning models for the TruthScan project.
#' It runs the data preprocessing and model training scripts in sequence.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Check if required packages are installed
required_packages <- c("here", "glue")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("The following required packages are missing: ", 
       paste(missing_packages, collapse = ", "), 
       ". Please run install_dependencies.R first.")
}

# Load required packages
library(here)

# Set up logging
if (requireNamespace("logger", quietly = TRUE)) {
  library(logger)
  library(glue)
  log_threshold(INFO)
  log_formatter(logger::formatter_glue)
  log_info("Starting model training")
}

# Create models directory if it doesn't exist
if (!dir.exists(here("models"))) {
  dir.create(here("models"), recursive = TRUE)
  message("Created models directory")
}

# Run data preprocessing
message("Running data preprocessing...")
tryCatch({
  source(here("R", "data_preprocessing.R"))
  message("Data preprocessing completed successfully")
}, error = function(e) {
  stop("Error in data preprocessing: ", e$message)
})

# Train Random Forest model
message("Training Random Forest model...")
tryCatch({
  source(here("R", "binary_rf_model.R"))
  message("Random Forest model training completed successfully")
}, error = function(e) {
  warning("Error in Random Forest model training: ", e$message)
})

# Train XGBoost model
message("Training XGBoost model...")
tryCatch({
  source(here("R", "binary_xgb_model.R"))
  message("XGBoost model training completed successfully")
}, error = function(e) {
  warning("Error in XGBoost model training: ", e$message)
})

# Train Neural Network model
message("Training Neural Network model...")
tryCatch({
  source(here("R", "binary_nn_model.R"))
  message("Neural Network model training completed successfully")
}, error = function(e) {
  warning("Error in Neural Network model training: ", e$message)
})

message("Model training completed!")
message("You can now run the Shiny application using run_app.R")

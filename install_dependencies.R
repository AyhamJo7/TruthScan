#' Install Dependencies for TruthScan
#'
#' This script installs all the required packages for the TruthScan project.
#' Run this script before using TruthScan to ensure all dependencies are installed.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    message("Installing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, repos = "https://cloud.r-project.org")
  } else {
    message("All required packages are already installed.")
  }
}

# List of required packages
required_packages <- c(
  # Data manipulation
  "tidyverse",
  "here",
  "glue",
  "janitor",
  "lubridate",
  
  # Text processing
  "tidytext",
  "textrecipes",
  "stopwords",
  "tokenizers",
  "text2vec",
  "irlba",
  
  # Machine learning
  "tidymodels",
  "ranger",
  "xgboost",
  "vip",
  
  # Neural networks
  "keras",
  "tensorflow",
  "tfruns",
  
  # Web scraping
  "rvest",
  "httr",
  "polite",
  
  # Parallel processing
  "furrr",
  "doParallel",
  
  # Logging
  "logger",
  
  # Shiny app
  "shiny",
  "shinydashboard",
  "shinyjs",
  "DT",
  "plotly",
  "waiter",
  "shinythemes",
  "shinyWidgets",
  "shinyBS",
  
  # Testing
  "testthat"
)

# Install required packages
message("Checking and installing required packages...")
install_if_missing(required_packages)

# Install Keras and TensorFlow if not already installed
if (!requireNamespace("keras", quietly = TRUE) || 
    !keras::is_keras_available(tensorflow = TRUE)) {
  message("Installing Keras and TensorFlow...")
  install.packages("keras")
  keras::install_keras()
} else {
  message("Keras and TensorFlow are already installed.")
}

message("All dependencies have been installed successfully!")
message("You can now use TruthScan.")

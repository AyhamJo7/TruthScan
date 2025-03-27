#' Collect Search Features for TruthScan
#'
#' This script collects search features for statements in the dataset.
#' It uses web scraping to get search result counts and other information.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Check if required packages are installed
required_packages <- c("here", "tidyverse", "rvest", "polite", "glue", "logger", "furrr")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("The following required packages are missing: ", 
       paste(missing_packages, collapse = ", "), 
       ". Please run install_dependencies.R first.")
}

# Load required packages
library(here)
library(tidyverse)

# Source the web scraping script
source(here("R", "web_scraping.R"))

# Set up logging
if (requireNamespace("logger", quietly = TRUE)) {
  library(logger)
  library(glue)
  log_threshold(INFO)
  log_formatter(logger::formatter_glue)
  log_info("Starting search feature collection")
}

# Check if data directory exists
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
  message("Created data directory")
}

# Function to extract statements from a dataset
extract_statements <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  data <- read_csv(file_path, col_types = cols())
  
  if (!"Statement" %in% names(data)) {
    stop("The file does not contain a 'Statement' column")
  }
  
  return(data)
}

# Main function to collect search features
collect_features <- function(input_file = here("data", "statements.csv"),
                            output_file = here("data", "statements_with_search.csv"),
                            search_engine = "google",
                            workers = 2,
                            sample_size = NULL) {
  
  message("Collecting search features...")
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    # Create a sample statements file if it doesn't exist
    message("Input file not found. Creating a sample statements file...")
    
    sample_statements <- tibble(
      Statement = c(
        "The earth is flat.",
        "Water boils at 100 degrees Celsius at sea level.",
        "The moon is made of cheese.",
        "Humans have walked on the moon.",
        "Vaccines cause autism.",
        "Climate change is a hoax.",
        "The sun revolves around the earth.",
        "Smoking causes cancer.",
        "Evolution is just a theory.",
        "The earth is billions of years old."
      ),
      Label = c(0, 1, 0, 1, 0, 0, 0, 1, 0, 1)
    )
    
    write_csv(sample_statements, input_file)
    message("Created sample statements file: ", input_file)
  }
  
  # Extract statements from the dataset
  data <- extract_statements(input_file)
  message("Extracted ", nrow(data), " statements from ", input_file)
  
  # Take a sample if requested
  if (!is.null(sample_size) && sample_size < nrow(data)) {
    set.seed(123)
    data <- data %>% sample_n(sample_size)
    message("Sampled ", sample_size, " statements for processing")
  }
  
  # Process statements and collect search features
  result <- process_statements(
    data_path = input_file,
    output_path = output_file,
    search_engine = search_engine,
    workers = workers
  )
  
  message("Search feature collection completed!")
  message("Results saved to: ", output_file)
  
  return(result)
}

# Run the feature collection if this script is run directly
if (sys.nframe() == 0) {
  # Check if statements.csv exists, otherwise use a default dataset
  input_file <- here("data", "statements.csv")
  
  if (!file.exists(input_file)) {
    # Try to find any CSV file in the data directory that might contain statements
    data_files <- list.files(here("data"), pattern = "\\.csv$", full.names = TRUE)
    
    if (length(data_files) > 0) {
      # Use the first CSV file found
      input_file <- data_files[1]
      message("Using existing data file: ", input_file)
    }
  }
  
  # Collect search features
  collect_features(
    input_file = input_file,
    output_file = here("data", "statements_with_search.csv"),
    search_engine = "google",
    workers = 2,
    sample_size = 10  # Limit to 10 statements for demonstration
  )
}

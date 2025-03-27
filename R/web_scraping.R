#' Web Scraping for TruthScan
#'
#' This script implements web scraping functionality to collect additional features
#' for the TruthScan fake news detection system.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Load required libraries
library(tidyverse)      # For data manipulation
library(rvest)          # For web scraping
library(httr)           # For HTTP requests
library(polite)         # For polite web scraping
library(glue)           # For string interpolation
library(here)           # For project-relative paths
library(logger)         # For logging
library(furrr)          # For parallel processing
library(progressr)      # For progress reporting

# Configure logging
log_threshold(INFO)
log_formatter(formatter_glue)

#' Scrape search results for a statement
#'
#' @param statement The statement to search for
#' @param search_engine The search engine to use (google, bing, or duckduckgo)
#' @param user_agent User agent string to use for requests
#' @param delay Delay between requests in seconds
#' @return A list containing search results information
scrape_search_results <- function(statement,
                                 search_engine = "google",
                                 user_agent = "TruthScan Research Bot (https://truthscan.org)",
                                 delay = 2) {
  
  log_info("Scraping search results for statement: {str_trunc(statement, 50)}")
  
  # Encode the statement for URL
  encoded_statement <- URLencode(statement)
  
  # Determine the search URL based on the search engine
  if (search_engine == "google") {
    search_url <- glue("https://www.google.com/search?q={encoded_statement}")
    result_count_selector <- "#result-stats"
    source_selector <- ".iUh30"
  } else if (search_engine == "bing") {
    search_url <- glue("https://www.bing.com/search?q={encoded_statement}")
    result_count_selector <- ".sb_count"
    source_selector <- ".b_attribution"
  } else if (search_engine == "duckduckgo") {
    search_url <- glue("https://html.duckduckgo.com/html/?q={encoded_statement}")
    result_count_selector <- ".results_links_count"
    source_selector <- ".result__url"
  } else {
    stop("Unsupported search engine. Use 'google', 'bing', or 'duckduckgo'.")
  }
  
  # Create a polite session
  session <- bow(
    url = search_url,
    user_agent = user_agent,
    delay = delay
  )
  
  # Scrape the search results page
  tryCatch({
    # Make the request
    response <- scrape(session)
    
    # Extract result count
    result_count_text <- response %>%
      html_nodes(result_count_selector) %>%
      html_text() %>%
      first()
    
    # Extract the numeric count from the text
    if (!is.na(result_count_text)) {
      # Different parsing for different search engines
      if (search_engine == "google") {
        result_count <- result_count_text %>%
          str_extract("About ([\\d,]+) results") %>%
          str_extract("([\\d,]+)") %>%
          str_remove_all(",") %>%
          as.numeric()
      } else if (search_engine == "bing") {
        result_count <- result_count_text %>%
          str_extract("([\\d,]+) results") %>%
          str_extract("([\\d,]+)") %>%
          str_remove_all(",") %>%
          as.numeric()
      } else if (search_engine == "duckduckgo") {
        # DuckDuckGo doesn't show total result count, so we count the results on the page
        result_count <- response %>%
          html_nodes(".result__a") %>%
          length()
      }
    } else {
      result_count <- 0
    }
    
    # Extract source domains
    source_domains <- response %>%
      html_nodes(source_selector) %>%
      html_text() %>%
      head(10)
    
    # Extract titles
    titles <- response %>%
      html_nodes("h3") %>%
      html_text() %>%
      head(10)
    
    # Return the results
    return(list(
      statement = statement,
      search_engine = search_engine,
      result_count = result_count,
      source_domains = source_domains,
      titles = titles,
      timestamp = Sys.time()
    ))
    
  }, error = function(e) {
    log_error("Error scraping search results: {e$message}")
    return(list(
      statement = statement,
      search_engine = search_engine,
      result_count = NA,
      source_domains = character(0),
      titles = character(0),
      error = e$message,
      timestamp = Sys.time()
    ))
  })
}

#' Scrape search results for multiple statements in parallel
#'
#' @param statements Vector of statements to search for
#' @param search_engine The search engine to use
#' @param user_agent User agent string to use for requests
#' @param delay Delay between requests in seconds
#' @param workers Number of parallel workers
#' @return A tibble containing search results information
scrape_search_results_batch <- function(statements,
                                       search_engine = "google",
                                       user_agent = "TruthScan Research Bot (https://truthscan.org)",
                                       delay = 2,
                                       workers = 2) {
  
  log_info("Scraping search results for {length(statements)} statements using {workers} workers")
  
  # Set up parallel processing
  plan(multisession, workers = workers)
  
  # Set up progress reporting
  handlers(global = TRUE)
  handlers("progress")
  
  # Create a progress bar
  p <- progressor(steps = length(statements))
  
  # Scrape search results in parallel
  results <- future_map(statements, function(statement) {
    # Scrape search results
    result <- scrape_search_results(
      statement = statement,
      search_engine = search_engine,
      user_agent = user_agent,
      delay = delay
    )
    
    # Update progress bar
    p()
    
    return(result)
  }, .options = furrr_options(seed = TRUE))
  
  # Convert results to a tibble
  results_tibble <- tibble(
    statement = map_chr(results, "statement"),
    search_engine = map_chr(results, "search_engine"),
    result_count = map_dbl(results, ~ ifelse(is.null(.x$result_count), NA_real_, .x$result_count)),
    source_domains = map(results, "source_domains"),
    titles = map(results, "titles"),
    error = map_chr(results, ~ ifelse(is.null(.x$error), NA_character_, .x$error)),
    timestamp = map(results, "timestamp")
  )
  
  # Clean up parallel processing
  plan(sequential)
  
  return(results_tibble)
}

#' Normalize search result counts
#'
#' @param result_counts Vector of search result counts
#' @param method Normalization method (min-max or z-score)
#' @return Normalized search result counts
normalize_search_results <- function(result_counts, method = "min-max") {
  
  # Handle missing values
  result_counts[is.na(result_counts)] <- 0
  
  # Apply normalization method
  if (method == "min-max") {
    # Min-max normalization
    min_val <- min(result_counts)
    max_val <- max(result_counts)
    
    if (min_val == max_val) {
      return(rep(0.5, length(result_counts)))
    }
    
    normalized <- (result_counts - min_val) / (max_val - min_val)
  } else if (method == "z-score") {
    # Z-score normalization
    mean_val <- mean(result_counts)
    sd_val <- sd(result_counts)
    
    if (sd_val == 0) {
      return(rep(0, length(result_counts)))
    }
    
    normalized <- (result_counts - mean_val) / sd_val
    
    # Scale to [0, 1] range
    min_norm <- min(normalized)
    max_norm <- max(normalized)
    
    if (min_norm == max_norm) {
      return(rep(0.5, length(normalized)))
    }
    
    normalized <- (normalized - min_norm) / (max_norm - min_norm)
  } else {
    stop("Unsupported normalization method. Use 'min-max' or 'z-score'.")
  }
  
  return(normalized)
}

#' Process statements and add search features
#'
#' @param data_path Path to the data file containing statements
#' @param output_path Path to save the processed data
#' @param search_engine The search engine to use
#' @param workers Number of parallel workers
#' @return A tibble with added search features
process_statements <- function(data_path = here("data", "statements.csv"),
                              output_path = here("data", "statements_with_search.csv"),
                              search_engine = "google",
                              workers = 2) {
  
  log_info("Processing statements from {data_path}")
  
  # Load the data
  data <- read_csv(data_path, col_types = cols())
  
  # Extract statements
  statements <- data$Statement
  
  # Scrape search results
  search_results <- scrape_search_results_batch(
    statements = statements,
    search_engine = search_engine,
    workers = workers
  )
  
  # Normalize search result counts
  search_results <- search_results %>%
    mutate(
      result_count_normalized = normalize_search_results(result_count, method = "min-max")
    )
  
  # Join search results with original data
  data_with_search <- data %>%
    bind_cols(
      search_results %>% select(result_count, result_count_normalized)
    )
  
  # Save the processed data
  write_csv(data_with_search, output_path)
  
  log_info("Processed data saved to {output_path}")
  
  return(data_with_search)
}

# If this script is run directly, execute the processing
if (sys.nframe() == 0) {
  process_statements()
}

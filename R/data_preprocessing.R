#' Data Preprocessing for TruthScan
#'
#' This script handles data preprocessing for the TruthScan fake news detection system.
#' It includes data loading, cleaning, feature engineering, and text processing.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Load required libraries
library(tidyverse)    # For data manipulation
library(tidytext)     # For text processing
library(textrecipes)  # For text feature engineering
library(glue)         # For string interpolation
library(here)         # For project-relative paths
library(janitor)      # For data cleaning
library(lubridate)    # For date handling
library(vip)          # For variable importance
library(stopwords)    # For stopwords
library(tokenizers)   # For tokenization
library(text2vec)     # For text vectorization
library(irlba)        # For SVD
library(logger)       # For logging

# Configure logging
log_threshold(INFO)
log_formatter(formatter_glue)

#' Load and preprocess the raw data
#'
#' @param train_path Path to training data
#' @param test_path Path to test data
#' @param valid_path Path to validation data
#' @return A list containing preprocessed training, testing, and validation data
preprocess_data <- function(train_path = here("data", "train.tsv"),
                           test_path = here("data", "test.tsv"),
                           valid_path = here("data", "valid.tsv")) {
  
  log_info("Loading data from {train_path}, {test_path}, and {valid_path}")
  
  # Load data
  train_news <- read_tsv(train_path, na = "")
  test_news <- read_tsv(test_path, na = "")
  valid_news <- read_tsv(valid_path, na = "")
  
  # Combine for preprocessing
  log_info("Combining datasets for preprocessing")
  merged_news <- bind_rows(train_news, test_news, valid_news)
  
  # Assign column names
  colnames(merged_news) <- c("ID", "Label", "Statement", "Subject", "Speaker", 
                            "Job_Title", "State", "Party", "Barely_True", 
                            "False_Counts", "Half_True", "Mostly_True", 
                            "Pants_Fire", "Context")
  
  # Clean ID attribute
  merged_news <- merged_news %>%
    mutate(ID = str_remove(ID, "\\.json$"),
           ID = as.integer(ID),
           Label = as.factor(Label),
           Subject_list = map(Subject, ~str_split(.x, ",")[[1]]),
           Speaker = as.factor(Speaker),
           Job_Title = as.factor(Job_Title),
           State = as.factor(State),
           Party = as.factor(Party),
           TextLength = nchar(Statement))
  
  log_info("Data loaded and basic preprocessing completed")
  
  # Handle missing values
  log_info("Handling missing values")
  merged_news <- merged_news %>%
    mutate(State = if_else(is.na(State), "Unknown", as.character(State)),
           State = as.factor(State))
  
  # Create different label versions
  log_info("Creating different label versions")
  
  # Original 6-class labels
  merged_news <- merged_news %>%
    mutate(Label_6class = Label)
  
  # 3-class labels
  merged_news <- merged_news %>%
    mutate(Label_3class = fct_collapse(Label,
                                      "false" = c("pants-fire", "false"),
                                      "half-true" = c("barely-true", "half-true"),
                                      "true" = c("mostly-true", "true")))
  
  # Binary labels
  merged_news <- merged_news %>%
    mutate(Label_binary = fct_collapse(Label,
                                      "false" = c("pants-fire", "false", "barely-true"),
                                      "true" = c("half-true", "mostly-true", "true")),
           Label_binary = factor(Label_binary, levels = c("false", "true"), 
                                labels = c(0, 1)))
  
  # Process Subject field
  log_info("Processing Subject field")
  subject_dtm <- merged_news %>%
    unnest(Subject_list) %>%
    count(ID, word = Subject_list) %>%
    cast_dtm(ID, word, n)
  
  # Get top subjects
  top_subjects <- subject_dtm %>%
    tidy() %>%
    count(term) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    pull(term)
  
  # Create dummy variables for top subjects
  subject_dummies <- subject_dtm %>%
    tidy() %>%
    filter(term %in% top_subjects) %>%
    mutate(value = 1) %>%
    pivot_wider(id_cols = document, 
                names_from = term, 
                values_from = value, 
                values_fill = list(value = 0)) %>%
    rename(ID = document)
  
  # Convert ID to integer for joining
  subject_dummies <- subject_dummies %>%
    mutate(ID = as.integer(ID))
  
  # Join subject dummies back to main data
  merged_news <- merged_news %>%
    left_join(subject_dummies, by = "ID")
  
  # Process Speaker field
  log_info("Processing Speaker field")
  top_speakers <- merged_news %>%
    count(Speaker) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    pull(Speaker)
  
  merged_news <- merged_news %>%
    mutate(Speaker = fct_other(Speaker, keep = top_speakers, other_level = "others"))
  
  # Create dummy variables for Speaker
  speaker_dummies <- merged_news %>%
    select(ID, Speaker) %>%
    mutate(value = 1) %>%
    pivot_wider(id_cols = ID, 
                names_from = Speaker, 
                values_from = value, 
                values_fill = list(value = 0))
  
  # Join speaker dummies back to main data
  merged_news <- merged_news %>%
    select(-Speaker) %>%
    left_join(speaker_dummies, by = "ID")
  
  # Process Party field
  log_info("Processing Party field")
  merged_news <- merged_news %>%
    mutate(Party = fct_collapse(Party,
                               "republican" = "republican",
                               "democrat" = "democrat",
                               "none" = c(levels(Party)[!levels(Party) %in% c("republican", "democrat")])))
  
  # Create dummy variables for Party
  party_dummies <- merged_news %>%
    select(ID, Party) %>%
    mutate(value = 1) %>%
    pivot_wider(id_cols = ID, 
                names_from = Party, 
                values_from = value, 
                values_fill = list(value = 0))
  
  # Join party dummies back to main data
  merged_news <- merged_news %>%
    select(-Party) %>%
    left_join(party_dummies, by = "ID")
  
  # Process State field
  log_info("Processing State field")
  top_states <- merged_news %>%
    count(State) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    pull(State)
  
  merged_news <- merged_news %>%
    mutate(State = fct_other(State, keep = top_states, other_level = "Rest"))
  
  # Create dummy variables for State
  state_dummies <- merged_news %>%
    select(ID, State) %>%
    mutate(value = 1) %>%
    pivot_wider(id_cols = ID, 
                names_from = State, 
                values_from = value, 
                values_fill = list(value = 0))
  
  # Join state dummies back to main data
  merged_news <- merged_news %>%
    select(-State) %>%
    left_join(state_dummies, by = "ID")
  
  # Process Statement text
  log_info("Processing Statement text")
  
  # Tokenize and clean text
  statement_tokens <- merged_news %>%
    select(ID, Statement) %>%
    unnest_tokens(word, Statement) %>%
    anti_join(get_stopwords(), by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$")) %>%
    mutate(word = wordStem(word))
  
  # Create document-term matrix
  statement_dtm <- statement_tokens %>%
    count(ID, word) %>%
    cast_dtm(ID, word, n)
  
  # Calculate TF-IDF
  tfidf <- TfIdf$new()
  statement_tfidf <- tfidf$fit_transform(statement_dtm)
  statement_tfidf <- as.matrix(statement_tfidf)
  
  # Perform SVD for dimensionality reduction
  log_info("Performing SVD for Statement text")
  set.seed(123)
  statement_svd <- irlba(statement_tfidf, nv = 100)
  
  # Create data frame with SVD features
  statement_features <- as.data.frame(statement_svd$v)
  colnames(statement_features) <- paste0("statement_svd_", 1:ncol(statement_features))
  statement_features$ID <- as.integer(rownames(statement_features))
  
  # Process Context text
  log_info("Processing Context text")
  
  # Tokenize and clean text
  context_tokens <- merged_news %>%
    select(ID, Context) %>%
    unnest_tokens(word, Context) %>%
    anti_join(get_stopwords(), by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$")) %>%
    mutate(word = wordStem(word))
  
  # Create document-term matrix
  context_dtm <- context_tokens %>%
    count(ID, word) %>%
    cast_dtm(ID, word, n)
  
  # Calculate TF-IDF
  context_tfidf <- tfidf$fit_transform(context_dtm)
  context_tfidf <- as.matrix(context_tfidf)
  
  # Perform SVD for dimensionality reduction
  log_info("Performing SVD for Context text")
  set.seed(123)
  context_svd <- irlba(context_tfidf, nv = 100)
  
  # Create data frame with SVD features
  context_features <- as.data.frame(context_svd$v)
  colnames(context_features) <- paste0("context_svd_", 1:ncol(context_features))
  context_features$ID <- as.integer(rownames(context_features))
  
  # Join all features together
  log_info("Joining all features")
  merged_news <- merged_news %>%
    left_join(statement_features, by = "ID") %>%
    left_join(context_features, by = "ID")
  
  # Split back into train, test, and validation sets
  log_info("Splitting back into train, test, and validation sets")
  train_size <- nrow(train_news)
  test_size <- nrow(test_news)
  
  train_data <- merged_news[1:train_size, ]
  test_data <- merged_news[(train_size + 1):(train_size + test_size), ]
  valid_data <- merged_news[(train_size + test_size + 1):nrow(merged_news), ]
  
  # Save preprocessed data
  log_info("Saving preprocessed data")
  
  # Binary classification
  train_binary <- train_data %>% select(-Label, -Label_3class, -Label_6class) %>% rename(Label = Label_binary)
  test_binary <- test_data %>% select(-Label, -Label_3class, -Label_6class) %>% rename(Label = Label_binary)
  valid_binary <- valid_data %>% select(-Label, -Label_3class, -Label_6class) %>% rename(Label = Label_binary)
  
  write_csv(train_binary, here("data", "binary_training.csv"))
  write_csv(test_binary, here("data", "binary_testing.csv"))
  write_csv(valid_binary, here("data", "binary_validating.csv"))
  
  # Three-class classification
  train_three <- train_data %>% select(-Label, -Label_binary, -Label_6class) %>% rename(Label = Label_3class)
  test_three <- test_data %>% select(-Label, -Label_binary, -Label_6class) %>% rename(Label = Label_3class)
  valid_three <- valid_data %>% select(-Label, -Label_binary, -Label_6class) %>% rename(Label = Label_3class)
  
  write_csv(train_three, here("data", "three_training.csv"))
  write_csv(test_three, here("data", "three_testing.csv"))
  write_csv(valid_three, here("data", "three_validating.csv"))
  
  # Multi-class classification
  train_multi <- train_data %>% select(-Label, -Label_binary, -Label_3class) %>% rename(Label = Label_6class)
  test_multi <- test_data %>% select(-Label, -Label_binary, -Label_3class) %>% rename(Label = Label_6class)
  valid_multi <- valid_data %>% select(-Label, -Label_binary, -Label_3class) %>% rename(Label = Label_6class)
  
  write_csv(train_multi, here("data", "multi_training.csv"))
  write_csv(test_multi, here("data", "multi_testing.csv"))
  write_csv(valid_multi, here("data", "multi_validating.csv"))
  
  log_info("Data preprocessing completed successfully")
  
  # Return the preprocessed data
  return(list(
    train_binary = train_binary,
    test_binary = test_binary,
    valid_binary = valid_binary,
    train_three = train_three,
    test_three = test_three,
    valid_three = valid_three,
    train_multi = train_multi,
    test_multi = test_multi,
    valid_multi = valid_multi
  ))
}

# If this script is run directly, execute the preprocessing
if (sys.nframe() == 0) {
  preprocess_data()
}

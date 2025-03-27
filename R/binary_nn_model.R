#' Binary Neural Network Model for TruthScan
#'
#' This script implements a neural network model for binary classification
#' of fake news using Keras and TensorFlow through the tidymodels framework.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Load required libraries
library(tidyverse)      # For data manipulation
library(tidymodels)     # For modeling framework
library(keras)          # For neural network implementation
library(tensorflow)     # For TensorFlow backend
library(here)           # For project-relative paths
library(glue)           # For string interpolation
library(logger)         # For logging
library(tfruns)         # For tracking TensorFlow runs

# Configure logging
log_threshold(INFO)
log_formatter(formatter_glue)

#' Train and evaluate a neural network model for binary classification
#'
#' @param train_path Path to training data
#' @param test_path Path to test data
#' @param valid_path Path to validation data
#' @param include_statement Whether to include statement SVD features
#' @param include_context Whether to include context SVD features
#' @param include_search Whether to include search features
#' @param epochs Number of epochs to train for
#' @param batch_size Batch size for training
#' @param dropout_rate Dropout rate for regularization
#' @param hidden_units Vector of hidden units for each layer
#' @return A list containing the trained model, predictions, and evaluation metrics
train_binary_nn <- function(train_path = here("data", "binary_training.csv"),
                           test_path = here("data", "binary_testing.csv"),
                           valid_path = here("data", "binary_validating.csv"),
                           include_statement = TRUE,
                           include_context = TRUE,
                           include_search = TRUE,
                           epochs = 100,
                           batch_size = 32,
                           dropout_rate = 0.3,
                           hidden_units = c(64, 32, 16)) {
  
  log_info("Loading data from {train_path}, {test_path}, and {valid_path}")
  
  # Load data
  train_data <- read_csv(train_path, col_types = cols())
  test_data <- read_csv(test_path, col_types = cols())
  valid_data <- read_csv(valid_path, col_types = cols())
  
  # Convert Label to numeric for neural network
  train_data <- train_data %>% mutate(Label = as.numeric(as.character(Label)))
  test_data <- test_data %>% mutate(Label = as.numeric(as.character(Label)))
  valid_data <- valid_data %>% mutate(Label = as.numeric(as.character(Label)))
  
  # Feature selection based on parameters
  log_info("Selecting features")
  
  # Identify feature columns
  statement_cols <- names(train_data)[str_detect(names(train_data), "^statement_svd_")]
  context_cols <- names(train_data)[str_detect(names(train_data), "^context_svd_")]
  search_col <- "Search"
  
  # Select features based on parameters
  selected_features <- c()
  if (include_statement) selected_features <- c(selected_features, statement_cols)
  if (include_context) selected_features <- c(selected_features, context_cols)
  if (include_search && search_col %in% names(train_data)) selected_features <- c(selected_features, search_col)
  
  # Add other features that are not text-based
  other_features <- names(train_data)[!names(train_data) %in% 
                                     c("ID", "Label", statement_cols, context_cols, search_col)]
  selected_features <- c(selected_features, other_features)
  
  log_info("Selected {length(selected_features)} features for modeling")
  
  # Prepare data for neural network
  log_info("Preparing data for neural network")
  
  # Create recipe for preprocessing
  nn_recipe <- recipe(Label ~ ., data = train_data %>% select(Label, all_of(selected_features))) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  # Prepare the training data
  nn_prep <- prep(nn_recipe, training = train_data)
  train_processed <- bake(nn_prep, new_data = train_data)
  test_processed <- bake(nn_prep, new_data = test_data)
  valid_processed <- bake(nn_prep, new_data = valid_data)
  
  # Extract features and labels
  x_train <- as.matrix(train_processed %>% select(-Label))
  y_train <- train_processed$Label
  
  x_test <- as.matrix(test_processed %>% select(-Label))
  y_test <- test_processed$Label
  
  x_valid <- as.matrix(valid_processed %>% select(-Label))
  y_valid <- valid_processed$Label
  
  # Get input dimensions
  input_dim <- ncol(x_train)
  
  # Define the model
  log_info("Defining neural network model")
  
  # Set random seed for reproducibility
  set.seed(123)
  tensorflow::set_random_seed(123)
  
  # Create sequential model
  model <- keras_model_sequential()
  
  # Add input layer and first hidden layer
  model %>%
    layer_dense(units = hidden_units[1], activation = "relu", input_shape = input_dim) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate)
  
  # Add additional hidden layers
  for (units in hidden_units[-1]) {
    model %>%
      layer_dense(units = units, activation = "relu") %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = dropout_rate)
  }
  
  # Add output layer
  model %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  # Compile the model
  log_info("Compiling neural network model")
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = c("accuracy")
  )
  
  # Define callbacks
  callbacks_list <- list(
    callback_early_stopping(
      monitor = "val_loss",
      patience = 10,
      restore_best_weights = TRUE
    ),
    callback_reduce_lr_on_plateau(
      monitor = "val_loss",
      factor = 0.5,
      patience = 5,
      min_lr = 0.00001
    ),
    callback_tensorboard(
      log_dir = here("logs", format(Sys.time(), "%Y%m%d-%H%M%S")),
      histogram_freq = 1
    )
  )
  
  # Train the model
  log_info("Training neural network model")
  history <- model %>% fit(
    x = x_train,
    y = y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_data = list(x_valid, y_valid),
    callbacks = callbacks_list,
    verbose = 1
  )
  
  # Evaluate the model
  log_info("Evaluating neural network model")
  evaluation <- model %>% evaluate(x_test, y_test)
  
  # Make predictions
  log_info("Making predictions")
  y_pred_prob <- model %>% predict(x_test)
  y_pred_class <- ifelse(y_pred_prob > 0.5, 1, 0)
  
  # Create confusion matrix
  conf_mat <- table(Predicted = y_pred_class, Actual = y_test)
  
  # Calculate metrics
  accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
  precision <- conf_mat[2, 2] / sum(conf_mat[2, ])
  recall <- conf_mat[2, 2] / sum(conf_mat[, 2])
  f1_score <- 2 * precision * recall / (precision + recall)
  
  metrics <- tibble(
    metric = c("accuracy", "precision", "recall", "f1"),
    value = c(accuracy, precision, recall, f1_score)
  )
  
  # Return results
  log_info("Neural network model training completed")
  return(list(
    model = model,
    history = history,
    evaluation = evaluation,
    predictions = list(
      probabilities = y_pred_prob,
      classes = y_pred_class,
      actual = y_test
    ),
    conf_mat = conf_mat,
    metrics = metrics,
    recipe = nn_recipe
  ))
}

#' Run multiple neural network model configurations and compare results
#'
#' @return A list containing results for all model configurations
run_binary_nn_experiments <- function() {
  log_info("Running neural network experiments for binary classification")
  
  # Model 1: Base features only
  log_info("Training Model 1: Base features only")
  model1 <- train_binary_nn(
    include_statement = FALSE,
    include_context = FALSE,
    include_search = FALSE
  )
  
  # Model 2: Base features + Search
  log_info("Training Model 2: Base features + Search")
  model2 <- train_binary_nn(
    include_statement = FALSE,
    include_context = FALSE,
    include_search = TRUE
  )
  
  # Model 3: Base features + Statement
  log_info("Training Model 3: Base features + Statement")
  model3 <- train_binary_nn(
    include_statement = TRUE,
    include_context = FALSE,
    include_search = FALSE
  )
  
  # Model 4: Base features + Context
  log_info("Training Model 4: Base features + Context")
  model4 <- train_binary_nn(
    include_statement = FALSE,
    include_context = TRUE,
    include_search = FALSE
  )
  
  # Model 5: Full model (all features)
  log_info("Training Model 5: Full model (all features)")
  model5 <- train_binary_nn(
    include_statement = TRUE,
    include_context = TRUE,
    include_search = TRUE
  )
  
  # Compare model metrics
  log_info("Comparing model metrics")
  model_metrics <- bind_rows(
    model1$metrics %>% pivot_wider(names_from = metric, values_from = value) %>% mutate(model = "Base features only"),
    model2$metrics %>% pivot_wider(names_from = metric, values_from = value) %>% mutate(model = "Base + Search"),
    model3$metrics %>% pivot_wider(names_from = metric, values_from = value) %>% mutate(model = "Base + Statement"),
    model4$metrics %>% pivot_wider(names_from = metric, values_from = value) %>% mutate(model = "Base + Context"),
    model5$metrics %>% pivot_wider(names_from = metric, values_from = value) %>% mutate(model = "Full model")
  )
  
  # Return all results
  return(list(
    model1 = model1,
    model2 = model2,
    model3 = model3,
    model4 = model4,
    model5 = model5,
    comparison = model_metrics
  ))
}

# If this script is run directly, execute the experiments
if (sys.nframe() == 0) {
  results <- run_binary_nn_experiments()
  print(results$comparison)
}

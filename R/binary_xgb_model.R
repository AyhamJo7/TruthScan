#' Binary XGBoost Model for TruthScan
#'
#' This script implements an XGBoost model for binary classification
#' of fake news using the tidymodels framework.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Load required libraries
library(tidyverse)      # For data manipulation
library(tidymodels)     # For modeling framework
library(xgboost)        # For XGBoost implementation
library(vip)            # For variable importance
library(here)           # For project-relative paths
library(glue)           # For string interpolation
library(logger)         # For logging
library(doParallel)     # For parallel processing

# Configure logging
log_threshold(INFO)
log_formatter(formatter_glue)

#' Train and evaluate an XGBoost model for binary classification
#'
#' @param train_path Path to training data
#' @param test_path Path to test data
#' @param valid_path Path to validation data
#' @param include_statement Whether to include statement SVD features
#' @param include_context Whether to include context SVD features
#' @param include_search Whether to include search features
#' @param cores Number of cores to use for parallel processing
#' @return A list containing the trained model, predictions, and evaluation metrics
train_binary_xgb <- function(train_path = here("data", "binary_training.csv"),
                            test_path = here("data", "binary_testing.csv"),
                            valid_path = here("data", "binary_validating.csv"),
                            include_statement = TRUE,
                            include_context = TRUE,
                            include_search = TRUE,
                            cores = parallel::detectCores(logical = FALSE)) {
  
  log_info("Loading data from {train_path}, {test_path}, and {valid_path}")
  
  # Load data
  train_data <- read_csv(train_path, col_types = cols())
  test_data <- read_csv(test_path, col_types = cols())
  valid_data <- read_csv(valid_path, col_types = cols())
  
  # Convert Label to factor
  train_data <- train_data %>% mutate(Label = factor(Label, levels = c("0", "1")))
  test_data <- test_data %>% mutate(Label = factor(Label, levels = c("0", "1")))
  valid_data <- valid_data %>% mutate(Label = factor(Label, levels = c("0", "1")))
  
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
  
  # Set up parallel processing
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Define the model specification
  log_info("Defining XGBoost model specification")
  xgb_spec <- boost_tree(
    trees = 500,
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    learn_rate = tune()
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  # Define the preprocessing recipe
  log_info("Creating preprocessing recipe")
  xgb_recipe <- recipe(Label ~ ., data = train_data %>% select(Label, all_of(selected_features))) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  # Create a workflow
  xgb_workflow <- workflow() %>%
    add_recipe(xgb_recipe) %>%
    add_model(xgb_spec)
  
  # Define the grid for tuning
  xgb_grid <- grid_latin_hypercube(
    tree_depth(range = c(3, 10)),
    min_n(range = c(5, 30)),
    loss_reduction(range = c(-10, 1), trans = log10_trans()),
    sample_size(range = c(0.5, 1.0)),
    mtry(range = c(5, 20)),
    learn_rate(range = c(-5, -1), trans = log10_trans()),
    size = 20
  )
  
  # Set up cross-validation
  log_info("Setting up cross-validation")
  train_cv <- vfold_cv(train_data, v = 5, strata = Label)
  
  # Tune the model
  log_info("Tuning XGBoost model")
  xgb_tune_results <- xgb_workflow %>%
    tune_grid(
      resamples = train_cv,
      grid = xgb_grid,
      metrics = metric_set(accuracy, roc_auc, precision, recall, f_meas),
      control = control_grid(save_pred = TRUE, verbose = TRUE)
    )
  
  # Select the best model based on ROC AUC
  log_info("Selecting best model")
  best_xgb <- xgb_tune_results %>%
    select_best(metric = "roc_auc")
  
  # Finalize the workflow with the best parameters
  final_xgb_workflow <- xgb_workflow %>%
    finalize_workflow(best_xgb)
  
  # Fit the final model on the full training data
  log_info("Fitting final model on full training data")
  final_xgb_fit <- final_xgb_workflow %>%
    fit(data = train_data)
  
  # Make predictions on test data
  log_info("Making predictions on test data")
  xgb_preds <- final_xgb_fit %>%
    predict(test_data) %>%
    bind_cols(
      predict(final_xgb_fit, test_data, type = "prob")
    ) %>%
    bind_cols(test_data %>% select(Label))
  
  # Calculate metrics
  log_info("Calculating evaluation metrics")
  xgb_metrics <- xgb_preds %>%
    metrics(truth = Label, estimate = .pred_class, .pred_0, .pred_1)
  
  # Create confusion matrix
  xgb_conf_mat <- xgb_preds %>%
    conf_mat(truth = Label, estimate = .pred_class)
  
  # Extract variable importance
  log_info("Extracting variable importance")
  xgb_vip <- final_xgb_fit %>%
    extract_fit_parsnip() %>%
    vip(num_features = 20)
  
  # Clean up parallel processing
  stopCluster(cl)
  
  # Return results
  log_info("XGBoost model training completed")
  return(list(
    workflow = final_xgb_workflow,
    fit = final_xgb_fit,
    predictions = xgb_preds,
    metrics = xgb_metrics,
    conf_mat = xgb_conf_mat,
    vip = xgb_vip,
    tune_results = xgb_tune_results,
    best_params = best_xgb
  ))
}

#' Run multiple XGBoost model configurations and compare results
#'
#' @param cores Number of cores to use for parallel processing
#' @return A list containing results for all model configurations
run_binary_xgb_experiments <- function(cores = parallel::detectCores(logical = FALSE)) {
  log_info("Running XGBoost experiments for binary classification")
  
  # Model 1: Base features only
  log_info("Training Model 1: Base features only")
  model1 <- train_binary_xgb(
    include_statement = FALSE,
    include_context = FALSE,
    include_search = FALSE,
    cores = cores
  )
  
  # Model 2: Base features + Search
  log_info("Training Model 2: Base features + Search")
  model2 <- train_binary_xgb(
    include_statement = FALSE,
    include_context = FALSE,
    include_search = TRUE,
    cores = cores
  )
  
  # Model 3: Base features + Statement
  log_info("Training Model 3: Base features + Statement")
  model3 <- train_binary_xgb(
    include_statement = TRUE,
    include_context = FALSE,
    include_search = FALSE,
    cores = cores
  )
  
  # Model 4: Base features + Context
  log_info("Training Model 4: Base features + Context")
  model4 <- train_binary_xgb(
    include_statement = FALSE,
    include_context = TRUE,
    include_search = FALSE,
    cores = cores
  )
  
  # Model 5: Full model (all features)
  log_info("Training Model 5: Full model (all features)")
  model5 <- train_binary_xgb(
    include_statement = TRUE,
    include_context = TRUE,
    include_search = TRUE,
    cores = cores
  )
  
  # Compare model metrics
  log_info("Comparing model metrics")
  model_metrics <- bind_rows(
    model1$metrics %>% mutate(model = "Base features only"),
    model2$metrics %>% mutate(model = "Base + Search"),
    model3$metrics %>% mutate(model = "Base + Statement"),
    model4$metrics %>% mutate(model = "Base + Context"),
    model5$metrics %>% mutate(model = "Full model")
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
  results <- run_binary_xgb_experiments()
  print(results$comparison)
}

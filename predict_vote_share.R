# Satellite Embeddings Vote Share Prediction
# Machine Learning Analysis using XGBoost and other models
# Predicts Trump vote share using satellite embeddings from Google Earth Engine

library(tidyverse)
library(xgboost)
library(ranger)
library(glmnet)
library(caret)
library(vip)
library(patchwork)
library(here)

# Set working directory and create output folders
setwd(here())
dir.create("results", showWarnings = FALSE)
dir.create("results/plots", showWarnings = FALSE)

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================

# Function to load and clean embeddings data
load_embeddings_data <- function(file_path) {
  cat("Loading embeddings data from:", file_path, "\n")
  
  # Read the CSV file
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Clean column names (remove any special characters)
  names(data) <- gsub("[^A-Za-z0-9_]", "_", names(data))
  
  # Identify embedding columns (those with 'embedding' in the name)
  embedding_cols <- names(data)[grepl("embedding", names(data), ignore.case = TRUE)]
  cat("Found", length(embedding_cols), "embedding columns\n")
  
  # Remove any rows with missing embeddings
  data <- data %>%
    filter(if_all(all_of(embedding_cols), ~ !is.na(.)))
  
  cat("Data dimensions after cleaning:", nrow(data), "x", ncol(data), "\n")
  
  return(list(
    data = data,
    embedding_cols = embedding_cols
  ))
}

# Function to merge embeddings with electoral returns
merge_with_returns <- function(embeddings_data, returns_file, level = "county") {
  cat("Loading electoral returns from:", returns_file, "\n")
  
  returns_data <- read_csv(returns_file, show_col_types = FALSE)
  
  # Standardize FIPS codes for merging
  if (level == "county") {
    # Ensure FIPS codes are properly formatted
    returns_data <- returns_data %>%
      mutate(
        fips_code = str_pad(as.character(county_fips), 5, pad = "0"),
        trump_vote_share = as.numeric(trump_vote_share)
      )
    
    # Merge on FIPS code
    merged_data <- embeddings_data %>%
      mutate(fips_code = str_pad(as.character(GEOID), 5, pad = "0")) %>%
      left_join(returns_data, by = "fips_code") %>%
      filter(!is.na(trump_vote_share))
    
  } else if (level == "precinct") {
    # For precincts, we'll need a different merging strategy
    # This depends on how the precinct data is structured
    merged_data <- embeddings_data %>%
      left_join(returns_data, by = c("precinct_id", "state")) %>%
      filter(!is.na(trump_vote_share))
  }
  
  cat("Merged data dimensions:", nrow(merged_data), "x", ncol(merged_data), "\n")
  cat("Trump vote share range:", round(range(merged_data$trump_vote_share, na.rm = TRUE), 3), "\n")
  
  return(merged_data)
}

# =============================================================================
# MODEL TRAINING AND EVALUATION
# =============================================================================

# Function to prepare data for modeling
prepare_modeling_data <- function(data, embedding_cols) {
  # Create feature matrix (X) and target vector (y)
  X <- as.matrix(data[, embedding_cols])
  y <- data$trump_vote_share
  
  # Remove any rows with missing values
  complete_cases <- complete.cases(X, y)
  X <- X[complete_cases, ]
  y <- y[complete_cases]
  
  cat("Modeling data dimensions:", nrow(X), "samples,", ncol(X), "features\n")
  
  return(list(X = X, y = y))
}

# Function to split data into train/test sets
split_data <- function(X, y, test_size = 0.2, seed = 123) {
  set.seed(seed)
  
  train_indices <- createDataPartition(y, p = 1 - test_size, list = FALSE)
  
  X_train <- X[train_indices, ]
  X_test <- X[-train_indices, ]
  y_train <- y[train_indices]
  y_test <- y[-train_indices]
  
  cat("Train set:", nrow(X_train), "samples\n")
  cat("Test set:", nrow(X_test), "samples\n")
  
  return(list(
    X_train = X_train, X_test = X_test,
    y_train = y_train, y_test = y_test
  ))
}

# Function to train XGBoost model
train_xgboost <- function(X_train, y_train, X_test, y_test) {
  cat("Training XGBoost model...\n")
  
  # Convert to DMatrix format
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Set parameters
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    gamma = 0
  )
  
  # Train model with early stopping
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 1000,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 50,
    verbose = 0
  )
  
  # Make predictions
  train_pred <- predict(model, dtrain)
  test_pred <- predict(model, dtest)
  
  return(list(
    model = model,
    train_pred = train_pred,
    test_pred = test_pred,
    n_rounds = model$best_iteration
  ))
}

# Function to train Random Forest model
train_random_forest <- function(X_train, y_train, X_test, y_test) {
  cat("Training Random Forest model...\n")
  
  # Convert to data frame
  train_df <- data.frame(X_train, y = y_train)
  test_df <- data.frame(X_test, y = y_test)
  
  # Train model
  model <- ranger(
    y ~ .,
    data = train_df,
    num.trees = 500,
    mtry = sqrt(ncol(X_train)),
    min.node.size = 5,
    importance = "permutation"
  )
  
  # Make predictions
  train_pred <- predict(model, train_df)$predictions
  test_pred <- predict(model, test_df)$predictions
  
  return(list(
    model = model,
    train_pred = train_pred,
    test_pred = test_pred
  ))
}

# Function to train Elastic Net model
train_elastic_net <- function(X_train, y_train, X_test, y_test) {
  cat("Training Elastic Net model...\n")
  
  # Cross-validation to find optimal alpha and lambda
  cv_model <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 5)
  
  # Train final model
  model <- glmnet(X_train, y_train, alpha = 0.5, lambda = cv_model$lambda.min)
  
  # Make predictions
  train_pred <- predict(model, X_train, s = cv_model$lambda.min)[, 1]
  test_pred <- predict(model, X_test, s = cv_model$lambda.min)[, 1]
  
  return(list(
    model = model,
    cv_model = cv_model,
    train_pred = train_pred,
    test_pred = test_pred
  ))
}

# Function to evaluate model performance
evaluate_model <- function(y_true, y_pred, model_name) {
  # Calculate metrics
  rmse <- sqrt(mean((y_true - y_pred)^2))
  mae <- mean(abs(y_true - y_pred))
  r_squared <- cor(y_true, y_pred)^2
  
  cat("\n", model_name, "Performance:\n")
  cat("RMSE:", round(rmse, 4), "\n")
  cat("MAE:", round(mae, 4), "\n")
  cat("R-squared:", round(r_squared, 4), "\n")
  
  return(data.frame(
    model = model_name,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared
  ))
}

# =============================================================================
# VISUALIZATION
# =============================================================================

# Function to create prediction plots
create_prediction_plots <- function(y_true, y_pred, model_name, level) {
  
  # Create data frame for plotting
  plot_data <- data.frame(
    actual = y_true,
    predicted = y_pred
  )
  
  # Actual vs Predicted scatter plot
  p1 <- ggplot(plot_data, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = paste(model_name, "- Actual vs Predicted"),
      x = "Actual Trump Vote Share",
      y = "Predicted Trump Vote Share"
    ) +
    theme_minimal() +
    coord_fixed()
  
  # Residuals plot
  residuals <- y_true - y_pred
  p2 <- ggplot(data.frame(predicted = y_pred, residuals = residuals), 
               aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = paste(model_name, "- Residuals"),
      x = "Predicted Trump Vote Share",
      y = "Residuals"
    ) +
    theme_minimal()
  
  # Combine plots
  combined_plot <- p1 / p2
  
  # Save plot
  ggsave(
    file.path("results/plots", paste0(model_name, "_", level, "_predictions.png")),
    combined_plot,
    width = 8, height = 10, dpi = 300
  )
  
  return(combined_plot)
}

# Function to create feature importance plot (for XGBoost)
create_importance_plot <- function(model, embedding_cols, level) {
  
  # Get feature importance
  importance_scores <- xgb.importance(model = model)
  
  # Take top 20 features
  top_features <- head(importance_scores, 20)
  
  # Create plot
  p <- ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top 20 Most Important Embedding Features",
      x = "Embedding Feature",
      y = "Importance (Gain)"
    ) +
    theme_minimal()
  
  # Save plot
  ggsave(
    file.path("results/plots", paste0("feature_importance_", level, ".png")),
    p,
    width = 10, height = 8, dpi = 300
  )
  
  return(p)
}

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

# Function to run complete analysis
run_analysis <- function(embeddings_file, returns_file, level = "county") {
  
  cat("Starting analysis for", level, "level data\n")
  cat("=" %R% 50, "\n")
  
  # Load and prepare data
  embeddings_list <- load_embeddings_data(embeddings_file)
  merged_data <- merge_with_returns(
    embeddings_list$data, 
    returns_file, 
    level = level
  )
  
  # Prepare modeling data
  modeling_data <- prepare_modeling_data(merged_data, embeddings_list$embedding_cols)
  split_data <- split_data(modeling_data$X, modeling_data$y)
  
  # Train models
  cat("\nTraining models...\n")
  
  xgb_results <- train_xgboost(
    split_data$X_train, split_data$y_train,
    split_data$X_test, split_data$y_test
  )
  
  rf_results <- train_random_forest(
    split_data$X_train, split_data$y_train,
    split_data$X_test, split_data$y_test
  )
  
  en_results <- train_elastic_net(
    split_data$X_train, split_data$y_train,
    split_data$X_test, split_data$y_test
  )
  
  # Evaluate models
  cat("\nEvaluating models...\n")
  
  xgb_metrics <- evaluate_model(
    split_data$y_test, xgb_results$test_pred, "XGBoost"
  )
  
  rf_metrics <- evaluate_model(
    split_data$y_test, rf_results$test_pred, "Random Forest"
  )
  
  en_metrics <- evaluate_model(
    split_data$y_test, en_results$test_pred, "Elastic Net"
  )
  
  # Combine results
  all_metrics <- rbind(xgb_metrics, rf_metrics, en_metrics)
  
  # Create visualizations
  cat("\nCreating visualizations...\n")
  
  xgb_plot <- create_prediction_plots(
    split_data$y_test, xgb_results$test_pred, "XGBoost", level
  )
  
  rf_plot <- create_prediction_plots(
    split_data$y_test, rf_results$test_pred, "Random Forest", level
  )
  
  en_plot <- create_prediction_plots(
    split_data$y_test, en_results$test_pred, "Elastic Net", level
  )
  
  # Feature importance for XGBoost
  importance_plot <- create_importance_plot(
    xgb_results$model, embeddings_list$embedding_cols, level
  )
  
  # Save results
  write_csv(all_metrics, file.path("results", paste0("model_performance_", level, ".csv")))
  
  cat("\nAnalysis complete!\n")
  cat("Results saved to results/ folder\n")
  
  return(list(
    metrics = all_metrics,
    models = list(
      xgb = xgb_results,
      rf = rf_results,
      en = en_results
    ),
    plots = list(
      xgb = xgb_plot,
      rf = rf_plot,
      en = en_plot,
      importance = importance_plot
    )
  ))
}

# =============================================================================
# EXECUTION
# =============================================================================

# Example usage - uncomment and modify paths as needed

# # County-level analysis
# county_results <- run_analysis(
#   embeddings_file = "path/to/county_embeddings.csv",
#   returns_file = "data/counties/county_returns_2020_processed.csv",
#   level = "county"
# )

# # Precinct-level analysis
# precinct_results <- run_analysis(
#   embeddings_file = "path/to/precinct_embeddings.csv", 
#   returns_file = "data/precincts/precinct_returns_2020_processed.csv",
#   level = "precinct"
# )

cat("Script loaded successfully!\n")
cat("To run analysis, call run_analysis() with your data file paths\n")
cat("Example:\n")
cat("results <- run_analysis('embeddings.csv', 'returns.csv', 'county')\n")

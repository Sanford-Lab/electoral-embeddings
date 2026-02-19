# ==============================================================================
# XGBoost Grid Search for Precinct Embeddings (HPC Version)
# Target: 2020 Trump Vote Share
# ==============================================================================

# 1. Library Setup
# ------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(caret)
  library(xgboost)
  library(doParallel)
})

# Get the number of cores assigned by Slurm
cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", 1))

cat("--- JOB STARTED:", as.character(Sys.time()), "---\n")
cat("--- DETECTED CORES:", cores, "---\n")

# 2. Parallel Backend
# ------------------------------------------------------------------------------
# We reserve 1 core for the OS, use the rest for training
cl <- makePSOCKcluster(max(1, cores - 1))
registerDoParallel(cl)

# 3. Data Loading & Cleaning
# ------------------------------------------------------------------------------
# Load the 2020 precinct file
# NOTE: We keep this 'raw_data' object safe so we can join predictions to it later
raw_data <- read_csv("data/prep/precincts/2020/clean_emb_precinct_20.csv", 
                     show_col_types = FALSE)

# CREATE MODELING DATA:
# STRICTLY matching your Random Forest predictors.
# We explicitly SELECT only what we need so the model never sees 'precinct_id'.
model_data <- raw_data %>%
  select(
    TRUMPSHARE = rep_share, 
    ends_with("_mean")
  ) %>%
  na.omit() 

# SYNC THE RAW DATA:
# IMPORTANT: na.omit() might have dropped rows in 'model_data'.
# We must drop the EXACT same rows in 'raw_data' so they align perfectly later.
raw_data <- raw_data[complete.cases(raw_data %>% select(rep_share, ends_with("_mean"))), ]

cat("Data Loaded. Rows:", nrow(model_data), "| Columns:", ncol(model_data), "\n")
cat("Predictors:", ncol(model_data) - 1, "(Embedding Means)\n")

# Free up memory
gc()

# 4. The Surgical Grid Search
# ------------------------------------------------------------------------------
# RATIONALE:
# Your Random Forest worked well with mtry=8 (approx 12% of features).
# We mimic that here with 'colsample_bytree' (0.2 ~= 12 features).
# We also use 'subsample' to prevent overfitting, similar to RF bootstrapping.

xgb_grid <- expand.grid(
  nrounds = c(1000, 2000),        # High tree count allows for slower, finer learning
  max_depth = c(4, 6, 8),         # Depth 6 is usually the sweet spot for spatial data
  eta = c(0.01, 0.05),            # Learning rate
  gamma = c(0, 1),                # Regularization to prevent overfitting
  colsample_bytree = c(0.2, 0.4), # The "Random Forest" effect (feature subsetting)
  min_child_weight = c(5, 10),    # Prevents the model from chasing outliers
  subsample = 0.7                 # Use 70% of data for each tree
)

# 5. Training Controls
# ------------------------------------------------------------------------------
train_control <- trainControl(
  method = "cv",
  number = 5,                     # 5-Fold Cross Validation
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE              # Save RAM by not storing the training data in the model
)

# 6. Execute Training
# ------------------------------------------------------------------------------
cat("Starting Training on", nrow(xgb_grid), "hyperparameter combinations...\n")

set.seed(42)
xgb_model <- train(
  TRUMPSHARE ~ ., 
  data = model_data,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "Rsquared",
  nthread = 1 # We let doParallel handle the threading
)

# Stop the cluster to free up resources for the prediction step
stopCluster(cl)

cat("\n--- BEST MODEL FOUND ---\n")
print(xgb_model$bestTune)
cat("Best R-Squared:", max(xgb_model$results$Rsquared), "\n")

# 7. Final Predictions & Joining
# ------------------------------------------------------------------------------

# A. Predict on the full dataset using the best model
cat("Generating final predictions for all precincts...\n")
final_preds <- predict(xgb_model, newdata = model_data)

# B. Join results back to the original metadata (ID, Density, etc.)
# Since we synchronized 'raw_data' and 'model_data' in Step 3, the rows match 1:1.
final_results <- raw_data %>%
  select(precinct_id, vote_density, rep_share) %>% # Keep the metadata you want
  mutate(
    TRUMPSHARE = rep_share,
    Predicted_Share = final_preds,
    Error = Predicted_Share - TRUMPSHARE,
    Abs_Error = abs(Error)
  )

# 8. Save Outputs
# ------------------------------------------------------------------------------

# Save the heavy model object (in case you need it later)
saveRDS(xgb_model, "outputs/xgb_precinct_2020_model.rds")

# Save the lightweight CSV for your visualizations
write_csv(final_results, "outputs/xgb_2020_precinct_results.csv")

cat("\n--- SUMMARY ---\n")
cat("RMSE:", sqrt(mean(final_results$Error^2)), "\n")
cat("Results saved to: outputs/xgb_2020_precinct_results.csv\n")
cat("--- JOB FINISHED:", as.character(Sys.time()), "---\n")
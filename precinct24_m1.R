library(tidyverse)
library(ranger)
library(scales) # for comma labels in plots

# ==========================================
# 1. Load and Clean Data (2024)
# ==========================================

# Load the clean 2024 precinct dataset
clean_data <- read_csv("data/prep/precincts/2024/clean_emb_precinct_24.csv")

# Prepare data:
# Keep ID and Density for analysis, but we will EXCLUDE them from the model formula later.
data_rf <- clean_data %>%
  select(
    precinct_id, 
    vote_density, 
    TRUMPSHARE = rep_share, 
    ends_with("_mean") # Only embedding means
  ) %>%
  na.omit()

cat("Data Loaded. 2024 Precincts:", nrow(data_rf), "\n")

# ==========================================
# 2. Setup 5-Fold Cross-Validation
# ==========================================

set.seed(42)

# Create 5 random folds
# We shuffle the data first, then assign a fold number (1 through 5)
data_rf <- data_rf %>%
  mutate(fold_id = sample(rep(1:5, length.out = n())))

# Initialize an empty list to store predictions
all_predictions <- list()

cat("Starting 5-Fold Cross-Validation on", nrow(data_rf), "precincts...\n")

# ==========================================
# 3. The CV Loop
# ==========================================

for(k in 1:5) {
  
  # A. Split Data
  train_df <- data_rf %>% filter(fold_id != k)
  test_df  <- data_rf %>% filter(fold_id == k)
  
  # B. Train Model
  # Standard Regression Rule: mtry = Number of features / 3
  n_features <- ncol(train_df) - 4 # Subtract ID, density, share, fold_id
  # my_mtry    <- floor(n_features / 3)
  
  rf_model <- ranger(
    TRUMPSHARE ~ . - precinct_id - vote_density - fold_id, 
    data       = train_df,
    num.trees  = 500,
    mtry       = 8, # my_mtry, # Dynamic (approx 21 for 64 embeddings)
    importance = "impurity",
    seed       = 42
  )
  
  # C. Predict
  preds <- predict(rf_model, data = test_df)$predictions
  
  # D. Store Results
  test_df$Predicted_Share <- preds
  all_predictions[[k]] <- test_df
  
  cat("  Fold", k, "complete. Rows predicted:", nrow(test_df), "\n")
}

# Combine all 5 folds back into one big dataframe
full_results <- bind_rows(all_predictions) %>%
  mutate(
    Error = Predicted_Share - TRUMPSHARE,
    Abs_Error = abs(Error)
  )

# Save results
write_csv(full_results, "data/results/precinct24_m1_results.csv")
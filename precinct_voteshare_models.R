library(tidyverse)
library(ranger)
library(scales)

# ==============================================================================
# 1. RANDOM CV FUNCTION (M1, M3, M4)
# ==============================================================================
run_precinct_random_model <- function(data_file, year_label, model_type) {
  output_file <- paste0("data/results/precinct", year_label, "_", model_type, "_results.csv")
  imp_output_file <- paste0("data/results/precinct", year_label, "_", model_type, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING PRECINCT RANDOM CV:", toupper(model_type), "| YEAR:", year_label, "\n")
  cat("==========================================\n")
  
  clean_data <- read_csv(data_file, show_col_types = FALSE)
  
  # A. Prepare Data and Formula
  if (model_type == "m1") {
    # M1: Embeddings ONLY
    data_rf <- clean_data %>% select(precinct_id, vote_density, TRUMPSHARE = rep_share, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - precinct_id - vote_density - fold_id
    n_features <- ncol(data_rf) - 3
  } else if (model_type == "m3") {
    # M3: Baseline ONLY
    data_rf <- clean_data %>% select(precinct_id, area_km2, vote_density, TRUMPSHARE = rep_share) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - precinct_id - fold_id
    n_features <- ncol(data_rf) - 2
  } else if (model_type == "m4") {
    # M4: Embeddings + Baseline
    data_rf <- clean_data %>% select(precinct_id, area_km2, vote_density, TRUMPSHARE = rep_share, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - precinct_id - fold_id
    n_features <- ncol(data_rf) - 2
  }
  
  my_mtry <- min(8, max(1, floor(n_features / 3)))
  
  # B. Setup Folds
  set.seed(42)
  data_rf <- data_rf %>% mutate(fold_id = sample(rep(1:5, length.out = n())))
  
  all_predictions <- list()
  fold_importances <- list()
  
  # C. CV Loop
  for(k in 1:5) {
    train_df <- data_rf %>% filter(fold_id != k)
    test_df  <- data_rf %>% filter(fold_id == k)
    
    rf_model <- ranger(formula = rf_formula, data = train_df, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42)
    
    preds <- predict(rf_model, data = test_df)$predictions
    test_df$Predicted_Share <- preds
    all_predictions[[k]] <- test_df
    fold_importances[[k]] <- rf_model$variable.importance
  }
  
  # D. Save Results and Importances
  full_results <- bind_rows(all_predictions) %>% mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# ==============================================================================
# 2. SPATIAL CV FUNCTION (M2, M5, M6)
# ==============================================================================
run_precinct_spatial_model <- function(data_file, year_label, model_type) {
  output_file <- paste0("data/results/precinct", year_label, "_", model_type, "_results.csv")
  imp_output_file <- paste0("data/results/precinct", year_label, "_", model_type, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING PRECINCT SPATIAL CV:", toupper(model_type), "| YEAR:", year_label, "\n")
  cat("==========================================\n")
  
  clean_data <- read_csv(data_file, show_col_types = FALSE)
  
  # A. Prepare Data and Formula
  if (model_type == "m2") {
    # M2: Embeddings ONLY
    data_rf <- clean_data %>% select(precinct_id, state, vote_density, TRUMPSHARE = rep_share, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - precinct_id - state - vote_density - fold_id
    n_features <- ncol(data_rf) - 4
  } else if (model_type == "m5") {
    # M5: Baseline ONLY
    data_rf <- clean_data %>% select(precinct_id, state, area_km2, vote_density, TRUMPSHARE = rep_share) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - precinct_id - state - fold_id
    n_features <- ncol(data_rf) - 3
  } else if (model_type == "m6") {
    # M6: Embeddings + Baseline
    data_rf <- clean_data %>% select(precinct_id, state, area_km2, vote_density, TRUMPSHARE = rep_share, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - precinct_id - state - fold_id
    n_features <- ncol(data_rf) - 3
  }
  
  my_mtry <- min(8, max(1, floor(n_features / 3)))
  
  # B. Setup Folds (Spatial)
  set.seed(42)
  unique_states <- unique(data_rf$state)
  state_folds <- tibble(state = sample(unique_states), fold_id = rep(1:5, length.out = length(unique_states)))
  data_rf <- data_rf %>% left_join(state_folds, by = "state")
  
  all_predictions <- list()
  fold_importances <- list()
  
  # C. CV Loop
  for(k in 1:5) {
    train_df <- data_rf %>% filter(fold_id != k)
    test_df  <- data_rf %>% filter(fold_id == k)
    
    rf_model <- ranger(formula = rf_formula, data = train_df, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42)
    
    preds <- predict(rf_model, data = test_df)$predictions
    test_df$Predicted_Share <- preds
    all_predictions[[k]] <- test_df
    fold_importances[[k]] <- rf_model$variable.importance
  }
  
  # D. Save Results and Importances
  full_results <- bind_rows(all_predictions) %>% mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# ==============================================================================
# 3. EXECUTE ALL 12 PRECINCT MODELS
# ==============================================================================

file_20 <- "data/prep/precincts/2020/clean_emb_precinct_20.csv"
file_24 <- "data/prep/precincts/2024/clean_emb_precinct_24.csv"
years <- list("20" = file_20, "24" = file_24)

# 1. Run Random CV (M1, M3, M4)
for (yr_label in names(years)) {
  for (mod in c("m1", "m3", "m4")) {
    run_precinct_random_model(data_file = years[[yr_label]], year_label = yr_label, model_type = mod)
  }
}

# 2. Run Spatial CV (M2, M5, M6)
for (yr_label in names(years)) {
  for (mod in c("m2", "m5", "m6")) {
    run_precinct_spatial_model(data_file = years[[yr_label]], year_label = yr_label, model_type = mod)
  }
}

cat("\n========================================================\n")
cat("SUCCESS: All 12 precinct models and importance files saved!\n")
cat("========================================================\n")
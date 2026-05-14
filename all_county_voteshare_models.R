library(tidyverse)
library(ranger)

# ==============================================================================
# 1. RANDOM SPATIAL CV FUNCTION (M1 - M4)
# ==============================================================================
run_county_model <- function(data_file, year_label, model_type) {
  output_file <- paste0("data/results/county", year_label, "_", model_type, "_results.csv")
  imp_output_file <- paste0("data/results/county", year_label, "_", model_type, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING RANDOM CV MODEL:", toupper(model_type), "| YEAR:", year_label, "\n")
  cat("==========================================\n")
  
  clean_data <- read_csv(data_file, show_col_types = FALSE)
  
  if (model_type == "m1") {
    data_rf <- clean_data %>% select(county_fips, vote_density, TRUMPSHARE = rep_share, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - vote_density - fold_id
    n_features <- ncol(data_rf) - 3
  } else if (model_type == "m2") {
    data_rf <- clean_data %>% select(county_fips, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - fold_id
    n_features <- ncol(data_rf) - 2
  } else if (model_type == "m3") {
    data_rf <- clean_data %>% select(county_fips, TRUMPSHARE = rep_share, area_km2, vote_density) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - fold_id
    n_features <- ncol(data_rf) - 2
  } else if (model_type == "m4") {
    data_rf <- clean_data %>% select(county_fips, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - fold_id
    n_features <- ncol(data_rf) - 2
  }
  
  my_mtry <- max(1, floor(n_features / 3))
  
  set.seed(10)
  data_rf <- data_rf %>% mutate(fold_id = sample(rep(1:5, length.out = n())))
  
  all_predictions <- list()
  fold_importances <- list() # Store importance per fold
  
  for(k in 1:5) {
    train_df <- data_rf %>% filter(fold_id != k)
    test_df  <- data_rf %>% filter(fold_id == k)
    
    rf_model <- ranger(formula = rf_formula, data = train_df, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42)
    
    preds <- predict(rf_model, data = test_df)$predictions
    test_df$Predicted_Share <- preds
    all_predictions[[k]] <- test_df
    fold_importances[[k]] <- rf_model$variable.importance # Extract importance
  }
  
  # Save Results
  full_results <- bind_rows(all_predictions) %>% mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  # Average and Save Importance
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# ==============================================================================
# 2. SPATIAL CV FUNCTION (M5 - M8)
# ==============================================================================
run_spatial_county_model <- function(data_file, year_label, model_type) {
  output_file <- paste0("data/results/county", year_label, "_", model_type, "_results.csv")
  imp_output_file <- paste0("data/results/county", year_label, "_", model_type, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING SPATIAL CV MODEL:", toupper(model_type), "| YEAR:", year_label, "\n")
  cat("==========================================\n")
  
  clean_data <- read_csv(data_file, show_col_types = FALSE)
  
  if (model_type == "m5") {
    data_rf <- clean_data %>% select(county_fips, state_po, vote_density, TRUMPSHARE = rep_share, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - state_po - vote_density - fold_id
    n_features <- ncol(data_rf) - 4
  } else if (model_type == "m6") {
    data_rf <- clean_data %>% select(county_fips, state_po, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force, ends_with("_mean")) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - state_po - fold_id
    n_features <- ncol(data_rf) - 3
  } else if (model_type == "m7") {
    data_rf <- clean_data %>% select(county_fips, state_po, TRUMPSHARE = rep_share, area_km2, vote_density) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - state_po - fold_id
    n_features <- ncol(data_rf) - 3
  } else if (model_type == "m8") {
    data_rf <- clean_data %>% select(county_fips, state_po, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force) %>% na.omit()
    rf_formula <- TRUMPSHARE ~ . - county_fips - state_po - fold_id
    n_features <- ncol(data_rf) - 3
  }
  
  my_mtry <- max(1, floor(n_features / 3))
  
  set.seed(10)
  unique_states <- unique(data_rf$state_po)
  state_folds <- tibble(state_po = sample(unique_states), fold_id = rep(1:5, length.out = length(unique_states)))
  data_rf <- data_rf %>% left_join(state_folds, by = "state_po")
  
  all_predictions <- list()
  fold_importances <- list()
  
  for(k in 1:5) {
    train_df <- data_rf %>% filter(fold_id != k)
    test_df  <- data_rf %>% filter(fold_id == k)
    
    rf_model <- ranger(formula = rf_formula, data = train_df, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42)
    
    preds <- predict(rf_model, data = test_df)$predictions
    test_df$Predicted_Share <- preds
    all_predictions[[k]] <- test_df
    fold_importances[[k]] <- rf_model$variable.importance
  }
  
  # Save Results
  full_results <- bind_rows(all_predictions) %>% mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  # Average and Save Importance
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# ==============================================================================
# 3. TEMPORAL CV FUNCTION (M9 - M12)
# ==============================================================================
run_temporal_model <- function(train_file, test_file, train_yr, test_yr, model_type) {
  output_file <- paste0("data/results/county", train_yr, "-", test_yr, "_", model_type, "_results.csv")
  imp_output_file <- paste0("data/results/county", train_yr, "-", test_yr, "_", model_type, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING TEMPORAL CV:", toupper(model_type), "|", train_yr, "->", test_yr, "\n")
  cat("==========================================\n")
  
  prep_data <- function(filepath, mod) {
    df <- read_csv(filepath, show_col_types = FALSE)
    if (mod == "m9") {
      df <- df %>% select(county_fips, vote_density, TRUMPSHARE = rep_share, ends_with("_mean"))
    } else if (mod == "m10") {
      df <- df %>% select(county_fips, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force, ends_with("_mean"))
    } else if (mod == "m11") {
      df <- df %>% select(county_fips, TRUMPSHARE = rep_share, area_km2, vote_density)
    } else if (mod == "m12") {
      df <- df %>% select(county_fips, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force)
    }
    return(na.omit(df))
  }
  
  train_df <- prep_data(train_file, model_type)
  test_df  <- prep_data(test_file, model_type)
  
  common_fips <- intersect(train_df$county_fips, test_df$county_fips)
  set.seed(42)
  county_folds <- tibble(county_fips = sample(common_fips), fold_id = rep(1:5, length.out = length(common_fips)))
  
  train_df <- train_df %>% inner_join(county_folds, by = "county_fips")
  test_df  <- test_df %>% inner_join(county_folds, by = "county_fips")
  
  if (model_type == "m9") {
    rf_formula <- TRUMPSHARE ~ . - county_fips - vote_density - fold_id
    n_features <- ncol(train_df) - 4
  } else {
    rf_formula <- TRUMPSHARE ~ . - county_fips - fold_id
    n_features <- ncol(train_df) - 3
  }
  
  my_mtry <- max(1, floor(n_features / 3))
  
  all_predictions <- list()
  fold_importances <- list()
  
  for(k in 1:5) {
    train_fold <- train_df %>% filter(fold_id != k)
    test_fold  <- test_df %>% filter(fold_id == k)
    
    rf_model <- ranger(formula = rf_formula, data = train_fold, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42)
    
    preds <- predict(rf_model, data = test_fold)$predictions
    test_fold$Predicted_Share <- preds
    all_predictions[[k]] <- test_fold
    fold_importances[[k]] <- rf_model$variable.importance
  }
  
  # Save Results
  full_results <- bind_rows(all_predictions) %>% mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  # Average and Save Importance
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# ==============================================================================
# 4. SPATIO-TEMPORAL CV FUNCTION (M13 - M16)
# ==============================================================================
run_spatiotemporal_model <- function(train_file, test_file, train_yr, test_yr, model_type) {
  output_file <- paste0("data/results/county", train_yr, "-", test_yr, "_", model_type, "_results.csv")
  imp_output_file <- paste0("data/results/county", train_yr, "-", test_yr, "_", model_type, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING SPATIO-TEMPORAL CV:", toupper(model_type), "|", train_yr, "->", test_yr, "\n")
  cat("==========================================\n")
  
  prep_data <- function(filepath, mod) {
    df <- read_csv(filepath, show_col_types = FALSE)
    if (mod == "m13") {
      df <- df %>% select(county_fips, state_po, vote_density, TRUMPSHARE = rep_share, ends_with("_mean"))
    } else if (mod == "m14") {
      df <- df %>% select(county_fips, state_po, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force, ends_with("_mean"))
    } else if (mod == "m15") {
      df <- df %>% select(county_fips, state_po, TRUMPSHARE = rep_share, area_km2, vote_density)
    } else if (mod == "m16") {
      df <- df %>% select(county_fips, state_po, TRUMPSHARE = rep_share, area_km2, vote_density, pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, unemployment_rate, civilian_labor_force)
    }
    return(na.omit(df))
  }
  
  train_df <- prep_data(train_file, model_type)
  test_df  <- prep_data(test_file, model_type)
  
  if (model_type == "m13") {
    rf_formula <- TRUMPSHARE ~ . - county_fips - state_po - vote_density - fold_id
    n_features <- ncol(train_df) - 4
  } else {
    rf_formula <- TRUMPSHARE ~ . - county_fips - state_po - fold_id
    n_features <- ncol(train_df) - 3
  }
  
  my_mtry <- max(1, floor(n_features / 3))
  
  set.seed(10)
  unique_states <- unique(train_df$state_po)
  state_folds <- tibble(state_po = sample(unique_states), fold_id = rep(1:5, length.out = length(unique_states)))
  
  train_df <- train_df %>% left_join(state_folds, by = "state_po")
  test_df  <- test_df %>% left_join(state_folds, by = "state_po")
  
  all_predictions <- list()
  fold_importances <- list()
  
  for(k in 1:5) {
    train_fold <- train_df %>% filter(fold_id != k)
    test_fold  <- test_df %>% filter(fold_id == k)
    
    rf_model <- ranger(formula = rf_formula, data = train_fold, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42)
    
    preds <- predict(rf_model, data = test_fold)$predictions
    test_fold$Predicted_Share <- preds
    all_predictions[[k]] <- test_fold
    fold_importances[[k]] <- rf_model$variable.importance
  }
  
  # Save Results
  full_results <- bind_rows(all_predictions) %>% mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  # Average and Save Importance
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# ==============================================================================
# 5. EXECUTE ALL 40 MODELS
# ==============================================================================

# 1. Define Master Data Paths
file_16 <- "data/prep/counties/master_county_emb_16.csv"
file_20 <- "data/prep/counties/master_county_emb_20.csv"
file_24 <- "data/prep/counties/master_county_emb_24.csv"

years <- list("16" = file_16, "20" = file_20, "24" = file_24)

temporal_pairs <- list(
  list(train = file_16, test = file_20, train_yr = "16", test_yr = "20"),
  list(train = file_20, test = file_24, train_yr = "20", test_yr = "24")
)

# 2. Run Random CV (M1 - M4) -> 12 Models
for (yr_label in names(years)) {
  for (mod in c("m1", "m2", "m3", "m4")) {
    run_county_model(data_file = years[[yr_label]], year_label = yr_label, model_type = mod)
  }
}

# 3. Run Spatial CV (M5 - M8) -> 12 Models
for (yr_label in names(years)) {
  for (mod in c("m5", "m6", "m7", "m8")) {
    run_spatial_county_model(data_file = years[[yr_label]], year_label = yr_label, model_type = mod)
  }
}

# 4. Run Temporal CV (M9 - M12) -> 8 Models
for (pair in temporal_pairs) {
  for (mod in c("m9", "m10", "m11", "m12")) {
    run_temporal_model(train_file = pair$train, test_file = pair$test, train_yr = pair$train_yr, test_yr = pair$test_yr, model_type = mod)
  }
}

# 5. Run Spatio-Temporal CV (M13 - M16) -> 8 Models
for (pair in temporal_pairs) {
  for (mod in c("m13", "m14", "m15", "m16")) {
    run_spatiotemporal_model(train_file = pair$train, test_file = pair$test, train_yr = pair$train_yr, test_yr = pair$test_yr, model_type = mod)
  }
}

cat("\n========================================================\n")
cat("SUCCESS: All 40 models and 40 importance files saved!\n")
cat("========================================================\n")



# ==============================================================================
# 6. MAXIMUM DEMOGRAPHIC & EMBEDDING MODELS (2020)
# ==============================================================================

var <- read.csv("data/prep/counties/poststratification_2020_county_v2.csv")

# Create the wide county-level dataset
county_wide <- var %>%
  
  # Step A: Group the 7 specific age codes into 3 broad categories
  mutate(
    age_group = case_when(
      age %in% 12:13 ~ "AgeBucket1",
      age %in% 14:15 ~ "AgeBucket2",
      age %in% 16:18 ~ "AgeBucket3",
      TRUE ~ "Unknown"
    )
  ) %>%
  
  # Step B: Aggregate the Frequencies
  # Group by county info, total pop, and our new simplified demographics
  group_by(state, county, county_fips, total_population, race, sex, age_group) %>%
  summarize(Frequency = sum(Frequency, na.rm = TRUE), .groups = "drop") %>%
  
  # Step C: Create a combined category name for our new columns
  # Example output: "race1_sex0_AgeBucket1"
  mutate(
    category_name = paste0("race", race, "_sex", sex, "_", age_group)
  ) %>%
  
  # Step D: Pivot from long to wide!
  select(-race, -sex, -age_group) %>%
  pivot_wider(
    names_from = category_name,
    values_from = Frequency,
    values_fill = list(Frequency = 0) # Fills missing combinations with 0
  ) %>%
  
  # Step E: Recalculate the ratios for all our new wide columns
  mutate(
    across(starts_with("race"), ~ . / total_population, .names = "{.col}_ratio")
  )

# 1. Load the 2020 master data and join it with our newly created wide demographic dataset
master_20 <- read_csv("data/prep/counties/master_county_emb_20.csv", show_col_types = FALSE)

# We drop 'state' and 'county' text columns from county_wide so they don't break the RF,
# but we keep 'total_population' as it might be a useful feature alongside the ratios.
joined_max_data <- master_20 %>%
  inner_join(county_wide %>% select(-state, -county), by = "county_fips")

# 2. Define the execution function for these specific custom models
run_max_demo_model <- function(joined_df, model_name, include_embeddings) {
  
  output_file <- paste0("data/results/", model_name, ".csv")
  imp_output_file <- paste0("data/results/", model_name, "_imp.csv")
  
  cat("\n==========================================\n")
  cat("RUNNING CUSTOM MODEL:", toupper(model_name), "\n")
  cat("==========================================\n")
  
  # Base feature selection: Contextual predictors + ALL our new demographic cols
  if (include_embeddings == FALSE) {
    data_rf <- joined_df %>% 
      select(
        county_fips, TRUMPSHARE = rep_share, 
        area_km2, vote_density, 
        pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, 
        unemployment_rate, civilian_labor_force, 
        total_population, starts_with("race") # captures all 12 freq and 12 ratio columns
      ) %>% 
      na.omit()
  } else {
    # Full kitchen sink: Everything above + all the embeddings
    data_rf <- joined_df %>% 
      select(
        county_fips, TRUMPSHARE = rep_share, 
        area_km2, vote_density, 
        pct_less_than_hs, pct_hs_only, pct_some_college, pct_bachelors_plus, 
        unemployment_rate, civilian_labor_force, 
        total_population, starts_with("race"), 
        ends_with("_mean")
      ) %>% 
      na.omit()
  }
  
  rf_formula <- TRUMPSHARE ~ . - county_fips - fold_id
  n_features <- ncol(data_rf) - 2 # subtracting TRUMPSHARE and county_fips
  
  my_mtry <- max(1, floor(n_features / 3))
  
  # Standard 5-fold random CV
  set.seed(10)
  data_rf <- data_rf %>% mutate(fold_id = sample(rep(1:5, length.out = n())))
  
  all_predictions <- list()
  fold_importances <- list() 
  
  for(k in 1:5) {
    train_df <- data_rf %>% filter(fold_id != k)
    test_df  <- data_rf %>% filter(fold_id == k)
    
    rf_model <- ranger(
      formula = rf_formula, 
      data = train_df, 
      num.trees = 500, 
      mtry = my_mtry, 
      importance = "impurity", 
      seed = 42
    )
    
    preds <- predict(rf_model, data = test_df)$predictions
    test_df$Predicted_Share <- preds
    all_predictions[[k]] <- test_df
    fold_importances[[k]] <- rf_model$variable.importance
  }
  
  # Save Results
  full_results <- bind_rows(all_predictions) %>% 
    mutate(Error = Predicted_Share - TRUMPSHARE, Abs_Error = abs(Error))
  write_csv(full_results, output_file)
  
  # Average and Save Importance
  avg_importance <- bind_rows(lapply(fold_importances, function(x) as.data.frame(t(x)))) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(everything(), names_to = "Feature", values_to = "Importance") %>%
    arrange(desc(Importance))
  write_csv(avg_importance, imp_output_file)
  
  cat("-> Saved:", output_file, "\n-> Saved:", imp_output_file, "\n")
}

# 3. Execute the two models
run_max_demo_model(joined_df = joined_max_data, model_name = "county20_max_demo", include_embeddings = FALSE)
run_max_demo_model(joined_df = joined_max_data, model_name = "county20_max_demo_emb", include_embeddings = TRUE)

cat("\n========================================================\n")
cat("SUCCESS: Custom demographic models completed!\n")
cat("========================================================\n")


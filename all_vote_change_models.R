library(tidyverse)
library(ranger)

# ==========================================
# 1. Load Data and Find Common Embeddings
# ==========================================

cat("Loading datasets...\n")
df_16 <- read_csv("data/prep/counties/clean_county_emb_16_17.csv", show_col_types = FALSE)
df_20 <- read_csv("data/prep/counties/clean_county_emb_20.csv", show_col_types = FALSE)
df_24 <- read_csv("data/prep/counties/clean_county_emb_24.csv", show_col_types = FALSE)

emb_cols_16 <- names(df_16) %>% keep(~ str_detect(.x, "_mean$"))
emb_cols_20 <- names(df_20) %>% keep(~ str_detect(.x, "_mean$"))
emb_cols_24 <- names(df_24) %>% keep(~ str_detect(.x, "_mean$"))
common_emb <- Reduce(intersect, list(emb_cols_16, emb_cols_20, emb_cols_24))

cat("Found", length(common_emb), "shared *_mean embedding columns.\n")

# ==========================================
# 2. Prepare 16-20 and 20-24 Datasets
# ==========================================

prep_shift_data <- function(df_early, df_late, early_suffix, late_suffix) {
  # Clean and rename base tables
  d1 <- df_early %>% select(county_fips, rep_share, total_votes, all_of(common_emb)) %>%
    rename_with(~ paste0(.x, "_", early_suffix), all_of(common_emb))
  
  d2 <- df_late %>% select(county_fips, county_name, state_po, vote_density, rep_share, total_votes, all_of(common_emb)) %>%
    rename_with(~ paste0(.x, "_", late_suffix), all_of(common_emb))
  
  # Join and calculate targets/turnout
  joined <- inner_join(d1, d2, by = "county_fips") %>%
    mutate(
      SHIFT = rep_share.y - rep_share.x,
      VOTE_CHANGE = (total_votes.y - total_votes.x) / total_votes.x
    ) %>%
    select(county_fips, county_name, state_po, vote_density, 
           rep_share_early = rep_share.x, rep_share_late = rep_share.y, 
           SHIFT, VOTE_CHANGE, everything(), -total_votes.x, -total_votes.y)
  
  # Calculate Deltas safely
  deltas <- purrr::map_dfc(common_emb, function(nm) {
    joined[[paste0(nm, "_", late_suffix)]] - joined[[paste0(nm, "_", early_suffix)]]
  })
  names(deltas) <- paste0(common_emb, "_delta")
  
  bind_cols(joined, deltas) %>% na.omit()
}

cat("\nPreparing specific election cycle datasets...\n")
data_1620 <- prep_shift_data(df_16, df_20, "16", "20")
data_2024 <- prep_shift_data(df_20, df_24, "20", "24")

# ==========================================
# 3. Apply 2x IQR Filter for Turnout Changes
# ==========================================

filter_turnout_iqr <- function(df, period_name) {
  n_before <- nrow(df)
  q1 <- quantile(df$VOTE_CHANGE, 0.25, na.rm = TRUE)
  q3 <- quantile(df$VOTE_CHANGE, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 2 * iqr
  upper_bound <- q3 + 2 * iqr
  
  df_clean <- df %>% filter(VOTE_CHANGE >= lower_bound & VOTE_CHANGE <= upper_bound)
  n_after <- nrow(df_clean)
  
  cat("[IQR Filter |", period_name, "] Kept", n_after, "of", n_before, 
      "counties. (Bounds:", round(lower_bound*100, 1), "% to", round(upper_bound*100, 1), "%)\n")
  
  return(df_clean %>% select(-VOTE_CHANGE))
}

data_1620 <- filter_turnout_iqr(data_1620, "2016-2020")
data_2024 <- filter_turnout_iqr(data_2024, "2020-2024")

# ==========================================
# 4. Define Helper to Select Predictors
# ==========================================

select_model_data <- function(df, model_num) {
  # All models keep these identifiers and the target
  base_cols <- c("county_fips", "county_name", "state_po", "vote_density", "rep_share_early", "rep_share_late", "SHIFT")
  
  if (model_num %in% c("m1", "m4")) {
    # M1/M4: Late year embeddings
    target_suffix <- if("A01_mean_20" %in% names(df)) "_20$" else "_24$"
    return(df %>% select(all_of(base_cols), matches(target_suffix)))
  } else if (model_num %in% c("m2", "m5")) {
    # M2/M5: Early year embeddings
    target_suffix <- if("A01_mean_16" %in% names(df)) "_16$" else "_20$"
    return(df %>% select(all_of(base_cols), matches(target_suffix)))
  } else if (model_num %in% c("m3", "m6")) {
    # M3/M6: Delta embeddings
    return(df %>% select(all_of(base_cols), ends_with("_delta")))
  }
}

# ==========================================
# 5. Modeling Function: Random Spatial CV
# ==========================================

run_spatial_change <- function(df, period_name, model_num) {
  out_name     <- paste0("data/results/change", period_name, "_", model_num, "_results.csv")
  imp_out_name <- paste0("data/results/change", period_name, "_", model_num, "_imp.csv")
  
  cat("\n--- Running Spatial", toupper(model_num), "for", period_name, "---\n")
  
  mod_df <- select_model_data(df, model_num)
  
  set.seed(42)
  mod_df <- mod_df %>% mutate(fold_id = sample(rep(1:5, length.out = n())))
  
  n_features <- ncol(mod_df) - 8 
  my_mtry <- min(8, max(1, floor(n_features / 3)))
  
  all_preds <- list()
  all_imps  <- list() # Store importances from each fold
  
  for (k in 1:5) {
    train_fold <- mod_df %>% filter(fold_id != k)
    test_fold  <- mod_df %>% filter(fold_id == k)
    
    rf <- ranger(
      SHIFT ~ . - county_fips - county_name - state_po - vote_density - rep_share_early - rep_share_late - fold_id,
      data = train_fold, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42
    )
    
    test_fold$Predicted_Shift <- predict(rf, data = test_fold)$predictions
    all_preds[[k]] <- test_fold
    all_imps[[k]]  <- enframe(rf$variable.importance, name = "Feature", value = paste0("imp_fold_", k))
  }
  
  # Save predictions
  results <- bind_rows(all_preds) %>%
    mutate(Error = Predicted_Shift - SHIFT, Abs_Error = abs(Error))
  write_csv(results, out_name)
  cat("Saved ->", out_name, "\n")
  
  # Average importances across the 5 folds and save
  imp_results <- reduce(all_imps, full_join, by = "Feature") %>%
    rowwise() %>%
    mutate(Importance = mean(c_across(starts_with("imp_fold_")), na.rm = TRUE)) %>%
    select(Feature, Importance) %>%
    arrange(desc(Importance))
  
  write_csv(imp_results, imp_out_name)
  cat("Saved ->", imp_out_name, "\n")
}

# ==========================================
# 6. Modeling Function: Temporal Transfer
# ==========================================

run_temporal_change <- function(train_df, test_df, model_num) {
  out_name     <- paste0("data/results/change_", model_num, "_results.csv")
  imp_out_name <- paste0("data/results/change_", model_num, "_imp.csv")
  
  cat("\n--- Running Temporal", toupper(model_num), "(Train: 16-20 -> Test: 20-24) ---\n")
  
  # Ensure column names match perfectly
  train_mod <- select_model_data(train_df, model_num)
  names(train_mod) <- str_replace(names(train_mod), "_16$|_20$|_24$", "")
  
  test_mod <- select_model_data(test_df, model_num)
  names(test_mod) <- str_replace(names(test_mod), "_16$|_20$|_24$", "")
  
  set.seed(55)
  train_mod <- train_mod %>% mutate(fold_id = sample(rep(1:5, length.out = n())))
  
  n_features <- ncol(train_mod) - 8 
  my_mtry <- min(8, max(1, floor(n_features / 3)))
  
  test_preds_by_fold <- list()
  all_imps           <- list() # Store importances from each fold
  
  for (k in 1:5) {
    t_fold <- train_mod %>% filter(fold_id != k)
    
    rf <- ranger(
      SHIFT ~ . - county_fips - county_name - state_po - vote_density - rep_share_early - rep_share_late - fold_id,
      data = t_fold, num.trees = 500, mtry = my_mtry, importance = "impurity", seed = 42
    )
    
    tmp <- test_mod %>% mutate(Predicted_Shift = predict(rf, data = test_mod)$predictions)
    test_preds_by_fold[[k]] <- tmp
    all_imps[[k]] <- enframe(rf$variable.importance, name = "Feature", value = paste0("imp_fold_", k))
  }
  
  # Average predictions across the 5 independently-trained models
  results <- bind_rows(test_preds_by_fold) %>%
    group_by(county_fips, county_name, state_po, vote_density, rep_share_early, rep_share_late, SHIFT) %>%
    summarise(Predicted_Shift = mean(Predicted_Shift), .groups = "drop") %>%
    mutate(Error = Predicted_Shift - SHIFT, Abs_Error = abs(Error))
  
  write_csv(results, out_name)
  cat("Saved ->", out_name, "\n")
  
  # Average importances across the 5 folds and save
  imp_results <- reduce(all_imps, full_join, by = "Feature") %>%
    rowwise() %>%
    mutate(Importance = mean(c_across(starts_with("imp_fold_")), na.rm = TRUE)) %>%
    select(Feature, Importance) %>%
    arrange(desc(Importance))
  
  write_csv(imp_results, imp_out_name)
  cat("Saved ->", imp_out_name, "\n")
}

# ==========================================
# 7. Execute All 9 Models
# ==========================================

cat("\n==========================================\n")
cat("EXECUTING SPATIAL MODELS (1-3)\n")
cat("==========================================\n")
# M1: Late Embeddings
run_spatial_change(data_1620, "16-20", "m1")
run_spatial_change(data_2024, "20-24", "m1")

# M2: Early Embeddings
run_spatial_change(data_1620, "16-20", "m2")
run_spatial_change(data_2024, "20-24", "m2")

# M3: Delta Embeddings
run_spatial_change(data_1620, "16-20", "m3")
run_spatial_change(data_2024, "20-24", "m3")

cat("\n==========================================\n")
cat("EXECUTING TEMPORAL MODELS (4-6)\n")
cat("==========================================\n")
# M4: Temporal (Late Embeddings)
run_temporal_change(data_1620, data_2024, "m4")

# M5: Temporal (Early Embeddings)
run_temporal_change(data_1620, data_2024, "m5")

# M6: Temporal (Delta Embeddings)
run_temporal_change(data_1620, data_2024, "m6")

cat("\nALL 9 MODELS SUCCESSFULLY TRAINED AND SAVED!\n")
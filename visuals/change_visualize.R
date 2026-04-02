library(tidyverse)
library(scales)
library(patchwork)

# ==============================================================================
# VOTE SWING / CHANGE MODEL VISUALIZATION FUNCTION
# ==============================================================================

visualize_change_model <- function(model_num, period = "20-24") {
  
  # 1. Handle File Naming Conventions (Spatial vs. Temporal)
  is_temporal <- model_num %in% c("m4", "m5", "m6")
  
  if (is_temporal) {
    base_name <- paste0("change_", model_num)
    dash_title <- paste("Temporal Swing Dashboard: Model", toupper(model_num))
  } else {
    base_name <- paste0("change", period, "_", model_num)
    dash_title <- paste("Spatial Swing Dashboard:", period, "| Model", toupper(model_num))
  }
  
  results_file <- paste0("data/results/", base_name, "_results.csv")
  imp_file     <- paste0("data/results/", base_name, "_imp.csv")
  
  if(!file.exists(results_file) | !file.exists(imp_file)) {
    stop(paste("Missing files! Ensure both", results_file, "and", imp_file, "exist."))
  }
  
  # 2. Load Data
  results  <- read_csv(results_file, show_col_types = FALSE)
  imp_data <- read_csv(imp_file, show_col_types = FALSE)
  
  # ==========================================
  # 3. Calculate and Print Overall Metrics
  # ==========================================
  
  rss  <- sum(results$Error^2, na.rm = TRUE)
  tss  <- sum((results$SHIFT - mean(results$SHIFT, na.rm = TRUE))^2, na.rm = TRUE)
  r2   <- 1 - (rss / tss)
  rmse <- sqrt(mean(results$Error^2, na.rm = TRUE))
  mae  <- mean(results$Abs_Error, na.rm = TRUE)
  
  cat("\n==========================================\n")
  cat(toupper(dash_title), "\n")
  cat("==========================================\n")
  cat("R-Squared: ", round(r2, 3), "\n")
  cat("RMSE:      ", round(rmse, 4), "(", round(rmse*100, 2), "pp )\n")
  cat("MAE:       ", round(mae, 4), "(", round(mae*100, 2), "pp )\n")
  cat("==========================================\n\n")
  
  # ==========================================
  # 4. Generate Plots (Ordered to match County layout)
  # ==========================================
  
  # --- Plot 1: Error Bias by Baseline Partisanship (Top-Left) ---
  p1 <- ggplot(results, aes(x = rep_share_early, y = Error)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.4, size = 0.8) +
    geom_smooth(method = "loess", color = "blue", fill = "gray90") +
    labs(
      title = "Where were the surprises?",
      subtitle = "+ = expected more pro-Trump shift",
      x = "Early Year GOP Vote Share (Baseline)",
      y = "Error (Pred Shift - Actual Shift)"
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_minimal()
  
  # --- Plot 2: R² by Vote Density Decile (Top-Right) ---
  r2_fun <- function(actual, pred) {
    rss_val <- sum((pred - actual)^2, na.rm = TRUE)
    tss_val <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
    if (is.na(tss_val) || tss_val == 0) return(NA_real_)
    1 - rss_val / tss_val
  }
  
  r2_by_decile <- results %>%
    mutate(density_decile = ntile(vote_density, 10)) %>%
    group_by(density_decile) %>%
    summarise(
      n = n(),
      r2 = r2_fun(SHIFT, Predicted_Shift),
      .groups = "drop"
    )
  
  p2 <- ggplot(r2_by_decile, aes(x = factor(density_decile), y = r2)) +
    geom_col(fill = "coral2") +
    geom_text(aes(label = round(r2, 2)), vjust = -0.5, size = 3) + # Added text labels
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Model Performance by Density",
      subtitle = "Out-of-sample R² by Vote Density Decile",
      x = "Density Decile (1 = Rural, 10 = Urban)",
      y = "R² (within decile)"
    ) +
    theme_minimal()
  
  # --- Plot 3: Actual vs. Predicted Shift (Bottom-Left) ---
  p3 <- ggplot(results, aes(x = SHIFT, y = Predicted_Shift)) +
    geom_point(alpha = 0.3, size = 1, color = "purple") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", color = "orange", se = FALSE) +
    labs(
      title = "Can Embeddings Predict Swings?",
      subtitle = paste0("R² = ", round(r2, 3), " | RMSE = ", round(rmse, 4)),
      x = "Actual Shift",
      y = "Predicted Shift"
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
  
  # --- Plot 4: Top Predictive Features (Bottom-Right) ---
  top_imp <- imp_data %>%
    slice_max(order_by = Importance, n = 15)
  
  p4 <- ggplot(top_imp, aes(x = Importance, y = reorder(Feature, Importance))) +
    geom_col(fill = "darkcyan") +
    labs(
      title = "Which Features Predict the Swing?",
      subtitle = "Top Feature Importances",
      x = "Variable Importance",
      y = NULL
    ) +
    theme_minimal()
  
  # ==========================================
  # 5. Assemble and Print 2x2 Grid
  # ==========================================
  
  # (p1: Error, p2: Decile) over (p3: Predicted, p4: Importance)
  grid_plot <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = dash_title,
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  print(grid_plot)
}

# ==============================================================================
# HOW TO RUN IT
# ==============================================================================

# Spatial Models (requires period):
# visualize_change_model(model_num = "m1", period = "16-20")
# visualize_change_model(model_num = "m3", period = "20-24")

# Temporal Models (period is ignored, automatically handles naming):
# visualize_change_model(model_num = "m4")
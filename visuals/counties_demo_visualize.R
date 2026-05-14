library(tidyverse)
library(scales)
library(patchwork) # For the 2x2 grid

# ==============================================================================
# CUSTOM MODEL VISUALIZATION FUNCTION
# ==============================================================================

visualize_custom_model <- function(model_name) {
  
  # 1. Define File Paths using the new custom naming convention
  results_file <- paste0("data/results/", model_name, ".csv")
  imp_file     <- paste0("data/results/", model_name, "_imp.csv")
  
  # Check if files exist
  if(!file.exists(results_file) | !file.exists(imp_file)) {
    stop("Could not find the results or importance files. Check your paths!")
  }
  
  # 2. Load Data
  full_results <- read_csv(results_file, show_col_types = FALSE)
  imp_data     <- read_csv(imp_file, show_col_types = FALSE)
  
  # ==========================================
  # 3. Overall Performance Metrics
  # ==========================================
  
  rss  <- sum(full_results$Error^2)
  tss  <- sum((full_results$TRUMPSHARE - mean(full_results$TRUMPSHARE))^2)
  r2   <- 1 - (rss / tss)
  rmse <- sqrt(mean(full_results$Error^2))
  mae  <- mean(full_results$Abs_Error)
  
  cat("\n==========================================\n")
  cat(toupper(paste("OVERALL RESULTS:", model_name)), "\n")
  cat("==========================================\n")
  cat("R-Squared:       ", round(r2, 3), "\n")
  cat("RMSE:            ", round(rmse, 4), "\n")
  cat("Mean Abs Error:  ", round(mae, 4), "\n")
  cat("==========================================\n\n")
  
  # ==========================================
  # 4. Generate Plots
  # ==========================================
  
  # --- Plot 1: Prediction Error vs. Density ---
  p1 <- ggplot(full_results, aes(x = vote_density, y = Error)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    scale_x_log10(labels = scales::comma) + 
    labs(
      title = "Prediction Error vs. Voter Density",
      subtitle = paste0("R² = ", round(r2, 3), " | RMSE = ", round(rmse, 3)),
      x = "Voters per km² (Log Scale)",
      y = "Error (Predicted - Actual)"
    ) +
    theme_minimal()
  
  # --- Plot 2: R-squared per Density Decile ---
  decile_stats <- full_results %>%
    mutate(density_decile = ntile(vote_density, 10)) %>%
    group_by(density_decile) %>%
    summarize(
      RSS = sum(Error^2),
      TSS = sum((TRUMPSHARE - mean(TRUMPSHARE))^2),
      R2 = if_else(TSS > 1e-6, 1 - (RSS / TSS), 0),
      .groups = "drop"
    )
  
  p2 <- ggplot(decile_stats, aes(x = factor(density_decile), y = R2)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = round(R2, 2)), vjust = -0.5, size = 3) +
    labs(
      title = "Model Performance by Density Decile",
      subtitle = "Decile 1 = Most Rural, 10 = Most Urban",
      x = "Density Decile",
      y = "R-squared"
    ) +
    theme_minimal()
  
  # --- Plot 3: Predicted vs. Actual ---
  p3 <- ggplot(full_results, aes(x = TRUMPSHARE, y = Predicted_Share)) +
    geom_point(alpha = 0.4, size = 1, color = "darkslategrey") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", color = "blue", se = TRUE) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = "Actual vs. Predicted Trump Vote Share",
      subtitle = paste0("County-Level | R² = ", round(r2, 3)),
      x = "Actual Trump Share",
      y = "Predicted Trump Share",
      caption = "Red dashed line represents perfect prediction."
    ) +
    theme_minimal()
  
  # --- Plot 4: Variable Importance ---
  top_imp <- imp_data %>%
    slice_max(order_by = Importance, n = 15)
  
  p4 <- ggplot(top_imp, aes(x = Importance, y = reorder(Feature, Importance))) +
    geom_col(fill = "forestgreen") +
    labs(
      title = "Top Feature Importances",
      subtitle = "Mean Impurity Decrease",
      x = "Importance",
      y = NULL
    ) +
    theme_minimal()
  
  # ==========================================
  # 5. Assemble and Print 2x2 Grid
  # ==========================================
  
  grid_plot <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = paste("Diagnostic Dashboard:", toupper(model_name)),
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  print(grid_plot)
}

# ==============================================================================
# HOW TO RUN IT
# ==============================================================================

# Call the function for your newly created custom models:
# visualize_custom_model("county20_max_demo")
# visualize_custom_model("county20_max_demo_emb")
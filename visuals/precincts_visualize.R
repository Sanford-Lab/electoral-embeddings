library(tidyverse)
library(scales)
library(patchwork) # Required for the 2x2 plot grid

# ==============================================================================
# MODEL VISUALIZATION FUNCTION
# ==============================================================================

visualize_precinct_model <- function(year_label, model_type) {
  
  # 1. Define File Paths
  results_file <- paste0("data/results/precinct", year_label, "_", model_type, "_results.csv")
  imp_file     <- paste0("data/results/precinct", year_label, "_", model_type, "_imp.csv")
  
  # Check if files exist before proceeding
  if(!file.exists(results_file) | !file.exists(imp_file)) {
    stop("Could not find the results or importance files. Check your file paths!")
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
  cat(toupper(paste("OVERALL", year_label, "PRECINCT CV RESULTS:", model_type)), "\n")
  cat("==========================================\n")
  cat("R-Squared:      ", round(r2, 3), "\n")
  cat("RMSE:           ", round(rmse, 4), "\n")
  cat("Mean Abs Error: ", round(mae, 4), "\n")
  cat("==========================================\n\n")
  
  # ==========================================
  # 4. Generate Plots
  # ==========================================
  
  # --- Plot 1: Prediction Error vs. Density ---
  p1 <- ggplot(full_results, aes(x = vote_density, y = Error)) +
    geom_point(alpha = 0.1, size = 0.2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    scale_x_log10(labels = scales::comma) + 
    labs(
      title = "Error vs. Voter Density",
      subtitle = paste0("R2 = ", round(r2, 3), " | RMSE = ", round(rmse, 3)),
      x = "Voters per km2 (Log Scale)",
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
      title = "Performance by Density Decile",
      subtitle = "Decile 1 = Most Rural, 10 = Most Urban",
      x = "Density Decile",
      y = "R-squared"
    ) +
    theme_minimal()
  
  # --- Plot 3: Predicted vs. Actual ---
  p3 <- ggplot(full_results, aes(x = TRUMPSHARE, y = Predicted_Share)) +
    geom_bin2d(bins = 100) + 
    scale_fill_viridis_c(option = "magma", trans = "log10") + 
    geom_abline(slope = 1, intercept = 0, color = "white", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", color = "cyan", se = FALSE) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = "Actual vs. Predicted",
      subtitle = "Dashed line represents perfect prediction",
      x = "Actual Vote Share",
      y = "Predicted Vote Share",
      fill = "Count (Log)"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # --- Plot 4: Variable Importance ---
  # Extract top 15 features for readability
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
  
  # Use patchwork to stitch them together
  grid_plot <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = paste("Diagnostic Dashboard: Model", toupper(model_type), "-", year_label),
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  print(grid_plot)
  
  # Optional: Return the plot object silently in case you want to save it later
  invisible(grid_plot)
}

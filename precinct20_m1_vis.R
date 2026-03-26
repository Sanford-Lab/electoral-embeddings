library(tidyverse)
library(scales) # for comma labels in plots

# ==========================================
# 4. Overall Performance Metrics
# ==========================================

full_results <- read_csv("data/results/precinct20_m1_results.csv")
#full_results <- read_csv("data/results/precinct24_m1_results.csv")

# Calculate metrics on the full dataset (all folds combined)
rss  <- sum(full_results$Error^2)
tss  <- sum((full_results$TRUMPSHARE - mean(full_results$TRUMPSHARE))^2)
r2   <- 1 - (rss / tss)
rmse <- sqrt(mean(full_results$Error^2))

cat("\n==========================================")
cat("\nOVERALL 2020 PRECINCT CV RESULTS")
cat("\n==========================================")
cat("\nR-Squared: ", round(r2, 3))
cat("\nRMSE:      ", round(rmse, 4))
cat("\nMean Abs Error:", round(mean(full_results$Abs_Error), 4))
cat("\n==========================================\n")

# ==========================================
# 5. Plot 1: Prediction Error vs. Density
# ==========================================

# Using log10 scale because precinct density varies wildly
plot1 <- ggplot(full_results, aes(x = vote_density, y = Error)) +
  geom_point(alpha = 0.1, size = 0.2) + # High transparency for dense data
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  scale_x_log10(labels = scales::comma) + 
  labs(
    title = "2020 Precinct Prediction Error vs. Voter Density (5-Fold CV)",
    subtitle = paste0("R² = ", round(r2, 3), " | RMSE = ", round(rmse, 3)),
    x = "Voters per km² (Log Scale)",
    y = "Error (Predicted - Actual)"
  ) +
  theme_minimal()

print(plot1)

# ==========================================
# 6. Plot 2: R-squared per Density Decile
# ==========================================

decile_stats <- full_results %>%
  mutate(density_decile = ntile(vote_density, 10)) %>%
  group_by(density_decile) %>%
  summarize(
    avg_density = median(vote_density),
    n = n(),
    RSS = sum(Error^2),
    TSS = sum((TRUMPSHARE - mean(TRUMPSHARE))^2),
    # Safety check: avoid dividing by zero if TSS is 0
    R2 = if_else(TSS > 1e-6, 1 - (RSS / TSS), 0)
  )

plot2 <- ggplot(decile_stats, aes(x = factor(density_decile), y = R2)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(R2, 2)), vjust = -0.5) +
  labs(
    title = "2020 Model Performance by Density Decile (Precincts)",
    subtitle = "Decile 1 = Most Rural, Decile 10 = Most Urban",
    x = "Density Decile",
    y = "R-squared"
  ) +
  theme_minimal()

print(plot2)

# ==========================================
# 7. Plot 3: Predicted vs. Actual
# ==========================================

# We use a 2D density heatmap (geom_bin2d) because there are too many 
# precincts for a standard scatter plot to be readable.
plot3 <- ggplot(full_results, aes(x = TRUMPSHARE, y = Predicted_Share)) +
  # Create a heatmap of point density
  geom_bin2d(bins = 100) + 
  scale_fill_viridis_c(option = "magma", trans = "log10") + # Log scale help see low-density areas
  # Add the "Perfect Prediction" line
  geom_abline(slope = 1, intercept = 0, color = "white", linetype = "dashed", size = 1) +
  # Add a trend line to see if the model under/over predicts at the extremes
  geom_smooth(method = "lm", color = "cyan", se = FALSE) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "2020 Actual vs. Predicted Trump Share (Precinct Level)",
    subtitle = paste0("R² = ", round(r2, 3), " | Dashed line represents perfect prediction"),
    x = "Actual Vote Share",
    y = "Predicted Vote Share",
    fill = "Precinct Count\n(Log Scale)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(plot3)
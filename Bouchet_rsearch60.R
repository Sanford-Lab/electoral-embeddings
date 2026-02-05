# Force immediate output to the log file (prevents 24-hour "blackouts")
sink(stdout(), type = "output", split = TRUE)

# Load necessary libraries
library(xgboost)
library(caret)
library(dplyr)
library(doParallel)

cat("--- Job Started at:", as.character(Sys.time()), "---\n")

# Load data
cat("Reading data...\n")
df <- read.csv("project_pi_ls2375/ccs74/processed_area_std_uuid.csv")
cat("Data loaded. Dimensions:", nrow(df), "rows x", ncol(df), "cols\n")

# 1. Setup Parallel Processing (Pulling core count from Slurm)
# -------------------------------------------------------------------------
cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", 1))
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
cat("Using", cores, "cores for parallel tuning...\n")

# 2. Data Preparation
# -------------------------------------------------------------------------
set.seed(42)

data_modeling <- df %>%
  select(TRUMPSHARE, precinct_area, matches("A[0-9]{2}_(mean|skew|stdDev)")) %>%
  na.omit()

cat("Modeling data filtered. Rows remaining:", nrow(data_modeling), "\n")

# Split into Train (80%) and Test (20%)
train_idx <- createDataPartition(data_modeling$TRUMPSHARE, p = 0.8, list = FALSE)
train_df  <- data_modeling[train_idx, ]
test_df   <- data_modeling[-train_idx, ]

# 3. Define Train Control for Random Search
# -------------------------------------------------------------------------
train_control <- trainControl(
  method = "cv",
  number = 5,              # 5-fold Cross-Validation
  search = "random",       # ENABLE RANDOM SEARCH
  allowParallel = TRUE,
  verboseIter = TRUE       # Now visible in real-time due to sink()
)

# 4. Train the Model
# -------------------------------------------------------------------------
cat("Starting Random Search (60 combinations). This is more efficient than Grid Search...\n")

xgb_model <- train(
  TRUMPSHARE ~ .,
  data = train_df,
  method = "xgbTree",
  trControl = train_control,
  tuneLength = 60,         # Caret will pick 40 random hyperparameter sets
  metric = "Rsquared"
)

# Stop the cluster once finished
stopCluster(cl)
cat("Model training complete at:", as.character(Sys.time()), "\n")

# 5. Evaluate on Test Set
# -------------------------------------------------------------------------
predictions <- predict(xgb_model, newdata = test_df)
results <- postResample(predictions, test_df$TRUMPSHARE)
print(results)

# 6. Check Feature Importance
# -------------------------------------------------------------------------
importance <- varImp(xgb_model, scale = FALSE)
print(importance)

# Save the best model
saveRDS(xgb_model, "best_trump_share_model_60.rds")
cat("Model saved successfully as best_trump_share_model.rds\n")
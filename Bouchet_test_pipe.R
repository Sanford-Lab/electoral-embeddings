sink(stdout(), type = "output", split = TRUE)

library(xgboost)
library(caret)
library(dplyr)
library(doParallel)

cat("--- SMOKE TEST STARTED at:", as.character(Sys.time()), "---\n")

# Load data
cat("Reading data...\n")
df <- read.csv("project_pi_ls2375/ccs74/processed_area_std_uuid.csv")

# 1. Setup Parallel Processing (PSOCK style with buffer)
cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", 1))
cl <- makePSOCKcluster(cores - 1) 
registerDoParallel(cl)
cat("Using", cores, "cores (Manager +", cores-1, "Workers)...\n")

# 2. Data Preparation
set.seed(42)
data_modeling <- df %>%
  select(TRUMPSHARE, precinct_area, matches("A[0-9]{2}_(mean|skew|stdDev)")) %>%
  na.omit()

# --- SPEED MODIFICATION: SUBSAMPLE 5% FOR TEST ---
data_modeling <- data_modeling %>% sample_frac(0.05)
cat("TEST MODE: Using 5% sample. Rows remaining:", nrow(data_modeling), "\n")

# Split into Train/Test
train_idx <- createDataPartition(data_modeling$TRUMPSHARE, p = 0.8, list = FALSE)
train_df  <- data_modeling[train_idx, ]
test_df   <- data_modeling[-train_idx, ]

# 3. Define Train Control
train_control <- trainControl(
  method = "cv",
  number = 3,              # SPEED MODIFICATION: 3-fold instead of 5
  search = "random",
  allowParallel = TRUE,
  verboseIter = TRUE
)

# 4. Train the Model (Smoke Test)
cat("Starting Smoke Test Random Search (2 combinations)...\n")

xgb_model <- train(
  TRUMPSHARE ~ .,
  data = train_df,
  method = "xgbTree",
  trControl = train_control,
  tuneLength = 2,          # SPEED MODIFICATION: Only 2 combos
  metric = "Rsquared",
  nthread = 1,             # Essential for parallel stability
  nrounds = 50             # SPEED MODIFICATION: Cap tree count
)

stopCluster(cl)
cat("Smoke test training complete at:", as.character(Sys.time()), "\n")

# 5. Evaluate and Save
predictions <- predict(xgb_model, newdata = test_df)
print(postResample(predictions, test_df$TRUMPSHARE))

saveRDS(xgb_model, "smoke_test_model.rds")
cat("Test successful. Model saved as smoke_test_model.rds\n")
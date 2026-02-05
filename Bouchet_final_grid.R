sink(stdout(), type = "output", split = TRUE)
library(xgboost); library(caret); library(dplyr); library(doParallel)

cat("--- SURGICAL GRID START:", as.character(Sys.time()), "---\n")

# Load data
df <- read.csv("project_pi_ls2375/ccs74/processed_area_std_uuid.csv")

# 1. Parallel Setup
cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", 1))
cl <- makePSOCKcluster(cores - 1) 
registerDoParallel(cl)

# 2. Data Prep
set.seed(42)
data_modeling <- df %>%
  select(TRUMPSHARE, precinct_area, matches("A[0-9]{2}_(mean|skew|stdDev)")) %>%
  na.omit()

# Safety: Remove constant columns
nzv <- nearZeroVar(data_modeling)
if(length(nzv) > 0) data_modeling <- data_modeling[, -nzv]

train_idx <- createDataPartition(data_modeling$TRUMPSHARE, p = 0.8, list = FALSE)
train_df  <- data_modeling[train_idx, ]
test_df   <- data_modeling[-train_idx, ]

# 3. Targeted Grid for High-Noise / High-Tree Count
# ---------------------------------------------------------
# We are testing 18 high-quality combinations
xgb_grid <- expand.grid(
  nrounds = c(1000, 2000, 3000),      # Your requested tree counts
  max_depth = c(4, 6),                # Shallower trees = less noise sensitivity
  eta = c(0.01, 0.05, 0.1),           # Slower learning for more trees
  gamma = 0.7,                        # Complexity penalty (from your winning 60-run)
  colsample_bytree = 0.5,             # Only use 50% of features per tree (Anti-noise)
  min_child_weight = 15,              # Minimum data per leaf (Anti-noise)
  subsample = 0.7                     # Row subsampling
)

train_control <- trainControl(
  method = "cv", number = 5,
  allowParallel = TRUE, verboseIter = TRUE
)

# 4. Training
cat("Starting Surgical Grid Search (", nrow(xgb_grid), " combinations)...\n")
xgb_model <- train(
  TRUMPSHARE ~ ., data = train_df,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "Rsquared",
  nthread = 1
)

stopCluster(cl)

# 5. Save Results
saveRDS(xgb_model, "surgical_grid_model.rds")
cat("Final R2:", max(xgb_model$results$Rsquared, na.rm=TRUE), "\n")
cat("--- JOB FINISHED:", as.character(Sys.time()), "---\n")
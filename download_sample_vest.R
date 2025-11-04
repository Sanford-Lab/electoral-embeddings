# Download Sample VEST Data
# This script downloads a sample VEST dataset to test the precinct processing functionality

library(httr)
library(jsonlite)

# Create directories
dir.create("data/precincts/CA_2020", showWarnings = FALSE, recursive = TRUE)

cat("=== DOWNLOADING SAMPLE VEST DATA ===\n")
cat("This will download a small sample of California 2020 VEST data for testing\n\n")

# Try to download a sample VEST file directly
# Note: This is a simplified approach - in practice, you'd need to navigate the VEST API

# Alternative approach: Create a sample dataset for testing
create_sample_precinct_data <- function() {
  cat("Creating sample precinct data for testing...\n")
  
  # Create sample data that mimics VEST structure
  sample_data <- data.frame(
    GEOID = paste0("06", sprintf("%07d", 1:100)),  # California FIPS + precinct IDs
    VOTES_GOP = sample(100:5000, 100, replace = TRUE),
    VOTES_DEM = sample(100:5000, 100, replace = TRUE),
    COUNTY_NAME = sample(c("Los Angeles", "Orange", "San Diego", "Riverside", "San Bernardino"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      VOTES_GOP = as.numeric(VOTES_GOP),
      VOTES_DEM = as.numeric(VOTES_DEM),
      TOTAL_VOTES = VOTES_GOP + VOTES_DEM,
      TRUMP_SHARE = VOTES_GOP / TOTAL_VOTES
    )
  
  # Save as CSV for testing
  write.csv(sample_data, "data/precincts/CA_2020/sample_precinct_data.csv", row.names = FALSE)
  
  cat("Created sample precinct data with", nrow(sample_data), "precincts\n")
  cat("File saved to: data/precincts/CA_2020/sample_precinct_data.csv\n")
  
  # Print summary
  cat("\nSample data summary:\n")
  print(summary(sample_data$TRUMP_SHARE))
  
  return(sample_data)
}

# Create the sample data
sample_data <- create_sample_precinct_data()

cat("\n=== SAMPLE DATA CREATED ===\n")
cat("You can now test the precinct processing by running:\n")
cat("Rscript download_data.R\n\n")

cat("Note: This is sample data for testing. For real analysis, you need to:\n")
cat("1. Download actual VEST shapefiles from Harvard Dataverse\n")
cat("2. Place them in the appropriate state directories\n")
cat("3. The script will automatically process them\n")

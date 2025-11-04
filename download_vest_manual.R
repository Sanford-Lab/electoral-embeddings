# Manual VEST Data Download Helper
# This script provides instructions and creates sample data for testing

library(tidyverse)
library(sf)

cat("=== VEST DATA MANUAL DOWNLOAD GUIDE ===\n\n")

# Create directories
dir.create("data/precincts", showWarnings = FALSE, recursive = TRUE)

# Function to create sample precinct data for testing
create_sample_precinct_data <- function(state_abbr = "CA", n_precincts = 100) {
  cat("Creating sample precinct data for", state_abbr, "\n")
  
  # Create sample data that mimics VEST structure
  sample_data <- data.frame(
    GEOID = paste0(
      case_when(
        state_abbr == "CA" ~ "06",
        state_abbr == "TX" ~ "48", 
        state_abbr == "FL" ~ "12",
        state_abbr == "NY" ~ "36",
        TRUE ~ "00"
      ),
      sprintf("%07d", 1:n_precincts)
    ),
    VOTES_GOP = sample(50:2000, n_precincts, replace = TRUE),
    VOTES_DEM = sample(50:2000, n_precincts, replace = TRUE),
    COUNTY_NAME = case_when(
      state_abbr == "CA" ~ sample(c("Los Angeles", "Orange", "San Diego", "Riverside", "San Bernardino"), n_precincts, replace = TRUE),
      state_abbr == "TX" ~ sample(c("Harris", "Dallas", "Tarrant", "Bexar", "Travis"), n_precincts, replace = TRUE),
      state_abbr == "FL" ~ sample(c("Miami-Dade", "Broward", "Palm Beach", "Hillsborough", "Orange"), n_precincts, replace = TRUE),
      state_abbr == "NY" ~ sample(c("Kings", "Queens", "New York", "Suffolk", "Nassau"), n_precincts, replace = TRUE),
      TRUE ~ sample(c("County A", "County B", "County C"), n_precincts, replace = TRUE)
    ),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      VOTES_GOP = as.numeric(VOTES_GOP),
      VOTES_DEM = as.numeric(VOTES_DEM),
      TOTAL_VOTES = VOTES_GOP + VOTES_DEM,
      TRUMP_SHARE = VOTES_GOP / TOTAL_VOTES
    )
  
  # Create simple geometries (random points for testing)
  set.seed(123)
  geometries <- st_sfc(
    lapply(1:n_precincts, function(i) {
      st_point(c(
        runif(1, -125, -114),  # CA longitude range
        runif(1, 32, 42)       # CA latitude range
      ))
    })
  )
  
  # Create sf object
  precinct_sf <- st_sf(sample_data, geometry = geometries, crs = 4326)
  
  # Save as shapefile
  state_dir <- file.path("data/precincts", paste0(tolower(state_abbr), "_2020"))
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  
  shapefile_path <- file.path(state_dir, paste0(tolower(state_abbr), "_2020.shp"))
  st_write(precinct_sf, shapefile_path, delete_dsn = TRUE, quiet = TRUE)
  
  cat("Created sample shapefile:", shapefile_path, "\n")
  cat("Sample data summary for", state_abbr, ":\n")
  print(summary(sample_data$TRUMP_SHARE))
  
  return(sample_data)
}

# Create sample data for testing
cat("Creating sample VEST data for testing...\n\n")

states_to_create <- c("CA", "TX", "FL", "NY")
for (state in states_to_create) {
  create_sample_precinct_data(state, n_precincts = 50)
}

cat("\n=== MANUAL DOWNLOAD INSTRUCTIONS ===\n")
cat("For real VEST data, manually download from:\n")
cat("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H\n\n")

cat("Steps:\n")
cat("1. Go to the VEST dataset URL above\n")
cat("2. Download ZIP files for states you want (e.g., ca_2020.zip, tx_2020.zip)\n")
cat("3. Extract ZIP files to data/precincts/[state]_2020/ directories\n")
cat("4. Run the main download_data.R script to process the shapefiles\n\n")

cat("Expected directory structure:\n")
cat("data/precincts/\n")
cat("├── ca_2020/\n")
cat("│   ├── ca_2020.shp\n")
cat("│   ├── ca_2020.dbf\n")
cat("│   ├── ca_2020.shx\n")
cat("│   └── ca_2020.prj\n")
cat("├── tx_2020/\n")
cat("│   └── ...\n")
cat("└── ...\n\n")

cat("Sample data created! You can now test the processing pipeline.\n")
cat("Run: Rscript download_data.R\n")

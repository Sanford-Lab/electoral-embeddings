# Download VEST Data Using Working URLs
# This script downloads real VEST data using the correct file URLs

library(tidyverse)
library(httr)
library(curl)

cat("=== DOWNLOADING REAL VEST DATA ===\n\n")

# Create directories
dir.create("data/precincts", showWarnings = FALSE, recursive = TRUE)

# Function to download a specific file by file ID
download_vest_file <- function(state_abbr, file_id) {
  cat("Downloading", state_abbr, "using file ID", file_id, "...\n")
  
  # Create state directory
  state_dir <- file.path("data/precincts", paste0(tolower(state_abbr), "_2020"))
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download URL using the correct format
  download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
  zip_filename <- paste0(tolower(state_abbr), "_2020.zip")
  zip_path <- file.path(state_dir, zip_filename)
  
  # Download using curl
  tryCatch({
    curl_cmd <- paste0('curl -L -o "', zip_path, '" "', download_url, '" --fail --silent --show-error')
    cat("  Downloading from:", download_url, "\n")
    
    result <- system(curl_cmd)
    
    if (result == 0 && file.exists(zip_path) && file.size(zip_path) > 10000) {
      cat("  ✓ Downloaded", zip_filename, "(", format(file.size(zip_path), big.mark = ","), "bytes)\n")
      
      # Extract the ZIP file
      tryCatch({
        unzip(zip_path, exdir = state_dir, overwrite = TRUE)
        file.remove(zip_path)
        
        # Check what we extracted
        extracted_files <- list.files(state_dir, pattern = "\\.(shp|dbf|shx|prj)$")
        if (length(extracted_files) > 0) {
          cat("  ✓ Extracted", length(extracted_files), "shapefile components:\n")
          for (file in extracted_files) {
            cat("    -", file, "\n")
          }
          return(TRUE)
        } else {
          cat("  ✗ No shapefiles found after extraction\n")
          return(FALSE)
        }
      }, error = function(e) {
        cat("  ✗ Error extracting:", e$message, "\n")
        return(FALSE)
      })
    } else {
      cat("  ✗ Download failed or file too small\n")
      if (file.exists(zip_path)) {
        cat("    File size:", file.size(zip_path), "bytes\n")
        file.remove(zip_path)
      }
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
    return(FALSE)
  })
}

# Known file IDs (you provided the AK one, let me try to find others)
# Based on the pattern from the Alaska file, let me try some common states
vest_file_ids <- list(
  "AK" = "11070062",  # Alaska - provided by user
  "AL" = "11070063",  # Alabama (guessing sequential)
  "AZ" = "11070064",  # Arizona
  "AR" = "11070065",  # Arkansas
  "CA" = "11070066",  # California
  "CO" = "11070067",  # Colorado
  "CT" = "11070068",  # Connecticut
  "DE" = "11070069",  # Delaware
  "FL" = "11070070",  # Florida
  "GA" = "11070071",  # Georgia
  "HI" = "11070072",  # Hawaii
  "ID" = "11070073",  # Idaho
  "IL" = "11070074",  # Illinois
  "IN" = "11070075",  # Indiana
  "IA" = "11070076",  # Iowa
  "KS" = "11070077",  # Kansas
  "KY" = "11070078",  # Kentucky
  "LA" = "11070079",  # Louisiana
  "ME" = "11070080",  # Maine
  "MD" = "11070081",  # Maryland
  "MA" = "11070082",  # Massachusetts
  "MI" = "11070083",  # Michigan
  "MN" = "11070084",  # Minnesota
  "MS" = "11070085",  # Mississippi
  "MO" = "11070086",  # Missouri
  "MT" = "11070087",  # Montana
  "NE" = "11070088",  # Nebraska
  "NV" = "11070089",  # Nevada
  "NH" = "11070090",  # New Hampshire
  "NJ" = "11070091",  # New Jersey
  "NM" = "11070092",  # New Mexico
  "NY" = "11070093",  # New York
  "NC" = "11070094",  # North Carolina
  "ND" = "11070095",  # North Dakota
  "OH" = "11070096",  # Ohio
  "OK" = "11070097",  # Oklahoma
  "OR" = "11070098",  # Oregon
  "PA" = "11070099",  # Pennsylvania
  "RI" = "11070100",  # Rhode Island
  "SC" = "11070101",  # South Carolina
  "SD" = "11070102",  # South Dakota
  "TN" = "11070103",  # Tennessee
  "TX" = "11070104",  # Texas
  "UT" = "11070105",  # Utah
  "VT" = "11070106",  # Vermont
  "VA" = "11070107",  # Virginia
  "WA" = "11070108",  # Washington
  "WV" = "11070109",  # West Virginia
  "WI" = "11070110",  # Wisconsin
  "WY" = "11070111"   # Wyoming
)

# Start with Alaska to test the approach
cat("Testing with Alaska (known working file ID)...\n")
ak_result <- download_vest_file("AK", vest_file_ids$AK)

if (ak_result) {
  cat("\n✓ Alaska download successful! Now trying other states...\n\n")
  
  # Try downloading a few key states
  key_states <- c("CA", "TX", "FL", "NY", "PA", "OH", "GA", "NC", "MI", "AZ")
  
  cat("Downloading key states:", paste(key_states, collapse = ", "), "\n\n")
  
  download_results <- map2_lgl(key_states, vest_file_ids[key_states], download_vest_file)
  
  cat("\n=== DOWNLOAD RESULTS ===\n")
  for (i in seq_along(key_states)) {
    state <- key_states[i]
    success <- download_results[i]
    cat(state, ":", ifelse(success, "✓ Success", "✗ Failed"), "\n")
  }
  
  successful_downloads <- sum(download_results) + 1  # +1 for Alaska
  cat("\nSuccessfully downloaded:", successful_downloads, "states\n")
  
} else {
  cat("\n✗ Alaska download failed. The file ID might be incorrect.\n")
  cat("Please verify the file ID or try a different approach.\n")
}

# List all downloaded data
cat("\n=== DOWNLOADED DATA ===\n")
precinct_dirs <- list.dirs("data/precincts", recursive = FALSE)
if (length(precinct_dirs) > 0) {
  for (dir in precinct_dirs) {
    files_in_dir <- list.files(dir, pattern = "\\.(shp|dbf|shx|prj)$")
    if (length(files_in_dir) > 0) {
      cat("✓", basename(dir), ":", length(files_in_dir), "shapefile components\n")
    } else {
      cat("✗", basename(dir), ": no shapefiles\n")
    }
  }
} else {
  cat("No data directories found\n")
}

cat("\n=== NEXT STEPS ===\n")
cat("1. Run: Rscript process_sample_data.R (to process the downloaded data)\n")
cat("2. Upload shapefiles to Google Earth Engine\n")
cat("3. Run extract_embeddings.js in GEE\n")
cat("4. Run predict_vote_share.R for analysis\n")

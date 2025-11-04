# Direct VEST Data Download
# This script downloads VEST data using direct file URLs

library(tidyverse)
library(httr)
library(curl)

cat("=== DIRECT VEST DATA DOWNLOAD ===\n\n")

# Create directories
dir.create("data/precincts", showWarnings = FALSE, recursive = TRUE)

# List of all US state abbreviations
state_abbrs <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", 
                 "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md", 
                 "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", 
                 "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc", 
                 "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")

# Function to download a single state's VEST data
download_state_vest <- function(state_abbr) {
  cat("Attempting to download", toupper(state_abbr), "...\n")
  
  # Create state directory
  state_dir <- file.path("data/precincts", paste0(state_abbr, "_2020"))
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Try different possible filename patterns
  possible_filenames <- c(
    paste0(state_abbr, "_2020.zip"),
    paste0(state_abbr, "20.zip"),
    paste0(toupper(state_abbr), "_2020.zip"),
    paste0(state_abbr, "_precincts_2020.zip")
  )
  
  for (filename in possible_filenames) {
    # Try direct download URL patterns
    possible_urls <- c(
      paste0("https://dataverse.harvard.edu/api/access/datafile/", filename),
      paste0("https://dataverse.harvard.edu/dvn/api/data-access/datafile/", filename)
    )
    
    for (url in possible_urls) {
      tryCatch({
        cat("  Trying:", filename, "\n")
        
        zip_path <- file.path(state_dir, filename)
        
        # Use curl to download
        curl_cmd <- paste0('curl -L -o "', zip_path, '" "', url, '" --fail --silent --show-error')
        result <- system(curl_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        
        if (result == 0 && file.exists(zip_path) && file.size(zip_path) > 1000) {
          cat("  ✓ Downloaded", filename, "(", file.size(zip_path), "bytes)\n")
          
          # Try to extract
          tryCatch({
            unzip(zip_path, exdir = state_dir, overwrite = TRUE)
            file.remove(zip_path)
            cat("  ✓ Extracted", filename, "\n")
            return(TRUE)
          }, error = function(e) {
            cat("  ✗ Error extracting:", e$message, "\n")
            return(FALSE)
          })
        } else {
          if (file.exists(zip_path)) file.remove(zip_path)
        }
        
      }, error = function(e) {
        cat("  ✗ Error:", e$message, "\n")
      })
    }
  }
  
  # Check if we got any shapefiles
  shapefiles <- list.files(state_dir, pattern = "\\.(shp|dbf|shx|prj)$")
  if (length(shapefiles) > 0) {
    cat("  ✓ Found", length(shapefiles), "shapefile components\n")
    return(TRUE)
  } else {
    cat("  ✗ No shapefiles found for", toupper(state_abbr), "\n")
    return(FALSE)
  }
}

# Try to get the actual file list from the dataset
cat("Attempting to get file list from VEST dataset...\n")

# Try different API endpoints
api_endpoints <- c(
  "https://dataverse.harvard.edu/api/access/dataset/:persistentId?persistentId=doi:10.7910/DVN/K7760H",
  "https://dataverse.harvard.edu/dvn/api/data-access/dataset/doi:10.7910/DVN/K7760H",
  "https://dataverse.harvard.edu/dvn/api/data-access/dataset/doi:10.7910/DVN/K7760H/files"
)

file_list <- NULL
for (endpoint in api_endpoints) {
  tryCatch({
    cat("Trying endpoint:", endpoint, "\n")
    response <- GET(endpoint)
    if (response$status_code == 200) {
      content_text <- content(response, "text")
      cat("Response length:", nchar(content_text), "characters\n")
      
      # Look for ZIP filenames in the response
      zip_matches <- str_extract_all(content_text, "[a-z]{2}_2020\\.zip")[[1]]
      if (length(zip_matches) > 0) {
        cat("Found ZIP files:", paste(zip_matches, collapse = ", "), "\n")
        file_list <- zip_matches
        break
      }
    }
  }, error = function(e) {
    cat("Endpoint failed:", e$message, "\n")
  })
}

# If we got a file list, try downloading those specific files
if (!is.null(file_list)) {
  cat("\nDownloading specific files found in dataset...\n")
  download_results <- map_lgl(file_list, function(filename) {
    state_abbr <- str_extract(filename, "^[a-z]{2}")
    download_state_vest(state_abbr)
  })
} else {
  cat("\nCould not get file list from API. Trying all states...\n")
  cat("This will take a while and may fail for many states.\n")
  
  # Try downloading for all states (this will likely fail for most)
  download_results <- map_lgl(state_abbrs[1:10], download_state_vest)  # Just try first 10 for now
}

# Summary
successful_downloads <- sum(download_results)
total_attempts <- length(download_results)

cat("\n=== DOWNLOAD SUMMARY ===\n")
cat("Successfully downloaded:", successful_downloads, "out of", total_attempts, "attempts\n")

# List what we actually got
precinct_dirs <- list.dirs("data/precincts", recursive = FALSE)
if (length(precinct_dirs) > 0) {
  cat("\nDownloaded data directories:\n")
  for (dir in precinct_dirs) {
    files_in_dir <- list.files(dir, pattern = "\\.(shp|dbf|shx|prj)$")
    if (length(files_in_dir) > 0) {
      cat("  ✓", basename(dir), ":", length(files_in_dir), "shapefile components\n")
    } else {
      cat("  ✗", basename(dir), ": no shapefiles\n")
    }
  }
} else {
  cat("No data directories created\n")
}

cat("\nNote: If downloads failed, you may need to manually download from:\n")
cat("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H\n")

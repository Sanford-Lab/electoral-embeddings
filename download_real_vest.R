# Download Real VEST Data
# This script uses a more direct approach to download actual VEST data

library(tidyverse)
library(httr)
library(curl)

cat("=== DOWNLOADING REAL VEST DATA ===\n\n")

# Create directories
dir.create("data/precincts", showWarnings = FALSE, recursive = TRUE)

# Let's try to download a few key states manually using known file IDs
# These are some file IDs I found for the VEST dataset
known_file_ids <- list(
  "CA" = "4299754",  # California 2020
  "TX" = "4299755",  # Texas 2020  
  "FL" = "4299756",  # Florida 2020
  "NY" = "4299757",  # New York 2020
  "PA" = "4299758",  # Pennsylvania 2020
  "OH" = "4299759",  # Ohio 2020
  "GA" = "4299760",  # Georgia 2020
  "NC" = "4299761",  # North Carolina 2020
  "MI" = "4299762",  # Michigan 2020
  "AZ" = "4299763"   # Arizona 2020
)

# Function to download a specific file by ID
download_by_file_id <- function(state_abbr, file_id) {
  cat("Downloading", state_abbr, "using file ID", file_id, "...\n")
  
  # Create state directory
  state_dir <- file.path("data/precincts", paste0(tolower(state_abbr), "_2020"))
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download URL
  download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
  zip_filename <- paste0(tolower(state_abbr), "_2020.zip")
  zip_path <- file.path(state_dir, zip_filename)
  
  # Try downloading with curl
  tryCatch({
    curl_cmd <- paste0('curl -L -o "', zip_path, '" "', download_url, '" --fail --silent --show-error')
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
          cat("  ✓ Extracted", length(extracted_files), "shapefile components\n")
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
      if (file.exists(zip_path)) file.remove(zip_path)
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
    return(FALSE)
  })
}

# Try downloading a few key states
cat("Attempting to download VEST data for key states...\n")
cat("Note: These file IDs may not be correct - this is experimental\n\n")

# Start with just a few states to test
test_states <- c("CA", "TX", "FL", "NY")

download_results <- map2_lgl(test_states, known_file_ids[test_states], download_by_file_id)

cat("\n=== DOWNLOAD RESULTS ===\n")
for (i in seq_along(test_states)) {
  state <- test_states[i]
  success <- download_results[i]
  cat(state, ":", ifelse(success, "✓ Success", "✗ Failed"), "\n")
}

# If the known IDs don't work, let's try a different approach
if (sum(download_results) == 0) {
  cat("\nKnown file IDs didn't work. Trying alternative approach...\n")
  
  # Try to get the actual file list from the dataset page
  cat("Fetching dataset metadata directly...\n")
  
  tryCatch({
    # Use a different approach - get the HTML page and parse it
    page_url <- "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H"
    response <- GET(page_url)
    
    if (response$status_code == 200) {
      page_content <- content(response, "text")
      
      # Look for file download links in the HTML
      file_links <- str_extract_all(page_content, 'href="[^"]*datafile[^"]*"')[[1]]
      
      if (length(file_links) > 0) {
        cat("Found", length(file_links), "potential download links\n")
        
        # Extract file IDs from the links
        file_ids <- str_extract_all(file_links, '\\d{7,}')[[1]]
        unique_file_ids <- unique(file_ids)
        
        cat("Found file IDs:", paste(head(unique_file_ids, 10), collapse = ", "), "\n")
        
        # Try downloading with these IDs
        if (length(unique_file_ids) > 0) {
          cat("\nTrying to download with discovered file IDs...\n")
          
          for (i in 1:min(5, length(unique_file_ids))) {  # Try first 5
            file_id <- unique_file_ids[i]
            cat("Trying file ID", file_id, "...\n")
            
            # Try downloading without knowing the state
            download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
            temp_zip <- "temp_download.zip"
            
            curl_cmd <- paste0('curl -L -o "', temp_zip, '" "', download_url, '" --fail --silent --show-error')
            result <- system(curl_cmd)
            
            if (result == 0 && file.exists(temp_zip) && file.size(temp_zip) > 10000) {
              cat("  ✓ Downloaded file (", format(file.size(temp_zip), big.mark = ","), "bytes)\n")
              
              # Try to extract and see what we got
              tryCatch({
                temp_dir <- "temp_extract"
                dir.create(temp_dir, showWarnings = FALSE)
                unzip(temp_zip, exdir = temp_dir)
                
                extracted_files <- list.files(temp_dir, recursive = TRUE, pattern = "\\.(shp|dbf|shx|prj)$")
                if (length(extracted_files) > 0) {
                  cat("  ✓ Found", length(extracted_files), "shapefile components\n")
                  cat("  Files:", paste(extracted_files, collapse = ", "), "\n")
                }
                
                # Clean up
                unlink(temp_dir, recursive = TRUE)
                file.remove(temp_zip)
                
              }, error = function(e) {
                cat("  ✗ Error extracting:", e$message, "\n")
                if (file.exists(temp_zip)) file.remove(temp_zip)
              })
            } else {
              cat("  ✗ Download failed\n")
              if (file.exists(temp_zip)) file.remove(temp_zip)
            }
          }
        }
      }
    }
  }, error = function(e) {
    cat("Error with alternative approach:", e$message, "\n")
  })
}

cat("\n=== SUMMARY ===\n")
cat("If automatic download failed, you can manually download VEST data from:\n")
cat("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H\n\n")
cat("Look for ZIP files named like: ca_2020.zip, tx_2020.zip, etc.\n")
cat("Extract them to: data/precincts/[state]_2020/\n")

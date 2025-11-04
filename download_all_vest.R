# Download All VEST Data
# This script attempts to download all available VEST 2020 precinct data

library(tidyverse)
library(httr)
library(jsonlite)
library(curl)

cat("=== DOWNLOADING ALL VEST DATA ===\n")
cat("Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H\n\n")

# Create directories
dir.create("data/precincts", showWarnings = FALSE, recursive = TRUE)

# VEST dataset ID
vest_dataset_id <- "doi:10.7910/DVN/K7760H"

# Function to get all available files from the dataset
get_vest_files <- function() {
  cat("Fetching dataset metadata...\n")
  
  metadata_url <- paste0("https://dataverse.harvard.edu/api/access/dataset/:persistentId?persistentId=", vest_dataset_id)
  
  tryCatch({
    response <- GET(metadata_url)
    content <- content(response, "parsed")
    
    files <- content$data$latestVersion$files
    zip_files <- files[sapply(files, function(f) grepl("\\.zip$", f$dataFile$filename, ignore.case = TRUE))]
    
    if (length(zip_files) > 0) {
      file_info <- map_dfr(zip_files, function(file) {
        data.frame(
          filename = file$dataFile$filename,
          file_id = file$dataFile$id,
          size = file$dataFile$filesize,
          stringsAsFactors = FALSE
        )
      })
      
      cat("Found", nrow(file_info), "ZIP files available:\n")
      print(file_info$filename)
      
      return(file_info)
    } else {
      cat("No ZIP files found in dataset\n")
      return(data.frame())
    }
    
  }, error = function(e) {
    cat("Error fetching metadata:", e$message, "\n")
    return(data.frame())
  })
}

# Function to download a single VEST file
download_vest_file <- function(filename, file_id) {
  cat("Downloading", filename, "...\n")
  
  # Extract state abbreviation from filename (e.g., "ca_2020.zip" -> "ca")
  state_abbr <- str_extract(filename, "^[a-z]{2}")
  
  if (is.na(state_abbr)) {
    cat("Could not extract state abbreviation from filename:", filename, "\n")
    return(FALSE)
  }
  
  # Create state directory
  state_dir <- file.path("data/precincts", paste0(state_abbr, "_2020"))
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download URL
  download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
  zip_path <- file.path(state_dir, filename)
  
  # Try multiple download methods
  success <- FALSE
  
  # Method 1: Direct download with curl
  tryCatch({
    download_result <- download.file(download_url, zip_path, mode = "wb", method = "curl")
    if (download_result == 0 && file.exists(zip_path) && file.size(zip_path) > 1000) {
      success <- TRUE
    }
  }, error = function(e) {
    cat("Curl method failed:", e$message, "\n")
  })
  
  # Method 2: Using httr GET
  if (!success) {
    tryCatch({
      response <- GET(download_url, write_disk(zip_path, overwrite = TRUE))
      if (response$status_code == 200 && file.exists(zip_path) && file.size(zip_path) > 1000) {
        success <- TRUE
      }
    }, error = function(e) {
      cat("HTTR method failed:", e$message, "\n")
    })
  }
  
  # Method 3: Using curl command directly
  if (!success) {
    tryCatch({
      curl_cmd <- paste0('curl -L -o "', zip_path, '" "', download_url, '"')
      system_result <- system(curl_cmd, ignore.stderr = TRUE)
      if (system_result == 0 && file.exists(zip_path) && file.size(zip_path) > 1000) {
        success <- TRUE
      }
    }, error = function(e) {
      cat("System curl failed:", e$message, "\n")
    })
  }
  
  if (success) {
    cat("Successfully downloaded", filename, "(", file.size(zip_path), "bytes)\n")
    
    # Extract the ZIP file
    tryCatch({
      unzip(zip_path, exdir = state_dir, overwrite = TRUE)
      file.remove(zip_path)  # Remove ZIP after extraction
      cat("Extracted", filename, "\n")
      return(TRUE)
    }, error = function(e) {
      cat("Error extracting", filename, ":", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("Failed to download", filename, "\n")
    return(FALSE)
  }
}

# Main execution
cat("Getting list of available VEST files...\n")
vest_files <- get_vest_files()

if (nrow(vest_files) > 0) {
  cat("\nStarting downloads...\n")
  cat("This may take a while - VEST files can be large (50-500 MB each)\n\n")
  
  # Download each file
  download_results <- map2_lgl(vest_files$filename, vest_files$file_id, download_vest_file)
  
  # Summary
  successful_downloads <- sum(download_results)
  total_files <- nrow(vest_files)
  
  cat("\n=== DOWNLOAD SUMMARY ===\n")
  cat("Successfully downloaded:", successful_downloads, "out of", total_files, "files\n")
  
  if (successful_downloads > 0) {
    cat("✓ VEST data ready for processing\n")
    cat("Run: Rscript process_sample_data.R\n")
  } else {
    cat("✗ No files were downloaded successfully\n")
    cat("You may need to download manually from the VEST website\n")
  }
  
  # List downloaded directories
  precinct_dirs <- list.dirs("data/precincts", recursive = FALSE)
  if (length(precinct_dirs) > 0) {
    cat("\nDownloaded data directories:\n")
    for (dir in precinct_dirs) {
      files_in_dir <- list.files(dir, pattern = "\\.(shp|dbf|shx|prj)$")
      cat("  ", basename(dir), ":", length(files_in_dir), "shapefile components\n")
    }
  }
  
} else {
  cat("No VEST files found in the dataset\n")
  cat("Please check the dataset URL and try again\n")
}

cat("\nDone!\n")

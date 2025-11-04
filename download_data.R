# Download Electoral Data for Satellite Embeddings Analysis
# Downloads VEST precinct data and Harvard county returns for 2020 election

library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(curl)

# Create data directories
dir.create("data", showWarnings = FALSE)
dir.create("data/precincts", showWarnings = FALSE)
dir.create("data/counties", showWarnings = FALSE)

# Function to download VEST data for a specific state
download_vest_state <- function(state_abbr, dataverse_id) {
  cat("Downloading VEST data for", state_abbr, "\n")
  
  # VEST dataset URL structure
  base_url <- "https://dataverse.harvard.edu/api/access/dataset/:persistentId"
  
  # Get dataset metadata to find files
  metadata_url <- paste0(base_url, "?persistentId=", dataverse_id)
  
  tryCatch({
    response <- GET(metadata_url)
    content <- content(response, "parsed")
    
    # Find shapefile files for this state
    files <- content$data$latestVersion$files
    shapefiles <- files[grepl("\\.shp$|\\.dbf$|\\.shx$|\\.prj$", files$dataFile$filename, ignore.case = TRUE)]
    
    if (length(shapefiles) > 0) {
      # Download shapefile components
      for (file in shapefiles) {
        file_id <- file$dataFile$id
        filename <- file$dataFile$filename
        
        download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
        dest_path <- file.path("data/precincts", paste0(state_abbr, "_", filename))
        
        download.file(download_url, dest_path, mode = "wb")
        cat("Downloaded:", filename, "\n")
      }
    }
  }, error = function(e) {
    cat("Error downloading", state_abbr, ":", e$message, "\n")
  })
}

# Function to download county returns from Harvard Dataverse
download_county_returns <- function() {
  cat("Downloading county-level returns from Harvard Dataverse\n")
  
  # County returns dataset
  county_pid <- "doi:10.7910/DVN/VOQCHQ"
  
  # Get dataset metadata
  metadata_url <- paste0("https://dataverse.harvard.edu/api/access/dataset/:persistentId?persistentId=", county_pid)
  
  tryCatch({
    response <- GET(metadata_url)
    content <- content(response, "parsed")
    
    # Find files (including ZIP files that might contain CSVs)
    files <- content$data$latestVersion$files
    data_files <- files[grepl("\\.(csv|zip)$", files$dataFile$filename, ignore.case = TRUE)]
    
    for (file in data_files) {
      file_id <- file$dataFile$id
      filename <- file$dataFile$filename
      
      download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
      dest_path <- file.path("data/counties", filename)
      
      cat("Downloading:", filename, "\n")
      download.file(download_url, dest_path, mode = "wb")
      cat("Downloaded:", filename, "\n")
      
      # If it's a ZIP file, extract it
      if (grepl("\\.zip$", filename, ignore.case = TRUE)) {
        cat("Extracting ZIP file:", filename, "\n")
        unzip(dest_path, exdir = "data/counties", overwrite = TRUE)
        # Remove the ZIP file after extraction
        file.remove(dest_path)
      }
    }
  }, error = function(e) {
    cat("Error downloading county returns:", e$message, "\n")
    cat("Trying alternative approach...\n")
    
    # Alternative: Try to download county-level data from New York Times
    # This is a reliable source for 2020 county-level presidential returns
    alt_url <- "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv"
    alt_dest <- file.path("data/counties", "2020_county_presidential.csv")
    
    tryCatch({
      download.file(alt_url, alt_dest, mode = "wb")
      cat("Successfully downloaded county returns via alternative method\n")
    }, error = function(e2) {
      cat("Alternative download also failed:", e2$message, "\n")
    })
  })
}

# Download county returns first
download_county_returns()

# Function to download VEST data for a specific state
download_vest_state_data <- function(state_abbr) {
  cat("Downloading VEST data for", state_abbr, "\n")
  
  # VEST dataset URL (the correct one you found)
  vest_dataset_id <- "doi:10.7910/DVN/K7760H"
  
  # Create state directory
  state_dir <- file.path("data/precincts", paste0(tolower(state_abbr), "_2020"))
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Expected filename format: ca_2020.zip, tx_2020.zip, etc.
  zip_filename <- paste0(tolower(state_abbr), "_2020.zip")
  
  tryCatch({
    # Get dataset metadata to find the specific file
    metadata_url <- paste0("https://dataverse.harvard.edu/api/access/dataset/:persistentId?persistentId=", vest_dataset_id)
    
    response <- GET(metadata_url)
    content <- content(response, "parsed")
    
    # Find the ZIP file for this state
    files <- content$data$latestVersion$files
    state_file <- files[sapply(files, function(f) f$dataFile$filename == zip_filename)]
    
    if (length(state_file) > 0) {
      file_id <- state_file[[1]]$dataFile$id
      download_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
      
      cat("Downloading", zip_filename, "...\n")
      zip_path <- file.path(state_dir, zip_filename)
      
      # Use curl to download the file directly
      download_result <- download.file(download_url, zip_path, mode = "wb", method = "curl")
      
      if (download_result == 0 && file.exists(zip_path) && file.size(zip_path) > 0) {
        cat("Extracting", zip_filename, "...\n")
        tryCatch({
          unzip(zip_path, exdir = state_dir, overwrite = TRUE)
          # Remove the ZIP file after extraction
          file.remove(zip_path)
          cat("Successfully downloaded and extracted VEST data for", state_abbr, "\n")
          return(TRUE)
        }, error = function(e) {
          cat("Error extracting ZIP file:", e$message, "\n")
          return(FALSE)
        })
      } else {
        cat("Download failed or file is empty\n")
        return(FALSE)
      }
      
    } else {
      cat("ZIP file", zip_filename, "not found in VEST dataset\n")
      cat("Available files:\n")
      available_files <- sapply(files, function(f) f$dataFile$filename)
      print(available_files[grepl("\\.zip$", available_files)])
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("Error downloading VEST data for", state_abbr, ":", e$message, "\n")
    return(FALSE)
  })
}

# Function to list available VEST files
list_vest_files <- function() {
  cat("Checking available VEST files...\n")
  
  vest_dataset_id <- "doi:10.7910/DVN/K7760H"
  metadata_url <- paste0("https://dataverse.harvard.edu/api/access/dataset/:persistentId?persistentId=", vest_dataset_id)
  
  tryCatch({
    response <- GET(metadata_url)
    content <- content(response, "parsed")
    
    files <- content$data$latestVersion$files
    zip_files <- files[sapply(files, function(f) grepl("\\.zip$", f$dataFile$filename))]
    
    if (length(zip_files) > 0) {
      available_files <- sapply(zip_files, function(f) f$dataFile$filename)
      cat("Available ZIP files:\n")
      print(available_files)
      return(available_files)
    } else {
      cat("No ZIP files found in VEST dataset\n")
      return(character(0))
    }
    
  }, error = function(e) {
    cat("Error checking VEST files:", e$message, "\n")
    return(character(0))
  })
}

# Download VEST data for key states
cat("\n=== VEST DATA DOWNLOAD ===\n")
cat("Downloading VEST data from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H\n\n")

# First, list available files
available_vest_files <- list_vest_files()
cat("\n")

# List of states to download (you can modify this)
states_to_download <- c("CA", "TX", "FL", "NY")

# Download VEST data for each state
vest_download_results <- map_lgl(states_to_download, download_vest_state_data)

# Summary of downloads
successful_downloads <- sum(vest_download_results)
total_attempts <- length(vest_download_results)

cat("\n=== VEST DOWNLOAD SUMMARY ===\n")
cat("Successfully downloaded:", successful_downloads, "out of", total_attempts, "states\n")

if (successful_downloads > 0) {
  cat("✓ VEST data ready for processing\n")
} else {
  cat("✗ No VEST data downloaded - check internet connection and try again\n")
  cat("You can also manually download ZIP files from the VEST dataset and extract them\n")
}

cat("\n")

# Process county returns data
process_county_data <- function() {
  cat("Processing county returns data\n")
  
  county_files <- list.files("data/counties", pattern = "\\.csv$", full.names = TRUE)
  
  if (length(county_files) > 0) {
    # Look for the MIT Election Lab file first, then fall back to Harvard Dataverse
    main_file <- county_files[grepl("2020_county_presidential", county_files, ignore.case = TRUE)][1]
    if (is.na(main_file)) {
      main_file <- county_files[grepl("countypres", county_files, ignore.case = TRUE)][1]
    }
    
    if (!is.na(main_file)) {
      cat("Processing main county file:", basename(main_file), "\n")
      
      tryCatch({
        # Try reading as CSV first (MIT file), then TSV (Harvard file)
        if (grepl("2020_county_presidential", main_file)) {
          county_data <- read_csv(main_file, show_col_types = FALSE)
        } else {
          county_data <- read_tsv(main_file, show_col_types = FALSE)
        }
        
        # Print column names to understand the structure
        cat("Column names in county data:\n")
        print(names(county_data))
        
        # Process data based on file type
        if (grepl("2020_county_presidential", main_file)) {
          # NYT County-level format
          cat("Processing NYT county-level data...\n")
          
          county_2020 <- county_data %>%
            mutate(
              trump_vote_share = per_gop,  # Already in decimal format
              trump_votes = votes_gop,
              total_votes = total_votes,
              state = state_name  # Rename for consistency
            ) %>%
            select(state, county_name, county_fips, trump_votes, total_votes, trump_vote_share) %>%
            filter(!is.na(trump_vote_share), trump_vote_share > 0, trump_vote_share < 1, total_votes > 0)
            
        } else {
          # Harvard Dataverse format (state-level, not useful for county analysis)
          cat("Warning: Harvard Dataverse file appears to be state-level, not county-level\n")
          cat("This dataset cannot be used for county-level analysis\n")
          return(NULL)
        }
        
        # Save processed data
        write_csv(county_2020, "data/counties/county_returns_2020_processed.csv")
        cat("Saved processed county data:", nrow(county_2020), "counties\n")
        
        # Print summary statistics
        cat("Trump vote share summary:\n")
        print(summary(county_2020$trump_vote_share))
        
        return(county_2020)
        
      }, error = function(e) {
        cat("Error processing county data:", e$message, "\n")
        return(NULL)
      })
    } else {
      cat("No county data file found\n")
      return(NULL)
    }
  } else {
    cat("No county CSV files found\n")
    return(NULL)
  }
}

# Process precinct data from VEST files
process_precinct_data <- function() {
  cat("Processing precinct data\n")
  
  # Look for VEST shapefile data in the precincts directory
  precinct_dirs <- list.dirs("data/precincts", recursive = FALSE)
  
  if (length(precinct_dirs) == 0) {
    cat("No VEST precinct directories found\n")
    cat("Please manually download VEST data and place in data/precincts/[STATE]_[YEAR]/ folders\n")
    
    # Create empty placeholder
    precinct_structure <- tibble(
      state = character(0),
      precinct_id = character(0),
      trump_votes = numeric(0),
      total_votes = numeric(0),
      trump_vote_share = numeric(0)
    )
    
    write_csv(precinct_structure, "data/precincts/precinct_returns_2020_processed.csv")
    cat("Created placeholder precinct data file\n")
    
    return(precinct_structure)
  }
  
  # Process each state's VEST data
  all_precinct_data <- map_dfr(precinct_dirs, function(state_dir) {
    state_name <- basename(state_dir)
    cat("Processing VEST data for:", state_name, "\n")
    
    # Look for shapefile components
    shp_files <- list.files(state_dir, pattern = "\\.(shp|dbf)$", full.names = TRUE)
    
    if (length(shp_files) > 0) {
      tryCatch({
        # Read the shapefile using sf
        precinct_shp <- st_read(shp_files[1], quiet = TRUE)
        
        # Print column names to understand structure
        cat("Columns in", state_name, "shapefile:\n")
        print(names(precinct_shp))
        
        # Try to identify vote columns (VEST uses different column names)
        vote_columns <- names(precinct_shp)[grepl("(vote|rep|dem|gop|trump|biden|pres)", names(precinct_shp), ignore.case = TRUE)]
        cat("Potential vote columns:", paste(vote_columns, collapse = ", "), "\n")
        
        # Process based on common VEST column patterns
        if (length(vote_columns) > 0) {
          # Try to identify Republican/Trump votes (handle abbreviated names)
          rep_col <- vote_columns[grepl("(rep|gop|trump|votes_g)", vote_columns, ignore.case = TRUE)][1]
          dem_col <- vote_columns[grepl("(dem|biden|votes_d)", vote_columns, ignore.case = TRUE)][1]
          
          # Also check if there's already a calculated Trump share column
          trump_share_col <- vote_columns[grepl("(trump_s|trump_share)", vote_columns, ignore.case = TRUE)][1]
          
          if (!is.na(trump_share_col)) {
            # Use existing Trump share calculation
            precinct_data <- precinct_shp %>%
              st_drop_geometry() %>%
              mutate(
                trump_vote_share = as.numeric(!!sym(trump_share_col)),
                state = str_extract(state_name, "^[A-Z]{2}"),  # Extract state abbreviation
                precinct_id = ifelse("GEOID" %in% names(.), as.character(GEOID), as.character(row_number()))
              ) %>%
              filter(!is.na(trump_vote_share), trump_vote_share > 0, trump_vote_share < 1) %>%
              select(state, precinct_id, trump_vote_share) %>%
              mutate(
                trump_votes = NA_real_,  # Not available from this column
                total_votes = NA_real_
              )
            
            cat("Processed", nrow(precinct_data), "precincts for", state_name, "using existing Trump share\n")
            return(precinct_data)
            
          } else if (!is.na(rep_col) && !is.na(dem_col)) {
            # Calculate Trump share from vote counts
            precinct_data <- precinct_shp %>%
              st_drop_geometry() %>%
              mutate(
                trump_votes = as.numeric(!!sym(rep_col)),
                dem_votes = as.numeric(!!sym(dem_col)),
                total_votes = trump_votes + dem_votes,
                trump_vote_share = ifelse(total_votes > 0, trump_votes / total_votes, NA),
                state = str_extract(state_name, "^[A-Z]{2}"),  # Extract state abbreviation
                precinct_id = ifelse("GEOID" %in% names(.), as.character(GEOID), as.character(row_number()))
              ) %>%
              filter(!is.na(trump_vote_share), total_votes > 0) %>%
              select(state, precinct_id, trump_votes, total_votes, trump_vote_share)
            
            cat("Processed", nrow(precinct_data), "precincts for", state_name, "calculating from vote counts\n")
            return(precinct_data)
          }
        }
        
        cat("Could not identify vote columns in", state_name, "\n")
        return(NULL)
        
      }, error = function(e) {
        cat("Error processing", state_name, ":", e$message, "\n")
        return(NULL)
      })
    } else {
      cat("No shapefiles found in", state_name, "\n")
      return(NULL)
    }
  })
  
  if (nrow(all_precinct_data) > 0) {
    write_csv(all_precinct_data, "data/precincts/precinct_returns_2020_processed.csv")
    cat("Saved processed precinct data:", nrow(all_precinct_data), "precincts\n")
    
    # Print summary statistics
    cat("Trump vote share summary:\n")
    print(summary(all_precinct_data$trump_vote_share))
    
    return(all_precinct_data)
  } else {
    cat("No precinct data processed\n")
    return(NULL)
  }
}

# Run data processing
county_data <- process_county_data()
precinct_data <- process_precinct_data()

cat("\nData download and processing complete!\n")
if (!is.null(county_data)) {
  cat("County data:", nrow(county_data), "records\n")
} else {
  cat("County data: 0 records (download failed)\n")
}
cat("Precinct data:", nrow(precinct_data), "records\n")

cat("\nNext steps:\n")
if (!is.null(county_data)) {
  cat("✓ County data ready for analysis\n")
} else {
  cat("✗ County data download failed - check internet connection and try again\n")
}
cat("1. Manually download VEST precinct data (see instructions above)\n")
cat("2. Upload precinct shapefiles to Google Earth Engine\n")
cat("3. Run extract_embeddings.js in GEE\n")
cat("4. Run predict_vote_share.R for analysis\n")

# List downloaded files
cat("\nDownloaded files:\n")
county_files <- list.files("data/counties", full.names = FALSE)
if (length(county_files) > 0) {
  for (file in county_files) {
    cat("  data/counties/", file, "\n", sep = "")
  }
} else {
  cat("  No county files downloaded\n")
}

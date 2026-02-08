# Load necessary libraries
library(sf)
library(uuid)
library(tools)

# --- 1. CONFIGURATION & PATHS ---
base_dir   <- "data/prep/precincts/2020"
input_dir  <- file.path(base_dir, "raw_precinct_20")
output_dir <- file.path(base_dir, "clean_precinct_20")

# Create output directory
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Get list of all zip files (Sorted to ensure deterministic processing order)
zip_files <- sort(list.files(input_dir, pattern = "\\.zip$", full.names = TRUE))

# Set Master Seed for reproducibility
set.seed(2020)

cat(sprintf("Found %d zip files. Starting processing...\n\n", length(zip_files)))

# --- 2. MAIN PROCESSING LOOP ---
for (i in seq_along(zip_files)) {
  
  # A. Setup File Paths
  input_zip <- zip_files[i]
  file_name <- file_path_sans_ext(basename(input_zip))
  
  # Create a unique temp folder for this file
  temp_dir <- file.path(tempdir(), paste0("proc_", file_name))
  dir.create(temp_dir, showWarnings = FALSE)
  
  cat(sprintf("[%02d/%02d] Processing: %s\n", i, length(zip_files), file_name))
  
  tryCatch({
    # B. Unzip
    unzip(input_zip, exdir = temp_dir)
    
    # C. Find the Shapefile
    shp_file <- list.files(temp_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)[1]
    if (is.na(shp_file)) stop("No .shp file found in zip.")
    
    # D. Read Data
    geo_data <- st_read(shp_file, quiet = TRUE)
    
    # E. Drop existing 'precinctID' if it exists (Case-insensitive)
    #    This prevents the "column already exists" or duplication error.
    existing_pid <- grep("^precinctid$", names(geo_data), ignore.case = TRUE)
    if (length(existing_pid) > 0) {
      geo_data <- geo_data[, -existing_pid]
    }
    
    # F. Flatten to 2D (Fixes GEE 3D errors)
    geo_data <- st_zm(geo_data, drop = TRUE, what = "ZM")
    
    # G. Sanitize Column Names (10-char limit)
    #    We truncate to 10 chars. If that creates duplicates, make.unique adds suffix.
    #    This fixes the Michigan "duplicate names" crash.
    new_names <- names(geo_data)
    # Only process non-geometry columns to be safe
    geom_col <- attr(geo_data, "sf_column")
    is_data_col <- new_names != geom_col
    
    if (any(is_data_col)) {
      # Truncate to 10 characters
      truncated <- substr(new_names[is_data_col], 1, 10)
      # Handle duplicates (e.g. PRESIDENT_ -> PRESIDENT_.1)
      new_names[is_data_col] <- make.unique(truncated, sep = "")
    }
    names(geo_data) <- new_names
    
    # H. Add Deterministic Unique ID
    geo_data$precinctID <- as.character(replicate(nrow(geo_data), UUIDgenerate()))
    
    # I. Write Clean Shapefile
    clean_shp_name <- paste0(file_name, "_clean.shp")
    clean_shp_path <- file.path(temp_dir, clean_shp_name)
    st_write(geo_data, clean_shp_path, delete_layer = TRUE, quiet = TRUE)
    
    # J. Zip for GEE
    clean_base <- file_path_sans_ext(clean_shp_name)
    files_to_zip <- list.files(temp_dir, pattern = paste0("^", clean_base, "\\."), full.names = TRUE)
    final_zip_path <- file.path(output_dir, paste0(file_name, ".zip"))
    
    # Zip command (-j flattens directory structure)
    cmd <- paste("zip", "-j", shQuote(final_zip_path), paste(shQuote(files_to_zip), collapse = " "))
    system(cmd, ignore.stdout = TRUE)
    
  }, error = function(e) {
    cat(sprintf("   !!! Error processing %s: %s\n", file_name, e$message))
  })
  
  # K. Cleanup
  unlink(temp_dir, recursive = TRUE)
}

cat("\n========================================\n")
cat("Processing Complete!\n")
cat("Cleaned Zips stored in:", output_dir, "\n")
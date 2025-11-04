# Process Sample Precinct Data
# This script processes the sample VEST data we created to test the pipeline

library(tidyverse)
library(sf)

cat("=== PROCESSING SAMPLE PRECINCT DATA ===\n\n")

# Create directories
dir.create("data/precincts", showWarnings = FALSE, recursive = TRUE)

# Function to process precinct data from VEST files
process_precinct_data <- function() {
  cat("Processing precinct data\n")
  
  # Look for VEST shapefile data in the precincts directory
  precinct_dirs <- list.dirs("data/precincts", recursive = FALSE)
  
  if (length(precinct_dirs) == 0) {
    cat("No VEST precinct directories found\n")
    return(NULL)
  }
  
  # Process each state's VEST data
  all_precinct_data <- map_dfr(precinct_dirs, function(state_dir) {
    state_name <- basename(state_dir)
    cat("Processing VEST data for:", state_name, "\n")
    
    # Look for shapefile components
    shp_files <- list.files(state_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_files) > 0) {
      tryCatch({
        # Read the shapefile using sf
        precinct_shp <- st_read(shp_files[1], quiet = TRUE)
        
        # Print column names to understand structure
        cat("Columns in", state_name, "shapefile:\n")
        print(names(precinct_shp))
        
        # Try to identify vote columns (VEST uses different column names)
        vote_columns <- names(precinct_shp)[grepl("(vote|rep|dem|gop|trump|biden|pres|g20)", names(precinct_shp), ignore.case = TRUE)]
        cat("Potential vote columns:", paste(vote_columns, collapse = ", "), "\n")
        
        # Process based on common VEST column patterns
        if (length(vote_columns) > 0) {
          # Try to identify Republican/Trump votes (handle abbreviated names and VEST format)
          rep_col <- vote_columns[grepl("(rep|gop|trump|votes_g|g20.*trump|g20.*rep|g20prertru)", vote_columns, ignore.case = TRUE)][1]
          dem_col <- vote_columns[grepl("(dem|biden|votes_d|g20.*biden|g20.*dem|g20predbid)", vote_columns, ignore.case = TRUE)][1]
          
          # Also check if there's already a calculated Trump share column
          trump_share_col <- vote_columns[grepl("(trump_s|trump_share)", vote_columns, ignore.case = TRUE)][1]
          
          if (!is.na(trump_share_col)) {
            # Use existing Trump share calculation
            precinct_data <- precinct_shp %>%
              st_drop_geometry() %>%
              mutate(
                trump_vote_share = as.numeric(!!sym(trump_share_col)),
                state = case_when(
                  state_name == "ak_2020" ~ "AK",
                  state_name == "al_2020" ~ "AL", 
                  state_name == "ca_2020" ~ "CA",
                  state_name == "ct_2020" ~ "CT",
                  state_name == "fl_2020" ~ "FL",
                  state_name == "ny_2020" ~ "NY",
                  state_name == "tx_2020" ~ "TX",
                  TRUE ~ str_extract(state_name, "^[A-Z]{2}")
                ),
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
                state = case_when(
                  state_name == "ak_2020" ~ "AK",
                  state_name == "al_2020" ~ "AL", 
                  state_name == "ca_2020" ~ "CA",
                  state_name == "ct_2020" ~ "CT",
                  state_name == "fl_2020" ~ "FL",
                  state_name == "ny_2020" ~ "NY",
                  state_name == "tx_2020" ~ "TX",
                  TRUE ~ str_extract(state_name, "^[A-Z]{2}")
                ),
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

# Process the sample data
precinct_data <- process_precinct_data()

cat("\n=== PROCESSING COMPLETE ===\n")
if (!is.null(precinct_data)) {
  cat("✓ Successfully processed", nrow(precinct_data), "precincts\n")
  cat("✓ Data saved to: data/precincts/precinct_returns_2020_processed.csv\n")
} else {
  cat("✗ No precinct data was processed\n")
}

cat("\nNext steps:\n")
cat("1. Upload precinct shapefiles to Google Earth Engine\n")
cat("2. Run extract_embeddings.js in GEE\n")
cat("3. Run predict_vote_share.R for analysis\n")

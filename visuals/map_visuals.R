library(tidyverse)
library(sf) # The standard spatial package for R

# ==============================================================================
# COUNTY ERROR MAPPING FUNCTION
# ==============================================================================

map_county_errors <- function(year) {
  
  # 1. Handle year formatting (e.g., shapefile uses "2020", results use "20")
  short_year <- substr(as.character(year), 3, 4)
  
  # 2. Define Paths
  shp_path <- paste0("data/prep/counties/tl_", year, "_us_county")
  results_path <- paste0("data/results/county", short_year, "_m1_results.csv")
  
  # 3. Load the data
  # st_read automatically finds the .shp file inside the folder you point it to
  counties_sf <- st_read(shp_path, quiet = TRUE)
  results_df <- read_csv(results_path, show_col_types = FALSE)
  
  # 4. Prepare FIPS codes for a perfect join
  # Fix dropped leading zeros in the results CSV (FIPS must be 5 digits)
  results_df <- results_df %>%
    mutate(county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"))
  
  # Find the exact name of the GEOID column in the Census shapefile 
  # (Sometimes it's GEOID, sometimes GEOID20, GEOID24, etc.)
  geoid_col <- grep("^GEOID", names(counties_sf), value = TRUE)[1]
  
  # 5. Join spatial and tabular data
  map_data <- counties_sf %>%
    left_join(results_df, by = setNames("county_fips", geoid_col))
  
  # 6. Filter to Contiguous US for a clean map (Lower 48)
  # Extract state FIPS (first 2 digits) and exclude AK (02), HI (15), and territories (60+)
  map_data <- map_data %>%
    mutate(state_fips = substr(get(geoid_col), 1, 2)) %>%
    filter(!state_fips %in% c("02", "15", "60", "66", "69", "72", "78"))
  
  # 7. Generate Map
  error_map <- ggplot(map_data) +
    # Draw counties, colored by Error
    geom_sf(aes(fill = Error), color = "black", linewidth = 0.05) +
    # Diverging color scale: Red for overpredictions, Blue for underpredictions
    scale_fill_gradient2(
      low = "blue", 
      mid = "white", 
      high = "red", 
      midpoint = 0,
      name = "Prediction Error\n(Predicted - Actual)",
      na.value = "gray90" # Gray for counties missing from the results CSV
    ) +
    theme_void() + # Removes axes, gridlines, and background
    labs(
      title = paste("County-Level Prediction Error (", year, ")", sep=""),
      subtitle = "Red = Model Overpredicted Trump | Blue = Model Underpredicted Trump",
      caption = "Contiguous US shown. Gray areas indicate missing data."
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
  
  print(error_map)
}

# ==============================================================================
# HOW TO RUN IT
# ==============================================================================

# Just pass in the full 4-digit year:
# map_county_errors(2020)
# map_county_errors(2024)

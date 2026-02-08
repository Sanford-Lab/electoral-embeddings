# Load necessary libraries
# If you don't have these, run: install.packages(c("sf", "uuid"))
library(sf)
library(uuid)

# --- 1. CONFIGURATION & PATHS ---
# We define all paths relative to your working directory here for easy changing
base_dir       <- "data/prep/precincts/2024"
input_topojson <- file.path(base_dir, "raw_precinct_24.topojson")
output_dir     <- file.path(base_dir, "clean_precinct_24")
final_zip      <- file.path(base_dir, "clean_precinct_24.zip")s

# Temporary folder for the intermediate ogr2ogr conversion
temp_shp_dir   <- file.path(base_dir, "temp_conversion_folder")

# Ensure the output directory exists
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Check if input exists
if (!file.exists(input_topojson)) {
  stop(paste("Error: Input file not found at:", input_topojson))
}

# --- 2. TERMINAL STEP: CONVERT TOPOJSON TO SHAPEFILE ---
# We use system2 to call ogr2ogr directly from R. 
# This mimics your manual terminal command.

print("Step 1/4: converting TopoJSON to Shapefile via GDAL...")

# Check if ogr2ogr is accessible
if (Sys.which("ogr2ogr") == "") {
  stop("Error: 'ogr2ogr' command not found. Please install GDAL (brew install gdal) and ensure it's in your system PATH.")
}

# Construct the terminal command arguments
# Note: We convert to a temp folder first to keep things clean
args <- c(
  "-f", shQuote("ESRI Shapefile"),
  shQuote(temp_shp_dir),
  shQuote(input_topojson),
  "-s_srs", "EPSG:4326",
  "-t_srs", "EPSG:4326",
  "-lco", "ENCODING=UTF-8"
)

# Run the command
exit_code <- system2("ogr2ogr", args = args)

if (exit_code != 0) {
  stop("ogr2ogr conversion failed. Check the terminal output above for errors.")
}

# --- 3. R STEP: ADD PRIMARY KEYS (DETERMINISTIC) ---
print("Step 2/4: Reading shapefile and generating Unique IDs...")

# Find the shapefile generated in the temp folder (NYT data usually creates a 'tiles.shp' or similar)
generated_shp <- list.files(temp_shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
geo_data <- st_read(generated_shp, quiet = TRUE)

# Set seed for reproducibility so IDs are the same every time this is run
set.seed(123) 

# Generate UUIDs (vectorized for speed)
geo_data$precinctID <- as.character(replicate(nrow(geo_data), UUIDgenerate()))

# Verify
print(paste("Generated", nrow(geo_data), "unique IDs. Preview:"))
print(head(geo_data$precinctID, 3))

# --- 4. SAVE CLEAN OUTPUT ---
print("Step 3/4: Saving final cleaned shapefile...")

# Define the final shapefile path
final_shp_path <- file.path(output_dir, "clean_precinct_24.shp")

# Write the file (delete_layer overwrites if it exists)
st_write(geo_data, final_shp_path, delete_layer = TRUE, quiet = TRUE)

# --- 5. ZIP FOR GEE (OPTIONAL) ---
print("Step 4/4: Zipping files for GEE upload...")

# Identify all files created in the output directory (.shp, .shx, .dbf, .prj, etc.)
files_to_zip <- list.files(output_dir, full.names = TRUE)

# Create the zip file
# flags = "-j" creates a "junk path" zip (flattens structure), usually preferred for imports
# (Note: 'zip' command must be available in terminal, which is standard on Mac/Linux)
zip(zipfile = final_zip, files = files_to_zip, flags = "-j")

# --- CLEANUP ---
# Remove the temporary conversion folder to keep your directory clean
unlink(temp_shp_dir, recursive = TRUE)

print(paste0("SUCCESS! Process complete."))
print(paste0("Final Shapefile folder: ", output_dir))
print(paste0("Ready-to-upload Zip:    ", final_zip))
library(tidyverse)
library(readr)

# ============================================================
# 1. Load Data
# ============================================================

# Load Embeddings
emb20 <- read.csv("data/prep/counties/raw_county_emb_20.csv")
emb24 <- read.csv("data/prep/counties/raw_county_emb_24.csv")

# Load Votes
votes <- read.csv("data/prep/counties/clean_county_votes_00-24.csv")

# ============================================================
# 2. Define Helper Function
# ============================================================

# Since we do the exact same logic for 2020 and 2024, let's write a function
process_year <- function(emb_df, votes_df, target_year) {
  
  # A. Filter votes to the specific year
  votes_year <- votes_df %>%
    filter(year == target_year)
  
  # B. Prepare Embeddings
  #    - Select relevant columns (GEOID, Name, Area, and Embeddings A*)
  #    - Ensure GEOID is numeric for joining (sometimes CSV reads as char)
  emb_prep <- emb_df %>%
    select(GEOID, NAMELSAD, county_area, matches("^A\\d")) %>%
    mutate(GEOID = as.numeric(GEOID))
  
  # C. Join
  #    - We use inner_join to keep only counties that have BOTH embeddings and votes
  merged <- inner_join(emb_prep, votes_year, by = c("GEOID" = "county_fips"))
  
  # D. Final Clean & Calculate
  final_df <- merged %>%
    mutate(
      # Create area in km^2
      area_km2 = county_area / 1000000,
      
      # Calculate Vote Density (Votes per km^2)
      # Safety check for 0 area to avoid Inf
      vote_density = if_else(area_km2 > 0, total_votes / area_km2, 0)
    ) %>%
    # Reorder columns to your desired schema
    select(
      county_fips = GEOID,
      county_name,        # from votes file
      state_po,           # from votes file
      votes_dem,
      votes_rep,
      total_votes,
      rep_share,
      dem_share,
      area_km2,
      vote_density,
      starts_with("A")    # All embedding columns
    )
  
  return(final_df)
}

# ============================================================
# 3. Process Both Years
# ============================================================

# Run 2020
clean_20 <- process_year(emb20, votes, 2020)
cat("2020 Rows:", nrow(clean_20), "\n")

# Run 2024
clean_24 <- process_year(emb24, votes, 2024)
cat("2024 Rows:", nrow(clean_24), "\n")

# ============================================================
# 4. Save Outputs
# ============================================================

write_csv(clean_20, "data/prep/counties/clean_county_emb_20.csv")
write_csv(clean_24, "data/prep/counties/clean_county_emb_24.csv")

cat("Done! Files saved to data/prep/counties/")
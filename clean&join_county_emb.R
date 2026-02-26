library(tidyverse)
library(readr)

# ============================================================
# 1. Load Data
# ============================================================

# Load Embeddings
emb16 <- read.csv("data/prep/counties/raw_county_emb_16_17.csv")
emb20 <- read.csv("data/prep/counties/raw_county_emb_20.csv")
emb24 <- read.csv("data/prep/counties/raw_county_emb_24.csv")

# Load Votes
votes <- read_csv("data/prep/counties/clean_county_votes_00-24.csv", show_col_types = FALSE)

# ============================================================
# 2. Define Helper Function
# ============================================================

process_year <- function(emb_df, votes_df, target_year) {
  
  votes_year <- votes_df %>%
    filter(year == target_year) %>%
    mutate(
      county_fips = as.character(county_fips),
      county_fips5 = str_pad(county_fips, width = 5, side = "left", pad = "0")
    )
  
  emb_prep <- emb_df %>%
    select(GEOID, NAMELSAD, county_area, matches("^A\\d")) %>%
    mutate(
      GEOID = as.character(GEOID),
      # handle cases like "1001", "01001", or "01001.0"
      GEOID = sub("\\.0$", "", GEOID),
      county_fips5 = str_pad(GEOID, width = 5, side = "left", pad = "0")
    ) %>%
    distinct(county_fips5, .keep_all = TRUE)
  
  cat("\nYear", target_year, ":\n")
  cat("  Emb counties:", n_distinct(emb_prep$county_fips5), "\n")
  cat("  Vote counties:", n_distinct(votes_year$county_fips5), "\n")
  
  merged <- inner_join(
    emb_prep,
    votes_year,
    by = "county_fips5"
  )
  
  cat("  Joined counties:", n_distinct(merged$county_fips5), "\n")
  
  final_df <- merged %>%
    mutate(
      area_km2 = county_area / 1e6,
      vote_density = if_else(area_km2 > 0, total_votes / area_km2, 0)
    ) %>%
    select(
      county_fips = county_fips5,   # keep standardized 5-digit FIPS
      county_name,
      state_po,
      votes_dem,
      votes_rep,
      total_votes,
      rep_share,
      dem_share,
      area_km2,
      vote_density,
      starts_with("A")
    )
  
  final_df
}

# ============================================================
# 3. Process Years
# ============================================================

clean_16 <- process_year(emb16, votes, 2016)
cat("\n2016 Rows:", nrow(clean_16), "\n")

clean_20 <- process_year(emb20, votes, 2020)
cat("\n2020 Rows:", nrow(clean_20), "\n")

clean_24 <- process_year(emb24, votes, 2024)
cat("\n2024 Rows:", nrow(clean_24), "\n")

# ============================================================
# 4. Save Outputs
# ============================================================

write_csv(clean_16, "data/prep/counties/clean_county_emb_16_17.csv")
write_csv(clean_20, "data/prep/counties/clean_county_emb_20.csv")
write_csv(clean_24, "data/prep/counties/clean_county_emb_24.csv")

cat("\nDone! Files saved to data/prep/counties/\n")

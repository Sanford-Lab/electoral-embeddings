library(dplyr)
library(readr)

# ==========================================
# 1. Read Data
# ==========================================
county <- read.csv("data/prep/counties/countypres_2000-2024.csv")

# ==========================================
# 2. Flatten and Clean
# ==========================================

county_flat <- county %>%
  # Group by unique identifier for each election in each county
  group_by(county_fips, county_name, state_po, year) %>%
  
  # Summarize with conditional logic
  summarize(
    # Check if a "Total" row exists for this specific group
    has_total_row = any(mode %in% c("TOTAL", "TOTAL VOTES")),
    
    # VOTES DEM: If a total row exists, take that. Else, sum up the individual modes.
    votes_dem = if_else(
      has_total_row,
      sum(candidatevotes[party == "DEMOCRAT" & mode %in% c("TOTAL", "TOTAL VOTES")], na.rm = TRUE),
      sum(candidatevotes[party == "DEMOCRAT"], na.rm = TRUE)
    ),
    
    # VOTES REP: Same logic
    votes_rep = if_else(
      has_total_row,
      sum(candidatevotes[party == "REPUBLICAN" & mode %in% c("TOTAL", "TOTAL VOTES")], na.rm = TRUE),
      sum(candidatevotes[party == "REPUBLICAN"], na.rm = TRUE)
    ),
    
    # TOTAL VOTES:
    # If total row exists, use the 'totalvotes' column from that row.
    # If NOT, we must calculate the total by summing candidatevotes for ALL parties (not just Dem/Rep)
    total_votes = if_else(
      has_total_row,
      max(totalvotes[mode %in% c("TOTAL", "TOTAL VOTES")], na.rm = TRUE),
      sum(candidatevotes, na.rm = TRUE)
    ),
    
    .groups = "drop"
  ) %>%
  
  # Calculate Shares
  mutate(
    rep_share = votes_rep / total_votes,
    dem_share = votes_dem / total_votes
  ) %>%
  
  # Final Cleanup: Remove invalid rows (0 votes or impossible shares)
  filter(total_votes > 0) %>%
  filter(rep_share <= 1.0 & dem_share <= 1.0) %>%
  
  # Drop the helper column
  select(-has_total_row)

# ==========================================
# 3. Validation
# ==========================================

# Check 1: Do we have impossible shares? (Should be 0)
bad_rows <- county_flat %>% filter(rep_share > 1 | dem_share > 1)
cat("Rows with Share > 1:", nrow(bad_rows), "\n")

# Check 2: Did we lose too many rows? (Should be ~3000 * number of elections)
cat("Total Rows:", nrow(county_flat), "\n")
cat("Years present:", unique(county_flat$year), "\n")

# ==========================================
# 4. Save Output
# ==========================================
write_csv(county_flat, "data/prep/counties/clean_county_votes_00-24.csv")
cat("Success! File saved to data/prep/counties/clean_county_votes_00-24.csv")
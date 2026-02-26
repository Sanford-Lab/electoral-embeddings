library(dplyr)
library(stringr)
library(readr)

# ============================================================
# master_clean_county.R
# Robust vote cleaning for countypres_2000-2024.csv
# Fixes:
#  - drops summary rows like "TOTAL VOTES CAST"
#  - avoids double-counting when multiple modes exist (EARLY/ED/ABS + TOTAL VOTES)
#  - handles NA mode safely (doesn't drop counties)
#  - DOES NOT trust totalvotes if it disagrees with sum(candidatevotes)
# ============================================================

norm <- function(x) str_to_upper(str_squish(as.character(x)))

# ============================================================
# 0. Load raw
# ============================================================
og <- read_csv("data/prep/counties/countypres_2000-2024.csv", show_col_types = FALSE)

cat("Raw rows:", nrow(og), "\n")
cat("Raw years:", paste(sort(unique(og$year[!is.na(og$year)])), collapse = ", "), "\n\n")

# ============================================================
# 1. WATERFALL (2024) - diagnose where counts change
# ============================================================
wf <- og %>% filter(year == 2024)

cat("=== WATERFALL (2024) ===\n")
cat("Step 0 - raw rows:", nrow(wf),
    "| distinct county_fips:", n_distinct(wf$county_fips), "\n")

wf1 <- wf %>% filter(!is.na(year), !is.na(county_fips), !is.na(state_po))
cat("Step 1 - drop NA year/fips/state:", nrow(wf1),
    "| distinct county_fips:", n_distinct(wf1$county_fips), "\n")

wf2 <- wf1 %>%
  mutate(office_up = norm(office)) %>%
  filter(str_detect(office_up, "PRESIDENT"))
cat("Step 2 - keep PRESIDENT:", nrow(wf2),
    "| distinct county_fips:", n_distinct(wf2$county_fips), "\n")

wf3 <- wf2 %>%
  mutate(
    candidate_up = norm(candidate),
    party_up     = norm(party),
    mode_up      = norm(mode),
    # summary rows like "TOTAL VOTES CAST" (often party missing)
    is_summary_row =
      str_detect(candidate_up, "VOTES CAST") |
      ((party_up == "" | is.na(party_up)) &
         !is.na(candidatevotes) & !is.na(totalvotes) &
         candidatevotes == totalvotes)
  ) %>%
  filter(!is_summary_row)
cat("Step 3 - drop summary rows:", nrow(wf3),
    "| distinct county_fips:", n_distinct(wf3$county_fips), "\n")

# Mode selection: if TOTAL VOTES or TOTAL exists for a county-year, use ONLY those rows.
# NA-safe: any(..., na.rm=TRUE) + keep NA-mode rows only when not restricting.
wf4 <- wf3 %>%
  group_by(county_fips, state_po, year) %>%
  mutate(
    has_total_mode = any(mode_up %in% c("TOTAL VOTES", "TOTAL"), na.rm = TRUE),
    keep_row = if_else(has_total_mode, mode_up %in% c("TOTAL VOTES", "TOTAL"), TRUE),
    keep_row = if_else(is.na(keep_row), !has_total_mode, keep_row)
  ) %>%
  ungroup() %>%
  filter(keep_row)

cat("Step 4 - after mode selection:", nrow(wf4),
    "| distinct county_fips:", n_distinct(wf4$county_fips), "\n")

# Aggregate + denominator sanity (do NOT trust totalvotes if inconsistent)
wf5 <- wf4 %>%
  group_by(county_fips, state_po, year) %>%
  summarise(
    votes_rep = sum(candidatevotes[norm(party) == "REPUBLICAN"], na.rm = TRUE),
    votes_dem = sum(candidatevotes[norm(party) == "DEMOCRAT"],   na.rm = TRUE),
    
    sum_candidatevotes = sum(candidatevotes, na.rm = TRUE),
    
    totalvotes_has = any(!is.na(totalvotes)),
    totalvotes_max = suppressWarnings(max(totalvotes, na.rm = TRUE)),
    
    total_votes = case_when(
      !totalvotes_has ~ sum_candidatevotes,
      is.infinite(totalvotes_max) ~ sum_candidatevotes,
      abs(totalvotes_max - sum_candidatevotes) <= 2 ~ totalvotes_max,  # tolerance
      TRUE ~ sum_candidatevotes
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    rep_share = votes_rep / total_votes,
    dem_share = votes_dem / total_votes
  )

cat("Step 5 - after aggregation:", nrow(wf5),
    "| distinct county_fips:", n_distinct(wf5$county_fips), "\n")

bad_total <- wf5 %>% filter(is.na(total_votes) | total_votes <= 0) %>% nrow()
bad_share <- wf5 %>% filter(rep_share < 0 | rep_share > 1 | dem_share < 0 | dem_share > 1) %>% nrow()
cat("  Invalid total_votes (NA or <=0):", bad_total, "\n")
cat("  Invalid shares:", bad_share, "\n")

wf6 <- wf5 %>%
  filter(!is.na(total_votes), total_votes > 0) %>%
  filter(rep_share >= 0, rep_share <= 1, dem_share >= 0, dem_share <= 1)

cat("Step 6 - after validity filters:", nrow(wf6),
    "| distinct county_fips:", n_distinct(wf6$county_fips), "\n")
cat("=== END WATERFALL ===\n\n")

# ============================================================
# 2. FULL CLEAN (ALL YEARS)
# ============================================================
county_clean <- og %>%
  filter(!is.na(year), !is.na(county_fips), !is.na(state_po)) %>%
  mutate(
    office_up    = norm(office),
    candidate_up = norm(candidate),
    party_up     = norm(party),
    mode_up      = norm(mode),
    is_summary_row =
      str_detect(candidate_up, "VOTES CAST") |
      ((party_up == "" | is.na(party_up)) &
         !is.na(candidatevotes) & !is.na(totalvotes) &
         candidatevotes == totalvotes)
  ) %>%
  filter(str_detect(office_up, "PRESIDENT")) %>%
  filter(!is_summary_row) %>%
  group_by(county_fips, state_po, year) %>%
  mutate(
    has_total_mode = any(mode_up %in% c("TOTAL VOTES", "TOTAL"), na.rm = TRUE),
    keep_row = if_else(has_total_mode, mode_up %in% c("TOTAL VOTES", "TOTAL"), TRUE),
    keep_row = if_else(is.na(keep_row), !has_total_mode, keep_row)
  ) %>%
  ungroup() %>%
  filter(keep_row) %>%
  group_by(county_fips, state_po, year) %>%
  summarise(
    county_name = first(na.omit(county_name)),
    
    votes_rep = sum(candidatevotes[party_up == "REPUBLICAN"], na.rm = TRUE),
    votes_dem = sum(candidatevotes[party_up == "DEMOCRAT"],   na.rm = TRUE),
    
    sum_candidatevotes = sum(candidatevotes, na.rm = TRUE),
    
    totalvotes_has = any(!is.na(totalvotes)),
    totalvotes_max = suppressWarnings(max(totalvotes, na.rm = TRUE)),
    
    # Key: trust totalvotes ONLY when it matches candidate sums
    total_votes = case_when(
      !totalvotes_has ~ sum_candidatevotes,
      is.infinite(totalvotes_max) ~ sum_candidatevotes,
      abs(totalvotes_max - sum_candidatevotes) <= 2 ~ totalvotes_max,
      TRUE ~ sum_candidatevotes
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    rep_share = votes_rep / total_votes,
    dem_share = votes_dem / total_votes
  ) %>%
  filter(!is.na(total_votes), total_votes > 0) %>%
  filter(rep_share >= 0, rep_share <= 1, dem_share >= 0, dem_share <= 1) %>%
  select(county_fips, county_name, state_po, year,
         votes_dem, votes_rep, total_votes, rep_share, dem_share, sum_candidatevotes)

# Coverage check
print(county_clean %>% count(year) %>% arrange(year))

# Save
write_csv(county_clean, "data/prep/counties/clean_county_votes_00-24.csv")
cat("Saved: data/prep/counties/clean_county_votes_00-24.csv\n")
library(dplyr) 
library(readr)

emb_24 <- read.csv("data/prep/precincts/2024/raw_emb_precinct_24.csv")

# Filter out tiny precincts AND zero-area slivers
emb_24 <- emb_24[emb_24$votes_tota >= 7 & emb_24$precinct_area > 0, ]

emb_24 <- emb_24 %>%
  mutate(precinct_id = precinctID) %>%
  mutate(total_votes = votes_tota) %>%
  mutate(area_km2 = precinct_area / 1000000) %>%
  mutate(vote_density = total_votes / area_km2) %>%
  mutate(rep_share = votes_rep / total_votes) %>%
  mutate(dem_share = votes_dem / total_votes) %>%
  select(precinct_id, area_km2, state, total_votes, vote_density, rep_share, dem_share, votes_rep, votes_dem, starts_with("A"))

write_csv(emb_24, "data/prep/precincts/2024/clean_emb_precinct_24.csv")
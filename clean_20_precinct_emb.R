library(dplyr) 
library(readr)

emb_20 <- read.csv("data/prep/precincts/2020/raw_emb_precinct_20.csv")

# Filter out tiny precincts AND zero-area slivers
emb_20 <- emb_20[emb_20$TOTALVOTES >= 7 & emb_20$precinct_area > 0, ]

emb_20 <- emb_20 %>%
  mutate(precinct_id = precinctID) %>%
  mutate(total_votes = TOTALVOTES) %>%
  mutate(area_km2 = precinct_area / 1000000) %>%
  mutate(vote_density = total_votes / area_km2) %>%
  mutate(votes_rep = G20PRERTRU) %>%
  mutate(votes_dem = G20PREDBID) %>%
  mutate(rep_share = votes_rep / total_votes) %>%
  mutate(dem_share = votes_dem / total_votes) %>%
  mutate(state = toupper(state)) %>%
  select(precinct_id, area_km2, state, total_votes, vote_density, rep_share, dem_share, votes_rep, votes_dem, starts_with("A"))

write_csv(emb_20, "data/prep/precincts/2020/clean_emb_precinct_20.csv")
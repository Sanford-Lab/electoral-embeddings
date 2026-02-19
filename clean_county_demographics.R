library(tidyverse)

# ==============================================================================
# PART 1: LOAD AND CLEAN EDUCATION DATA
# ==============================================================================

edu <- read.csv("data/prep/counties/Education2023.csv")

edu_flat <- edu %>%
  pivot_wider(
    id_cols     = c(FIPS.Code, State, Area.name),
    names_from  = Attribute,
    values_from = Value
  )

edu_clean <- edu_flat %>%
  pivot_longer(
    cols = -c(FIPS.Code, State, Area.name,
              contains("Urban"), contains("Continuum")),
    names_to  = "raw_name",
    values_to = "value"
  ) %>%
  separate(raw_name, into = c("desc", "year_range"), sep = ",\\s(?=[0-9])") %>%
  mutate(
    desc_clean = str_to_lower(desc),
    
    measure_type = if_else(str_detect(desc_clean, "percent"), "pct", "count"),
    
    edu_level = case_when(
      str_detect(desc_clean, "bachelor|four years")      ~ "bachelors_plus",
      str_detect(desc_clean, "some college")             ~ "some_college",
      str_detect(desc_clean, "less than|not high school")~ "less_than_hs",
      str_detect(desc_clean, "high school")              ~ "hs_only",
      TRUE ~ "ERROR"
    ),
    
    new_col_name = paste(measure_type, edu_level, sep = "_")
  ) %>%
  filter(!str_detect(new_col_name, "ERROR")) %>%
  select(FIPS.Code, State, Area.name, year_range, contains("Code"), new_col_name, value) %>%
  pivot_wider(
    names_from  = new_col_name,
    values_from = value
  ) %>%
  rename(
    county_fips  = FIPS.Code,
    state        = State,
    county_name  = Area.name
  )

cat("Education data cleaned.\n")

# ==============================================================================
# PART 2: LOAD AND CLEAN EMPLOYMENT DATA
# ==============================================================================

emp <- read.csv("data/prep/counties/Unemployment2023.csv")

emp_flat <- emp %>%
  pivot_wider(
    id_cols     = c(FIPS_Code, State, Area_Name),
    names_from  = Attribute,
    values_from = Value
  )

emp_clean <- emp_flat %>%
  pivot_longer(
    cols = -c(FIPS_Code, State, Area_Name,
              Median_Household_Income_2022,
              Med_HH_Income_Percent_of_State_Total_2022,
              Rural_Urban_Continuum_Code_2023,
              Urban_Influence_Code_2013,
              Metro_2023),
    names_to       = c(".value", "year"),
    names_pattern  = "(.*)_(\\d{4})"
  ) %>%
  rename(
    county_fips            = FIPS_Code,
    state                  = State,
    county_name            = Area_Name,
    civilian_labor_force   = Civilian_labor_force,
    employed               = Employed,
    unemployed             = Unemployed,
    unemployment_rate      = Unemployment_rate,
    median_hh_income_2022  = Median_Household_Income_2022,
    pct_state_income_2022  = Med_HH_Income_Percent_of_State_Total_2022,
    ruc_code_2023          = Rural_Urban_Continuum_Code_2023,
    ui_code_2013           = Urban_Influence_Code_2013,
    metro_2023             = Metro_2023
  ) %>%
  mutate(year = as.numeric(year)) %>%
  select(county_fips, state, county_name, year,
         unemployment_rate, unemployed, employed, civilian_labor_force,
         everything())

cat("Employment data cleaned.\n")

# ==============================================================================
# PART 3: READ EMBEDDINGS AND CREATE MASTER FILES (2016 + existing 2020/2024)
# ==============================================================================

# Read the embedding files
emb_16 <- read.csv("data/prep/counties/clean_county_emb_16_17.csv")
emb_20 <- read.csv("data/prep/counties/clean_county_emb_20.csv")
emb_24 <- read.csv("data/prep/counties/clean_county_emb_24.csv")

# --- PREPARE DATA SUBSETS TO AVOID DUPLICATE COLUMNS ---

# Education subsets
edu_subset_20_24 <- edu_clean %>%
  filter(year_range == "2019-23") %>%
  select(county_fips, starts_with("count_"), starts_with("pct_"))

edu_subset_16 <- edu_clean %>%
  filter(year_range == "2008-12") %>%
  select(county_fips, starts_with("count_"), starts_with("pct_"))

# Employment subsets
emp_subset_16 <- emp_clean %>%
  filter(year == 2016) %>%
  select(county_fips, unemployment_rate, unemployed, employed, civilian_labor_force)

emp_subset_20 <- emp_clean %>%
  filter(year == 2020) %>%
  select(county_fips, unemployment_rate, unemployed, employed, civilian_labor_force)

emp_subset_24 <- emp_clean %>%
  filter(year == 2023) %>%
  select(county_fips, unemployment_rate, unemployed, employed, civilian_labor_force,
         median_hh_income_2022, pct_state_income_2022, ruc_code_2023, ui_code_2013, metro_2023)

# ==============================================================================
# PART 4: JOIN + SAVE
# ==============================================================================

# --- 2016 MASTER ---
master_16 <- emb_16 %>%
  left_join(edu_subset_16, by = "county_fips") %>%
  left_join(emp_subset_16, by = "county_fips")

write_csv(master_16, "data/prep/counties/master_county_emb_16.csv")
cat("Success: master_county_emb_16.csv saved (Rows:", nrow(master_16), ")\n")

# --- 2020 MASTER ---
master_20 <- emb_20 %>%
  left_join(edu_subset_20_24, by = "county_fips") %>%
  left_join(emp_subset_20, by = "county_fips")

write_csv(master_20, "data/prep/counties/master_county_emb_20.csv")
cat("Success: master_county_emb_20.csv saved (Rows:", nrow(master_20), ")\n")

# --- 2024 MASTER ---
master_24 <- emb_24 %>%
  left_join(edu_subset_20_24, by = "county_fips") %>%
  left_join(emp_subset_24, by = "county_fips")

write_csv(master_24, "data/prep/counties/master_county_emb_24.csv")
cat("Success: master_county_emb_24.csv saved (Rows:", nrow(master_24), ")\n")


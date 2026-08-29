# ============================================================================
# SCRIPT: 09_manuscript_tables_20260530_v1.R
# ============================================================================
#
# PURPOSE
# -------
# Extracts top 10 and bottom 10 districts by TFR for both rounds and exports manuscript summary tables.
#
# INPUT DATA
# ----------
# - data/nfhs4_tfr_sf.rds
# - data/nfhs5_tfr_sf_20250807.rds
#
# OUTPUTS
# -------
# - output/nfhs4/top10_nfhs4.xlsx & lower10_nfhs4.xlsx (Table 1 parts)
# - output/nfhs5/top10_nfhs5.xlsx & lower10_nfhs5.xlsx (Table 1 parts)
# - output/nfhs4/tfr_nfhs4.xlsx (Supplementary S5)
# - output/nfhs5/tfr_nfhs5.xlsx (Supplementary S6)
#
# DEPENDENCIES
# ------------
# - script/03_..., script/04_...
# ============================================================================

# ============================================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================================
library(tidyverse)
library(here)
library(sf)
library(writexl)

# ============================================================================
# 2. LOAD DATA
# ============================================================================
nfhs4_tfr_sf <- read_rds(here("data", "nfhs4_tfr_sf.rds"))

nfhs5_tfr_sf_20250807 <- read_rds(here("data", "nfhs5_tfr_sf_20250807.rds"))

nfhs4 <- nfhs4_tfr_sf |> 
  sf::st_drop_geometry()

nfhs5 <- nfhs5_tfr_sf_20250807  |> 
  sf::st_drop_geometry()

# ============================================================================
# 3. PREPARE TOP AND BOTTOM 10 TABLES
# ============================================================================
top10_nfhs4 <- nfhs4 |> 
  arrange(-tfr_median) |> 
  head(10) |> 
  mutate(tfr = paste0(round(tfr_median, 2), " (", round(tfr_lower, 2), "-", round(tfr_upper, 2), ")")) |> 
  select(state.x, district.x, tfr) 

top10_nfhs5 <- nfhs5 |> 
  arrange(-tfr_median) |> 
  head(10) |> 
  mutate(tfr = paste0(round(tfr_median, 2), " (", round(tfr_lower, 2), "-", round(tfr_upper, 2), ")")) |> 
  select(state.x, district.x, tfr) 

lower10_nfhs4 <- nfhs4 |> 
  arrange(tfr_median) |> 
  head(10) |> 
  mutate(tfr = paste0(round(tfr_median, 2), " (", round(tfr_lower, 2), "-", round(tfr_upper, 2), ")")) |> 
  select(state.x, district.x, tfr) 

lower10_nfhs5 <- nfhs5 |> 
  arrange(tfr_median) |> 
  head(10) |> 
  mutate(tfr = paste0(round(tfr_median, 2), " (", round(tfr_lower, 2), "-", round(tfr_upper, 2), ")")) |> 
  select(state.x, district.x, tfr) 
  
# ============================================================================
# 4. PREPARE FULL TABLES
# ============================================================================
nfhs4_t1 <- nfhs4 |> 
  arrange(state.x, district.x, -tfr_median) |> 
  mutate(
    State = str_to_title(state.x),
    District = str_to_title(district.x),
    TFR = paste0(round(tfr_median, 2), " (", round(tfr_lower, 2), "-", round(tfr_upper, 2), ")")) |> 
  select(State, District, TFR) 

nfhs5_t1 <- nfhs5 |> 
  arrange(state.x, district.x, -tfr_median) |> 
  mutate(
    State = str_to_title(state.x),
    District = str_to_title(district.x),
    TFR = paste0(round(tfr_median, 2), " (", round(tfr_lower, 2), "-", round(tfr_upper, 2), ")")) |> 
  select(State, District, TFR) 

# ============================================================================
# 5. EXPORT TABLES
# ============================================================================
write_xlsx(top10_nfhs4,
           here("output", "nfhs4", "top10_nfhs4.xlsx"))

write_xlsx(top10_nfhs5,
           here("output", "nfhs5", "top10_nfhs5.xlsx"))

write_xlsx(lower10_nfhs4,
           here("output",  "nfhs4", "lower10_nfhs4.xlsx"))

write_xlsx(lower10_nfhs5,
           here("output",  "nfhs5", "lower10_nfhs5.xlsx"))

write_xlsx(nfhs4_t1,
           here("output",  "nfhs4", "tfr_nfhs4.xlsx"))

write_xlsx(nfhs5_t1,
           here("output",  "nfhs5", "tfr_nfhs5.xlsx"))

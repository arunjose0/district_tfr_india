# ============================================================================
# SCRIPT: 00_prior_from_srs_1971_2023.R
# ============================================================================
#
# PURPOSE
# -------
# Extracts and harmonizes Sample Registration System (SRS) data from 1971-2023 to derive informed priors for the Bayesian SAE models.
#
# INPUT DATA
# ----------
# - data/srs_asfr_1971_2012_v1.rds (Included)
# - data/srs_fertility_rates_2013_23.rds (Included)
#
# OUTPUTS
# -------
# - No formal file outputs. Generates mean/sd parameters directly for model priors.
#
# DEPENDENCIES
# ------------
# - None (First script in workflow)
# ============================================================================

# ============================================================================ #
# 1. Load required packages
# ============================================================================ #
library(tidyverse)
library(here)

# ============================================================================ #
# 2. Load data
# ============================================================================ #
df <- read_rds(here("data", "srs_asfr_1971_2012_v1.rds"))

srs_13_23 <- read_rds(here("data", "srs_fertility_rates_2013_23.rds")) |>
  filter(
    indicator %in% c("Age specific fertility rates", "Age-Specific Fertility Rates"),
    geography == "total",
    year > 2013
  ) |>
  rename(indicators = indicator, rate = value, state = region) |>
  select(-geography)

# ============================================================================ #
# 3. Clean and harmonize data
# ============================================================================ #
df_clean <- bind_rows(df, srs_13_23) |>
  mutate(state = case_when(
    state == "Chhatisgarh" ~ "Chhattisgarh",
    state == "Himachal"    ~ "Himachal Pradesh",
    state == "Telengana"   ~ "Telangana",
    state == "Pondicherry" ~ "Puducherry",
    TRUE ~ state
  )) |>
  mutate(state = str_replace_all(state, " & ", " and ")) |>
  filter(indicators %in% c("Age specific fertility rates", "Age-Specific Fertility Rates")) |>
  mutate(
    year_final = coalesce(year, as.integer(str_sub(period_year, 1, 4)) + 1L),
    rate = rate / 1000
  ) |>
  filter(!is.na(rate), rate > 0, state != "India")

# ============================================================================ #
# 4. Calculate prior parameters
# ============================================================================ #
round(mean(log(df_clean$rate)), 1)
round(sd(log(df_clean$rate)), 1)

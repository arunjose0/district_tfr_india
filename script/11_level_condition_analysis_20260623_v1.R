# ============================================================================
# SCRIPT: 11_level_condition_analysis_20260623_v1.R
# ============================================================================
#
# PURPOSE
# -------
# Analyses the magnitude of district-level TFR decline between NFHS-4 and NFHS-5 conditional on baseline fertility levels.
#
# INPUT DATA
# ----------
# - data/nfhs4_district_lookup.rds & data/nfhs5_district_lookup.rds
# - data/nfhs4_tfr_sf.rds & data/nfhs5_tfr_sf_20250807.rds
#
# OUTPUTS
# -------
# - Generates intermediate level-conditioning summary statistics (Printed to console)
#
# DEPENDENCIES
# ------------
# - script/03_..., script/04_...
# ============================================================================

#
# Analyzes the decline in TFR between NFHS-4 and NFHS-5 conditioned on baseline fertility levels from NFHS-4.
# data/nfhs4_district_lookup.rds
# data/nfhs5_district_lookup.rds
# data/nfhs4_tfr_sf.rds
# data/nfhs5_tfr_sf_20250807.rds
# Generates tfr_combined object, level conditioning plots, and summary tables.

# ============================================================================
# 1. Load Required Packages
# ============================================================================

library(tidyverse)
library(here)
library(sf)

# ============================================================================
# 2. Load Datasets
# ============================================================================

nfhs4_district_lookup <- read_rds(here("data", "nfhs4_district_lookup.rds"))
nfhs5_district_lookup <- read_rds(here("data", "nfhs5_district_lookup.rds"))
nfhs4_tfr <- read_rds(here("data", "nfhs4_tfr_sf.rds"))
nfhs5_tfr <- read_rds(here("data", "nfhs5_tfr_sf_20250807.rds"))

# ============================================================================
# 3. Identify Comparable Districts
# ============================================================================

nfhs5_lookup <- nfhs5_district_lookup %>%
  mutate(district_code = haven::zap_labels(district_code) %>% as.numeric()) %>%
  filter(district_code < 801) %>%
  select(district_code, district_name, state_code, state_name, unique_id)

cat("NFHS5 comparable districts:", nrow(nfhs5_lookup), "\n\n")

matched_districts <- inner_join(
  nfhs4_district_lookup %>% 
    select(unique_id, district_code, district_name, state_name),
  nfhs5_lookup %>% 
    select(unique_id, district_code, district_name, state_name),
  by = "unique_id",
  suffix = c("_nfhs4", "_nfhs5")
)

comparable_unique_ids <- unique(matched_districts$unique_id)

# ============================================================================
# 4. Standardize NFHS-4 TFR Data
# ============================================================================

nfhs4_tfr_raw <- nfhs4_tfr %>%
  sf::st_drop_geometry()

nfhs4_tfr_std <- nfhs4_tfr_raw %>%
  mutate(
    # Extract state and district names from TFR file
    state_name_raw = state.x,
    district_name_raw = district.x
  ) %>%
  mutate(
    # STATE STANDARDIZATION (same as lookup)
    state_name = case_when(
      state_name_raw == "Himanchal Pradesh" ~ "Himachal Pradesh", 
      state_name_raw == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      TRUE ~ state_name_raw
    )
  ) %>%
  mutate(
    # CORRECT STATE NAMES FOR SPECIFIC DISTRICTS (to match lookup)
    state_name = case_when(
      district_name_raw == "Leh" ~ "Ladakh",
      district_name_raw == "Kargil" ~ "Ladakh",
      district_name_raw == "Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
      district_name_raw == "Daman" ~ "Dadra and Nagar Haveli and Daman and Diu",
      district_name_raw == "Dadra & Nagar Haveli" ~ "Dadra and Nagar Haveli and Daman and Diu",
      TRUE ~ state_name
    )
  ) %>%
  mutate(
    # DISTRICT NAME CORRECTIONS (same as lookup)
    district_name = case_when(
      district_name_raw == "Leh" ~ "Leh",
      district_name_raw == "Kargil" ~ "Kargil",
      district_name_raw == "Diu" ~ "Diu",
      district_name_raw == "Daman" ~ "Daman",
      district_name_raw == "Dadra & Nagar Haveli" ~ "Dadra and Nagar Haveli",
      TRUE ~ district_name_raw
    )
  ) %>%
  # CREATE unique_id - REMOVE parentheses, "&" AND word "and" (same as lookup)
  mutate(
    unique_id = paste0(str_to_lower(state_name), " ", str_to_lower(district_name)),
    unique_id = str_remove_all(unique_id, "\\(.*?\\)"),  # Remove anything in parentheses
    unique_id = str_remove_all(unique_id, "\\band\\b|&"),  # Remove "and" and "&"
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  ) %>%
  filter(unique_id %in% comparable_unique_ids) %>%
  select(
    unique_id,
    state = state.x, 
    district = district.x, 
    tfr_nfhs4 = tfr_median,
    tfr_lower_nfhs4 = tfr_lower,
    tfr_upper_nfhs4 = tfr_upper
  ) %>%
  mutate(
    baseline_tfr_cat = case_when(
      tfr_nfhs4 < 2.1 ~ "<2.1",
      tfr_nfhs4 >= 2.1 & tfr_nfhs4 < 3 ~ "2.1 to 3",
      tfr_nfhs4 >= 3 ~ "≥3.0",
      TRUE ~ NA_character_
    ),
    baseline_tfr_cat = factor(baseline_tfr_cat, levels = c("<2.1", "2.1 to 3", "≥3.0"))
  )

cat("NFHS4 TFR filtered to comparable districts:", nrow(nfhs4_tfr_std), "\n")

# ============================================================================
# 5. Standardize NFHS-5 TFR Data
# ============================================================================

nfhs5_tfr_raw <- nfhs5_tfr %>%
  sf::st_drop_geometry()

nfhs5_tfr_std <- nfhs5_tfr_raw %>%
  mutate(
    state_name_raw = state.x,
    district_name_raw = district.x
  ) %>%
  mutate(
    state_name = case_when(
      state_name_raw == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      state_name_raw == "Jammu  Kashmir" ~ "Jammu and Kashmir",
      state_name_raw == "Nct Of Delhi" ~ "Delhi",
      state_name_raw == "Andaman & Nicobar Islands" ~ "Andaman and Nicobar Islands",
      state_name_raw == "Andaman  Nicobar Islands" ~ "Andaman and Nicobar Islands",
      state_name_raw == "Dadra & Nagar Haveli And Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
      state_name_raw == "Dadra  Nagar Haveli And Daman  Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
      TRUE ~ state_name_raw
    )
  ) %>%
  mutate(
    district_name = case_when(
      district_name_raw == "Leh(Ladakh)" ~ "Leh",
      district_name_raw == "Kargil" ~ "Kargil",
      district_name_raw == "Papum Pare" ~ "Papumpare",
      district_name_raw == "Saraikela-Kharsawan" ~ "Saraikela Kharsawan",
      district_name_raw == "Koriya" ~ "Korea (Koriya)",
      district_name_raw == "Buxer" ~ "Buxar",
      district_name_raw == "Kabeerdham" ~ "Kabirdham",
      district_name_raw == "Banas Kantha" ~ "Banaskantha",
      district_name_raw == "Janjgir-Champa" ~ "Janjgir - Champa",
      district_name_raw == "Siddharthnagar" ~ "Siddharth Nagar",
      district_name_raw == "Sant Ravidas Nagar (Bhadohi)" ~ "Sant Ravidas Nagar",
      district_name_raw == "Maharajganj" ~ "Mahrajganj",
      district_name_raw == "Buxar" ~ "Buxer",
      district_name_raw == "Diu" ~ "Diu",
      district_name_raw == "Daman" ~ "Daman",
      district_name_raw == "Dadra & Nagar Haveli" ~ "Dadra and Nagar Haveli",
      TRUE ~ district_name_raw
    )
  ) %>%
  mutate(
    unique_id = paste0(str_to_lower(state_name), " ", str_to_lower(district_name)),
    unique_id = str_remove_all(unique_id, "\\(.*?\\)"),
    unique_id = str_remove_all(unique_id, "\\band\\b|&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  ) %>%
  filter(unique_id %in% comparable_unique_ids) %>%
  select(
    unique_id,
    state = state.x,
    district = district.x,
    tfr_nfhs5 = tfr_median,
    tfr_lower_nfhs5 = tfr_lower,
    tfr_upper_nfhs5 = tfr_upper
  )

cat("NFHS5 TFR filtered to comparable districts:", nrow(nfhs5_tfr_std), "\n")


# ============================================================================
# 6. Merge TFR Data and Calculate Decline
# ============================================================================

# Merge by unique_id
tfr_combined <- nfhs4_tfr_std %>%
  inner_join(
    nfhs5_tfr_std,
    by = "unique_id",
    suffix = c("_nfhs4", "_nfhs5")
  ) %>%
  mutate(
    tfr_decline = tfr_nfhs4 - tfr_nfhs5,
    tfr_decline_pct = (tfr_decline / tfr_nfhs4) * 100
  ) %>%
  select(
    unique_id,
    state_nfhs4,
    district_nfhs4,
    state_nfhs5,
    district_nfhs5,
    baseline_tfr_cat,
    tfr_nfhs4,
    tfr_nfhs5,
    tfr_lower_nfhs4,
    tfr_upper_nfhs4,
    tfr_lower_nfhs5,
    tfr_upper_nfhs5,
    tfr_decline,
    tfr_decline_pct
  )

cat("Final merged dataset:", nrow(tfr_combined), "\n\n")

# ============================================================================
# 7. Visualization: TFR Decline by Baseline Category
# ============================================================================

# Boxplot: TFR Decline by Baseline Category
tfr_combined %>%
  ggplot(aes(x = baseline_tfr_cat, y = tfr_decline, fill = baseline_tfr_cat)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(
    x = "Baseline TFR Category (NFHS4)",
    y = "TFR Decline (NFHS4 - NFHS5)",
    title = "Level Conditioning: TFR Decline by Baseline Stratum",
    fill = "Baseline\nCategory"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12)
  )

# ============================================================================
# 8. Summary Statistics
# ============================================================================

# Summary statistics for caption
tfr_combined %>%
  group_by(baseline_tfr_cat) %>%
  summarise(
    n = n(),
    median_decline = round(median(tfr_decline), 3),
    q1 = round(quantile(tfr_decline, 0.25), 3),
    q3 = round(quantile(tfr_decline, 0.75), 3),
    .groups = "drop"
  ) %>%
  print()


# ============================================================================
# 9. Level Conditioning: Summary Section
# ============================================================================

cat("========== LEVEL CONDITIONING: TFR DECLINE BY BASELINE FERTILITY LEVEL ==========\n\n")

level_conditioning_summary <- tfr_combined %>%
  group_by(baseline_tfr_cat) %>%
  summarise(
    n = n(),
    median_decline_abs = median(tfr_decline, na.rm = TRUE),
    q1_decline_abs = quantile(tfr_decline, 0.25, na.rm = TRUE),
    q3_decline_abs = quantile(tfr_decline, 0.75, na.rm = TRUE),
    median_decline_pct = median(tfr_decline_pct, na.rm = TRUE),
    q1_decline_pct = quantile(tfr_decline_pct, 0.25, na.rm = TRUE),
    q3_decline_pct = quantile(tfr_decline_pct, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Display results by fertility level with proper labels
cat("BELOW-REPLACEMENT FERTILITY (TFR ≤2.1):\n")
cat(sprintf("  Districts: %d\n", level_conditioning_summary$n[1]))
cat(sprintf("  Median decline: %.3f TFR units (IQR: %.3f to %.3f)\n",
            level_conditioning_summary$median_decline_abs[1],
            level_conditioning_summary$q1_decline_abs[1],
            level_conditioning_summary$q3_decline_abs[1]))
cat(sprintf("  Relative decline: %.1f%% (IQR: %.1f%% to %.1f%%)\n\n",
            level_conditioning_summary$median_decline_pct[1],
            level_conditioning_summary$q1_decline_pct[1],
            level_conditioning_summary$q3_decline_pct[1]))

cat("INTERMEDIATE FERTILITY (TFR 2.1–3.0):\n")
cat(sprintf("  Districts: %d\n", level_conditioning_summary$n[2]))
cat(sprintf("  Median decline: %.3f TFR units (IQR: %.3f to %.3f)\n",
            level_conditioning_summary$median_decline_abs[2],
            level_conditioning_summary$q1_decline_abs[2],
            level_conditioning_summary$q3_decline_abs[2]))
cat(sprintf("  Relative decline: %.1f%% (IQR: %.1f%% to %.1f%%)\n\n",
            level_conditioning_summary$median_decline_pct[2],
            level_conditioning_summary$q1_decline_pct[2],
            level_conditioning_summary$q3_decline_pct[2]))

cat("HIGH FERTILITY (TFR ≥3.0):\n")
cat(sprintf("  Districts: %d\n", level_conditioning_summary$n[3]))
cat(sprintf("  Median decline: %.3f TFR units (IQR: %.3f to %.3f)\n",
            level_conditioning_summary$median_decline_abs[3],
            level_conditioning_summary$q1_decline_abs[3],
            level_conditioning_summary$q3_decline_abs[3]))
cat(sprintf("  Relative decline: %.1f%% (IQR: %.1f%% to %.1f%%)\n\n",
            level_conditioning_summary$median_decline_pct[3],
            level_conditioning_summary$q1_decline_pct[3],
            level_conditioning_summary$q3_decline_pct[3]))

# Summary table
cat("\n========== SUMMARY TABLE ==========\n\n")
level_conditioning_summary %>%
  mutate(
    Fertility_Level = c("Below-replacement (≤2.1)", "Intermediate (2.1–3.0)", "High (≥3.0)"),
    Absolute_Decline = sprintf("%.3f (%.3f-%.3f)", 
                               median_decline_abs, q1_decline_abs, q3_decline_abs),
    Percentage_Decline = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                 median_decline_pct, q1_decline_pct, q3_decline_pct)
  ) %>%
  select(Fertility_Level, n, Absolute_Decline, Percentage_Decline) %>%
  rename("Fertility Level" = Fertility_Level, "N" = n, "TFR Decline" = Absolute_Decline, "Relative Decline (%)" = Percentage_Decline) %>%
  print()

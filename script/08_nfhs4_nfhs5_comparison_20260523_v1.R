# ============================================================================
# SCRIPT: 08_nfhs4_nfhs5_comparison_20260523_v1.R
# ============================================================================
#
# PURPOSE
# -------
# Compares fertility categories and profiles between NFHS-4 and NFHS-5, generating aggregate shifts and stacked bar charts.
#
# INPUT DATA
# ----------
# - data/nfhs4_tfr_sf.rds
# - data/nfhs5_tfr_sf_20250807.rds
#
# OUTPUTS
# -------
# - output/comparison/nfhs4_nfhs5_comp_fertility_stacked_chart.png (Supplementary S11)
# - output/census_nfhs45_tfr_combined.rds
#
# DEPENDENCIES
# ------------
# - script/03_..., script/04_...
# ============================================================================

# ============================================================================
# <SCRIPT: 08_nfhs4_nfhs5_comparison_20260523_v1.R>
#
# Generates a state-level stacked bar chart comparing the distribution of 
# district TFR categories (<2.1, 2.1-3.0, >=3.0) between NFHS-4 and NFHS-5.
#
# - data/nfhs4_tfr_sf.rds (Generated locally)
# - data/nfhs5_tfr_sf_20250807.rds (Generated locally)
#
# - output/comparison/nfhs4_nfhs5_comp_fertility_stacked_chart.png
#
# NFHS-4 vs NFHS-5: STACKED BAR CHART (Three TFR Categories)
# ============================================================================

library(tidyverse)
library(sf)
library(here)
library(ggplot2)

# ============================================================================
# LOAD RAW DATA
# ============================================================================

cat("Loading NFHS-4 and NFHS-5 data...\n")

nfhs4_tfr_sf <- read_rds(here("data", "nfhs4_tfr_sf.rds"))
nfhs5_tfr_sf <- read_rds(here("data", "nfhs5_tfr_sf_20250807.rds"))

nfhs4 <- nfhs4_tfr_sf |> sf::st_drop_geometry()
nfhs5 <- nfhs5_tfr_sf |> sf::st_drop_geometry()

# ============================================================================
# STANDARDIZE STATE NAMES INLINE
# ============================================================================

cat("Standardizing state names...\n")

# NFHS-4: Apply standardization mapping
nfhs4 <- nfhs4 |>
  mutate(
    state.x = case_when(
      state.x == "Andaman and Nicobar Islands" ~ "Andaman & Nicobar Islands",
      state.x == "Dadra and Nagar Haveli" ~ "Dadra & Nagar Haveli & Daman & Diu",
      state.x == "Daman and Diu" ~ "Dadra & Nagar Haveli & Daman & Diu",
      state.x == "Delhi" ~ "NCT of Delhi",
      state.x == "Jammu and Kashmir" ~ "Jammu & Kashmir",
      TRUE ~ state.x
    )
  )

# NFHS-5: Apply standardization mapping and exclude Ladakh
nfhs5 <- nfhs5 |>
  mutate(
    state.x = case_when(
      state.x == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar Islands",
      state.x == "Dadra & Nagar Haveli & Daman & Diu" ~ "Dadra & Nagar Haveli & Daman & Diu",
      state.x == "Nct Of Delhi" ~ "NCT of Delhi",
      state.x == "Jammu & Kashmir" ~ "Jammu & Kashmir",
      state.x == "Ladakh" ~ NA_character_,
      TRUE ~ state.x
    )
  ) |>
  filter(!is.na(state.x))

cat("✓ State names standardized\n\n")

# ============================================================================
# CREATE THREE TFR CATEGORIES
# ============================================================================

nfhs4 <- nfhs4 |>
  mutate(
    tfr_cat = case_when(
      tfr_median < 2.1 ~ "<2.1",
      tfr_median >= 2.1 & tfr_median < 3.0 ~ "2.1–3.0",
      tfr_median >= 3.0 ~ "≥3.0",
      TRUE ~ NA_character_
    ),
    tfr_cat = factor(tfr_cat, levels = c("<2.1", "2.1–3.0", "≥3.0"))
  )

nfhs5 <- nfhs5 |>
  mutate(
    tfr_cat = case_when(
      tfr_median < 2.1 ~ "<2.1",
      tfr_median >= 2.1 & tfr_median < 3.0 ~ "2.1–3.0",
      tfr_median >= 3.0 ~ "≥3.0",
      TRUE ~ NA_character_
    ),
    tfr_cat = factor(tfr_cat, levels = c("<2.1", "2.1–3.0", "≥3.0"))
  )

cat("✓ TFR categories created\n\n")

# ============================================================================
# CREATE STATE PROFILES (BY TFR CATEGORY)
# ============================================================================

nfhs4_state_profile <- nfhs4 |>
  group_by(state.x) |>
  summarise(
    total_districts = n(),
    cat_lt_2_1 = round(sum(tfr_cat == "<2.1", na.rm = TRUE) / total_districts * 100, 1),
    cat_2_1_3 = round(sum(tfr_cat == "2.1–3.0", na.rm = TRUE) / total_districts * 100, 1),
    cat_3_plus = round(sum(tfr_cat == "≥3.0", na.rm = TRUE) / total_districts * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(cat_lt_2_1)) |>
  mutate(survey = "2015-16")

nfhs5_state_profile <- nfhs5 |>
  group_by(state.x) |>
  summarise(
    total_districts = n(),
    cat_lt_2_1 = round(sum(tfr_cat == "<2.1", na.rm = TRUE) / total_districts * 100, 1),
    cat_2_1_3 = round(sum(tfr_cat == "2.1–3.0", na.rm = TRUE) / total_districts * 100, 1),
    cat_3_plus = round(sum(tfr_cat == "≥3.0", na.rm = TRUE) / total_districts * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(cat_lt_2_1)) |>
  mutate(survey = "2019-21")

cat("✓ NFHS-4 (2015-16) state profile created\n")
cat("✓ NFHS-5 (2019-21) state profile created\n\n")

# ============================================================================
# COMBINE AND RESHAPE FOR STACKED BAR CHART
# ============================================================================

combined_profile <- bind_rows(nfhs4_state_profile, nfhs5_state_profile) |>
  rename(state = state.x)

stacked_data <- combined_profile |>
  select(state, survey, cat_lt_2_1, cat_2_1_3, cat_3_plus) |>
  pivot_longer(
    cols = starts_with("cat_"),
    names_to = "category",
    values_to = "percentage"
  ) |>
  mutate(
    category = case_when(
      category == "cat_lt_2_1" ~ "<2.1",
      category == "cat_2_1_3" ~ "2.1–3.0",
      category == "cat_3_plus" ~ "≥3.0",
      TRUE ~ category
    ),
    category = factor(category, levels = c("<2.1", "2.1–3.0", "≥3.0"))
  )

cat("✓ Data prepared for stacked bar chart\n\n")

# ============================================================================
# CREATE STACKED BAR CHART (ALL STATES)
# ============================================================================

cat("Creating stacked bar chart...\n")

# Sort states by below-replacement fertility
all_states_sorted <- nfhs5_state_profile |>
  arrange(desc(cat_lt_2_1)) |>
  pull(state.x)

p_stacked <- ggplot(
  stacked_data |> mutate(state = factor(state, levels = all_states_sorted)),
  aes(x = survey, y = percentage, fill = category)
) +
  geom_col(position = "stack", alpha = 0.9, width = 0.6) +
  facet_wrap(~state, scales = "free_y", ncol = 6) +
  scale_fill_manual(
    values = c(
      "<2.1" = "#4575b4",      # Blue (Below replacement)
      "2.1–3.0" = "#ffffbf",   # Yellow (Intermediate)
      "≥3.0" = "#d73027"       # Red (High fertility)
    ),
    name = "TFR Category"
  ) +
  labs(
    title = "District-Level Fertility Distribution by State",
    subtitle = "NFHS-4 (2015-16) vs NFHS-5 (2019-21)",
    y = "% of Districts",
    x = "Survey Year",
    fill = "TFR Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    strip.text = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.major.x = element_blank()
  )

print(p_stacked)
cat("(Stacked bar chart ready)\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("========== NATIONAL SUMMARY ==========\n\n")

cat("2015-16 (NFHS-4) National Distribution:\n")
nfhs4_summary <- nfhs4 |>
  summarise(
    "<2.1" = round(sum(tfr_cat == "<2.1", na.rm = TRUE) / n() * 100, 1),
    "2.1–3.0" = round(sum(tfr_cat == "2.1–3.0", na.rm = TRUE) / n() * 100, 1),
    "≥3.0" = round(sum(tfr_cat == "≥3.0", na.rm = TRUE) / n() * 100, 1)
  )
print(nfhs4_summary)

cat("\n2019-21 (NFHS-5) National Distribution:\n")
nfhs5_summary <- nfhs5 |>
  summarise(
    "<2.1" = round(sum(tfr_cat == "<2.1", na.rm = TRUE) / n() * 100, 1),
    "2.1–3.0" = round(sum(tfr_cat == "2.1–3.0", na.rm = TRUE) / n() * 100, 1),
    "≥3.0" = round(sum(tfr_cat == "≥3.0", na.rm = TRUE) / n() * 100, 1)
  )
print(nfhs5_summary)

ggsave(here("output", "comparison", 'nfhs4_nfhs5_comp_fertility_stacked_chart.png'), p_stacked, width=18, height=14, dpi=300)

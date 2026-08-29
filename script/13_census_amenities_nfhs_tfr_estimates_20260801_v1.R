# ============================================================================
# SCRIPT: 13_census_amenities_nfhs_tfr_estimates_20260801_v1.R
# ============================================================================
#
# PURPOSE
# -------
# Validates Bayesian TFR estimates against 2011 Census amenities (e.g., electricity, water) for socioeconomic consistency.
#
# INPUT DATA
# ----------
# - data/census_amenities_district_std.csv (Included)
# - data/india_shp_census11.rds (External)
# - data/nfhs4_tfr_sf.rds & data/nfhs5_tfr_sf_20250807.rds
#
# OUTPUTS
# -------
# - output/Figure_SES_validation_final_NFHS4.png (Components for Figure 4)
# - output/Figure_SES_validation_final_NFHS5.png
#
# DEPENDENCIES
# ------------
# - script/03_..., script/04_...
# ============================================================================

#
# Socioeconomic validation of district-level TFR estimates against 2011 Census amenities.
# data/census_amenities_district_std.rds
# output/census_nfhs45_tfr_combined.rds
# output/Figure_SES_validation_final_NFHS4.png
# output/Figure_SES_validation_final_NFHS5.png

# ============================================================================
# 1. Load Required Packages
# ============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(here)

cat("========== SOCIOECONOMIC VALIDATION ==========\n\n")

# ============================================================================
# 2. Load Census Amenities Data
# ============================================================================

census_amenities <- read_rds(here("data", "census_amenities_district_std.rds"))

# ============================================================================
# 3. Load and Standardize NFHS-4 and NFHS-5 District-Level TFR
# ============================================================================

df_all <- read_rds(here("output", "census_nfhs45_tfr_combined.rds")) |>
  sf::st_drop_geometry()

df_all_clean <- df_all |>
  mutate(
    state_std = str_to_lower(state) |> str_squish(),
    district_std = str_to_lower(district) |> str_squish(),
    district_std = str_replace_all(district_std, " - ", " "),
    district_std = str_replace_all(district_std, " and ", " "),
    district_std = str_replace_all(district_std, "&", "and"),
    district_std = str_remove(district_std, "\\s*\\([^)]*\\)$")
  ) |>
  mutate(
    district_std = case_when(
      district_std == "nicobars" ~ "nicobar",
      district_std == "papumpare" ~ "papum pare",
      district_std == "morigaon" ~ "marigaon",
      district_std == "janjgir champa" ~ "janjgir-champa",
      district_std == "kabirdham" ~ "kabeerdham",
      district_std == "korea" ~ "koriya",
      district_std == "banaskantha" ~ "banas kantha",
      district_std == "panchmahal" ~ "panch mahals",
      district_std == "sabarkantha" ~ "sabar kantha",
      district_std == "lahul spiti" ~ "lahul and spiti",
      district_std == "saraikela kharsawan" ~ "saraikela-kharsawan",
      district_std == "chamarajanagar" ~ "chamrajnagar",
      district_std == "khandwa" ~ "east nimar",
      district_std == "khargone" ~ "west nimar",
      district_std == "gadchiroli" ~ "garhchiroli",
      district_std == "ribhoi" ~ "ri bhoi",
      district_std == "lawngtlai" ~ "lawangtlai",
      district_std == "baudh" ~ "bauda",
      district_std == "east district" ~ "east",
      district_std == "north district" ~ "north",
      district_std == "south district" ~ "south",
      district_std == "west district" ~ "west",
      district_std == "nagapattinam" ~ "nagappattinam",
      district_std == "virudhunagar" ~ "virudunagar",
      district_std == "kanshiram nagar" ~ "kansiram nagar",
      district_std == "mahrajganj" ~ "maharajganj",
      district_std == "sant ravidas nagar" ~ "sant ravi das nagar",
      district_std == "north twenty four parganas" ~ "north 24 parganas",
      district_std == "south twenty four parganas" ~ "south 24 parganas",
      TRUE ~ district_std
    )
  ) |>
  mutate(
    state_std = case_when(
      state_std == "telangana" ~ "andhra pradesh",
      TRUE ~ state_std
    ),
    district_std = case_when(
      district_std == "paschim medinipur" ~ "pashchim medinipur",
      TRUE ~ district_std
    )
  )

nfhs4_data <- df_all_clean |>
  filter(data == "NFHS 4") |>
  select(state, district, state_std, district_std, tfr_median, tfr_lower, tfr_upper) |>
  rename(nfhs4_tfr = tfr_median, nfhs4_tfr_lower = tfr_lower, nfhs4_tfr_upper = tfr_upper) |>
  distinct()

nfhs5_data <- df_all_clean |>
  filter(data == "NFHS 5") |>
  select(state, district, state_std, district_std, tfr_median, tfr_lower, tfr_upper) |>
  rename(nfhs5_tfr = tfr_median, nfhs5_tfr_lower = tfr_lower, nfhs5_tfr_upper = tfr_upper) |>
  distinct()

# ============================================================================
# 4. Prepare Socioeconomic Variables
# ============================================================================

cat("\n========== 6-VARIABLE VALIDATION ==========\n\n")

ses_vars_final <- census_amenities |>
  mutate(
    pct_bathing_facility = number_of_households_having_bathing_facility_within_the_premises_yes_bathroom +
      number_of_households_having_bathing_facility_within_the_premises_yes_enclosure_without_roof
  ) |>
  select(
    state_std, district_std,
    pct_electricity        = main_source_of_lighting_electricity,
    pct_lpg_cooking        = type_of_fuel_used_for_cooking_lpg_png,
    pct_television          = availability_of_assets_television,
    pct_bathing_facility,
    pct_handpump_water      = main_source_of_drinking_water_handpump,
    pct_permanent_housing   = households_by_type_of_structure_of_census_houses_permanent
  )

ses_labels_final <- c(
  pct_electricity        = "% Households with Electricity",
  pct_lpg_cooking        = "% Households Using LPG/PNG for Cooking",
  pct_television         = "% Households with Television",
  pct_bathing_facility   = "% Households with Bathing Facility",
  pct_handpump_water     = "% Households Using Handpump Water",
  pct_permanent_housing  = "% Households in Permanent Housing"
)

# ============================================================================
# 5. Merge TFR Data with Socioeconomic Variables
# ============================================================================

tfr_ses_final_nfhs4 <- nfhs4_data |>
  inner_join(ses_vars_final, by = c("state_std", "district_std"))

tfr_ses_final_nfhs5 <- nfhs5_data |>
  inner_join(ses_vars_final, by = c("state_std", "district_std"))

cat(sprintf("Matched %d districts (NFHS-4) / %d districts (NFHS-5)\n\n",
            nrow(tfr_ses_final_nfhs4), nrow(tfr_ses_final_nfhs5)))

anti_join(nfhs4_data, ses_vars_final, by = c("state_std", "district_std")) |>
  distinct(state_std, district_std)

anti_join(nfhs5_data, ses_vars_final, by = c("state_std", "district_std")) |>
  distinct(state_std, district_std)

# ============================================================================
# 6. Panel Building Function
# ============================================================================

make_final_panel <- function(data, var, label, tfr_col, tfr_round, point_color) {
  
  cor_val <- cor.test(data[[tfr_col]], data[[var]])$estimate
  
  panel_title <- sprintf("TFR (%s) vs %s (Census 2011)", tfr_round, label)
  
  ggplot(data, aes(x = .data[[var]], y = .data[[tfr_col]])) +
    
    geom_point(alpha = 0.5, size = 2, color = point_color) +
    
    geom_smooth(method = "lm", se = TRUE, color = "red",
                fill = "red", alpha = 0.1, linewidth = 1) +
    
    labs(
      title = panel_title,
      subtitle = sprintf("r = %.3f", cor_val),
      x = label,
      y = sprintf("TFR (%s)", tfr_round)
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 9, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
      axis.title.y = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank()
    )
}

# ============================================================================
# 7. Build NFHS-4 Panels
# ============================================================================

panels_nfhs4 <- purrr::map(names(ses_labels_final), function(v) {
  make_final_panel(
    data = tfr_ses_final_nfhs4,
    var = v,
    label = ses_labels_final[[v]],
    tfr_col = "nfhs4_tfr",
    tfr_round = "NFHS-4",
    point_color = "#2c7fb8"
  )
})

p_final_nfhs4 <- (panels_nfhs4[[1]] | panels_nfhs4[[2]] | panels_nfhs4[[3]]) /
  (panels_nfhs4[[4]] | panels_nfhs4[[5]] | panels_nfhs4[[6]]) +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

print(p_final_nfhs4)

ggsave(
  here("output", "Figure_SES_validation_final_NFHS4.png"),
  p_final_nfhs4, width = 14, height = 8, dpi = 300
)

# ============================================================================
# 8. Build NFHS-5 Panels
# ============================================================================

panels_nfhs5 <- purrr::map(names(ses_labels_final), function(v) {
  make_final_panel(
    data = tfr_ses_final_nfhs5,
    var = v,
    label = ses_labels_final[[v]],
    tfr_col = "nfhs5_tfr",
    tfr_round = "NFHS-5",
    point_color = "#2c7fb8"
  )
})

p_final_nfhs5 <- (panels_nfhs5[[1]] | panels_nfhs5[[2]] | panels_nfhs5[[3]]) /
  (panels_nfhs5[[4]] | panels_nfhs5[[5]] | panels_nfhs5[[6]]) +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

print(p_final_nfhs5)

ggsave(
  here("output", "Figure_SES_validation_final_NFHS5.png"),
  p_final_nfhs5, width = 14, height = 8, dpi = 300
)

cat("\n✓ Final 6-variable validation figures saved (NFHS-4 and NFHS-5)\n")

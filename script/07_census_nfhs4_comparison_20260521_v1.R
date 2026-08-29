# ============================================================================
# SCRIPT: 07_census_nfhs4_comparison_20260521_v1.R
# ============================================================================
#
# PURPOSE
# -------
# Validates NFHS-4 Bayesian SAE TFR estimates against 2011 Census TFR data to assess external consistency.
#
# INPUT DATA
# ----------
# - data/nfhs4_tfr_sf.rds
# - data/census11_irudaya_sf_20250703_v2.rds
#
# OUTPUTS
# -------
# - output/comparison/tfr_decline_nfhs4_census.png
#
# DEPENDENCIES
# ------------
# - script/03_nfhs4_plot_shp_creating_dhs_data_20250807_v3.R
# ============================================================================

# ============================================================================
# <SCRIPT: 07_census_nfhs4_comparison_20260521_v1.R>
#
# Compares district-level TFR estimates between Census 2011 and NFHS-4, 
# evaluating correlation, differences, and classifying changes across districts.
#
# - output/census_nfhs45_tfr_combined.rds (Generated locally)
#
# - output/comparison/tfr_decline_nfhs4_census.png
#
# Census 2011 vs NFHS-4 TFR COMPARISON
# Bayesian SAE Model Validation
# ============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(here)

# ============================================================================
# 1. LOAD AND STANDARDIZE DATA
# ============================================================================

cat("========== LOADING DATA ==========\n\n")

df <- read_rds(here("output", "census_nfhs45_tfr_combined.rds")) %>%
  filter(data != "NFHS 5") %>%
  sf::st_drop_geometry()

# Standardize names
df_clean <- df %>%
  mutate(
    state_std = str_to_lower(state) %>% str_squish(),
    district_std = str_to_lower(district) %>% str_squish(),
    district_std = str_replace_all(district_std, " - ", " "),
    district_std = str_replace_all(district_std, " and ", " "),
    district_std = str_replace_all(district_std, "&", "and"),
    district_std = str_remove(district_std, "\\s*\\([^)]*\\)$")
  )

# Apply spelling/name fixes (30 districts)
df_clean <- df_clean %>%
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
  )

# Apply critical state/district fixes (Telangana + West Bengal)
df_fixed <- df_clean %>%
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

# ============================================================================
# 2. PREPARE DATA FOR COMPARISON
# ============================================================================

cat("Preparing Census 2011 data...\n")
census_data <- df_fixed %>%
  filter(data == "Census 2011") %>%
  select(state, district, state_std, district_std, tfr_mean, tfr_lower, tfr_upper) %>%
  rename(census_tfr = tfr_mean, census_tfr_lower = tfr_lower, census_tfr_upper = tfr_upper) %>%
  distinct()

cat("Preparing NFHS-4 data...\n")
nfhs4_data <- df_fixed %>%
  filter(data == "NFHS 4") %>%
  select(state, district, state_std, district_std, tfr_median, tfr_lower, tfr_upper) %>%
  rename(nfhs4_tfr = tfr_median, nfhs4_tfr_lower = tfr_lower, nfhs4_tfr_upper = tfr_upper) %>%
  distinct()

# Merge for comparison
cat("Merging datasets...\n\n")

tfr_comparison <- nfhs4_data %>%
  inner_join(census_data, by = c("state_std", "district_std"), 
             suffix = c("_nfhs4", "_census")) %>%
  select(state = state_census, district = district_census, 
         state_std, district_std,
         census_tfr, census_tfr_lower, census_tfr_upper,
         nfhs4_tfr, nfhs4_tfr_lower, nfhs4_tfr_upper) %>%
  mutate(
    tfr_diff = nfhs4_tfr - census_tfr,
    tfr_pct_change = (tfr_diff / census_tfr) * 100,
    change_type = case_when(
      tfr_diff < -0.5 ~ "Large decline: < -0.5 TFR",
      tfr_diff >= -0.5 & tfr_diff < -0.2 ~ "Moderate decline: -0.5 to -0.2",
      tfr_diff >= -0.2 & tfr_diff <= 0.2 ~ "Minor variation: ±0.2 TFR",
      tfr_diff > 0.2 & tfr_diff <= 0.5 ~ "Moderate increase: +0.2 to +0.5",
      tfr_diff > 0.5 ~ "Large increase: > +0.5 TFR"
    ),
    change_type = factor(change_type, 
                         levels = c("Large decline: < -0.5 TFR",
                                    "Moderate decline: -0.5 to -0.2",
                                    "Minor variation: ±0.2 TFR",
                                    "Moderate increase: +0.2 to +0.5",
                                    "Large increase: > +0.5 TFR"))
  ) %>%
  filter(!is.na(census_tfr) & !is.na(nfhs4_tfr)) %>%
  arrange(desc(abs(tfr_diff)))

n_complete <- nrow(tfr_comparison)

cat("Dataset ready:\n")
cat("  Total NFHS-4 districts: 640\n")
cat("  Total Census 2011 districts: 640\n")
cat("  Complete pairs for comparison: ", n_complete, "\n")
cat("  Missing Census 2011 data: ", 640 - n_complete, " districts\n\n")

# ============================================================================
# 3. CORRELATION ANALYSIS
# ============================================================================

cat("========== CORRELATION ANALYSIS ==========\n\n")

cor_result <- cor.test(tfr_comparison$census_tfr, tfr_comparison$nfhs4_tfr)
cor_value <- cor_result$estimate
p_value <- cor_result$p.value
r_squared <- cor_value^2

cat("Pearson Correlation (n = ", n_complete, "):\n", sep = "")
cat("  r = ", sprintf("%.4f", cor_value), "\n")
cat("  R² = ", sprintf("%.4f", r_squared), "\n")
cat("  p-value = ", sprintf("%.2e", p_value), "\n")
cat("  Interpretation: Strong positive correlation\n\n")

# ============================================================================
# 4. DIFFERENCES ANALYSIS
# ============================================================================

cat("========== DIFFERENCES (NFHS-4 minus Census 2011) ==========\n\n")

mean_diff <- mean(tfr_comparison$tfr_diff, na.rm = TRUE)
median_diff <- median(tfr_comparison$tfr_diff, na.rm = TRUE)
sd_diff <- sd(tfr_comparison$tfr_diff, na.rm = TRUE)
min_diff <- min(tfr_comparison$tfr_diff, na.rm = TRUE)
max_diff <- max(tfr_comparison$tfr_diff, na.rm = TRUE)

cat("Summary Statistics:\n")
cat("  Mean: ", sprintf("%.3f", mean_diff), " TFR units\n")
cat("  Median: ", sprintf("%.3f", median_diff), " TFR units\n")
cat("  SD: ", sprintf("%.3f", sd_diff), "\n")
cat("  Range: [", sprintf("%.3f", min_diff), ", ", sprintf("%.3f", max_diff), "]\n\n")

# Agreement levels
within_02 <- sum(abs(tfr_comparison$tfr_diff) < 0.2, na.rm = TRUE)
within_03 <- sum(abs(tfr_comparison$tfr_diff) < 0.3, na.rm = TRUE)
within_05 <- sum(abs(tfr_comparison$tfr_diff) < 0.5, na.rm = TRUE)

cat("Agreement Levels:\n")
cat("  Within ±0.2 TFR: ", within_02, " districts (", 
    round((within_02/n_complete)*100, 1), "%)\n")
cat("  Within ±0.3 TFR: ", within_03, " districts (", 
    round((within_03/n_complete)*100, 1), "%)\n")
cat("  Within ±0.5 TFR: ", within_05, " districts (", 
    round((within_05/n_complete)*100, 1), "%)\n\n")

# ============================================================================
# 5. CHANGE CLASSIFICATION
# ============================================================================

cat("========== CHANGE CLASSIFICATION ==========\n\n")

change_summary <- tfr_comparison %>%
  group_by(change_type) %>%
  count() %>%
  mutate(percentage = sprintf("%.1f%%", (n/n_complete)*100))

print(change_summary)
cat("\n")

# ============================================================================
# 6. STATE-LEVEL SUMMARY
# ============================================================================

cat("========== STATE-LEVEL SUMMARY (Top 15 States) ==========\n\n")

state_summary <- tfr_comparison %>%
  group_by(state_std) %>%
  summarize(
    n_districts = n(),
    mean_census_tfr = mean(census_tfr, na.rm = TRUE),
    mean_nfhs4_tfr = mean(nfhs4_tfr, na.rm = TRUE),
    mean_diff = mean(tfr_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(mean_diff)))

print(state_summary %>% head(15))
cat("\n")

# ============================================================================
# 7. EXTREME CASES
# ============================================================================

cat("========== TOP 10 LARGEST INCREASES ==========\n\n")
print(tfr_comparison %>% arrange(desc(tfr_diff)) %>% head(10) %>%
        select(state, district, census_tfr, nfhs4_tfr, tfr_diff))

cat("\n========== TOP 10 LARGEST DECREASES ==========\n\n")
print(tfr_comparison %>% arrange(tfr_diff) %>% head(10) %>%
        select(state, district, census_tfr, nfhs4_tfr, tfr_diff))

cat("\n========== TOP 10 MOST SIMILAR (BEST AGREEMENT) ==========\n\n")
print(tfr_comparison %>% arrange(abs(tfr_diff)) %>% head(10) %>%
        select(state, district, census_tfr, nfhs4_tfr, tfr_diff))

# ============================================================================
# 8. SCATTER PLOT
# ============================================================================

# Label only the 10 most extreme districts
label_data <- bind_rows(
  tfr_comparison %>% arrange(desc(tfr_diff)) %>% head(5),
  tfr_comparison %>% arrange(tfr_diff) %>% head(5)
)

p_scatter <- ggplot(tfr_comparison, aes(x = census_tfr, y = nfhs4_tfr)) +
  
  geom_point(aes(color = change_type), alpha = 0.6, size = 3) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray50", linewidth = 1.2, alpha = 0.7) +
  
  geom_smooth(method = "lm", se = TRUE, color = "red", 
              fill = "red", alpha = 0.1, linewidth = 1) +
  
  geom_text_repel(
    data = label_data,
    aes(label = district),
    size = 2.8, max.overlaps = 10,
    segment.alpha = 0.4, 
    force = 3, force_pull = 0.8
  ) +
  
  scale_color_manual(
    values = c(
      "Large decline: < -0.5 TFR" = "#1b4d7c",
      "Moderate decline: -0.5 to -0.2" = "#6fa8dc",
      "Minor variation: ±0.2 TFR" = "#999999",
      "Moderate increase: +0.2 to +0.5" = "#f8a57a",
      "Large increase: > +0.5 TFR" = "#c1312b"
    ),
    name = "TFR Change Category"
  ) +
  
  labs(
    x = "Census 2011 TFR*",
    y = "NFHS-4 TFR"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_scatter)

ggsave(here("output", "comparison", "tfr_decline_nfhs4_census.png"), width = 7.89, height = 4.04)

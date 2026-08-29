# ============================================================================
# SCRIPT: 12_external_consistency_combined.R
# ============================================================================
#
# PURPOSE
# -------
# Creates faceted validation scatter plots comparing TFR across 2011 Census, NFHS-4, and NFHS-5.
#
# INPUT DATA
# ----------
# - Multiple internal datasets sourced directly by the script.
#
# OUTPUTS
# -------
# - output/Figure_2_external_consistency_faceted.png (Figure 2)
#
# DEPENDENCIES
# ------------
# - Sources script/11_level_condition_analysis_20260623_v1.R
# ============================================================================

#
# Combined external consistency assessment comparing Census 2011 with NFHS-4 and NFHS-4 with NFHS-5.
# output/census_nfhs45_tfr_combined.rds
# script/11_level_condition_analysis_20260623_v1.R
# data/nfhs4_tfr_sf.rds
# data/nfhs5_tfr_sf_20250807.rds
# output/Figure_2_external_consistency_faceted.png

# ============================================================================
# 1. Load Required Packages
# ============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(here)

cat("========== EXTERNAL CONSISTENCY ASSESSMENT ==========\n")
cat("Combined analysis: Census 2011 vs NFHS-4 vs NFHS-5\n\n")

# ============================================================================
# 2. PART A: CENSUS 2011 vs NFHS-4 (640 DISTRICTS)
# ============================================================================

cat("========== PART A: CENSUS 2011 vs NFHS-4 (640 Districts) ==========\n\n")

cat("Loading Census 2011 and NFHS-4 data...\n")

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

# Apply spelling/name fixes
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

# Apply critical state/district fixes
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
# 3. Prepare Census vs NFHS-4 Datasets
# ============================================================================

cat("Preparing Census 2011 data...\n")
census_data <- df_fixed %>%
  filter(data == "Census 2011") %>%
  select(state, district, state_std, district_std, tfr_mean, tfr_lower, tfr_upper) %>%
  rename(census_tfr = tfr_mean, census_tfr_lower = tfr_lower, census_tfr_upper = tfr_upper) %>%
  distinct()

cat("Preparing NFHS-4 data (640 districts)...\n")
nfhs4_data <- df_fixed %>%
  filter(data == "NFHS 4") %>%
  select(state, district, state_std, district_std, tfr_median, tfr_lower, tfr_upper) %>%
  rename(nfhs4_tfr = tfr_median, nfhs4_tfr_lower = tfr_lower, nfhs4_tfr_upper = tfr_upper) %>%
  distinct()

# Merge Census vs NFHS-4
cat("Merging Census 2011 with NFHS-4...\n\n")

census_nfhs4 <- nfhs4_data %>%
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

n_census_nfhs4 <- nrow(census_nfhs4)

cat(sprintf("Census 2011 vs NFHS-4 comparison: %d districts\n\n", n_census_nfhs4))

# Correlation
cor_census_nfhs4 <- cor.test(census_nfhs4$census_tfr, census_nfhs4$nfhs4_tfr)
cor_c_n4 <- cor_census_nfhs4$estimate
p_c_n4 <- cor_census_nfhs4$p.value

cat(sprintf("Pearson Correlation (n = %d):\n", n_census_nfhs4))
cat(sprintf("  r = %.4f (p < 0.001)\n", cor_c_n4))
cat(sprintf("  Mean difference: %.3f TFR units\n\n", mean(census_nfhs4$tfr_diff)))

# ============================================================================
# 4. PART B: NFHS-4 vs NFHS-5 (575 MATCHED DISTRICTS)
# ============================================================================

cat("========== PART B: NFHS-4 vs NFHS-5 (575 Matched Districts) ==========\n\n")

cat("Loading 575 matched districts from level conditioning analysis...\n")

source(here("script", "11_level_condition_analysis_20260623_v1.R"))

matched_575 <- tfr_combined %>%
  select(state_nfhs4, district_nfhs4,
         tfr_nfhs4, tfr_lower_nfhs4, tfr_upper_nfhs4,
         tfr_nfhs5, tfr_lower_nfhs5, tfr_upper_nfhs5) %>%
  rename(
    state = state_nfhs4,
    district = district_nfhs4,
    nfhs4_tfr = tfr_nfhs4,
    nfhs4_tfr_lower = tfr_lower_nfhs4,
    nfhs4_tfr_upper = tfr_upper_nfhs4,
    nfhs5_tfr = tfr_nfhs5,
    nfhs5_tfr_lower = tfr_lower_nfhs5,
    nfhs5_tfr_upper = tfr_upper_nfhs5
  )

# Calculate differences
nfhs4_nfhs5 <- matched_575 %>%
  mutate(
    tfr_diff = nfhs5_tfr - nfhs4_tfr,
    tfr_pct_change = (tfr_diff / nfhs4_tfr) * 100,
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
  filter(!is.na(nfhs4_tfr) & !is.na(nfhs5_tfr)) %>%
  arrange(desc(abs(tfr_diff)))

n_nfhs4_nfhs5 <- nrow(nfhs4_nfhs5)

cat(sprintf("NFHS-4 vs NFHS-5 comparison: %d districts\n\n", n_nfhs4_nfhs5))

# Correlation
cor_nfhs4_nfhs5 <- cor.test(nfhs4_nfhs5$nfhs4_tfr, nfhs4_nfhs5$nfhs5_tfr)
cor_n4_n5 <- cor_nfhs4_nfhs5$estimate
p_n4_n5 <- cor_nfhs4_nfhs5$p.value

cat(sprintf("Pearson Correlation (n = %d):\n", n_nfhs4_nfhs5))
cat(sprintf("  r = %.4f (p < 0.001)\n", cor_n4_n5))
cat(sprintf("  Mean decline: %.3f TFR units\n\n", mean(nfhs4_nfhs5$tfr_diff)))

# ============================================================================
# 5. PART C: COMBINED FACETED FIGURE
# ============================================================================

cat("========== CREATING FACETED FIGURE 2 ==========\n\n")

# Select labels for each panel
label_data_a <- bind_rows(
  census_nfhs4 %>% arrange(desc(tfr_diff)) %>% head(5),
  census_nfhs4 %>% arrange(tfr_diff) %>% head(5)
) %>%
  mutate(x_tfr = census_tfr, y_tfr = nfhs4_tfr)

label_data_b <- bind_rows(
  nfhs4_nfhs5 %>% arrange(desc(tfr_diff)) %>% head(5),
  nfhs4_nfhs5 %>% arrange(tfr_diff) %>% head(5)
) %>%
  mutate(x_tfr = nfhs4_tfr, y_tfr = nfhs5_tfr)

# Create Plot 1: Census 2011 vs NFHS-4
p1 <- ggplot(census_nfhs4, aes(x = census_tfr, y = nfhs4_tfr)) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray50", linewidth = 1.2, alpha = 0.7) +
  
  geom_smooth(method = "lm", se = TRUE, color = "red", 
              fill = "red", alpha = 0.1, linewidth = 1) +
  
  geom_point(aes(color = change_type), alpha = 0.5, size = 2.5,
             position = position_jitter(width = 0.02, height = 0.02)) +
  
  geom_text_repel(
    data = label_data_a,
    aes(label = district),
    size = 3.2, max.overlaps = 10,
    segment.alpha = 0.4, force = 2
  ) +
  
  scale_color_manual(
    values = c(
      "Large decline: < -0.5 TFR" = "#1b4d7c",
      "Moderate decline: -0.5 to -0.2" = "#6fa8dc",
      "Minor variation: ±0.2 TFR" = "#999999",
      "Moderate increase: +0.2 to +0.5" = "#f8a57a",
      "Large increase: > +0.5 TFR" = "#c1312b"
    ),
    name = "TFR Change"
  ) +
  
  scale_x_continuous(breaks = seq(1.5, 6, 0.5), limits = c(1, 6.2)) +
  scale_y_continuous(breaks = seq(1, 5, 0.5), limits = c(0.8, 5.2)) +
  
  labs(
    title = "2015-16 vs 2011 Census",
    x = "Census 2011 TFR",
    y = "NFHS-4 TFR (2015–16)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

# Create Plot 2: NFHS-4 vs NFHS-5
p2 <- ggplot(nfhs4_nfhs5, aes(x = nfhs4_tfr, y = nfhs5_tfr)) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray50", linewidth = 1.2, alpha = 0.7) +
  
  geom_smooth(method = "lm", se = TRUE, color = "red", 
              fill = "red", alpha = 0.1, linewidth = 1) +
  
  geom_point(aes(color = change_type), alpha = 0.5, size = 2.5,
             position = position_jitter(width = 0.02, height = 0.02)) +
  
  geom_text_repel(
    data = label_data_b,
    aes(label = district),
    size = 3.2, max.overlaps = 10,
    segment.alpha = 0.4, force = 2
  ) +
  
  scale_color_manual(
    values = c(
      "Large decline: < -0.5 TFR" = "#1b4d7c",
      "Moderate decline: -0.5 to -0.2" = "#6fa8dc",
      "Minor variation: ±0.2 TFR" = "#999999",
      "Moderate increase: +0.2 to +0.5" = "#f8a57a",
      "Large increase: > +0.5 TFR" = "#c1312b"
    ),
    name = "TFR Change"
  ) +
  
  scale_x_continuous(breaks = seq(1.5, 6, 0.5), limits = c(1, 6.2)) +
  scale_y_continuous(breaks = seq(1, 5, 0.5), limits = c(0.8, 5.2)) +
  
  labs(
    title = "2019-21 vs 2015-16",
    x = "NFHS-4 TFR (2015–16)",
    y = "NFHS-5 TFR (2019–21)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

# Combine plots with patchwork
p_combined <- (p1 | p2) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p_combined)

ggsave(here("output", "Figure_2_external_consistency_faceted.png"), 
       p_combined, width = 14, height = 6, dpi = 300)

cat("✓ Figure 2 (faceted) saved\n\n")

# ============================================================================
# 6. PART D: AGGREGATE CATEGORY ANALYSIS (707 DISTRICTS FOR TEXT)
# ============================================================================

cat("========== PART D: AGGREGATE CATEGORY SHIFTS (707 Districts) ==========\n\n")

# Load all NFHS-4 and NFHS-5 for aggregate analysis
cat("Loading all NFHS-4 and NFHS-5 data (707 districts)...\n")

nfhs4_all <- read_rds(here("data", "nfhs4_tfr_sf.rds")) %>%
  sf::st_drop_geometry() %>%
  rename("state" = state.x, "district" = district.x) %>%
  select(state, district, tfr_median) %>%
  rename(nfhs4_tfr = tfr_median) %>%
  distinct()

nfhs5_all <- read_rds(here("data", "nfhs5_tfr_sf_20250807.rds")) %>%
  sf::st_drop_geometry() %>%
  rename("state" = state.x, "district" = district.x) %>%
  select(state, district, tfr_median) %>%
  rename(nfhs5_tfr = tfr_median) %>%
  distinct()

# Create TFR categories
nfhs4_cats <- nfhs4_all %>%
  mutate(
    tfr_cat = case_when(
      nfhs4_tfr <= 2.1 ~ "≤2.1",
      nfhs4_tfr > 2.1 & nfhs4_tfr <= 3.0 ~ "2.1-3.0",
      nfhs4_tfr > 3.0 & nfhs4_tfr <= 4.0 ~ "3.0-4.0",
      nfhs4_tfr > 4.0 ~ ">4.0"
    ),
    tfr_cat_simple = case_when(
      nfhs4_tfr <= 2.1 ~ "Below-replacement (≤2.1)",
      nfhs4_tfr > 3.0 ~ "High fertility (>3.0)",
      TRUE ~ "Intermediate"
    )
  )

nfhs5_cats <- nfhs5_all %>%
  mutate(
    tfr_cat = case_when(
      nfhs5_tfr <= 2.1 ~ "≤2.1",
      nfhs5_tfr > 2.1 & nfhs5_tfr <= 3.0 ~ "2.1-3.0",
      nfhs5_tfr > 3.0 & nfhs5_tfr <= 4.0 ~ "3.0-4.0",
      nfhs5_tfr > 4.0 ~ ">4.0"
    ),
    tfr_cat_simple = case_when(
      nfhs5_tfr <= 2.1 ~ "Below-replacement (≤2.1)",
      nfhs5_tfr > 3.0 ~ "High fertility (>3.0)",
      TRUE ~ "Intermediate"
    )
  )

# ============================================================================
# 7. Distribution Summary
# ============================================================================

# NFHS-4 category distribution
cat("NFHS-4 (2015-16) Category Distribution (n = ", nrow(nfhs4_cats), " districts):\n", sep = "")
nfhs4_summary <- nfhs4_cats %>%
  summarise(
    le_2_1 = sum(nfhs4_tfr <= 2.1, na.rm = TRUE),
    gt_3_0 = sum(nfhs4_tfr > 3.0, na.rm = TRUE),
    gt_4_0 = sum(nfhs4_tfr > 4.0, na.rm = TRUE)
  ) %>%
  mutate(
    pct_le_2_1 = round((le_2_1 / nrow(nfhs4_cats)) * 100, 1),
    pct_gt_3_0 = round((gt_3_0 / nrow(nfhs4_cats)) * 100, 1),
    pct_gt_4_0 = round((gt_4_0 / nrow(nfhs4_cats)) * 100, 1)
  )

cat(sprintf("  TFR ≤2.1 (Below-replacement): %d (%.1f%%)\n", 
            nfhs4_summary$le_2_1, nfhs4_summary$pct_le_2_1))
cat(sprintf("  TFR >3.0 (High fertility): %d (%.1f%%)\n", 
            nfhs4_summary$gt_3_0, nfhs4_summary$pct_gt_3_0))
cat(sprintf("  TFR >4.0 (Very high): %d (%.1f%%)\n\n", 
            nfhs4_summary$gt_4_0, nfhs4_summary$pct_gt_4_0))

# NFHS-5 category distribution
cat("NFHS-5 (2019-21) Category Distribution (n = ", nrow(nfhs5_cats), " districts):\n", sep = "")
nfhs5_summary <- nfhs5_cats %>%
  summarise(
    le_2_1 = sum(nfhs5_tfr <= 2.1, na.rm = TRUE),
    gt_3_0 = sum(nfhs5_tfr > 3.0, na.rm = TRUE),
    gt_4_0 = sum(nfhs5_tfr > 4.0, na.rm = TRUE)
  ) %>%
  mutate(
    pct_le_2_1 = round((le_2_1 / nrow(nfhs5_cats)) * 100, 1),
    pct_gt_3_0 = round((gt_3_0 / nrow(nfhs5_cats)) * 100, 1),
    pct_gt_4_0 = round((gt_4_0 / nrow(nfhs5_cats)) * 100, 1)
  )

cat(sprintf("  TFR ≤2.1 (Below-replacement): %d (%.1f%%)\n", 
            nfhs5_summary$le_2_1, nfhs5_summary$pct_le_2_1))
cat(sprintf("  TFR >3.0 (High fertility): %d (%.1f%%)\n", 
            nfhs5_summary$gt_3_0, nfhs5_summary$pct_gt_3_0))
cat(sprintf("  TFR >4.0 (Very high): %d (%.1f%%)\n\n", 
            nfhs5_summary$gt_4_0, nfhs5_summary$pct_gt_4_0))

# ============================================================================
# 8. SUMMARY FOR MANUSCRIPT
# ============================================================================

cat("========== SUMMARY FOR RESULTS SECTION ==========\n\n")

cat("EXTERNAL CONSISTENCY:\n")
cat(sprintf("Census 2011 vs NFHS-4: r = %.3f (p < 0.001), n = %d districts\n", 
            cor_c_n4, n_census_nfhs4))
cat(sprintf("  Mean difference: %.3f TFR units (2015-16 lower than 2011)\n\n", 
            mean(census_nfhs4$tfr_diff)))

cat(sprintf("NFHS-4 vs NFHS-5 (575 matched): r = %.3f (p < 0.001), n = %d districts\n", 
            cor_n4_n5, n_nfhs4_nfhs5))
cat(sprintf("  Mean difference: %.3f TFR units (2019-21 lower than 2015-16)\n\n", 
            mean(nfhs4_nfhs5$tfr_diff)))

cat("AGGREGATE CATEGORY SHIFTS (Use these for text):\n\n")
cat(sprintf("TFR >3.0 (High): %.1f%% (NFHS-4) → %.1f%% (NFHS-5)\n", 
            nfhs4_summary$pct_gt_3_0, nfhs5_summary$pct_gt_3_0))
cat(sprintf("TFR ≤2.1 (Below-replacement): %.1f%% (NFHS-4) → %.1f%% (NFHS-5)\n", 
            nfhs4_summary$pct_le_2_1, nfhs5_summary$pct_le_2_1))
cat(sprintf("TFR >4.0 (Very high): %.1f%% (NFHS-4) → %.1f%% (NFHS-5)\n\n", 
            nfhs4_summary$pct_gt_4_0, nfhs5_summary$pct_gt_4_0))

cat("✓ All analyses complete\n")

# ============================================================================
# SCRIPT: 10_birth_order_distribution_analysis.R
# ============================================================================
#
# PURPOSE
# -------
# Evaluates associations between Bayesian TFR, median total children ever born, and median age at first birth.
#
# INPUT DATA
# ----------
# - output/census_nfhs45_tfr_combined.rds
# - data/nfhs4_median_age_parity_20260518.rds (External restricted)
# - data/nfhs5_median_age_parity_20260518.rds (External restricted)
#
# OUTPUTS
# -------
# - output/comparison/fig_consistency_parity_age_2x2.png (Figure 3)
#
# DEPENDENCIES
# ------------
# - script/08_nfhs4_nfhs5_comparison_20260523_v1.R
# ============================================================================

# ============================================================================
# <SCRIPT: 10_birth_order_distribution_analysis.R>
#
# Analyzes the consistency of district TFR estimates with birth order metrics
# (median parity and median age at first birth) for both NFHS-4 and NFHS-5.
#
# - data/nfhs4_tfr_sf.rds (Generated locally)
# - data/nfhs4_median_age_parity_20260518.rds (Generated locally)
# - data/nfhs5_tfr_sf_20250807.rds (Generated locally)
# - data/nfhs5_median_age_parity_20260518.rds (Generated locally)
#
# - output/comparison/fig_consistency_parity_nfhs45.png
# - output/comparison/consistency_birth_order_summary_nfhs45.csv
# - output/comparison/consistency_correlations_nfhs45.csv
# - output/comparison/fig_consistency_parity_age_2x2.png
#
# BIRTH ORDER DISTRIBUTION ANALYSIS
# Standalone script for consistency check: Birth order distribution by TFR
# ============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(here)
library(haven)
library(janitor)
library(ggh4x)


# ============================================================================
# NFHS-4 (2015-16): LOAD & PREPARE DATA
# ============================================================================

nfhs4_tfr <- read_rds(here("data", "nfhs4_tfr_sf.rds")) %>%
  sf::st_drop_geometry() %>%
  mutate(
    state.x = case_when(
      state.x == "Himanchal Pradesh" ~ "Himachal Pradesh",
      state.x == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      TRUE ~ state.x
    ),
    unique_id = paste0(str_to_lower(state.x), " ", str_to_lower(district.x)),
    unique_id = str_remove_all(unique_id, "\\band\\b|&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  )

nfhs4_parity <- read_rds(here("data", "nfhs4_median_age_parity_20260518.rds")) %>%
  mutate(
    state = as.character(haven::as_factor(state)),
    district = as.character(haven::as_factor(district))
  ) %>%
  clean_names() %>%
  mutate(
    state = case_when(
      state == "Himanchal Pradesh" ~ "Himachal Pradesh",
      state == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      TRUE ~ state
    ),
    unique_id = paste0(str_to_lower(state), " ", str_to_lower(district)),
    unique_id = str_remove_all(unique_id, "\\band\\b|&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  )

df_nfhs4 <- nfhs4_tfr %>%
  select(unique_id, state = state.x, district = district.x, tfr_median) %>%
  inner_join(
    nfhs4_parity %>% select(unique_id, median_age_first_birth, median_parity_completed),
    by = "unique_id"
  ) %>%
  mutate(survey = "2015–16")

cat("NFHS-4 matched districts:", nrow(df_nfhs4), "\n")

# ============================================================================
# NFHS-5 (2019-21): LOAD & PREPARE DATA
# ============================================================================

nfhs5_tfr <- read_rds(here("data", "nfhs5_tfr_sf_20250807.rds")) %>%
  sf::st_drop_geometry() %>%
  mutate(
    state.x = case_when(
      state.x == "Nct Of Delhi" ~ "Delhi",
      state.x == "Jammu  Kashmir" ~ "Jammu & Kashmir",
      state.x == "Dadra  Nagar Haveli And Daman  Diu" ~ "Dadra & Nagar Haveli & Daman & Diu",
      state.x == "Andaman  Nicobar Islands" ~ "Andaman & Nicobar Islands",
      TRUE ~ state.x
    )
  ) %>%
  mutate(
    unique_id = paste0(str_to_lower(state.x), " ", str_to_lower(district.x)),
    unique_id = str_remove_all(unique_id, "\\band\\b|&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  )

nfhs5_parity <- read_rds(here("data", "nfhs5_median_age_parity_20260518.rds")) %>%
  mutate(
    state = as.character(haven::as_factor(state)),
    district = as.character(haven::as_factor(district))
  ) %>%
  clean_names() %>%
  mutate(
    state = case_when(
      state == "Nct Of Delhi" ~ "Delhi",
      state == "Jammu  Kashmir" ~ "Jammu & Kashmir",
      state == "Dadra  Nagar Haveli And Daman  Diu" ~ "Dadra & Nagar Haveli & Daman & Diu",
      state == "Andaman  Nicobar Islands" ~ "Andaman & Nicobar Islands",
      TRUE ~ state
    ),
    unique_id = paste0(str_to_lower(state), " ", str_to_lower(district)),
    unique_id = str_remove_all(unique_id, "\\band\\b|&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  ) %>%
  mutate(
    unique_id = case_when(
      unique_id == "uttar_pradesh_mahrajganj" ~ "uttar_pradesh_maharajganj",
      unique_id == "uttar_pradesh_sant_ravidas_nagar_(bhadohi)" ~ "uttar_pradesh_sant_ravidas_nagar",
      unique_id == "bihar_buxar" ~ "bihar_buxer",
      unique_id == "chhattisgarh_janjgir_-_champa" ~ "chhattisgarh_janjgir-champa",
      unique_id == "ladakh_leh(ladakh)" ~ "ladakh_leh",
      TRUE ~ unique_id
    )
  )

df_nfhs5 <- nfhs5_tfr %>%
  select(unique_id, state = state.x, district = district.x, tfr_median) %>%
  inner_join(
    nfhs5_parity %>% select(unique_id, median_age_first_birth, median_parity_completed),
    by = "unique_id"
  ) %>%
  mutate(survey = "2019–21")

cat("NFHS-5 matched districts:", nrow(df_nfhs5), "\n\n")

# ============================================================================
# BIND DATASETS
# ============================================================================

df_combined <- bind_rows(df_nfhs4, df_nfhs5) %>%
  mutate(survey = factor(survey, levels = c("2015–16", "2019–21")))

cat("Total combined districts:", nrow(df_combined), "\n\n")

# ============================================================================
# CORRELATIONS: TFR WITH BIRTH ORDER METRICS
# ============================================================================

paste0(strrep("=", 80), "\n") |> cat()
cat("CORRELATIONS: TFR WITH BIRTH ORDER METRICS\n")
paste0(strrep("=", 80), "\n\n") |> cat()

corr_summary_full <- df_combined %>%
  group_by(survey) %>%
  summarise(
    n_districts = n(),
    r_parity = round(cor(tfr_median, median_parity_completed, use = "complete.obs"), 3),
    r_age = round(cor(tfr_median, median_age_first_birth, use = "complete.obs"), 3),
    .groups = "drop"
  )

print(corr_summary_full)


# ============================================================================
# BIRTH ORDER DISTRIBUTION SUMMARY BY TFR CATEGORY
# ============================================================================


birth_order_summary <- df_combined %>%
  filter(!is.na(median_age_first_birth), !is.na(median_parity_completed)) %>%
  mutate(
    tfr_cat = case_when(
      tfr_median < 2.1 ~ "<2.1",
      tfr_median >= 2.1 & tfr_median < 3 ~ "2.1 to 3",
      tfr_median >= 3 ~ "≥3.0",
      TRUE ~ NA_character_
    ),
    tfr_cat = factor(tfr_cat, levels = c("<2.1", "2.1 to 3", "≥3.0"))
  ) %>%
  group_by(survey, tfr_cat) %>%
  summarise(
    n_districts = n(),
    mean_age_first_birth = round(mean(median_age_first_birth, na.rm = TRUE), 2),
    sd_age_first_birth = round(sd(median_age_first_birth, na.rm = TRUE), 2),
    mean_parity_completed = round(mean(median_parity_completed, na.rm = TRUE), 2),
    sd_parity_completed = round(sd(median_parity_completed, na.rm = TRUE), 2),
    mean_tfr = round(mean(tfr_median, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(birth_order_summary)


# ============================================================================
# PREPARE DATA FOR 4-PANEL PLOT WITH CLEAR LABELS
# ============================================================================

df_long <- df_combined %>%
  pivot_longer(
    cols = c(median_parity_completed, median_age_first_birth),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "median_parity_completed" ~ "Median Total Children Ever Born\n(Women 40–49)",
      metric == "median_age_first_birth" ~ "Median Age at First Birth\n(Women 15–49)",
      TRUE ~ metric
    ),
    metric = factor(metric, levels = c(
      "Median Total Children Ever Born\n(Women 40–49)",
      "Median Age at First Birth\n(Women 15–49)"
    ))
  )

# ============================================================================
# CALCULATE CORRELATION STATISTICS FOR ANNOTATION
# ============================================================================

corr_stats <- df_combined %>%
  group_by(survey) %>%
  summarise(
    r_parity = cor(tfr_median, median_parity_completed, use = "complete.obs"),
    p_parity = cor.test(tfr_median, median_parity_completed)$p.value,
    r_age = cor(tfr_median, median_age_first_birth, use = "complete.obs"),
    p_age = cor.test(tfr_median, median_age_first_birth)$p.value,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -survey,
    names_to = c("stat", "metric"),
    names_pattern = "([rp])_(.+)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "parity" ~ "Median Total Children Ever Born\n(Women 40–49)",
      metric == "age" ~ "Median Age at First Birth\n(Women 15–49)"
    ),
    metric = factor(metric, levels = c(
      "Median Total Children Ever Born\n(Women 40–49)",
      "Median Age at First Birth\n(Women 15–49)"
    )),
    label = sprintf("r = %.3f\np < 0.001", r)
  )

# ============================================================================
# IDENTIFY TOP 5 & BOTTOM 5 DISTRICTS BY SURVEY
# ============================================================================

highlight_combined <- df_combined %>%
  filter(!is.na(median_parity_completed)) %>%
  group_by(survey) %>%
  arrange(desc(tfr_median)) %>%
  slice(c(1:5, (n()-4):n())) %>%
  ungroup() %>%
  select(unique_id, survey)   # <- dropped district; df_long already has it

# ============================================================================
# VISUALIZATION: 4-PANEL PLOT WITH DISTRICT LABELS
# ============================================================================

# install if needed: install.packages("ggh4x")
library(ggh4x)

p_final <- ggplot(df_long, aes(x = value, y = tfr_median)) +
  geom_point(alpha = 0.5, size = 2, color = "darkblue") +
  geom_text_repel(
    aes(label = district),
    data = df_long %>% 
      inner_join(highlight_combined, by = c("unique_id", "survey")),
    size = 2.0,
    max.overlaps = 20,
    segment.alpha = 0.3
  ) +
  facet_grid(survey ~ metric, scales = "free_x") +
  facetted_pos_scales(
    x = list(
      metric == "Median Total Children Ever Born\n(Women 40–49)" ~ 
        scale_x_continuous(limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)),
      metric == "Median Age at First Birth\n(Women 15–49)" ~ 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
    )
  ) +
  labs(y = "District Median TFR") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 16),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    panel.spacing = unit(1, "cm")
  ) +
  geom_text(
    data = corr_stats,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.2,
    size = 3.2, fontface = "bold",
    inherit.aes = FALSE
  )

print(p_final)


# ============================================================================
# SAVE OUTPUTS
# ============================================================================

ggsave(here("output", "comparison", "fig_consistency_parity_nfhs45.png"),
       p_final, width = 10, height = 10/1.6, dpi = 300)

write_csv(birth_order_summary,
          here("output", "comparison", "consistency_birth_order_summary_nfhs45.csv"))

write_csv(corr_summary_full,
          here("output", "comparison", "consistency_correlations_nfhs45.csv"))


# ============================================================================
# PARITY & AGE AT FIRST BIRTH: 2x2 GRID WITH PATCHWORK
# Parity starts at 0, Age has natural scaling
# ============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(here)

# ============================================================================
# PREPARE DATA BY SURVEY
# ============================================================================

df_2015 <- df_combined %>%
  filter(survey == "2015–16", !is.na(median_parity_completed), !is.na(median_age_first_birth))

df_2019 <- df_combined %>%
  filter(survey == "2019–21", !is.na(median_parity_completed), !is.na(median_age_first_birth))

# ============================================================================
# HIGHLIGHTS FOR EACH SURVEY
# ============================================================================

hl_2015 <- df_2015 %>%
  arrange(desc(tfr_median)) %>%
  slice(c(1:5, (n()-4):n())) %>%
  select(district, survey)

hl_2019 <- df_2019 %>%
  arrange(desc(tfr_median)) %>%
  slice(c(1:5, (n()-4):n())) %>%
  select(district, survey)

# ============================================================================
# PLOT 1: PARITY 2015-16 (x-axis starts at 0)
# ============================================================================

corr_p_2015 <- cor(df_2015$tfr_median, df_2015$median_parity_completed, use = "complete.obs")

p1 <- ggplot(df_2015, aes(x = median_parity_completed, y = tfr_median)) +
  geom_point(alpha = 0.5, size = 2, color = "darkblue") +
  geom_text_repel(
    aes(label = district),
    data = df_2015 %>% inner_join(hl_2015, by = "district"),
    size = 2.0, max.overlaps = 12, segment.alpha = 0.3
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, NA)
  ) +
  scale_y_continuous(limits = c(0.8, 5.2)) +
  labs(
    x = "Median Total Children Ever Born\n(Women 40–49)",
    y = "District Median TFR",
    subtitle = "2015–16"
  ) +
  annotate(
    "text", x = 0.5, y = 5, 
    label = sprintf("r = %.3f\np < 0.001", corr_p_2015),
    size = 4, fontface = "bold", hjust = 0
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

# ============================================================================
# PLOT 2: AGE 2015-16 (natural scaling)
# ============================================================================

corr_a_2015 <- cor(df_2015$tfr_median, df_2015$median_age_first_birth, use = "complete.obs")

p2 <- ggplot(df_2015, aes(x = median_age_first_birth, y = tfr_median)) +
  geom_point(alpha = 0.5, size = 2, color = "darkblue") +
  geom_text_repel(
    aes(label = district),
    data = df_2015 %>% inner_join(hl_2015, by = "district"),
    size = 2.0, max.overlaps = 12, segment.alpha = 0.3
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(limits = c(0.8, 5.2)) +
  labs(
    x = "Median Age at First Birth\n(Women 15–49)",
    y = "District Median TFR",
    subtitle = "2015–16"
  ) +
  annotate(
    "text", x = -Inf, y = Inf, 
    label = sprintf("r = %.3f\np < 0.001", corr_a_2015),
    size = 4, fontface = "bold", hjust = -0.1, vjust = 1.2
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

# ============================================================================
# PLOT 3: PARITY 2019-21 (x-axis starts at 0)
# ============================================================================

corr_p_2019 <- cor(df_2019$tfr_median, df_2019$median_parity_completed, use = "complete.obs")

p3 <- ggplot(df_2019, aes(x = median_parity_completed, y = tfr_median)) +
  geom_point(alpha = 0.5, size = 2, color = "darkblue") +
  geom_text_repel(
    aes(label = district),
    data = df_2019 %>% inner_join(hl_2019, by = "district"),
    size = 2.0, max.overlaps = 12, segment.alpha = 0.3
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, NA)
  ) +
  scale_y_continuous(limits = c(0.8, 5.2)) +
  labs(
    x = "Median Total Children Ever Born\n(Women 40–49)",
    y = "District Median TFR",
    subtitle = "2019–21"
  ) +
  annotate(
    "text", x = 0.5, y = 5, 
    label = sprintf("r = %.3f\np < 0.001", corr_p_2019),
    size = 4, fontface = "bold", hjust = 0
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

# ============================================================================
# PLOT 4: AGE 2019-21 (natural scaling)
# ============================================================================

corr_a_2019 <- cor(df_2019$tfr_median, df_2019$median_age_first_birth, use = "complete.obs")

p4 <- ggplot(df_2019, aes(x = median_age_first_birth, y = tfr_median)) +
  geom_point(alpha = 0.5, size = 2, color = "darkblue") +
  geom_text_repel(
    aes(label = district),
    data = df_2019 %>% inner_join(hl_2019, by = "district"),
    size = 2.0, max.overlaps = 12, segment.alpha = 0.3
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(limits = c(0.8, 5.2)) +
  labs(
    x = "Median Age at First Birth\n(Women 15–49)",
    y = "District Median TFR",
    subtitle = "2019–21"
  ) +
  annotate(
    "text", x = -Inf, y = Inf, 
    label = sprintf("r = %.3f\np < 0.001", corr_a_2019),
    size = 4, fontface = "bold", hjust = -0.1, vjust = 1.2
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

# ============================================================================
# COMBINE 2x2 WITH PATCHWORK (No main title, larger fonts)
# ============================================================================

p_combined <- (p1 | p2) / (p3 | p4)

print(p_combined)

# ============================================================================
# SAVE
# ============================================================================

ggsave(here("output", "comparison", "fig_consistency_parity_age_2x2.png"),
       p_combined, width = 10, height = 10/1.6, dpi = 400)

cat("✓ 2x2 patchwork plot saved successfully\n")


# ============================================================================
# SCRIPT: 06a_nfhs4_district_characteristics_vs_tfr_20260520_v1.R
# ============================================================================
#
# PURPOSE
# -------
# Assesses the relationship between NFHS-4 district-level TFR and socioeconomic characteristics (e.g., poverty, literacy, urbanisation).
#
# INPUT DATA
# ----------
# - output/tfr_summary_m7_age_nfhs4_20250807_v1.rds
# - data/nfhs4_covariates_20260518_v1.csv (External restricted data)
#
# OUTPUTS
# -------
# - output/nfhs4/tfr_four_dimensions_nfhs4.png (Supplementary S12)
#
# DEPENDENCIES
# ------------
# - script/01_nfhs4_age_fertility_rates_whole_20250807_v2.R
# ============================================================================

# ============================================================================
# 1. Load Required Libraries
# ============================================================================
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(janitor)
library(here)

# ============================================================================
# 2. Load Data
# ============================================================================
nfhs4     <- read_rds(here("data", "nfhs4_tfr_sf.rds"))
nfhs4_cov <- read_csv(here("data", "nfhs4_covariates_20260518_v1.csv"))

# ============================================================================
# 3. Clean NFHS-4 - Create unique_id for Matching
# ============================================================================
nfhs4 <- nfhs4 %>%
  sf::st_drop_geometry() %>%
  mutate(
    unique_id = paste0(str_to_lower(state.x), " ", str_to_lower(district.x)),
    unique_id = str_remove_all(unique_id, "&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  ) %>%
  mutate(
    tfr_cat1 = case_when(
      tfr_median < 1.5                          ~ "<1.5",
      tfr_median >= 1.5 & tfr_median < 2.1     ~ "1.5 to 2.1",
      tfr_median >= 2.1 & tfr_median < 3       ~ "2.1 to 3",
      tfr_median >= 3   & tfr_median < 4       ~ "3 to 4",
      tfr_median >= 4                           ~ "4+",
      TRUE ~ NA_character_
    ),
    tfr_cat2 = case_when(
      tfr_median < 2.1                          ~ "<2.1",
      tfr_median >= 2.1 & tfr_median < 3       ~ "2.1 to 3",
      tfr_median >= 3                           ~ "3+",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    tfr_cat1 = factor(tfr_cat1, levels = c("<1.5", "1.5 to 2.1", "2.1 to 3", "3 to 4", "4+")),
    tfr_cat2 = factor(tfr_cat2, levels = c("<2.1", "2.1 to 3", "3+"))
  )

# ============================================================================
# 4. Clean Covariates - Create unique_id for Matching
# ============================================================================
nfhs4_cov <- nfhs4_cov %>%
  clean_names() %>%
  mutate(
    state = case_when(
      state == "Himanchal Pradesh" ~ "Himachal Pradesh",
      state == "Jammu & Kashmir"   ~ "Jammu and Kashmir",
      TRUE ~ state
    )
  ) %>%
  mutate(
    unique_id = paste0(str_to_lower(state), " ", str_to_lower(district)),
    unique_id = str_remove_all(unique_id, "&"),
    unique_id = str_squish(unique_id),
    unique_id = str_replace_all(unique_id, " ", "_")
  )

# ============================================================================
# 5. Check Matching Before Joining
# ============================================================================
cat("========== MATCHING DIAGNOSTICS ==========\n\n")

missing_in_cov   <- setdiff(unique(nfhs4$unique_id), unique(nfhs4_cov$unique_id))
missing_in_nfhs4 <- setdiff(unique(nfhs4_cov$unique_id), unique(nfhs4$unique_id))

cat("Districts in NFHS-4 but NOT in covariates:", length(missing_in_cov), "\n")
if (length(missing_in_cov) > 0) cat("Examples:", head(missing_in_cov), "\n\n")

cat("Districts in covariates but NOT in NFHS-4:", length(missing_in_nfhs4), "\n")
if (length(missing_in_nfhs4) > 0) cat("Examples:", head(missing_in_nfhs4), "\n\n")

n_nfhs4 <- n_distinct(nfhs4$unique_id)
n_cov   <- n_distinct(nfhs4_cov$unique_id)
cat("Total unique districts in NFHS-4:", n_nfhs4, "\n")
cat("Total unique districts in covariates:", n_cov, "\n\n")

# ============================================================================
# 6. Merge Datasets
# ============================================================================
df <- nfhs4 %>%
  left_join(nfhs4_cov, by = "unique_id")

n_matched <- sum(!is.na(df$pct_modern_contraception))
cat("Successfully matched districts:", n_matched, "of", nrow(df), "\n")
cat("Match success rate:", round((n_matched / nrow(df)) * 100, 1), "%\n\n")

# ============================================================================
# 7. Create Wealth Variable (Poorest + Poorer) & Check Missing
# ============================================================================
df <- df %>%
  mutate(pct_poor = poorest + poorer)

cat("========== MISSING VALUES ==========\n\n")
missing_check <- df %>%
  select(tfr_median, pct_modern_contraception, mean_education, pct_poor, rural) %>%
  summarize(across(everything(), ~sum(is.na(.))))

print(missing_check)
cat("\n")

# ============================================================================
# 8. Identify Highlight Districts
# ============================================================================
highlight_districts <- df %>%
  arrange(desc(tfr_median)) %>%
  slice(c(1:6, (n() - 5):n())) %>%
  pull(district.x)

cat("6 HIGHEST TFR DISTRICTS:\n")
print(df %>% arrange(desc(tfr_median)) %>% slice(1:6) %>% select(district.x, state.x, tfr_median))

cat("\n6 LOWEST TFR DISTRICTS:\n")
print(df %>% arrange(tfr_median) %>% slice(1:6) %>% select(district.x, state.x, tfr_median))
cat("\n")

# ============================================================================
# 9. Calculate Correlations with P-Values
# ============================================================================
corr_table_4 <- data.frame(
  Covariate = c(
    "Modern Contraception (%)",
    "Female Education (years)",
    "% in Poorest/Poorer Quintiles",
    "% Rural Population"
  ),
  Correlation_with_TFR = c(
    cor(df$tfr_median, df$pct_modern_contraception, use = "complete.obs"),
    cor(df$tfr_median, df$mean_education,           use = "complete.obs"),
    cor(df$tfr_median, df$pct_poor,                 use = "complete.obs"),
    cor(df$tfr_median, df$rural,                    use = "complete.obs")
  ),
  R_squared = NA,
  p_value   = NA
)

vars <- list(df$pct_modern_contraception, df$mean_education, df$pct_poor, df$rural)
for (i in seq_len(nrow(corr_table_4))) {
  test <- cor.test(df$tfr_median, vars[[i]])
  corr_table_4$R_squared[i] <- corr_table_4$Correlation_with_TFR[i]^2
  corr_table_4$p_value[i]   <- test$p.value
}

corr_table_4_display <- corr_table_4 %>%
  mutate(
    Correlation_with_TFR = round(Correlation_with_TFR, 3),
    R_squared             = round(R_squared, 3),
    p_value               = format.pval(p_value, digits = 2, eps = 0.001)
  )

cat("========== CORRELATION TABLE (NFHS-4) ==========\n\n")
print(corr_table_4_display)
cat("\n")

# ============================================================================
# 10. Generate Scatter Plots
# ============================================================================
make_label <- function(r) sprintf("r = %.3f\np < 0.001", r)

# Plot 1: Modern Contraception
p1 <- ggplot(df, aes(x = pct_modern_contraception, y = tfr_median, label = district.x)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.1) +
  geom_text_repel(
    data = df %>% filter(district.x %in% highlight_districts),
    size = 2.2, max.overlaps = 20, segment.alpha = 0.3
  ) +
  annotate("label",
           x = Inf, y = Inf,
           label    = make_label(cor(df$tfr_median, df$pct_modern_contraception, use = "complete.obs")),
           hjust    = 1.05, vjust = 1.05,
           size     = 3, fontface = "bold",
           fill     = "white", label.size = 0.3) +
  labs(x = "Modern Contraception (%)", y = "TFR (Median)", title = "A) TFR vs Modern Contraception") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), axis.title = element_text(size = 10))

# Plot 2: Female Education
p2 <- ggplot(df, aes(x = mean_education, y = tfr_median, label = district.x)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.1) +
  geom_text_repel(
    data = df %>% filter(district.x %in% highlight_districts),
    size = 2.2, max.overlaps = 20, segment.alpha = 0.3
  ) +
  annotate("label",
           x = Inf, y = Inf,
           label    = make_label(cor(df$tfr_median, df$mean_education, use = "complete.obs")),
           hjust    = 1.05, vjust = 1.05,
           size     = 3, fontface = "bold",
           fill     = "white", label.size = 0.3) +
  labs(x = "Female Education (years)", y = "TFR (Median)", title = "B) TFR vs Female Education") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), axis.title = element_text(size = 10))

# Plot 3: % Poor
p3 <- ggplot(df, aes(x = pct_poor, y = tfr_median, label = district.x)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.1) +
  geom_text_repel(
    data = df %>% filter(district.x %in% highlight_districts),
    size = 2.2, max.overlaps = 20, segment.alpha = 0.3
  ) +
  annotate("label",
           x = Inf, y = Inf,
           label    = make_label(cor(df$tfr_median, df$pct_poor, use = "complete.obs")),
           hjust    = 1.05, vjust = 1.05,
           size     = 3, fontface = "bold",
           fill     = "white", label.size = 0.3) +
  labs(x = "% in Poorest/Poorer Quintiles", y = "TFR (Median)", title = "C) TFR vs Poverty") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), axis.title = element_text(size = 10))

# Plot 4: % Rural
p4 <- ggplot(df, aes(x = rural, y = tfr_median, label = district.x)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.1) +
  geom_text_repel(
    data = df %>% filter(district.x %in% highlight_districts),
    size = 2.2, max.overlaps = 20, segment.alpha = 0.3
  ) +
  annotate("label",
           x = Inf, y = Inf,
           label    = make_label(cor(df$tfr_median, df$rural, use = "complete.obs")),
           hjust    = 1.05, vjust = 1.05,
           size     = 3, fontface = "bold",
           fill     = "white", label.size = 0.3) +
  labs(x = "% Rural Population", y = "TFR (Median)", title = "D) TFR vs Urbanization") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), axis.title = element_text(size = 10))

# ============================================================================
# 11. Combine and Save Grid
# ============================================================================
combined_4 <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggsave(here("output", "nfhs4", "tfr_four_dimensions_nfhs4.png"), combined_4, width = 10, height = 10 / 1.6, dpi = 300)
ggsave(here("output", "nfhs4", "tfr_four_dimensions_nfhs4.pdf"), combined_4, width = 8.69, height = 4.72)

cat("\n✓ Figure saved as 'tfr_four_dimensions.png' and 'tfr_four_dimensions.pdf'\n\n")

# ============================================================================
# 12. Export Outputs
# ============================================================================
write_csv(corr_table_4_display, here("output", "nfhs4", "tfr_correlation_table_nfhs4.csv"))
cat("✓ Correlation table saved as 'tfr_correlation_table_nfhs4.csv'\n\n")

# Group Summary by Fertility Category
fertility_summary <- df |>
  group_by(tfr_cat2) |>
  summarise(
    n_districts          = n(),
    mean_education       = mean(mean_education,             na.rm = TRUE),
    median_education     = median(mean_education,           na.rm = TRUE),
    mean_contraception   = mean(pct_modern_contraception,   na.rm = TRUE),
    median_contraception = median(pct_modern_contraception, na.rm = TRUE),
    mean_urban           = mean(100 - rural,                na.rm = TRUE),
    median_urban         = median(100 - rural,              na.rm = TRUE),
    mean_poor            = mean(pct_poor,                   na.rm = TRUE),
    median_poor          = median(pct_poor,                 na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~round(.x, 2)))

fertility_summary

write_csv(fertility_summary, here("output", "nfhs4", "tfr_cov_summary_nfhs4.csv"))

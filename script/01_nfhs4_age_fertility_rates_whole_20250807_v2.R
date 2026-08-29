# ============================================================================
# SCRIPT: 01_nfhs4_age_fertility_rates_whole_20250807_v2.R
# ============================================================================
#
# PURPOSE
# -------
# Estimates district-level Age-Specific Fertility Rates (ASFR) and Total Fertility Rate (TFR) for NFHS-4 (2015-16) using hierarchical Bayesian Poisson models.
#
# INPUT DATA
# ----------
# - data/birth_nfhs4.rds (External restricted data)
#
# OUTPUTS
# -------
# - output/tfr_summary_m7_age_nfhs4_20250807_v1.rds
# - output/nfhs4_age_prior_pc/ (Prior predictive plots)
# - output/sensitivity_analysis/nfhs4/nfhs4_tfr_m7_senistivity.png
# - output/nfhs4_ppc_m7/ (Posterior predictive plots)
#
# DEPENDENCIES
# ------------
# - script/functions/extract_tfr_asfr_20250807.R (Helper)
# ============================================================================

# ============================================================================ #
# 1. Load required packages and source functions
# ============================================================================ #
library(tidyverse)    # Data manipulation and visualization
library(here)         # Easy file path handling
library(rethinking)   # Bayesian modeling framework
library(bayesplot)    # Visualization of Bayesian model outputs
library(patchwork)    # Combine multiple ggplots

source(here("script", "functions", "extract_tfr_asfr_20250807.R"))

# ============================================================================ #
# 2. NFHS-4 Data Preparation
# ============================================================================ #
set.seed(123478)  # Ensure reproducibility

nfhs4 <- read_rds(here("data", "birth_nfhs4.rds"))

# Total births count check
sum(nfhs4$births)

# Create numeric age group IDs
nfhs4 <- nfhs4 |> 
  mutate(age_id = case_when(
    age == "15-19" ~ 1,
    age == "20-24" ~ 2,
    age == "25-29" ~ 3,
    age == "30-34" ~ 4,
    age == "35-39" ~ 5,
    age == "40-44" ~ 6,
    age == "45-49" ~ 7,
    TRUE           ~ NA
  ))

unique(nfhs4$age)  # Check unique age groups

# Create district and state ID mappings
district <- nfhs4 |> 
  select(district, state) |> 
  distinct() |> 
  mutate(district_id = row_number())

state <- nfhs4 |> 
  select(state) |> 
  distinct() |> 
  mutate(state_id = row_number())

# Merge IDs into main data
nfhs4 <- nfhs4 |> 
  left_join(state) |> 
  left_join(district)

# Compute ASFR by state and median ages for each age group
nfhs4 <- nfhs4 |> 
  group_by(age, state) |> 
  mutate(
    asfr_state = sum(births) / sum(n),
    median_age = case_when(
      age_id == 1 ~ 17,
      age_id == 2 ~ 22, 
      age_id == 3 ~ 27,
      age_id == 4 ~ 32,
      age_id == 5 ~ 37,
      age_id == 6 ~ 42, 
      age_id == 7 ~ 47
    )
  )

# Identify ASFR peak age for each state
nfhs4 <- nfhs4 |> 
  ungroup() |> 
  group_by(state) |> 
  mutate(asfr_peak = median_age[which.max(asfr_state)]) |> 
  ungroup()

# Calculate age parameter (distance from peak age)
nfhs4 <- nfhs4 |> 
  mutate(age_para = median_age - asfr_peak)

# Standardize age parameter
nfhs4 <- nfhs4 |> 
  mutate(age_para_scaled = scale(age_para))

# ============================================================================ #
# 3. Prepare data list for ulam() model
# ============================================================================ #
data_list <- list(
  births          = round(nfhs4$births),          # Integer births
  women           = round(nfhs4$n),               # Exposure
  age_id          = as.integer(nfhs4$age_id),     # Age group index
  district_id     = as.integer(nfhs4$district_id),# District index
  state_id        = as.integer(nfhs4$state_id),   # State index
  age_para_scaled = nfhs4$age_para_scaled         # Scaled age parameter
)

# Rename for clarity
nfhs4 <- nfhs4 |> rename(age_group = age)

# ============================================================================ #
# 4. Poisson Hierarchical Model (Original Model)
# ============================================================================ #
m7 <- ulam(
  alist(
    # Likelihood
    births ~ dpois(lambda),
    log(lambda) <- alpha[age_id] +
      r_dist[district_id] +
      r_state[state_id] +
      b1 * age_para_scaled^2 + 
      log(women),
    
    # Hierarchical priors
    r_dist[district_id] ~ dnorm(0, sigma_district),
    r_state[state_id]   ~ dnorm(0, sigma_state),
    alpha[age_id]       ~ dnorm(abar, sigma),
    
    # Prior for quadratic effect (expected inverted-U)
    b1 ~ dnorm(-0.5, 0.5),
    
    # Hyperpriors
    sigma_state   ~ dexp(2),
    sigma_district~ dexp(2),
    abar          ~ dnorm(-3.4, 1.5),
    sigma         ~ dexp(2)
  ),
  data   = data_list,
  chains = 4,
  cores  = 4,
  log_lik= TRUE,
  iter   = 4000
)

# Extract model results
m7_result <- generate_summary(
  model         = m7,
  nfhs_data     = nfhs4,
  n_districts   = 640,
  date          = 20250807,
  district_data = district,
  state_data    = state,
  data_list     = data_list
)

# Total Fertility Rate summary
m7_tfr <- m7_result$tfr_summary

# ============================================================================ #
# 5. District-Level Uncertainty Summary
# ============================================================================ #
# Calculate 95% CrI width for each district
uncertainty_summary <- m7_tfr |>
  mutate(cri_width = tfr_upper - tfr_lower) |>
  select(state, district, tfr_median, cri_width) |>
  group_by() |>
  summarise(
    median_cri_width = median(cri_width, na.rm = TRUE),
    iqr_lower = quantile(cri_width, 0.25, na.rm = TRUE),
    iqr_upper = quantile(cri_width, 0.75, na.rm = TRUE),
    min_cri_width = min(cri_width, na.rm = TRUE),
    max_cri_width = max(cri_width, na.rm = TRUE)
  )

# Display uncertainty summary
cat("\n=== DISTRICT-LEVEL UNCERTAINTY SUMMARY (NFHS-4) ===\n")
print(uncertainty_summary)

# ============================================================================ #
# 6. Variance Components and Pooling Strength
# ============================================================================ #
# Extract posterior samples
post_m7 <- extract.samples(m7)

# Calculate posterior summaries for variance components
variance_components <- tibble(
  parameter = c("σ_district", "σ_state", "σ_α (age)"),
  posterior_median = c(
    median(post_m7$sigma_district),
    median(post_m7$sigma_state),
    median(post_m7$sigma)
  ),
  posterior_lower = c(
    quantile(post_m7$sigma_district, 0.025),
    quantile(post_m7$sigma_state, 0.025),
    quantile(post_m7$sigma, 0.025)
  ),
  posterior_upper = c(
    quantile(post_m7$sigma_district, 0.975),
    quantile(post_m7$sigma_state, 0.975),
    quantile(post_m7$sigma, 0.975)
  )
)

# Display variance components
cat("\n=== VARIANCE COMPONENTS (Hierarchical Structure) ===\n")
print(variance_components)

# ============================================================================ #
# 7. Sensitivity Analysis: Weak vs. Original Priors
# ============================================================================ #
m7_weak <- ulam(
  alist(
    # Likelihood
    births ~ dpois(lambda),
    log(lambda) <- alpha[age_id] +
      r_dist[district_id] +
      r_state[state_id] +
      b1 * age_para_scaled^2 +
      log(women),
    
    # Hierarchical priors
    r_dist[district_id] ~ dnorm(0, sigma_district),
    r_state[state_id]   ~ dnorm(0, sigma_state),
    alpha[age_id]       ~ dnorm(abar, sigma),
    
    # Prior for quadratic effect
    b1 ~ dnorm(-0.5, 5),
    
    # Hyperpriors (weaker shrinkage)
    abar           ~ dnorm(0, 10),
    sigma_state    ~ dstudent(3, 0, 2.5),
    sigma_district ~ dstudent(3, 0, 2.5),
    sigma          ~ dstudent(3, 0, 2.5)  
  ),
  data    = data_list,
  chains  = 4,
  cores   = 4,
  log_lik = TRUE,
  iter    = 4000,
  constraints = list(
    sigma_state    = "lower=0",
    sigma_district = "lower=0",
    sigma          = "lower=0"
  )
)

# Extract results for weak priors
m7_weak_result <- generate_summary(
  model         = m7_weak,
  nfhs_data     = nfhs4,
  n_districts   = 640,
  date          = 20250807,
  district_data = district,
  state_data    = state,
  data_list     = data_list
)

m7_weak_tfr <- m7_weak_result$tfr_summary

# ============================================================================ #
# 8. Combine and Compare Results
# ============================================================================ #
# Combine TFR results from both models
results_original <- m7_tfr |> 
  mutate(model = "Original Model")

results_weak <- m7_weak_tfr |> 
  mutate(model = "Weak Priors")

combined_tfr <- results_original |> 
  bind_rows(results_weak)

# Combine ASFR results from both models
combined_asfr <- bind_rows(
  m7_result$asfr_summary     |> mutate(model = "Original Model"),
  m7_weak_result$asfr_summary|> mutate(model = "Weak Priors")
) |> 
  mutate(
    age_group   = factor(age_group, levels = c("15-19", "20-24", "25-29", "30-34", 
                                               "35-39", "40-44", "45-49")),
    district_id = as.factor(district_id),
    model       = factor(model, levels = c("Weak Priors", "Original Model"))
  )

# ============================================================================ #
# 9. ASFR Comparison Plots
# ============================================================================ #
plot_asfr_comparison <- function(data, age_filter) {
  data %>%
    filter(age_group == age_filter) %>%
    ggplot(aes(x = district, 
               y = lambda_median_asfr,
               ymin = lambda_lower_asfr,
               ymax = lambda_upper_asfr,
               color = model)) +
    geom_pointrange(position = position_dodge(width = 0.3),
                    size = 0.6,
                    alpha = 0.8,
                    fatten = 1.5) +
    labs(
      title = paste("ASFR Comparison:", age_filter),
      x     = NULL,
      y     = "Fertility Rate"
    ) +
    theme_minimal() +
    theme(
      axis.text.x        = element_blank(),
      legend.position    = "bottom",
      panel.grid.major.x = element_blank()
    )
}

# Generate and save ASFR comparison plots for all age groups
output_dir <- here("output", "sensitivity_analysis", "nfhs4")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (age in unique(combined_asfr$age_group)) {
  plot_name <- paste0("asfr_comparison_", gsub("-", "_", age), ".png")
  
  ggsave(
    filename = here(output_dir, plot_name),
    plot     = plot_asfr_comparison(combined_asfr, age),
    width    = 8,
    height   = 5,
    dpi      = 300
  )
  
  message("Saved: ", plot_name)
}

# ============================================================================ #
# 10. TFR Comparison Plot: Weak priors (base) vs. strong priors
# ============================================================================ #
ggplot(combined_tfr, aes(x = factor(district_id), y = tfr_median, color = model)) +
  
  # Weak priors: base layer
  geom_point(
    data     = filter(combined_tfr, model == "Weak Priors"),
    position = position_dodge(width = 0.5), 
    size     = 1.5,
    alpha    = 0.8
  ) +
  geom_errorbar(
    data     = filter(combined_tfr, model == "Weak Priors"),
    aes(ymin = tfr_lower, ymax = tfr_upper),
    width    = 0.2, 
    position = position_dodge(width = 0.5),
    alpha    = 0.8
  ) +
  
  # Original model: overlay
  geom_point(
    data     = filter(combined_tfr, model == "Original Model"),
    position = position_dodge(width = 0.5), 
    size     = 1,
    alpha    = 0.9
  ) +
  geom_errorbar(
    data     = filter(combined_tfr, model == "Original Model"),
    aes(ymin = tfr_lower, ymax = tfr_upper),
    width    = 0.2, 
    position = position_dodge(width = 0.5),
    alpha    = 0.9
  ) +
  
  labs(
    title    = "TFR Estimates Comparison",
    subtitle = "Weak priors (base) with strong priors overlaid",
    x        = "District ID", 
    y        = "Total Fertility Rate",
    color    = "Prior Strength"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(
    values = c("#ef8a62", "#67a9cf"),
    breaks = c("Weak Priors", "Original Model")
  ) +
  geom_hline(
    yintercept = 2.1, 
    linetype   = "dashed", 
    color      = "gray50"
  )

# Save TFR sensitivity plot
ggsave(
  here("output", "sensitivity_analysis", "nfhs4", "nfhs4_tfr_m7_senistivity.png"),
  width  = 16,
  height = 6
)

# ============================================================================ #
# 11. Posterior Predictive Check
# ============================================================================ #
posterior_pred <- extract.samples(m7)    # Extract posterior samples from model m7

# Extract observed data
y_obs <- data_list$births                # Vector of observed birth counts
y_rep <- posterior_pred$lambda           # Posterior predicted λ values for each observation

# Setup for batch plotting
N <- length(y_obs)
batch_size <- 140
n_batches <- ceiling(N / batch_size)
plot_list <- list()

# Loop through batches
for (i in seq_len(n_batches)) {
  # Define start and end indices for this batch
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, N)
  
  # Extract subset of observed and replicated data
  y_obs_sub <- y_obs[start_idx:end_idx]
  y_rep_sub <- y_rep[, start_idx:end_idx, drop = FALSE]
  
  # Create PPC intervals plot for this batch
  p <- ppc_intervals(y = y_obs_sub, yrep = y_rep_sub, prob = 0.9) +
    ggtitle(paste("PPC for observations", start_idx, "to", end_idx)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Store plot in list
  plot_list[[i]] <- p
}

# Example: print specific plots
print(plot_list[[1]])     # First batch
print(plot_list[[30]])    # 30th batch

# Save plots in pages of 4
output_dir <- here("output", "nfhs4_ppc_m7")   # Define output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq(1, 32, by = 4)) {
  # Combine up to 4 plots in a page
  plots <- wrap_plots(plot_list[i:min(i + 3, 32)], ncol = 2)
  
  # Print to console
  print(plots)
  
  # Define filename
  file_name <- sprintf("ppc_page_%02d.png", (i - 1) / 4 + 1)
  file_path <- file.path(output_dir, file_name)
  
  # Save file
  ggsave(file_path, plots, width = 10, height = 8, dpi = 300)
}

# ============================================================================ #
# 12. Prior Predictive Check (Original Model)
# ============================================================================ #
# Extract prior samples from model
prior <- extract_prior_ulam(m7, chains = 2)

# Simulate outcomes from the prior distribution
prior_sim <- sim(m7, post = prior, data = data_list, n = 500)

# Flatten simulations into single vector
all_simulated_births <- as.vector(prior_sim)

# Compute 95% interval of simulated values
lower_bound <- quantile(all_simulated_births, 0.025)
upper_bound <- quantile(all_simulated_births, 0.975)

# Keep only values within the 95% interval
filtered_sim <- all_simulated_births[
  all_simulated_births >= lower_bound & all_simulated_births <= upper_bound
]

# Inspect ranges before and after filtering
range(all_simulated_births)
range(filtered_sim)

# Observed data
y_obs <- data_list$births
y_rep <- prior_sim    # Simulated replicated data

# Prepare for posterior predictive checks (PPCs)
N <- length(y_obs)
batch_size <- 147
n_batches <- ceiling(N / batch_size)
plot_list <- list()

# Loop over batches of data for PPC plots
for (i in seq_len(n_batches)) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, N)
  
  y_obs_sub <- y_obs[start_idx:end_idx]
  y_rep_sub <- y_rep[, start_idx:end_idx, drop = FALSE]
  
  p <- ppc_intervals(y = y_obs_sub, yrep = y_rep_sub, prob = 0.9) +
    ggtitle(paste("PPC for observations", start_idx, "to", end_idx)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list[[i]] <- p
}

# Example PPC plot output
print(plot_list[[1]])
print(plot_list[[30]])

# Create output directory
output_dir <- here("output", "nfhs4_age_prior_pc")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Filter only ggplot objects
is_ggplot <- sapply(plot_list, function(x) inherits(x, "ggplot"))
plot_list <- plot_list[is_ggplot]

# Save PPC plots in groups of 4
for (i in seq(1, length(plot_list), by = 4)) {
  current_plots <- plot_list[i:min(i + 3, length(plot_list))]
  if (length(current_plots) == 0) next
  
  combined <- wrap_plots(current_plots, ncol = 2)
  print(combined)
  
  page_num <- (i - 1) %/% 4 + 1
  ggsave(
    file.path(output_dir, sprintf("ppc_page_%02d.png", page_num)),
    combined, width = 10, height = 8, dpi = 300
  )
}

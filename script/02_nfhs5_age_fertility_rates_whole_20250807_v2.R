# ============================================================================
# SCRIPT: 02_nfhs5_age_fertility_rates_whole_20250807_v2.R
# ============================================================================
#
# PURPOSE
# -------
# Estimates district-level ASFR and TFR for NFHS-5 (2019-21) using hierarchical Bayesian Poisson models.
#
# INPUT DATA
# ----------
# - data/asfr_district_nfhs5.rds (External restricted data)
#
# OUTPUTS
# -------
# - output/tfr_summary_m7_age_nfhs5_20250807_v1.rds
# - output/nfhs5_age_prior_pc/ (Prior predictive plots)
# - output/sensitivity_analysis/nfhs5/nfhs5_tfr_m7_senistivity.png
# - output/nfhs5_ppc_m7/ (Posterior predictive plots)
#
# DEPENDENCIES
# ------------
# - script/functions/extract_tfr_asfr_20250807.R (Helper)
# ============================================================================

# ============================================================================ #
# 1. Load required packages and source functions
# ============================================================================ #
library(tidyverse)    # Data wrangling and visualization
library(here)         # File path handling
library(rethinking)   # Bayesian modeling (ulam function)
library(bayesplot)    # Plotting Bayesian model outputs
library(patchwork)    # Combining ggplots

# Custom function for extracting TFR and ASFR
source(here("script", "functions", "extract_tfr_asfr_20250807.R"))

# ============================================================================ #
# 2. NFHS-5 Data Preparation
# ============================================================================ #
set.seed(12326)   # For reproducibility

# Load datasets
nfhs5 <- read_rds(here("data", "asfr_district_nfhs5.rds"))

# Total births check
sum(nfhs5$births)

# Assign numeric IDs for age groups
nfhs5 <- nfhs5 |> 
  mutate(age_id = case_when(
    age == "15-19" ~ 1,
    age == "20-24" ~ 2,
    age == "25-29" ~ 3,
    age == "30-34" ~ 4,
    age == "35-39" ~ 5,
    age == "40-44" ~ 6,
    age == "45-49" ~ 7,
    T              ~ NA
  ))

unique(nfhs5$age)

# Create district and state ID mappings
district <- nfhs5 |> 
  select(district, state) |> 
  distinct() |> 
  mutate(district_id = row_number())

state <- nfhs5 |> 
  select(state) |> 
  distinct() |> 
  mutate(state_id = row_number())

# Merge state and district IDs into main dataset
nfhs5 <- nfhs5 |> 
  left_join(state) |> 
  left_join(district) 

# Calculate ASFR at state level and median age per age group
nfhs5 <- nfhs5 |> 
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

# Identify peak ASFR age per state
nfhs5 <- nfhs5 |> 
  ungroup() |> 
  group_by(state) |> 
  mutate(asfr_peak = median_age[which.max(asfr_state)]) |> 
  ungroup()

# Create age parameter variables (centered & scaled)
nfhs5 <- nfhs5 |> 
  mutate(age_para = median_age - asfr_peak)

nfhs5 <- nfhs5 |> 
  mutate(age_para_scaled = scale(age_para))

# ============================================================================ #
# 3. Prepare data list for ulam model
# ============================================================================ #
data_list <- list(
  births           = round(nfhs5$births),          
  women            = round(nfhs5$n),               
  age_id           = as.integer(nfhs5$age_id),     
  district_id      = as.integer(nfhs5$district_id),
  state_id         = as.integer(nfhs5$state_id),
  age_para_scaled  = nfhs5$age_para_scaled
)

# Rename for consistency
nfhs5 <- nfhs5 |> rename(age_group = age)

# ============================================================================ #
# 4. Poisson Model (Original Model)
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
    
    # Random effects
    r_dist[district_id] ~ dnorm(0, sigma_district),
    r_state[state_id]   ~ dnorm(0, sigma_state),
    alpha[age_id]       ~ dnorm(abar, sigma),
    
    # Fixed effect
    b1 ~ dnorm(-0.5, 0.5),
    
    # Hyperpriors
    sigma_state   ~ dexp(2),
    sigma_district~ dexp(2),
    abar          ~ dnorm(-3.4, 1.5),
    sigma         ~ dexp(2)
  ),
  data    = data_list,
  chains  = 4,
  cores   = 4,
  log_lik = TRUE,
  iter    = 4000
)

# Summarise original model results
m7_result <- generate_summary(
  model         = m7,
  nfhs_data     = nfhs5,
  n_districts   = 707,
  date          = 20250807,
  district_data = district,
  state_data    = state,
  data_list     = data_list
)

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
cat("\n=== DISTRICT-LEVEL UNCERTAINTY SUMMARY (NFHS-5) ===\n")
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
# 7. Sensitivity Analysis (Weaker Priors)
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

# Summarise weak priors model results
m7_weak_result <- generate_summary(
  model         = m7_weak,
  nfhs_data     = nfhs5,
  n_districts   = 707,
  date          = 20250807,
  district_data = district,
  state_data    = state,
  data_list     = data_list
)
m7_weak_tfr <- m7_weak_result$tfr_summary

# ============================================================================ #
# 8. Combine Results for Comparison
# ============================================================================ #
results_original <- m7_tfr |> mutate(model = "Original Model")
results_weak     <- m7_weak_tfr |> mutate(model = "Weak Priors")

combined_tfr <- results_original |> bind_rows(results_weak)

combined_asfr <- bind_rows(
  m7_result$asfr_summary |> mutate(model = "Original Model"),
  m7_weak_result$asfr_summary |> mutate(model = "Weak Priors")
) |> 
  mutate(
    age_group   = factor(age_group, levels = c("15-19", "20-24", "25-29", 
                                               "30-34", "35-39", "40-44", "45-49")),
    district_id = as.factor(district_id),
    model       = factor(model, levels = c("Weak Priors", "Original Model"))
  )

# ============================================================================ #
# 9. Function to Plot ASFR Comparison by Age Group
# ============================================================================ #
plot_asfr_comparison <- function(data, age_filter) {
  data %>%
    filter(age_group == age_filter) %>%
    ggplot(aes(x = district, 
               y = lambda_median_asfr,
               ymin = lambda_lower_asfr,
               ymax = lambda_upper_asfr,
               color = model)) +
    geom_pointrange(
      position = position_dodge(width = 0.3),
      size     = 0.6,
      alpha    = 0.8,
      fatten   = 1.5
    ) +
    labs(
      title = paste("ASFR Comparison:", age_filter),
      x     = NULL,
      y     = "Fertility Rate"
    ) +
    theme_minimal() +
    theme(
      axis.text.x       = element_blank(),
      legend.position   = "bottom",
      panel.grid.major.x= element_blank()
    )
}

# Save ASFR Plots by Age Group
output_dir <- here("output", "sensitivity_analysis", "nfhs5")
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
# 10. TFR Comparison Plot
# ============================================================================ #
ggplot(combined_tfr, aes(x = factor(district_id), y = tfr_median, color = model)) +
  # Weak priors layer
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
  # Original model overlay
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

# Save TFR comparison plot
ggsave(
  here("output","sensitivity_analysis", "nfhs5", "nfhs5_tfr_m7_senistivity.png"), 
  width  = 16, 
  height = 6
)

# ============================================================================ #
# 11. Posterior Predictive Check
# ============================================================================ #
posterior_pred <- extract.samples(m7)   # Extract posterior samples from model m7

# Extract observed data
y_obs <- data_list$births               # Vector of observed births
y_rep <- posterior_pred$lambda          # Posterior predictive lambda matrix

N <- length(y_obs)
batch_size <- 140
n_batches <- ceiling(N / batch_size)

plot_list <- vector("list", n_batches)

for (i in seq_len(n_batches)) {
  idx <- ((i - 1) * batch_size + 1):(i * batch_size)
  idx <- idx[idx <= N]
  
  p <- ppc_intervals(
    y = y_obs[idx],
    yrep = y_rep[, idx, drop = FALSE],
    prob = 0.9
  ) +
    ggtitle(sprintf("PPC: %d–%d", min(idx), max(idx))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list[[i]] <- p
}

# Save pages of 4 plots each
output_dir <- here("output", "nfhs5_ppc_m7")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

pages <- split(plot_list, ceiling(seq_along(plot_list) / 4))

for (p in seq_along(pages)) {
  page_plot <- wrap_plots(pages[[p]], ncol = 2)
  ggsave(
    filename = sprintf("%s/ppc_page_%02d.png", output_dir, p),
    plot = page_plot,
    width = 10, height = 8, dpi = 300
  )
}

# ============================================================================ #
# 12. Prior Predictive Check (Original Model)
# ============================================================================ #
# Extract prior samples from model m7
prior <- extract_prior_ulam(m7, chains = 2)

# Generate predictions from the prior
prior_sim <- sim(m7, post = prior, data = data_list, n = 500)

# Flatten all simulations into a single vector for summarization
all_simulated_births <- as.vector(prior_sim)

# Compute 95% credible interval for simulated births
lower_bound <- quantile(all_simulated_births, 0.025)
upper_bound <- quantile(all_simulated_births, 0.975)

# Keep only simulations within the 95% interval
filtered_sim <- all_simulated_births[
  all_simulated_births >= lower_bound & 
    all_simulated_births <= upper_bound
]

# Check ranges before and after filtering
range(all_simulated_births)
range(filtered_sim)

# Extract observed birth counts
y_obs <- data_list$births
y_rep <- prior_sim  

# Number of observations and batching parameters for plotting
N <- length(y_obs)
batch_size <- 147
n_batches <- ceiling(N / batch_size)
plot_list <- list()

# Loop over batches of observations for PPC intervals
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

# Preview example plots
print(plot_list[[1]])
print(plot_list[[30]])

# Create output directory for saving plots
output_dir <- here("output", "nfhs5_age_prior_pc")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save plots in pages of 4
for (i in seq(1, 34, by = 4)) {
  
  plots <- wrap_plots(plot_list[i:min(i + 3, 32)], ncol = 2)
  
  # Print to console
  print(plots)
  
  # Save as PNG
  file_name <- sprintf("ppc_page_%02d.png", (i - 1) / 4 + 1)
  file_path <- file.path(output_dir, file_name)
  ggsave(file_path, plots, width = 10, height = 8, dpi = 300)
}

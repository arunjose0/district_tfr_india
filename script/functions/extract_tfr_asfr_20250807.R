# ============================================================================
# SCRIPT: extract_tfr_asfr_20250807.R
# ============================================================================
#
# PURPOSE
# -------
# Helper function to extract TFR and ASFR posterior summaries from the fitted rethink/ulam Bayesian models.
#
# INPUT DATA
# ----------
# - Model fit objects (passed dynamically)
#
# OUTPUTS
# -------
# - List containing district-level and state-level TFR/ASFR summaries.
#
# DEPENDENCIES
# ------------
# - Sourced by scripts 01 and 02.
# ============================================================================

#
# Function to generate TFR and ASFR summary files based on provided model fits and data.
# Uses inputs from model objects passed to the function.
# Generates tfr_summary RDS file in output/ directory.

generate_summary <- function(
    model, 
    nfhs_data, 
    n_districts, 
    date,
    district_data = district,
    state_data = state,
    data_list = data_list
) {
  
  # ============================================================================
  # 1. Extract Model Summary
  # ============================================================================
  model_precis <- precis(model, depth = 2)
  
  # ============================================================================
  # 2. Extract Posterior Samples
  # ============================================================================
  post <- extract.samples(model)
  
  # ============================================================================
  # 3. District-State Mapping
  # ============================================================================
  # Create a unique mapping between districts and states
  district_state_map <- unique(data.frame(
    district_id = data_list$district_id,
    state_id = data_list$state_id
  ))
  
  # ============================================================================
  # 4. Define Age Groups
  # ============================================================================
  n_age <- 7
  age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  
  # ============================================================================
  # 5. Reshape Lambda into 3D Array
  # ============================================================================
  # Rearrange lambda into dimensions: [posterior draws, districts, age groups]
  lambda_array <- array(
    post$lambda,
    dim = c(nrow(post$lambda), n_districts, n_age),
    dimnames = list(
      NULL,
      district_id = 1:n_districts,
      age_group = age_groups
    )
  )
  
  # ============================================================================
  # 6. Create Lambda Summary
  # ============================================================================
  lambda_summary <- expand_grid(
    district_id = 1:n_districts,
    age_group = age_groups
  ) %>%
    mutate(
      state_id = district_state_map$state_id[district_id],
      lambda_mean   = as.vector(apply(lambda_array, c(2, 3), mean)),
      lambda_median = as.vector(apply(lambda_array, c(2, 3), median)),
      lambda_lower  = as.vector(apply(lambda_array, c(2, 3), quantile, 0.025)),
      lambda_upper  = as.vector(apply(lambda_array, c(2, 3), quantile, 0.975))
    ) %>%
    left_join(district_data) %>%
    left_join(state_data)
  
  # ============================================================================
  # 7. Join NFHS Data
  # ============================================================================
  lambda_summary <- lambda_summary |> left_join(nfhs_data)
  
  # ============================================================================
  # 8. Calculate ASFR (Age-Specific Fertility Rate)
  # ============================================================================
  asfr_summary <- lambda_summary |>
    mutate(across(
      .cols = starts_with("lambda_"),
      .fns = ~ .x / n,             # Divide by denominator from NFHS
      .names = "{.col}_asfr"
    ))
  
  # ============================================================================
  # 9. Calculate TFR (Total Fertility Rate)
  # ============================================================================
  # Multiply ASFR sum by 5 to approximate TFR
  tfr_summary <- asfr_summary |>
    group_by(district_id, district, state_id, state) |>
    summarise(
      tfr_mean   = sum(lambda_mean_asfr,   na.rm = TRUE) * 5,
      tfr_median = sum(lambda_median_asfr, na.rm = TRUE) * 5,
      tfr_lower  = sum(lambda_lower_asfr,  na.rm = TRUE) * 5,
      tfr_upper  = sum(lambda_upper_asfr,  na.rm = TRUE) * 5,
      .groups = "drop"
    )
  
  # ============================================================================
  # 10. Save Output
  # ============================================================================
  model_name  <- deparse(substitute(model))
  survey_name <- ifelse(grepl("5", deparse(substitute(nfhs_data))), "nfhs5", "nfhs4")
  
  output_filename <- paste0(
    "tfr_summary_", model_name, "_age_", survey_name, "_", date, "_v1.rds"
  )
  
  write_rds(
    list(
      precis = model_precis,
      lambda = lambda_summary,
      asfr   = asfr_summary,
      tfr_district    = tfr_summary
    ),
    here::here("output", output_filename)
  )
  
  # ============================================================================
  # 11. Return Results
  # ============================================================================
  return(list(
    precis        = model_precis,
    lambda_summary = lambda_summary,
    asfr_summary   = asfr_summary,
    tfr_summary    = tfr_summary
  ))
}

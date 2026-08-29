# ============================================================================
# SCRIPT: 14_export_manuscript_items.R
# ============================================================================
#
# PURPOSE
# -------
# Aggregates and copies all generated figures and tables into a centralized 
# output/manuscript_figures_tables/ directory, renaming them to their final 
# manuscript titles (e.g., Figure_1, Table_1, Supplementary_S1, etc.).
#
# INPUT DATA
# ----------
# - All final figures and tables generated in output/ by scripts 05-13.
#
# OUTPUTS
# -------
# - output/manuscript_figures_tables/
#
# DEPENDENCIES
# ------------
# - Run all preceding scripts to ensure files are generated before copying.
# ============================================================================

library(here)

# Create the final manuscript directory
out_dir <- here("output", "manuscript_figures_tables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("Aggregating manuscript figures and tables to:", out_dir, "\n\n")

# Define the file mapping (source path relative to output/ -> destination name)
mappings <- list(
  # Main Manuscript Figures
  c("map/tfr_india_20250807.png", "Figure_1_TFR_Map.png"),
  c("Figure_2_external_consistency_faceted.png", "Figure_2_External_Consistency.png"),
  c("comparison/fig_consistency_parity_age_2x2.png", "Figure_3_Parity_Age_Consistency.png"),
  c("Figure_SES_validation_final_NFHS4.png", "Figure_4_SES_Validation_NFHS4.png"),
  c("Figure_SES_validation_final_NFHS5.png", "Figure_4_SES_Validation_NFHS5.png"),
  
  # Main Manuscript Tables
  c("nfhs4/top10_nfhs4.xlsx", "Table_1_NFHS4_Top_10_Districts.xlsx"),
  c("nfhs4/lower10_nfhs4.xlsx", "Table_1_NFHS4_Lower_10_Districts.xlsx"),
  c("nfhs5/top10_nfhs5.xlsx", "Table_1_NFHS5_Top_10_Districts.xlsx"),
  c("nfhs5/lower10_nfhs5.xlsx", "Table_1_NFHS5_Lower_10_Districts.xlsx"),
  
  # Supplementary Tables
  c("nfhs4/tfr_nfhs4.xlsx", "Supplementary_Table_S5_NFHS4_All_Districts.xlsx"),
  c("nfhs5/tfr_nfhs5.xlsx", "Supplementary_Table_S6_NFHS5_All_Districts.xlsx"),
  
  # Supplementary Figures
  c("comparison/nfhs4_nfhs5_comp_fertility_stacked_chart.png", "Supplementary_Figure_S11_Stacked_Categories.png"),
  c("nfhs4/tfr_four_dimensions_nfhs4.png", "Supplementary_Figure_S12_Dimensions_NFHS4.png"),
  c("nfhs5/tfr_four_dimensions_nfhs5.png", "Supplementary_Figure_S13_Dimensions_NFHS5.png")
)

# Copy files
for (map_pair in mappings) {
  src <- here("output", map_pair[1])
  dest <- file.path(out_dir, map_pair[2])
  
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    cat("[COPIED] ->", map_pair[2], "\n")
  } else {
    cat("[MISSING] ", src, "\n")
  }
}

# Handle directories (S3 and S4 Prior Predictive Checks)
dir_mappings <- list(
  c("nfhs4_age_prior_pc", "Supplementary_S3_PPC_NFHS4"),
  c("nfhs5_age_prior_pc", "Supplementary_S4_PPC_NFHS5")
)

for (dir_pair in dir_mappings) {
  src_dir <- here("output", dir_pair[1])
  dest_dir <- file.path(out_dir, dir_pair[2])
  
  if (dir.exists(src_dir)) {
    if (!dir.exists(dest_dir)) dir.create(dest_dir)
    # Copy contents
    files <- list.files(src_dir, full.names = TRUE)
    file.copy(files, dest_dir, overwrite = TRUE)
    cat("[COPIED FOLDER] ->", dir_pair[2], " (", length(files), "files )\n")
  } else {
    cat("[MISSING FOLDER] ", src_dir, "\n")
  }
}

cat("\nDone!\n")

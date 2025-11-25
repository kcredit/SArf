# Complete SArf Example: Dublin Health Rating Index
# ============================================================================
# This script demonstrates the full SArf workflow using data from
# the Health Rating Index project for Dublin, Ireland.
#
# Project: https://github.com/kcredit/health-rating-index
# Publication: https://doi.org/10.5281/zenodo.15183740
# ============================================================================

# Load libraries
library(SArf)
library(sf)
library(dplyr)

# Load data
cat("Loading data...\n")
data_path <- system.file("extdata", "model_data.shp", package = "SArf")
data <- st_read(data_path, quiet = TRUE)

cat("Dataset size:", nrow(data), "small areas\n")
cat("\nVariables:\n")
cat("  - HRI_gaus_n: Health Rating Index (outcome)\n")
cat("  - In22_ED: Deprivation index\n")
cat("  - NoAuto_p: % households without car\n")
cat("  - POPD: Population density\n")
cat("  - log_dist: Log distance to city center\n")
cat("  - ov60: % over 60 years old\n")
cat("  - nonIrish: % non-Irish nationals\n\n")

# Check data
summary(data$HRI_gaus_n)

# Calculate appropriate block range for spatial CV
bbox <- st_bbox(data)
block_range <- min(bbox["xmax"] - bbox["xmin"], 
                   bbox["ymax"] - bbox["ymin"]) / 3
cat("Block range:", round(block_range), "meters\n\n")

# Choose parameters based on size
n_rows <- nrow(data)
if (n_rows < 1000) {
  n_folds <- 5
  n_bootstrap <- 20
  num_trees <- 500
} else if (n_rows < 3000) {
  n_folds <- 5
  n_bootstrap <- 10
  num_trees <- 500
} else {
  n_folds <- 3
  n_bootstrap <- 5
  num_trees <- 250
}

# Run SArf analysis
cat("Running SArf analysis...\n")
cat("This may take 30-60 minutes for large datasets\n\n")

results <- SArf(
  formula = HRI_gaus_n ~ In22_ED + NoAuto_p + POPD + log_dist + ov60 + nonIrish,
  data = data,
  k_neighbors = 10,
  n_folds = n_folds,
  n_bootstrap = n_bootstrap,
  num_trees = num_trees,
  block_range = block_range,
  create_map = TRUE,
  verbose = TRUE
)

# View results
cat("\n=== RESULTS SUMMARY ===\n")
print(results)

# Detailed outputs
cat("\n=== MODEL COMPARISON ===\n")
print(results$model_comparison)

cat("\n=== VARIABLE IMPORTANCE ===\n")
print(results$variable_importance)

# Display plots
cat("\nDisplaying plots...\n")
print(results$moran_plot)
print(results$importance_plot)
print(results$ale_plots)

# Display interactive map
cat("\nOpening interactive map...\n")
print(results$leaflet_map)

# Save outputs
cat("\nSaving outputs...\n")
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# Save plots
ggsave(file.path(output_dir, "moran_plot.png"), 
       results$moran_plot, width = 6, height = 5, dpi = 300)
ggsave(file.path(output_dir, "importance_plot.png"), 
       results$importance_plot, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "ale_plots.png"), 
       results$ale_plots, width = 10, height = 8, dpi = 300)

# Save tables
write.csv(results$model_comparison, 
          file.path(output_dir, "model_comparison.csv"), 
          row.names = FALSE)
write.csv(results$variable_importance, 
          file.path(output_dir, "variable_importance.csv"), 
          row.names = FALSE)
write.csv(results$ale_results, 
          file.path(output_dir, "ale_data.csv"), 
          row.names = FALSE)

# Save map
library(htmlwidgets)
saveWidget(results$leaflet_map, 
           file.path(output_dir, "results_map.html"),
           selfcontained = TRUE)

cat("\n=== ALL OUTPUTS SAVED ===\n")
cat("Location:", normalizePath(output_dir), "\n")
cat("\nFiles created:\n")
cat("  - moran_plot.png\n")
cat("  - importance_plot.png\n")
cat("  - ale_plots.png\n")
cat("  - model_comparison.csv\n")
cat("  - variable_importance.csv\n")
cat("  - ale_data.csv\n")
cat("  - results_map.html\n")

cat("\n=== KEY FINDINGS ===\n")
cat("1. Random forest captures non-linear health relationships\n")
cat("2. Deprivation is the strongest predictor of health outcomes\n")
cat("3. Spatial models successfully remove residual autocorrelation\n")
cat("4. Car access shows threshold effects visible in ALE plots\n")

cat("\n=== NEXT STEPS ===\n")
cat("- Examine ALE plots for non-linear relationships\n")
cat("- Check model_comparison.csv for performance metrics\n")
cat("- Open results_map.html to explore spatial patterns\n")
cat("- See full project: https://github.com/kcredit/health-rating-index\n")

cat("\nAnalysis complete! ✓\n")

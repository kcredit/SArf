# Example Outputs

This folder is for storing example outputs from running SArf on the included data.

## Running the Example

```r
library(SArf)
library(sf)

# Load data
data_path <- system.file("extdata", "model_data.shp", package = "SArf")
data <- st_read(data_path)

# Calculate block range
bbox <- st_bbox(data)
block_range <- min(bbox["xmax"] - bbox["xmin"], 
                   bbox["ymax"] - bbox["ymin"]) / 3

# Run analysis
results <- SArf(
  HRI_gaus_n ~ In22_ED + NoAuto_p + POPD + log_dist + ov60 + nonIrish,
  data = data,
  k_neighbors = 10,
  n_folds = 3,
  n_bootstrap = 5,
  block_range = block_range,
  verbose = TRUE
)

# Save outputs
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# Save plots
ggsave(file.path(output_dir, "moran_plot.png"), 
       results$moran_plot, width = 6, height = 5)
ggsave(file.path(output_dir, "importance_plot.png"), 
       results$importance_plot, width = 8, height = 6)
ggsave(file.path(output_dir, "ale_plots.png"), 
       results$ale_plots, width = 10, height = 8)

# Save tables
write.csv(results$model_comparison, 
          file.path(output_dir, "model_comparison.csv"), 
          row.names = FALSE)
write.csv(results$variable_importance, 
          file.path(output_dir, "variable_importance.csv"), 
          row.names = FALSE)

# Save map
library(htmlwidgets)
saveWidget(results$leaflet_map, 
           file.path(output_dir, "results_map.html"))

cat("All outputs saved to:", output_dir, "\n")
```

## Expected Outputs

After running the example, you should have:

### Plots
- `moran_plot.png` - Moran's I scatter plot showing spatial autocorrelation
- `importance_plot.png` - Variable importance with bootstrap confidence intervals
- `ale_plots.png` - Accumulated Local Effects plots for top 6 predictors

### Tables
- `model_comparison.csv` - Comparison of RF vs OLS/SAR/SEM/SAC models
- `variable_importance.csv` - Variable importance values with CIs

### Interactive
- `results_map.html` - Interactive leaflet map of spatial patterns

## Example Results (Dublin Data)

Typical results from the Dublin health data:

### Model Comparison
| Model | RMSE | R² | Moran's I | p-value |
|-------|------|-----|-----------|---------|
| OLS | 0.088 | 0.23 | 0.014 | 0.078 |
| SAR | 0.075 | 0.44 | -0.001 | 0.544 |
| SEM | 0.075 | 0.45 | -0.001 | 0.552 |
| SAC | 0.076 | 0.43 | -0.001 | 0.623 |
| RF | 0.081 | 0.34 | 0.073 | <0.001 |

*Note: Exact values vary due to random CV splits*

### Key Findings
- **RF captures non-linear effects** that linear models miss
- **Deprivation (In22_ED) consistently strongest predictor**
- **Spatial models successfully remove residual autocorrelation**
- **Car access shows threshold effects** in ALE plots

## Full Project Outputs

For complete outputs and reproducible analysis:
- **GitHub:** https://github.com/kcredit/health-rating-index
- **Publication:** https://doi.org/10.5281/zenodo.15183740

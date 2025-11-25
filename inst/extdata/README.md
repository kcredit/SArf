# Data

## model_data.shp

Full dataset from the **Health Rating Index for Dublin** project analyzing environmental health across all Dublin small areas.

### Source

- **Project GitHub:** https://github.com/kcredit/health-rating-index
- **Publication:** Credit, K., Kumar, D., and Eccles, E. (2025). "Exploring the transport-health-environment nexus through a new 'Health Rating Index' for Dublin, Ireland." Proceedings of the 33rd GISRUK Conference. DOI: 10.5281/zenodo.15183740

### Variables

| Variable | Description |
|----------|-------------|
| `SA_PUB2022` | Small area identifier |
| `HRI_gaus_n` | Health Rating Index (standardized, 0-1) - **OUTCOME** |
| `In22_ED` | Deprivation index score |
| `NoAuto_p` | Percentage of households without a car |
| `POPD` | Population density (persons per km²) |
| `log_dist` | Log-transformed distance to city center |
| `ov60` | Percentage of population over 60 years |
| `nonIrish` | Percentage of non-Irish nationals |
| `geometry` | Spatial geometry (POLYGON) |

### Coordinate Reference System

- **EPSG:3857** - Web Mercator (projected for spatial analysis)

### Usage

```r
library(SArf)
library(sf)

# Load data
data_path <- system.file("extdata", "model_data.shp", package = "SArf")
data <- st_read(data_path)

# Run SArf analysis
results <- SArf(
  HRI_gaus_n ~ In22_ED + NoAuto_p + POPD + log_dist + ov60 + nonIrish,
  data = data,
  k_neighbors = 10,
  n_folds = 3,
  n_bootstrap = 5,
  verbose = TRUE
)

# Save outputs
dir.create("output", showWarnings = FALSE)
ggsave("output/importance_plot.png", results$importance_plot)
write.csv(results$model_comparison, "output/model_comparison.csv")
```

### Citation

If using this data in publications, please cite:

```
Credit, K., Kumar, D., and Eccles, E. (2025). Exploring the 
transport-health-environment nexus through a new 'Health Rating Index' 
for Dublin, Ireland. Proceedings of the 33rd GISRUK Conference. 
DOI: 10.5281/zenodo.15183740
```

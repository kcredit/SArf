# SArf Package - Quick Reference Card

## 🚀 Installation (Choose One)

```r
# From GitHub (once you push)
devtools::install_github("yourusername/SArf")

# From local source
devtools::install("/path/to/SArf")

# From built package
install.packages("/path/to/SArf_0.1.0.tar.gz", repos = NULL, type = "source")
```

## 📖 Basic Usage

```r
library(SArf)

# Run complete analysis
results <- SArf(
  formula = outcome ~ predictor1 + predictor2,
  data = your_sf_data
)

# View results
print(results)      # Summary
summary(results)    # Detailed summary
plot(results)       # Visualizations
```

## 🎯 Main Function Arguments

```r
SArf(
  formula,                    # REQUIRED: y ~ x1 + x2
  data,                       # REQUIRED: sf object with spatial data
  
  # Spatial parameters
  coords = NULL,              # Optional: coordinate matrix
  k_neighbors = 20,           # Neighbors for spatial weights
  
  # Cross-validation
  n_folds = 5,                # Number of CV folds
  n_bootstrap = 20,           # Bootstrap iterations
  block_range = 1000,         # Spatial block size (meters)
  
  # Random forest
  num_trees = 500,            # Trees in forest
  mtry = NULL,                # Variables per split (auto if NULL)
  
  # Options
  compare_models = c("OLS", "SAR", "SEM", "SAC"),
  create_map = TRUE,          # Generate leaflet map?
  alpha = 0.05,               # Significance level
  seed = 1111,                # Random seed
  verbose = TRUE              # Print progress?
)
```

## 📊 Output Components

```r
results$moran_test          # Moran's I test results
results$moran_plot          # Moran's I scatter plot (ggplot)

results$spatial_cv_results  # CV predictions & metrics
results$model_comparison    # Comparison table (data.frame)
results$model_details       # Fitted model objects (list)

results$variable_importance # Importance with CIs (data.frame)
results$importance_plot     # Importance plot (ggplot)

results$ale_results         # ALE data (data.frame)
results$ale_plots           # ALE plots 3×2 grid (ggplot)

results$leaflet_map         # Interactive map (leaflet)
results$data                # Original data + predictions
results$spatial_weights     # Spatial weights matrix
```

## 🔍 Common Tasks

### Save Plots
```r
ggsave("importance.png", results$importance_plot, width = 8, height = 6)
ggsave("ale_plots.png", results$ale_plots, width = 10, height = 8)
ggsave("moran.png", results$moran_plot, width = 6, height = 5)
```

### Save Map
```r
library(htmlwidgets)
saveWidget(results$leaflet_map, "map.html")
```

### Export Tables
```r
write.csv(results$model_comparison, "model_comparison.csv")
write.csv(results$variable_importance, "importance.csv")
write.csv(results$ale_results, "ale_data.csv")
```

### Access Predictions
```r
preds <- results$spatial_cv_results$predictions_summary
# pred_mean, pred_sd, observed

# Add to original data
your_data$prediction <- preds$pred_mean
your_data$uncertainty <- preds$pred_sd
```

### Access Models
```r
# Random forest models
rf_models <- results$spatial_cv_results$models

# Spatial econometric models
sar_model <- results$model_details$SAR
sac_model <- results$model_details$SAC
summary(sac_model)
```

## 📈 Interpretation Guide

### Moran's I
- **Value ≈ 0:** No spatial autocorrelation
- **Value > 0:** Positive autocorrelation (clustering)
- **p < 0.05:** Significant spatial structure

### Model Comparison
- **Lower RMSE:** Better predictions
- **Higher R²:** More variance explained
- **Moran's I (residuals) ≈ 0:** No residual spatial structure

### Variable Importance
- **Higher value:** More important predictor
- **Non-overlapping CIs:** Significantly different importance
- **spatial_lag high:** Strong neighborhood effects

### ALE Plots
- **Flat line:** Variable has no effect
- **Steep slope:** Strong effect
- **Curved line:** Non-linear relationship
- **Wide ribbon:** More uncertainty

## ⚡ Speed vs Accuracy Tradeoffs

```r
# Faster (less robust)
SArf(..., n_folds = 3, n_bootstrap = 10, num_trees = 250)

# Balanced (default)
SArf(..., n_folds = 5, n_bootstrap = 20, num_trees = 500)

# Slower (more robust)
SArf(..., n_folds = 10, n_bootstrap = 50, num_trees = 1000)
```

## 🛠️ Troubleshooting

### Long runtime
```r
# Reduce iterations
SArf(..., n_bootstrap = 10)
# or reduce folds
SArf(..., n_folds = 3)
# or skip map
SArf(..., create_map = FALSE)
```

### Memory issues
```r
# Sample data first
library(dplyr)
data_sample <- data %>% slice_sample(n = 1000)
results <- SArf(..., data = data_sample)
```

### No spatial autocorrelation
```r
# SArf still works, but spatial methods may not be necessary
# Compare to regular RF without spatial lag
```

## 📚 Help & Documentation

```r
# Function help
?SArf
?spatial_cv_rf
?calculate_ale_ci

# Vignette
browseVignettes("SArf")

# Example
example(SArf)
```

## 🔗 Links

- **GitHub:** github.com/yourusername/SArf
- **Issues:** github.com/yourusername/SArf/issues
- **Paper:** doi.org/10.5281/zenodo.15183740

## 📄 Citation

```r
citation("SArf")
```

```bibtex
@software{credit2025sarf,
  author = {Credit, Kevin and Damanpreet, Kaur and Eccles, Emma},
  title = {SArf: Spatial Autoregressive Random Forest},
  year = {2025},
  url = {https://github.com/yourusername/SArf}
}
```

---

**Quick Start:** Load data → Run `SArf()` → View `results` → Export plots/tables

**Full Guide:** See README.md, INSTALLATION_GUIDE.md, and vignettes

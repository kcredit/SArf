# SArf v1.0 - New Features Guide

## 🎨 Changes in Version 1.0

### 1. Viridis Color Palette (Default)

Maps now use the **viridis** color palette by default instead of RdYlBu.

**Why viridis?**
- Colorblind-friendly
- Perceptually uniform
- Better for scientific visualization
- Print-friendly

**Default behavior:**
```r
results <- SArf(outcome ~ predictors, data = data)
results$leaflet_map  # Uses viridis automatically
```

**Customize palette:**
```r
# Access internal function if needed (advanced)
# Note: Map is created automatically, but you can recreate:
library(leaflet)

# Create custom map with different palette
custom_map <- leaflet(data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~colorBin("magma", outcome)(outcome),  # Different viridis variant
    # or
    fillColor = ~colorBin("RdYlBu", outcome)(outcome)  # Original style
  )
```

**Viridis variants available:**
- `"viridis"` (default - blue to yellow)
- `"magma"` (purple to yellow)
- `"plasma"` (purple to pink)
- `"inferno"` (black to yellow)
- `"cividis"` (blue to yellow, optimized for colorblind)

---

### 2. Easy Access to Spatial Econometric Models

You can now directly access individual spatial model results!

**Before v1.0:**
```r
results <- SArf(...)

# Had to dig into nested list
results$model_details$SAR
results$model_details$SAC
```

**Now in v1.0:**
```r
results <- SArf(...)

# Direct access
results$ols_model
results$sar_model
results$sem_model
results$sac_model

# Get full summaries
summary(results$sar_model)
summary(results$sac_model)
```

---

## 📊 Using the Spatial Model Results

### View Model Summaries

```r
# Run SArf
results <- SArf(
  outcome ~ predictor1 + predictor2,
  data = spatial_data
)

# Access specific models
ols <- results$ols_model
sar <- results$sar_model
sem <- results$sem_model
sac <- results$sac_model

# Get detailed summaries
summary(ols)   # Standard lm summary
summary(sar)   # Spatial lag model summary
summary(sem)   # Spatial error model summary
summary(sac)   # Combined spatial model summary
```

### Example: Detailed SAC Model

```r
# SAC (Spatial Lag + Error) model
summary(results$sac_model)

# Output includes:
# - Spatial lag coefficient (rho)
# - Spatial error coefficient (lambda)
# - Predictor coefficients
# - Model fit statistics
# - Likelihood ratio tests
```

### Extract Coefficients

```r
# OLS coefficients
coef(results$ols_model)

# SAR coefficients (includes rho)
coef(results$sar_model)

# SAC coefficients (includes rho and lambda)
coef(results$sac_model)

# Compare coefficients across models
data.frame(
  OLS = coef(results$ols_model),
  SAR = coef(results$sar_model)[-1],  # Remove rho
  SAC = coef(results$sac_model)[-c(1,2)]  # Remove rho and lambda
)
```

### Get Predictions from Spatial Models

```r
# Predictions from each model
ols_pred <- predict(results$ols_model)
sar_pred <- predict(results$sar_model)
sac_pred <- fitted(results$sac_model)  # Note: fitted() for SAC

# Compare predictions
comparison <- data.frame(
  observed = results$data$outcome,
  ols = ols_pred,
  sar = sar_pred,
  sac = sac_pred,
  rf = results$spatial_cv_results$predictions_summary$pred_mean
)

head(comparison)
```

### Extract Residuals

```r
# Get residuals from each model
ols_resid <- residuals(results$ols_model)
sar_resid <- residuals(results$sar_model)
sac_resid <- residuals(results$sac_model)

# Check for remaining spatial autocorrelation
library(spdep)
moran.test(ols_resid, results$spatial_weights, zero.policy = TRUE)
moran.test(sar_resid, results$spatial_weights, zero.policy = TRUE)
moran.test(sac_resid, results$spatial_weights, zero.policy = TRUE)
```

### Publication-Ready Model Table

```r
# Create comparison table
library(stargazer)

stargazer(
  results$ols_model,
  results$sar_model,
  results$sac_model,
  type = "text",  # or "latex" or "html"
  title = "Comparison of Spatial Models",
  column.labels = c("OLS", "SAR", "SAC"),
  model.names = FALSE
)
```

---

## 🎯 Complete Example Workflow

```r
library(SArf)
library(sf)
library(dplyr)

# Load data
data <- st_read("data.shp") %>%
  filter(!is.na(outcome)) %>%
  filter(complete.cases(outcome, pred1, pred2))

st_geometry(data) <- st_jitter(st_geometry(data), 0.0001)
data <- st_transform(data, 3857)

# Run SArf
results <- SArf(
  outcome ~ pred1 + pred2,
  data = data,
  n_folds = 3,
  n_bootstrap = 5,
  verbose = TRUE
)

# ============================================================
# NEW in v1.0: Easy model access
# ============================================================

# 1. Compare model fit
print(results$model_comparison)

# 2. Examine SAC model (usually best spatial model)
summary(results$sac_model)

# 3. Check spatial autocorrelation in residuals
library(spdep)
sac_resid <- residuals(results$sac_model)
moran.test(sac_resid, results$spatial_weights, zero.policy = TRUE)

# 4. Extract spatial parameters
rho <- coef(results$sac_model)["rho"]    # Spatial lag
lambda <- coef(results$sac_model)["lambda"]  # Spatial error
cat("Spatial lag (rho):", round(rho, 3), "\n")
cat("Spatial error (lambda):", round(lambda, 3), "\n")

# 5. Compare to RF performance
cat("\nModel Performance:\n")
cat("SAC R²:", round(results$model_comparison$R2[results$model_comparison$Model == "SAC"], 3), "\n")
cat("RF R²:", round(results$model_comparison$R2[results$model_comparison$Model == "RF_Spatial_CV"], 3), "\n")

# 6. View variable importance
print(results$variable_importance)

# 7. Check map (now in viridis!)
results$leaflet_map

# 8. Save all outputs
ggsave("importance.png", results$importance_plot)
write.csv(results$model_comparison, "models.csv")

# 9. Export SAC model results
library(stargazer)
stargazer(results$sac_model, type = "text", out = "sac_model.txt")
```

---

## 📋 Quick Reference

### Accessing Results

```r
results$moran_test          # Moran's I test
results$moran_plot          # Moran's I scatter plot
results$model_comparison    # All models compared
results$ols_model           # OLS model object ← NEW
results$sar_model           # SAR model object ← NEW
results$sem_model           # SEM model object ← NEW
results$sac_model           # SAC model object ← NEW
results$variable_importance # Importance with CIs
results$importance_plot     # Importance plot
results$ale_plots           # ALE plots
results$leaflet_map         # Interactive map (viridis!) ← UPDATED
```

### Model Functions

```r
summary(results$sac_model)    # Detailed summary
coef(results$sac_model)       # Coefficients
residuals(results$sac_model)  # Residuals
fitted(results$sac_model)     # Fitted values
AIC(results$sac_model)        # Model AIC
logLik(results$sac_model)     # Log-likelihood
```

---

## 🆕 What's New Summary

| Feature | Before | Now |
|---------|--------|-----|
| Map colors | RdYlBu | **Viridis** (colorblind-friendly) |
| Model access | `results$model_details$SAR` | `results$sar_model` |
| SAC access | `results$model_details$SAC` | `results$sac_model` |
| Print output | Basic info | Shows how to access models |
| Summary output | General stats | Includes model access instructions |

---

## 🎓 Tips

1. **Always check SAC model** - Usually best for spatial data
2. **Compare RF to SAC** - Shows benefit of non-parametric approach
3. **Use viridis maps** - Better for colorblind readers and printing
4. **Export model tables** - Use stargazer for publications

---

**Upgrade to v1.0 for easier spatial model access and better visualizations!** 🚀

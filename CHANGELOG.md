# SArf Package - Changelog

## Version 2.0 (CURRENT) - 2024

### ✨ New Features
- **ALE plots now use top 6 variables from importance ranking**
  - Previously: ALE recalculated importance internally (could differ slightly)
  - Now: ALE plots exactly match the top 6 from the importance plot
  - Ensures consistency between importance and ALE visualizations

### 🐛 Bug Fixes
- **Fixed Moran's I display in model comparison table**
  - Issue: Moran's I values showed as NA in table
  - Root cause: Named vector extraction returned names instead of values
  - Fix: Added `as.numeric()` conversion in data frame construction
  - Now displays: Moran's I values and p-values for all models

### 🎯 Performance
- Progress indicators for spatial econometric models
- Time estimates for large datasets (n > 1000)
- Memory warnings for datasets > 2000 rows

---

## Version 1.2 - Debug Release

### 🔧 Improvements
- Added diagnostic script (`diagnose_morans.R`)
- Better error messages for Moran's I calculation failures
- Verbose error reporting in model comparison

### 📊 Documentation
- Added results interpretation guide
- Included example of manual Moran's I calculation
- Clarified expected runtimes for different dataset sizes

---

## Version 1.1 - Progress Indicators

### ✨ New Features
- **Progress indicators for slow models**
  - Shows "Fitting SAR... [May take 1-3 minutes]"
  - Displays completion time for each model
  - Warns about large datasets before fitting

### 🎯 User Experience
- Real-time feedback during spatial model estimation
- Estimated time warnings based on dataset size
- Checkmarks (✓) for successful model fits
- X marks (✗) for failed models

---

## Version 1.0 - Initial Release

### ✨ Major Features
- **Viridis color palette as default**
  - Changed from RdYlBu to viridis
  - Colorblind-friendly
  - Print-friendly
  - Better for scientific publications

- **Direct model access**
  - `results$ols_model` instead of `results$model_details$OLS`
  - `results$sar_model`, `results$sem_model`, `results$sac_model`
  - Easy coefficient extraction: `coef(results$sac_model)`
  - Easy summaries: `summary(results$sar_model)`

### 📚 Documentation
- Updated print method to show model access
- Updated summary method with model instructions
- Added NEW_FEATURES_v1.0.md guide

---

## Core Features (All Versions)

### Spatial Analysis
- Moran's I test for spatial autocorrelation
- Spatial weights matrix creation (k-nearest neighbors)
- Spatial lag variable calculation
- Moran scatter plot

### Spatial Cross-Validation
- blockCV spatial blocking (with k-means fallback)
- Bootstrap confidence intervals
- Spatial lag included in training
- Proper out-of-sample validation
- Customizable folds and iterations

### Model Comparison
- OLS baseline
- SAR (Spatial Autoregressive)
- SEM (Spatial Error Model)
- SAC (Spatial Lag + Error)
- Random Forest with spatial CV
- RMSE, R², and Moran's I for all models

### Variable Importance
- Permutation importance from RF
- Bootstrap confidence intervals
- Top predictors visualization
- Mean and SD across CV iterations

### ALE Plots
- Accumulated Local Effects
- Bootstrap confidence intervals
- Top 6 most important variables
- Non-linear relationship visualization

### Interactive Mapping
- Leaflet interactive maps
- Viridis color palette
- Custom labels and legends
- Export to HTML

---

## Breaking Changes

### Version 2.0
- None (backward compatible)

### Version 1.0
- Default map palette changed from RdYlBu to viridis
  - To use old palette: manually recreate maps with RdYlBu

---

## Migration Guide

### From v1.x to v2.0
No changes needed! Just reinstall:
```r
devtools::install()
```

### From v0.x to v1.0+
- Update model access code:
  ```r
  # Old
  sar_model <- results$model_details$SAR
  
  # New
  sar_model <- results$sar_model
  ```

- Maps now use viridis by default (no action needed)

---

## Known Issues

### All Versions
- Very large datasets (n > 5000) may be slow
  - **Workaround**: Sample to 2000-3000 observations
  - **Workaround**: Reduce n_bootstrap to 3-5
  - **Workaround**: Use fewer predictors

- Spatial econometric models slow for n > 2000
  - **Expected**: SAR/SEM take 1-5 minutes
  - **Expected**: SAC takes 2-10 minutes
  - **This is normal** due to matrix operations

### Version-Specific
None currently!

---

## Performance Benchmarks

| Dataset Size | RF CV (3×5) | Spatial Models | Total Time |
|--------------|-------------|----------------|------------|
| n = 500 | ~3 min | ~30 sec | ~4 min |
| n = 1000 | ~8 min | ~2 min | ~10 min |
| n = 2000 | ~20 min | ~5 min | ~25 min |
| n = 3000 | ~35 min | ~10 min | ~45 min |
| n = 5000 | ~60 min | ~20 min | ~80 min |

*Benchmarks on MacBook Pro M1, 16GB RAM*

---

## Acknowledgments

Built on:
- `ranger` for random forests
- `spdep` for spatial weights and Moran's I
- `spatialreg` for spatial econometric models
- `blockCV` for spatial cross-validation
- `ALEPlot` for accumulated local effects
- `leaflet` for interactive mapping
- `sf` for spatial data handling

---

## Future Roadmap

Potential features for v3.0:
- [ ] Spatial random effects models
- [ ] Multiple spatial weight matrices comparison
- [ ] Spatial heterogeneity tests
- [ ] Geographically weighted RF
- [ ] Parallel processing for CV bootstrap
- [ ] Shiny app for interactive exploration
- [ ] Export to spatial econometrics formats

---

## Support

- **Documentation**: See README.md and vignettes
- **Examples**: See vignettes/getting-started.Rmd
- **Diagnostics**: Run `diagnose_morans.R` for issues
- **Bug reports**: Include output from `sessionInfo()`

---

## Citation

If using SArf in publications, please cite:

```
[Your Name] (2024). SArf: Spatial Autoregressive Random Forest. 
R package version 2.0.
```

---

**Current Version: 2.0** - Download: SArf_v2.0_FINAL.zip

Last Updated: November 2024

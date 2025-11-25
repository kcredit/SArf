# SArf: Spatial Autoregressive Random Forest

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://www.r-project.org/)

## Overview

SArf implements a **Spatial Autoregressive Random Forest** methodology that treats random forests as flexible spatial autoregressive (SAR) models. The package provides:

- 🗺️ **Spatial cross-validation** with proper handling of spatial autocorrelation
- 📊 **Model comparison** framework (RF vs OLS/SAR/SEM/SAC)
- 📈 **Variable importance** with bootstrap confidence intervals
- 🎯 **ALE plots** showing non-linear relationships with uncertainty
- 🌍 **Interactive maps** for visualizing spatial patterns
- ✅ **Complete workflow** from Moran's I test to publication-ready outputs

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("kcredit/SArf")
```

## Quick Start

```r
library(SArf)
library(sf)

# Load your spatial data
data <- st_read("your_data.shp") %>%
  st_transform(3857)

# Run complete spatial analysis
results <- SArf(
  formula = outcome ~ predictor1 + predictor2,
  data = data,
  k_neighbors = 8,
  n_folds = 3,
  n_bootstrap = 5
)

# View results
print(results)
results$model_comparison
results$importance_plot
results$ale_plots
results$leaflet_map
```

## Real-World Application

See the [Health Rating Index for Dublin](https://github.com/YOUR-USERNAME/health-rating-index) project for a complete application analyzing environmental health across 3,000+ small areas.

## Citation

```bibtex
@software{sarf2025,
  title = {SArf: Spatial Autoregressive Random Forest},
  author = {Credit, Kevin and Kumar, Damanpreet and Eccles, Elizabeth},
  year = {2025},
  url = {https://github.com/YOUR-USERNAME/SArf}
}
```

## Documentation

- [Quick Reference](QUICK_REFERENCE.md)
- [Getting Started Vignette](vignettes/getting-started.Rmd)
- [Package Summary](PACKAGE_SUMMARY.md)
- [Changelog](CHANGELOG.md)

## License

MIT License - see [LICENSE](LICENSE) file.

## Contact

Kevin Credit - kevin.credit@mu.ie - Maynooth University

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
devtools::install_github("YOUR-USERNAME/SArf")
```

## Quick Start

```r
library(SArf)
library(sf)

# Load data (included with package)
data_path <- system.file("extdata", "model_data.shp", package = "SArf")
data <- st_read(data_path)

# Run complete spatial analysis
results <- SArf(
  formula = HRI_gaus_n ~ In22_ED + NoAuto_p + POPD + log_dist + ov60 + nonIrish,
  data = data,
  k_neighbors = 10,
  n_folds = 3,
  n_bootstrap = 5
)

# View results
print(results)
results$model_comparison
results$importance_plot
results$ale_plots
results$leaflet_map

# Save outputs
dir.create("output", showWarnings = FALSE)
ggsave("output/importance_plot.png", results$importance_plot)
write.csv(results$model_comparison, "output/model_comparison.csv")
```

## Origin & Real-World Application

**SArf was developed as part of the Health Rating Index project for Dublin, Ireland.**

This package emerged from research analyzing environmental health burdens and benefits across 3,000+ small areas in Dublin. The methodology and package were created to properly handle spatial autocorrelation in health-environment relationships while capturing non-linear effects that traditional spatial econometric models miss.

**📊 Full Project:** [Health Rating Index for Dublin](https://github.com/kcredit/health-rating-index)
- Complete analysis code
- Full datasets (air quality, noise, accessibility, deprivation)
- Reproducible workflow
- Publication materials

**📖 Publication:** Credit, K., Kumar, D., and Eccles, E. (2025). "Exploring the transport-health-environment nexus through a new 'Health Rating Index' for Dublin, Ireland." *Proceedings of the 33rd GISRUK Conference.* DOI: [10.5281/zenodo.15183740](https://doi.org/10.5281/zenodo.15183740)

The sample data included with this package (`dublin_sample.shp`) is a subset of 100 small areas from the full Dublin analysis, allowing users to quickly test the package and understand the methodology.

## Citation

If you use SArf in your research, please cite:

```bibtex
@software{sarf2025,
  title = {SArf: Spatial Autoregressive Random Forest},
  author = {Credit, Kevin},
  year = {2025},
  url = {https://github.com/kcredit/SArf}
}
```

And if applicable, the methodological paper:

```bibtex
@inproceedings{credit2025health,
  title = {Exploring the transport-health-environment nexus through a new 'Health Rating Index' for Dublin, Ireland},
  author = {Credit, Kevin and Kumar, Damanpreet and Eccles, Elizabeth},
  booktitle = {Proceedings of the 33rd GISRUK Conference},
  year = {2025},
  doi = {10.5281/zenodo.15183740}
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

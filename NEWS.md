# SArf 1.0.0

## Initial Release

First public release of SArf package.

### Features

* Spatial autoregressive random forest implementation
* Spatial cross-validation with block CV
* Bootstrap confidence intervals on importance and ALE plots
* Automatic comparison to spatial econometric models (OLS, SAR, SEM, SAC)
* Interactive leaflet maps with viridis color scheme
* Complete documentation and vignettes
* Direct access to spatial model objects

### Functions

* `SArf()` - Main function for complete spatial RF analysis
* `print.SArf()` - Print method showing best spatial model summary
* `summary.SArf()` - Detailed summary with all metrics
* `plot.SArf()` - Visualization options

### Documentation

* Getting started vignette
* Health-environment example vignette
* Quick reference guide
* Complete package documentation

### Dependencies

* ranger (≥ 0.12.0)
* sf (≥ 1.0.0)
* spdep (≥ 1.2.0)
* spatialreg (≥ 1.2.0)
* blockCV (≥ 3.0.0)
* ggplot2 (≥ 3.4.0)
* dplyr (≥ 1.0.0)
* tidyr (≥ 1.2.0)
* leaflet (≥ 2.1.0)
* ALEPlot (≥ 1.1.0)
* htmltools (≥ 0.5.0)

### Notes

* Tested on R ≥ 4.0.0
* Works on Windows, macOS, and Linux
* Memory efficient for datasets up to 5000 observations
* Sample larger datasets for optimal performance

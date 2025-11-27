# Data for SArf Package

## Adding the Data File

The SArf package uses `model_data.shp` from the Health Rating Index project.

### To Add Data to Package:

1. **Copy your shapefile** (all components):
   ```
   model_data.shp
   model_data.shx
   model_data.dbf
   model_data.prj
   model_data.cpg (if exists)
   ```

2. **Place files in:** `inst/extdata/`

3. **The file should already be in EPSG:3857** (Web Mercator projection)

### For Package Users

Once installed, access data with:

```r
library(SArf)
library(sf)

# Load data
data_path <- system.file("extdata", "model_data.shp", package = "SArf")
data <- st_read(data_path)

# Run analysis
...
)

# Save outputs
dir.create("output", showWarnings = FALSE)
ggsave("output/importance_plot.png", results$importance_plot)
write.csv(results$model_comparison, "output/model_comparison.csv")
```

### Required Shapefile Components

All these files must be present:
- `.shp` - Geometry
- `.shx` - Index
- `.dbf` - Attributes
- `.prj` - Projection info

### Full Project

For complete analysis and all data:
- **GitHub:** https://github.com/kcredit/health-rating-index
- **Contact:** kevin.credit@mu.ie

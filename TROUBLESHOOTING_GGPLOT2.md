# Troubleshooting: ggplot2/S7 Namespace Conflict

## Problem

You're seeing errors like:
```
Error in ggplot2::ggplot(...) : non-numeric argument to binary operator
Warning: Incompatible methods ("Ops.S7_object", "+.gg") for "+"
```

## Cause

This is a known conflict between:
- **ggplot2** (especially newer versions)
- **blockCV** package's S7 objects
- The `+` operator used for building plots

## Solution 1: Restart R and Load in Correct Order (Recommended)

```r
# 1. Restart R completely
.rs.restartR()  # In RStudio

# 2. Load packages in this EXACT order:
library(sf)
library(dplyr)
library(SArf)

# 3. Run your analysis
results <- SArf(...)

# Should work now!
```

## Solution 2: Detach and Reload Packages

```r
# Detach all packages
detach("package:SArf", unload = TRUE)
detach("package:blockCV", unload = TRUE)
detach("package:ggplot2", unload = TRUE)

# Load in correct order
library(ggplot2)
library(blockCV)
library(SArf)

# Run analysis
results <- SArf(...)
```

## Solution 3: Downgrade ggplot2 (If Above Don't Work)

```r
# Remove current ggplot2
remove.packages("ggplot2")

# Install older stable version
install.packages("ggplot2", version = "3.4.4")

# Restart R
.rs.restartR()

# Reinstall SArf
devtools::install_github("kcredit/SArf", auth_token = NULL, force = TRUE)

# Load and use
library(SArf)
results <- SArf(...)
```

## Solution 4: Use suppressWarnings()

If plots still fail but the analysis completes:

```r
# Suppress the warnings
results <- suppressWarnings({
  SArf(
    formula = outcome ~ predictors,
    data = data,
    ...
  )
})

# Results are still valid!
# Just plot creation might fail
```

## Solution 5: Create Plots Manually

If automatic plots fail, you can create them from the results:

```r
# Run without plots
results <- SArf(...)

# Create importance plot manually
library(ggplot2)
ggplot(results$variable_importance, 
       aes(x = reorder(variable, mean), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variable", y = "Importance") +
  theme_minimal()
```

## Prevention

Always start your R session with:

```r
# Clear workspace
rm(list = ls())

# Load in this order
library(sf)
library(dplyr)
library(SArf)

# Now run analysis
```

## Still Having Issues?

1. **Check package versions:**
   ```r
   packageVersion("ggplot2")
   packageVersion("blockCV")
   packageVersion("SArf")
   ```

2. **Update all packages:**
   ```r
   update.packages(ask = FALSE)
   ```

3. **Reinstall SArf:**
   ```r
   remove.packages("SArf")
   devtools::install_github("kcredit/SArf", auth_token = NULL)
   ```

4. **Report issue:**
   - GitHub: https://github.com/kcredit/SArf/issues
   - Email: kevin.credit@mu.ie
   - Include: R version, package versions, full error message

## Technical Details

This conflict occurs because:
- blockCV uses S7 objects (new R object system)
- ggplot2's `+` operator conflicts with S7's `+` method
- The namespace collision prevents plot creation

This is a known issue with blockCV and will be resolved in future package updates.

## Workaround for Package Developers

If you're modifying SArf, wrap all ggplot calls in tryCatch:

```r
plot <- tryCatch({
  ggplot(...) + geom_point() + ...
}, error = function(e) {
  warning("Plot creation failed: ", e$message)
  NULL
})
```

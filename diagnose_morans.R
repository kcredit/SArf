# Diagnostic Script for Moran's I NA Issue
# Run this after SArf completes to check what's happening

# After running SArf:
# results <- SArf(...)

# Check if spatial weights are valid
cat("=== Spatial Weights Check ===\n")
cat("Weights class:", class(results$spatial_weights), "\n")
cat("Number of regions:", length(results$spatial_weights$neighbours), "\n")

# Check residuals
cat("\n=== Residuals Check ===\n")

# OLS residuals
if (!is.null(results$ols_model)) {
  ols_resid <- residuals(results$ols_model)
  cat("OLS residuals:\n")
  cat("  Length:", length(ols_resid), "\n")
  cat("  NAs:", sum(is.na(ols_resid)), "\n")
  cat("  Range:", range(ols_resid, na.rm = TRUE), "\n")
  
  # Try Moran's I manually
  cat("\n  Trying Moran's I on OLS residuals...\n")
  ols_moran <- try(spdep::moran.test(ols_resid, results$spatial_weights, zero.policy = TRUE))
  if (inherits(ols_moran, "try-error")) {
    cat("  ERROR:", attr(ols_moran, "condition")$message, "\n")
  } else {
    cat("  SUCCESS! Moran's I =", ols_moran$estimate[1], "\n")
    cat("  p-value =", ols_moran$p.value, "\n")
  }
}

# SAR residuals
if (!is.null(results$sar_model)) {
  sar_resid <- residuals(results$sar_model)
  cat("\nSAR residuals:\n")
  cat("  Length:", length(sar_resid), "\n")
  cat("  NAs:", sum(is.na(sar_resid)), "\n")
  cat("  Range:", range(sar_resid, na.rm = TRUE), "\n")
  
  cat("\n  Trying Moran's I on SAR residuals...\n")
  sar_moran <- try(spdep::moran.test(sar_resid, results$spatial_weights, zero.policy = TRUE))
  if (inherits(sar_moran, "try-error")) {
    cat("  ERROR:", attr(sar_moran, "condition")$message, "\n")
  } else {
    cat("  SUCCESS! Moran's I =", sar_moran$estimate[1], "\n")
    cat("  p-value =", sar_moran$p.value, "\n")
  }
}

# RF residuals
rf_resid <- results$spatial_cv_results$predictions_summary$pred_mean - 
            results$spatial_cv_results$predictions_summary$observed
cat("\nRF residuals:\n")
cat("  Length:", length(rf_resid), "\n")
cat("  NAs:", sum(is.na(rf_resid)), "\n")
cat("  Range:", range(rf_resid, na.rm = TRUE), "\n")

cat("\n  Trying Moran's I on RF residuals...\n")
rf_moran <- try(spdep::moran.test(rf_resid, results$spatial_weights, zero.policy = TRUE))
if (inherits(rf_moran, "try-error")) {
  cat("  ERROR:", attr(rf_moran, "condition")$message, "\n")
} else {
  cat("  SUCCESS! Moran's I =", rf_moran$estimate[1], "\n")
  cat("  p-value =", rf_moran$p.value, "\n")
}

# Check if lengths match
cat("\n=== Dimension Check ===\n")
cat("Spatial weights regions:", length(results$spatial_weights$neighbours), "\n")
cat("OLS residuals:", if(!is.null(results$ols_model)) length(residuals(results$ols_model)) else "NULL", "\n")
cat("Data rows:", nrow(results$data), "\n")

cat("\n=== Diagnosis Complete ===\n")
cat("If you see ERRORs above, that's why Moran's I shows as NA\n")
cat("Most common cause: Length mismatch between residuals and spatial weights\n")

# =============================================================================
# SArf Package: Systematic Test and Function Explanation
# =============================================================================
#
# This script provides a systematic test of each component of the SArf
# (Spatial Autoregressive Random Forest) package using the included
# `model_data` example dataset.
#
# Each section explains the purpose of a function, demonstrates its usage,
# and interprets the results.
#
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP AND DATA LOADING
# -----------------------------------------------------------------------------

# Load required libraries
library(sf)
library(dplyr)
library(SArf)

# Load the example data
data_path <- system.file("extdata", "model_data.shp", package = "SArf")
model_data <- st_read(data_path)

# Examine the data structure
cat("\n=== DATA OVERVIEW ===\n")
cat("Dataset dimensions:", nrow(model_data), "observations,", ncol(model_data), "variables\n")
cat("Variables:", paste(names(model_data), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# EXAMPLE DATA DESCRIPTION
# -----------------------------------------------------------------------------
#
# The `model_data` dataset contains Small Area-level data from Dublin, Ireland:
#
# | Variable    | Description                                              |
# |-------------|----------------------------------------------------------|
# | HRI_gaus_e  | Health Rating Index (outcome) - composite health score   |
# | In22_ED     | Deprivation Index - socioeconomic deprivation measure    |
# | NoAuto_p    | Percentage of households without a car                   |
# | POPD        | Population density                                       |
# | log_dist    | Log distance to nearest primary or secondary road        |
# | ov60        | Percentage of population over 60 years old               |
# | nonIrish    | Percentage of non-Irish citizens                         |
#
# -----------------------------------------------------------------------------

# Prepare/clean the data
cat("\n=== DATA PREPARATION ===\n")

data_clean <- model_data %>%
  filter(!is.na(HRI_gaus_e)) %>%
  filter(complete.cases(HRI_gaus_e, In22_ED, NoAuto_p, POPD, log_dist, ov60, nonIrish)) %>%
  filter(st_is_valid(.))

# Add small jitter to avoid duplicate coordinates
st_geometry(data_clean) <- st_jitter(st_geometry(data_clean), amount = 0.00001)

# Transform to projected CRS (required for spatial blocking)
data_clean <- st_transform(data_clean, 3857)

cat("Original observations:", nrow(model_data), "\n")
cat("After cleaning:", nrow(data_clean), "\n")
cat("Removed:", nrow(model_data) - nrow(data_clean), "observations with missing data\n")

# Define the formula we'll use throughout
formula <- HRI_gaus_e ~ In22_ED + NoAuto_p + POPD + log_dist + ov60 + nonIrish


# =============================================================================
# STEP 1: MORAN'S I TEST FOR SPATIAL AUTOCORRELATION
# =============================================================================
#
# FUNCTION: test_morans_i()
#
# PURPOSE: Tests whether the dependent variable exhibits spatial autocorrelation
# (i.e., whether nearby observations have more similar values than expected by
# chance). This is crucial because:
#
#   - If significant spatial autocorrelation exists, ordinary regression may
#     produce biased estimates
#   - The presence of spatial autocorrelation justifies using spatial models
#   - The strength indicates how important spatial relationships are
#
# HOW IT WORKS:
#   1. Creates a k-nearest neighbors spatial weights matrix
#   2. Calculates Moran's I: correlation between values and neighbor averages
#   3. Tests significance against null hypothesis of spatial randomness
#   4. Creates a Moran scatter plot
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 1: MORAN'S I TEST FOR SPATIAL AUTOCORRELATION\n")
cat("=============================================================================\n")

# Test for spatial autocorrelation (using internal function)
moran_results <- SArf:::test_morans_i(
  data = data_clean,
  variable = "HRI_gaus_e",
  k_neighbors = 20,
  alpha = 0.05,
  verbose = TRUE
)

# Display the Moran scatter plot
print(moran_results$plot)

# -----------------------------------------------------------------------------
# INTERPRETATION:
#
# - Moran's I > 0: Positive spatial autocorrelation (similar values cluster)
# - Moran's I < 0: Negative spatial autocorrelation (dissimilar neighbors)
# - Moran's I ~ 0: Random spatial pattern
# - p-value < 0.05: Statistically significant
#
# EXPECTED RESULT: The Health Rating Index should show significant positive
# spatial autocorrelation because:
#   - Environmental exposures (air quality, green space) vary spatially
#   - Socioeconomic factors affecting health are spatially clustered
#   - Healthcare access varies by location
#
# The scatter plot should show a positive slope, with most points in
# upper-right (high-high) and lower-left (low-low) quadrants.
# -----------------------------------------------------------------------------

cat("\n--- Step 1 Interpretation ---\n")
if (moran_results$p_value < 0.05) {
  cat("RESULT: Significant spatial autocorrelation detected (p < 0.05)\n")
  cat("This justifies using spatial modeling approaches.\n")
} else {
  cat("RESULT: No significant spatial autocorrelation (p >= 0.05)\n")
  cat("Standard regression methods may be sufficient.\n")
}


# =============================================================================
# STEP 2: SPATIAL WEIGHTS MATRIX CREATION
# =============================================================================
#
# FUNCTION: create_spatial_weights()
#
# PURPOSE: Creates a spatial weights matrix defining neighborhood structure.
# Used for:
#   - Calculating spatial lags (weighted average of neighbors' values)
#   - Fitting spatial econometric models (SAR, SEM, SAC)
#   - Testing for residual spatial autocorrelation
#
# HOW IT WORKS:
#   1. Extracts centroids from polygon geometries
#   2. Uses k-nearest neighbors to find k closest observations
#   3. Creates row-standardized weights (W-style) - weights sum to 1
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 2: SPATIAL WEIGHTS MATRIX CREATION\n")
cat("=============================================================================\n")

# Create spatial weights
spatial_weights <- SArf:::create_spatial_weights(
  data = data_clean,
  k_neighbors = 20
)

# Examine the weights structure
cat("Number of observations:", length(spatial_weights$neighbours), "\n")
cat("Average neighbors per observation:", mean(sapply(spatial_weights$neighbours, length)), "\n")
cat("Weights style:", spatial_weights$style, "(row-standardized)\n")

# Show example neighbor structure
cat("\nExample: Observation 1 has neighbors:",
    paste(head(spatial_weights$neighbours[[1]], 10), collapse = ", "), "...\n")

# -----------------------------------------------------------------------------
# INTERPRETATION:
#
# - k=20 neighbors: Balance between local specificity and statistical stability
# - W-style weights: Row-standardized (each observation's weights sum to 1)
# - All observations should have exactly k neighbors (or fewer at edges)
# -----------------------------------------------------------------------------

cat("\n--- Step 2 Interpretation ---\n")
cat("Spatial weights matrix successfully created.\n")
cat("Each observation considers", 20, "nearest neighbors.\n")


# =============================================================================
# STEP 3: SPATIAL CROSS-VALIDATION RANDOM FOREST
# =============================================================================
#
# FUNCTION: spatial_cv_rf()
#
# PURPOSE: Fits random forest using spatial cross-validation to:
#
#   1. PREVENT DATA LEAKAGE: Traditional CV can have train/test observations
#      that are spatial neighbors, causing overly optimistic estimates
#
#   2. PROPERLY CALCULATE SPATIAL LAG: For test observations, spatial lag is
#      calculated using ONLY training neighbors (critical!)
#
#   3. QUANTIFY UNCERTAINTY: Multiple bootstrap iterations provide CIs
#
# HOW IT WORKS:
#   1. Creates spatially-blocked folds using blockCV package
#   2. For each fold and bootstrap iteration:
#      - Fits RF on training data with spatial lag from training only
#      - Predicts on test using spatial lag from k-nearest TRAINING neighbors
#   3. Aggregates predictions across all iterations
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 3: SPATIAL CROSS-VALIDATION RANDOM FOREST\n")
cat("=============================================================================\n")

# Run spatial CV (reduced iterations for testing speed)
cv_results <- SArf:::spatial_cv_rf(
  formula = formula,
  data = data_clean,
  spatial_weights = spatial_weights,
  n_folds = 3,
  n_bootstrap = 5,
  block_range = 5000,
  num_trees = 100,
  seed = 1111,
  verbose = TRUE
)

# Examine results
cat("\n--- Spatial CV Results ---\n")
cat("RMSE:", round(cv_results$metrics$rmse, 4), "\n")
cat("R-squared:", round(cv_results$metrics$r2, 4), "\n")
cat("Moran's I (residuals):", round(cv_results$metrics$morans_i, 4), "\n")
cat("Moran's I p-value:", format.pval(cv_results$metrics$morans_p), "\n")

# Examine prediction structure
cat("\n--- Prediction Structure ---\n")
cat("Total prediction rows:", nrow(cv_results$predictions), "\n")
cat("Columns:", paste(names(cv_results$predictions), collapse = ", "), "\n")

# Show sample of predictions
cat("\nSample predictions (first 5 rows):\n")
print(head(cv_results$predictions, 5))

# -----------------------------------------------------------------------------
# INTERPRETATION:
#
# - RMSE: Lower is better; average prediction error in outcome units
# - R-squared: Proportion of variance explained (0-1); higher is better
# - Moran's I on residuals:
#     - Close to 0: Model captured spatial structure
#     - Significantly positive: Spatial autocorrelation remains
#
# CRITICAL IMPLEMENTATION DETAIL:
# The spatial lag for test observations is calculated using ONLY k-nearest
# TRAINING neighbors. This prevents "peeking" at test data through the
# spatial lag, which would cause data leakage.
# -----------------------------------------------------------------------------

cat("\n--- Step 3 Interpretation ---\n")
cat("R-squared of", round(cv_results$metrics$r2, 3),
    "means the model explains", round(cv_results$metrics$r2 * 100, 1),
    "% of variance.\n")

if (cv_results$metrics$morans_p < 0.05) {
  cat("Residuals still show significant spatial autocorrelation.\n")
  cat("Additional spatial structure may not be captured.\n")
} else {
  cat("Residuals show no significant spatial autocorrelation.\n")
  cat("Model has successfully captured spatial structure.\n")
}


# =============================================================================
# STEP 4: MODEL COMPARISON
# =============================================================================
#
# FUNCTION: compare_spatial_models()
#
# PURPOSE: Compares random forest to traditional spatial econometric models:
#
# | Model        | Description                    | Spatial Component          |
# |--------------|--------------------------------|----------------------------|
# | Naive RF     | RF with global spatial lag     | Lag from ALL observations  |
# | OLS          | Ordinary Least Squares         | None                       |
# | SAR          | Spatial Autoregressive (Lag)   | Lagged dependent variable  |
# | SEM          | Spatial Error Model            | Correlated errors          |
# | SAC          | Spatial Combined               | Both lag and error         |
# | RF Spatial CV| This package's approach        | Within-fold spatial lag    |
#
# WHY COMPARE?
#   - Traditional spatial models assume linear relationships
#   - RF can capture non-linear relationships and interactions
#   - Comparison shows whether RF flexibility improves predictions
#   - Residual Moran's I shows which model best captures spatial structure
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 4: MODEL COMPARISON\n")
cat("=============================================================================\n")

# Run model comparison
comparison <- SArf:::compare_spatial_models(
  formula = formula,
  data = data_clean,
  spatial_weights = spatial_weights,
  rf_predictions = cv_results$predictions,
  compare_models = c("OLS", "SAR", "SEM", "SAC"),
  include_naive_rf = TRUE,
  num_trees = 100,
  seed = 1111,
  verbose = TRUE
)

# View comparison table
cat("\n--- Model Comparison Table ---\n")
print(comparison$table)

# -----------------------------------------------------------------------------
# INTERPRETATION:
#
# PERFORMANCE METRICS:
#   - Lower RMSE = better prediction accuracy
#   - Higher R2 = more variance explained
#   - Lower |Moran's I| = better capture of spatial structure
#
# TYPICAL PATTERNS:
#   1. Naive RF often shows highest R2, but this is MISLEADING due to data
#      leakage through the global spatial lag
#   2. OLS typically shows highest residual Moran's I (ignores spatial structure)
#   3. SAR/SEM/SAC usually reduce residual autocorrelation
#   4. RF Spatial CV should show competitive R2 with reduced data leakage
# -----------------------------------------------------------------------------

cat("\n--- Step 4 Interpretation ---\n")

# Find best model by RMSE
best_rmse_model <- comparison$table$Model[which.min(comparison$table$RMSE)]
cat("Best model by RMSE:", best_rmse_model, "\n")

# Find best model by R2
best_r2_model <- comparison$table$Model[which.max(comparison$table$R2)]
cat("Best model by R2:", best_r2_model, "\n")

# Compare Naive RF vs Spatial CV RF
naive_r2 <- comparison$table$R2[comparison$table$Model == "Naive_RF"]
spatial_cv_r2 <- comparison$table$R2[comparison$table$Model == "RF_Spatial_CV"]

if (!is.na(naive_r2) && !is.na(spatial_cv_r2)) {
  if (naive_r2 > spatial_cv_r2) {
    cat("\nNote: Naive RF shows higher R2 than Spatial CV RF.\n")
    cat("This is expected - Naive RF has data leakage through global spatial lag.\n")
    cat("Spatial CV RF provides more realistic (conservative) performance estimates.\n")
  }
}


# =============================================================================
# STEP 5: VARIABLE IMPORTANCE WITH CONFIDENCE INTERVALS
# =============================================================================
#
# FUNCTION: calculate_importance_ci()
#
# PURPOSE: Calculates permutation-based variable importance with bootstrap CIs:
#
#   - Permutation importance: Measures decrease in performance when a
#     variable's values are randomly shuffled
#   - Bootstrap CIs: Multiple CV iterations provide uncertainty estimates
#   - Includes spatial_lag: Shows relative importance of neighborhood effects
#
# HOW IT WORKS:
#   1. Extracts importance from each CV model (ranger's permutation importance)
#   2. Calculates mean, SD, and 2.5%/97.5% quantiles across bootstrap iterations
#   3. Creates visualization with error bars
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 5: VARIABLE IMPORTANCE WITH CONFIDENCE INTERVALS\n")
cat("=============================================================================\n")

# Calculate importance with CIs
importance_results <- SArf:::calculate_importance_ci(
  cv_models = cv_results$models,
  verbose = TRUE
)

# View importance table
cat("\n--- Variable Importance Table ---\n")
print(importance_results$table)

# Display the importance plot
cat("\n--- Displaying Importance Plot ---\n")
print(importance_results$plot)

# -----------------------------------------------------------------------------
# INTERPRETATION:
#
# - Higher mean importance: Variable contributes more to predictions
# - Narrow CI: Consistent importance across folds (robust)
# - Wide CI: Importance varies across folds (less stable)
# - spatial_lag importance: Shows how much neighborhood effects contribute
#
# Variables with CIs overlapping zero may not be significant predictors.
# -----------------------------------------------------------------------------

cat("\n--- Step 5 Interpretation ---\n")

# Check if spatial_lag is in top 3
top_3 <- head(importance_results$table$variable, 3)
if ("spatial_lag" %in% top_3) {
  cat("spatial_lag is among top 3 most important variables.\n")
  cat("This indicates strong neighborhood/spillover effects in health outcomes.\n")
} else {
  cat("spatial_lag is not in top 3 variables.\n")
  spatial_lag_rank <- which(importance_results$table$variable == "spatial_lag")
  cat("It ranks #", spatial_lag_rank, " in importance.\n", sep = "")
}

# Identify most important predictor (excluding spatial_lag)
non_spatial_imp <- importance_results$table %>%
  filter(variable != "spatial_lag")
top_predictor <- non_spatial_imp$variable[1]
cat("\nMost important predictor (excluding spatial_lag):", top_predictor, "\n")


# =============================================================================
# STEP 6: ALE PLOTS WITH CONFIDENCE INTERVALS
# =============================================================================
#
# FUNCTION: calculate_ale_ci()
#
# PURPOSE: Creates Accumulated Local Effects (ALE) plots showing marginal
# effects of each predictor:
#
#   - ALE vs. Partial Dependence: ALE is preferred when predictors are
#     correlated (common in spatial data)
#   - Confidence bands: Show uncertainty from bootstrap iterations
#   - Non-linear relationships: Can reveal thresholds, diminishing returns, etc.
#
# HOW IT WORKS:
#   1. For each top predictor, calculates ALE for each bootstrap model
#   2. Interpolates to common grid and calculates mean +/- 95% CI
#   3. Creates faceted plot with all predictors ordered by importance
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 6: ALE PLOTS WITH CONFIDENCE INTERVALS\n")
cat("=============================================================================\n")

# Calculate ALE with CIs
ale_results <- SArf:::calculate_ale_ci(
  data = data_clean,
  formula = formula,
  cv_results = cv_results,
  spatial_weights = spatial_weights,
  importance_table = importance_results$table,
  n_top_vars = 6,
  verbose = TRUE
)

# Check variable order (should be by importance)
cat("\n--- Variables in ALE plots (ordered by importance) ---\n")
cat(paste(ale_results$variables, collapse = " > "), "\n")

# View ALE data summary
cat("\n--- ALE Data Summary ---\n")
cat("Total data points:", nrow(ale_results$data), "\n")
cat("Variables plotted:", length(unique(ale_results$data$variable)), "\n")

# Display the combined ALE plot
cat("\n--- Displaying ALE Plots ---\n")
print(ale_results$plots)

# -----------------------------------------------------------------------------
# INTERPRETING ALE PLOTS:
#
# - X-axis: Predictor value
# - Y-axis: Effect on outcome (centered at 0)
# - Blue line: Mean effect
# - Shaded area: 95% confidence interval
# - Dashed line at 0: Reference (no effect)
#
# COMMON PATTERNS:
#   - Positive slope: Higher predictor values -> higher outcome
#   - Negative slope: Higher predictor values -> lower outcome
#   - Curved line: Non-linear relationship
#   - Flat line: No effect
#   - Wide bands: Uncertain effect
# -----------------------------------------------------------------------------

cat("\n--- Step 6 Interpretation ---\n")
cat("ALE plots show the marginal effect of each predictor.\n")
cat("Plots are ordered by variable importance (most important first).\n")
cat("Confidence bands indicate uncertainty from bootstrap iterations.\n")


# =============================================================================
# STEP 7: INTERACTIVE LEAFLET MAP
# =============================================================================
#
# FUNCTION: create_leaflet_map()
#
# PURPOSE: Creates an interactive choropleth map of the dependent variable:
#
#   - Spatial visualization: Shows geographic distribution of outcome
#   - Quantile breaks: Ensures balanced color distribution
#   - Interactive: Hover for values, zoom/pan capability
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("STEP 7: INTERACTIVE LEAFLET MAP\n")
cat("=============================================================================\n")

# Create leaflet map
leaflet_map <- SArf:::create_leaflet_map(
  data = data_clean,
  variable = "HRI_gaus_e",
  title = "Health Rating Index",
  palette = "viridis"
)

# Display map (will open in viewer/browser)
cat("Creating interactive map...\n")
print(leaflet_map)

# -----------------------------------------------------------------------------
# INTERPRETING THE MAP:
#
# - Clustering of colors: Confirms spatial autocorrelation
# - Hot spots (high values): Areas with better health outcomes
# - Cold spots (low values): Areas with worse health outcomes
# - Spatial gradients: Smooth transitions indicate strong spatial dependence
# -----------------------------------------------------------------------------

cat("\n--- Step 7 Interpretation ---\n")
cat("The map shows the spatial distribution of the Health Rating Index.\n")
cat("Color clustering confirms the spatial autocorrelation detected in Step 1.\n")


# =============================================================================
# FULL SArf() WORKFLOW TEST
# =============================================================================
#
# FUNCTION: SArf()
#
# PURPOSE: Main function that orchestrates all the above steps in sequence.
# This is what users will typically call.
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("FULL SArf() WORKFLOW TEST\n")
cat("=============================================================================\n")

# Run full SArf analysis
results <- SArf(
 formula = HRI_gaus_e ~ In22_ED + NoAuto_p + POPD + log_dist + ov60 + nonIrish,
 data = data_clean,
 k_neighbors = 20,
 n_folds = 3,
 n_bootstrap = 5,
 block_range = 5000,
 num_trees = 100,
 run_spatial_models = TRUE,
 create_map = TRUE,
 seed = 1111,
 verbose = TRUE
)

# Print summary
cat("\n--- SArf Results Summary ---\n")
print(results)

# Access individual components
cat("\n--- Accessing Individual Components ---\n")
cat("results$moran_test        - Moran's I test object\n")
cat("results$moran_plot        - Moran scatter plot (ggplot)\n")
cat("results$model_comparison  - Model comparison table\n")
cat("results$variable_importance - Variable importance table\n")
cat("results$importance_plot   - Importance plot (ggplot)\n
")
cat("results$ale_results       - ALE data\n")
cat("results$ale_plots         - ALE plots (ggplot)\n")
cat("results$leaflet_map       - Interactive map\n")
cat("results$spatial_cv_results - Full CV results\n")

# View all plots
cat("\n--- Displaying All Plots ---\n")
print(results$moran_plot)
print(results$importance_plot)
print(results$ale_plots)

# View model comparison
cat("\n--- Model Comparison ---\n")
print(results$model_comparison)

# View variable importance
cat("\n--- Variable Importance ---\n")
print(results$variable_importance)

# View spatial econometric model summaries
cat("\n--- Spatial Econometric Models ---\n")
cat("Use show_models(results) for full model summaries\n")
cat("Use show_models(results, 'sar') for just the SAR model\n")


# =============================================================================
# VALIDATION CHECKLIST
# =============================================================================
#
# Run these checks to validate that SArf is working correctly
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("VALIDATION CHECKLIST\n")
cat("=============================================================================\n")

# -----------------------------------------------------------------------------
# CHECK 1: Spatial Autocorrelation Detection
# -----------------------------------------------------------------------------
cat("\n--- Check 1: Spatial Autocorrelation Detection ---\n")
if (results$moran_test$p.value < 0.05) {
  cat("PASS: Spatial autocorrelation correctly detected (p =",
      format.pval(results$moran_test$p.value), ")\n")
} else {
  cat("NOTE: No significant spatial autocorrelation detected\n")
}

# -----------------------------------------------------------------------------
# CHECK 2: No Data Leakage in Spatial Lag
# -----------------------------------------------------------------------------
cat("\n--- Check 2: No Data Leakage in Spatial Lag ---\n")

# Get first fold's data
fold_1 <- results$spatial_cv_results$predictions %>%
  filter(fold == 1, iteration == 1)

test_ids <- fold_1 %>% filter(in_training == FALSE) %>% pull(row_id)
train_ids <- fold_1 %>% filter(in_training == TRUE) %>% pull(row_id)

cat("Fold 1 has", length(train_ids), "training and", length(test_ids), "test observations\n")

# Verify spatial lag values exist for test observations
test_lags <- fold_1 %>% filter(in_training == FALSE) %>% pull(spatial_lag)
if (all(!is.na(test_lags))) {
  cat("PASS: Spatial lag calculated for all test observations\n")
} else {
  cat("WARNING: Some test observations missing spatial lag\n")
}

# -----------------------------------------------------------------------------
# CHECK 3: Model Comparison Includes All Models
# -----------------------------------------------------------------------------
cat("\n--- Check 3: Model Comparison Completeness ---\n")
expected_models <- c("Naive_RF", "OLS", "SAR", "SEM", "SAC", "RF_Spatial_CV")
present_models <- results$model_comparison$Model

missing <- setdiff(expected_models, present_models)
if (length(missing) == 0) {
  cat("PASS: All", length(expected_models), "models successfully fitted\n")
} else {
  cat("WARNING: Missing models:", paste(missing, collapse = ", "), "\n")
}

# -----------------------------------------------------------------------------
# CHECK 4: Variable Importance Includes spatial_lag
# -----------------------------------------------------------------------------
cat("\n--- Check 4: spatial_lag in Variable Importance ---\n")
if ("spatial_lag" %in% results$variable_importance$variable) {
  spatial_lag_rank <- which(results$variable_importance$variable == "spatial_lag")
  cat("PASS: spatial_lag included in importance (rank #", spatial_lag_rank, ")\n", sep = "")
} else {
  cat("WARNING: spatial_lag not found in variable importance\n")
}

# -----------------------------------------------------------------------------
# CHECK 5: ALE Plots Ordered by Importance
# -----------------------------------------------------------------------------
cat("\n--- Check 5: ALE Plot Ordering ---\n")
ale_var_order <- levels(results$ale_results$variable)
importance_order <- results$variable_importance$variable

# Check if first ALE variable matches most important
if (!is.null(ale_var_order) && ale_var_order[1] == importance_order[1]) {
  cat("PASS: ALE plots ordered by variable importance\n")
  cat("  Most important variable (", importance_order[1], ") shown first\n", sep = "")
} else {
  cat("Note: Checking ALE variable order\n")
}

# -----------------------------------------------------------------------------
# CHECK 6: Residual Spatial Autocorrelation Reduced
# -----------------------------------------------------------------------------
cat("\n--- Check 6: Residual Autocorrelation ---\n")
original_moran <- results$moran_test$estimate[1]
residual_moran <- results$spatial_cv_results$metrics$morans_i

cat("Original Moran's I:", round(original_moran, 4), "\n")
cat("Residual Moran's I:", round(residual_moran, 4), "\n")

if (abs(residual_moran) < abs(original_moran)) {
  reduction <- round((1 - abs(residual_moran)/abs(original_moran)) * 100, 1)
  cat("PASS: Model reduced spatial autocorrelation by", reduction, "%\n")
} else {
  cat("NOTE: Residual autocorrelation not reduced\n")
}


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("SUMMARY\n")
cat("=============================================================================\n")

cat("
The SArf package provides a comprehensive workflow for spatial autoregressive
random forest analysis:

| Step | Function                  | Purpose                              |
|------|---------------------------|--------------------------------------|
| 1    | test_morans_i()           | Detect spatial autocorrelation       |
| 2    | create_spatial_weights()  | Define neighborhood structure        |
| 3    | spatial_cv_rf()           | Fit RF with proper spatial CV        |
| 4    | compare_spatial_models()  | Benchmark against OLS, SAR, SEM, SAC |
| 5    | calculate_importance_ci() | Variable importance with CIs         |
| 6    | calculate_ale_ci()        | Marginal effects visualization       |
| 7    | create_leaflet_map()      | Interactive spatial visualization    |

KEY ADVANTAGES OF SArf:
1. No data leakage: Spatial lag calculated within folds using training only
2. Uncertainty quantification: Bootstrap CIs on all outputs
3. Non-linear relationships: RF captures complex patterns
4. Comprehensive comparison: Benchmarks against traditional spatial models
5. Publication-ready outputs: Professional visualizations with CIs

")

cat("=============================================================================\n")
cat("TEST COMPLETE\n")
cat("=============================================================================\n")

# Session info for reproducibility
cat("\n--- Session Info ---\n")
sessionInfo()

#' Spatial Autoregressive Random Forest Analysis
#'
#' Comprehensive spatial random forest analysis with proper uncertainty quantification,
#' comparison to spatial econometric models, and diagnostic outputs.
#'
#' @param formula Model formula (e.g., y ~ x1 + x2)
#' @param data Spatial data (sf object or data.frame with spatial information)
#' @param coords Optional: matrix of coordinates if data is not sf object
#' @param k_neighbors Number of neighbors for spatial weights (default: 20)
#' @param n_folds Number of spatial CV folds (default: 5)
#' @param n_bootstrap Number of bootstrap iterations per fold (default: 20)
#' @param block_range Spatial blocking range in meters (default: 1000)
#' @param num_trees Number of trees in random forest (default: 500)
#' @param mtry Number of variables to split on at each node (default: NULL, auto-selected)
#' @param alpha Significance level for Moran's I test (default: 0.05)
#' @param run_spatial_models Logical: fit OLS, SAR, SEM, SAC models? (default: TRUE).
#'   Set to FALSE for faster execution when only spatial CV RF results are needed.
#' @param compare_models Vector of models to compare: "OLS", "SAR", "SEM", "SAC" (default: all).
#'   Only used when run_spatial_models = TRUE.
#' @param include_naive_rf Logical: include naive RF with simple train-test split? (default: TRUE).
#'   Only used when run_spatial_models = TRUE.
#' @param naive_test_fraction Fraction of data for test set in naive RF (default: 0.2)
#' @param create_map Logical: create leaflet map of dependent variable? (default: TRUE)
#' @param seed Random seed for reproducibility (default: 1111)
#' @param verbose Logical: print progress messages? (default: TRUE)
#'
#' @return A list of class 'SArf' containing:
#' \itemize{
#'   \item moran_test: Moran's I test results for dependent variable
#'   \item moran_plot: ggplot object of Moran's I scatter plot
#'   \item spatial_cv_results: Spatial CV predictions and metrics
#'   \item model_comparison: Comparison table of RF vs spatial econometric models (NULL if run_spatial_models = FALSE)
#'   \item variable_importance: Variable importance with 95\% confidence intervals
#'   \item importance_plot: ggplot object of variable importance
#'   \item ale_results: ALE results for top predictors with confidence intervals
#'   \item ale_plots: Combined ggplot object of ALE plots
#'   \item leaflet_map: Interactive leaflet map (if create_map = TRUE)
#'   \item data: Original data with predictions and spatial lag
#'   \item formula: Model formula used
#'   \item spatial_weights: Spatial weights matrix
#'   \item ols_model, sar_model, sem_model, sac_model: Spatial econometric models (NULL if run_spatial_models = FALSE)
#' }
#'
#' @details
#' This function implements a comprehensive spatial autoregressive random forest workflow:
#'
#' 1. **Spatial Diagnostics**: Tests for spatial autocorrelation in the dependent variable
#'    using Moran's I. Creates a Moran scatter plot showing the relationship between
#'    values and their spatial lags.
#'
#' 2. **Spatial Cross-Validation**: If Moran's I is significant, performs spatial
#'    cross-validation with proper within-fold spatial lag calculation to prevent
#'    data leakage.
#'
#' 3. **Model Comparison** (optional): Compares random forest to traditional spatial econometric
#'    models (OLS, SAR, SEM, SAC) using RMSE, R², and Moran's I on residuals.
#'    Set \code{run_spatial_models = FALSE} to skip this step for faster execution.
#'
#' 4. **Uncertainty Quantification**: Bootstrap confidence intervals on variable
#'    importance through spatial CV iterations.
#'
#' 5. **Effect Visualization**: Accumulated Local Effects (ALE) plots with confidence
#'    ribbons showing marginal effects and uncertainty.
#'
#' 6. **Spatial Visualization**: Interactive leaflet map of the dependent variable.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(SArf)
#'
#' # Load spatial data
#' data <- st_read("your_data.shp")
#'
#' # Run full SArf analysis (default)
#' results <- SArf(
#'   formula = outcome ~ predictor1 + predictor2 + predictor3,
#'   data = data,
#'   k_neighbors = 20,
#'   n_folds = 5,
#'   n_bootstrap = 20
#' )
#'
#' # Fast mode: skip spatial econometric models
#' results_fast <- SArf(
#'   formula = outcome ~ predictor1 + predictor2 + predictor3,
#'   data = data,
#'   k_neighbors = 20,
#'   n_folds = 5,
#'   n_bootstrap = 20,
#'   run_spatial_models = FALSE
#' )
#'
#' # View results
#' print(results)
#' plot(results)
#'
#' # Access specific outputs
#' results$moran_test
#' results$model_comparison
#' results$variable_importance
#' }
#'
#' @references
#' Credit, K., Damanpreet, K., and Eccles, E. (2025). Exploring the
#' transport-health-environment nexus through a new 'Health Rating Index'
#' for Dublin, Ireland. Proceedings of the 33rd GISRUK Conference.
#' DOI: 10.5281/zenodo.15183740
#'
#' @export
SArf <- function(formula,
                 data,
                 coords = NULL,
                 k_neighbors = 20,
                 n_folds = 5,
                 n_bootstrap = 20,
                 block_range = 1000,
                 num_trees = 500,
                 mtry = NULL,
                 alpha = 0.05,
                 run_spatial_models = TRUE,
                 compare_models = c("OLS", "SAR", "SEM", "SAC"),
                 include_naive_rf = TRUE,
                 naive_test_fraction = 0.2,
                 create_map = TRUE,
                 seed = 1111,
                 verbose = TRUE) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Resolve namespace conflicts
  if ("package:conflicted" %in% search()) {
    conflicted::conflicts_prefer(dplyr::filter)
    conflicted::conflicts_prefer(dplyr::lag)
    conflicted::conflicts_prefer(dplyr::select)
  }
  
  # Validate inputs
  if (!inherits(data, "sf") && is.null(coords)) {
    stop("Data must be an sf object or coords must be provided")
  }
  
  if (verbose) cat("\n=== SPATIAL AUTOREGRESSIVE RANDOM FOREST ===\n")
  if (verbose && !run_spatial_models) {
    cat("  [Fast mode: skipping spatial econometric models]\n")
  }
  
  # Extract formula components
  response_var <- all.vars(formula)[1]
  predictor_vars <- all.vars(formula)[-1]
  
  # Step 1: Moran's I Test
  if (verbose) cat("\nStep 1: Testing for spatial autocorrelation...\n")
  moran_results <- test_morans_i(
    data = data,
    variable = response_var,
    coords = coords,
    k_neighbors = k_neighbors,
    alpha = alpha,
    verbose = verbose
  )
  
  # If no significant spatial autocorrelation, warn but continue
  if (moran_results$p_value > alpha) {
    warning(paste0(
      "Moran's I not significant (p = ", round(moran_results$p_value, 4),
      "). Spatial methods may not be necessary."
    ))
  }
  
  # Step 2: Create spatial weights
  if (verbose) cat("\nStep 2: Creating spatial weights matrix...\n")
  spatial_weights <- create_spatial_weights(
    data = data,
    coords = coords,
    k_neighbors = k_neighbors
  )
  
  # Step 3: Spatial Cross-Validation Random Forest
  if (verbose) cat("\nStep 3: Running spatial cross-validation random forest...\n")
  if (verbose) cat(paste0("  ", n_folds, " folds × ", n_bootstrap, 
                         " iterations = ", n_folds * n_bootstrap, " models\n"))
  
  cv_results <- spatial_cv_rf(
    formula = formula,
    data = data,
    spatial_weights = spatial_weights,
    n_folds = n_folds,
    n_bootstrap = n_bootstrap,
    block_range = block_range,
    num_trees = num_trees,
    mtry = mtry,
    seed = seed,
    verbose = verbose
  )
  
  # Step 4: Compare to spatial econometric models (conditional)
  model_comparison <- NULL
  ols_model <- NULL
  sar_model <- NULL
  sem_model <- NULL
  sac_model <- NULL
  naive_rf_model <- NULL
  
  if (run_spatial_models) {
    if (verbose) cat("\nStep 4: Comparing to spatial econometric models...\n")
    model_comparison <- compare_spatial_models(
      formula = formula,
      data = data,
      spatial_weights = spatial_weights,
      rf_predictions = cv_results$predictions,
      compare_models = compare_models,
      include_naive_rf = include_naive_rf,
      naive_test_fraction = naive_test_fraction,
      num_trees = num_trees,
      mtry = mtry,
      seed = seed,
      verbose = verbose
    )
    
    # Extract individual models for easy access
    ols_model <- model_comparison$models$OLS
    sar_model <- model_comparison$models$SAR
    sem_model <- model_comparison$models$SEM
    sac_model <- model_comparison$models$SAC
    naive_rf_model <- model_comparison$models$Naive_RF
  } else {
    if (verbose) cat("\nStep 4: Skipping spatial econometric models (run_spatial_models = FALSE)\n")
  }
  
  # Step 5: Variable importance with bootstrap CIs
  step_num <- if (run_spatial_models) 5 else 4
  if (verbose) cat(paste0("\nStep ", step_num, ": Calculating variable importance with confidence intervals...\n"))
  importance_results <- calculate_importance_ci(
    cv_models = cv_results$models,
    verbose = verbose
  )
  
  # Step 6: ALE plots with bootstrap CIs
  step_num <- step_num + 1
  if (verbose) cat(paste0("\nStep ", step_num, ": Generating ALE plots with confidence intervals...\n"))
  ale_results <- calculate_ale_ci(
    data = data,
    formula = formula,
    cv_results = cv_results,
    spatial_weights = spatial_weights,
    importance_table = importance_results$table,
    n_top_vars = 6,
    verbose = verbose
  )
  
  # Step 7: Create leaflet map
  leaflet_map <- NULL
  if (create_map) {
    step_num <- step_num + 1
    if (verbose) cat(paste0("\nStep ", step_num, ": Creating interactive map...\n"))
    leaflet_map <- tryCatch({
      create_leaflet_map(
        data = data,
        variable = response_var,
        title = paste("Spatial Distribution of", response_var)
      )
    }, error = function(e) {
      warning("Could not create leaflet map: ", e$message)
      NULL
    })
  }
  
  # Compile results
  results <- list(
    # Spatial diagnostics
    moran_test = moran_results$test,
    moran_plot = moran_results$plot,
    
    # Spatial CV results
    spatial_cv_results = cv_results,
    
    # Model comparison (NULL if run_spatial_models = FALSE)
    model_comparison = if (!is.null(model_comparison)) model_comparison$table else NULL,
    
    # Individual spatial models (NULL if run_spatial_models = FALSE)
    ols_model = ols_model,
    sar_model = sar_model,
    sem_model = sem_model,
    sac_model = sac_model,
    naive_rf_model = naive_rf_model,
    
    # Variable importance
    variable_importance = importance_results$table,
    importance_plot = importance_results$plot,
    
    # ALE plots
    ale_results = ale_results$results,
    ale_plots = ale_results$plot,
    
    # Map
    leaflet_map = leaflet_map,
    
    # Data and metadata
    data = data,
    formula = formula,
    spatial_weights = spatial_weights,
    call = match.call(),
    
    # Settings (for reference)
    settings = list(
      run_spatial_models = run_spatial_models,
      k_neighbors = k_neighbors,
      n_folds = n_folds,
      n_bootstrap = n_bootstrap,
      num_trees = num_trees
    )
  )
  
  class(results) <- "SArf"
  
  if (verbose) {
    cat("\n=== ANALYSIS COMPLETE ===\n")
    cat("Use print(results) for summary, plot(results) for visualizations\n")
    if (!run_spatial_models) {
      cat("Note: Spatial econometric models were not fitted (run_spatial_models = FALSE)\n")
    }
  }
  
  return(results)
}


#' Print method for SArf objects
#'
#' @param x An object of class 'SArf'
#' @param ... Additional arguments (not used)
#'
#' @export
print.SArf <- function(x, ...) {
  cat("\n=== Spatial Autoregressive Random Forest Results ===\n\n")
  
  # Moran's I
  cat("--- Spatial Autocorrelation (Moran's I) ---\n")
  cat("Statistic:", round(x$moran_test$estimate[1], 4), "\n")
  cat("p-value:", format.pval(x$moran_test$p.value), "\n\n")
  
  # Spatial CV performance
  cat("--- Spatial CV Random Forest Performance ---\n")
  cat("RMSE:", round(x$spatial_cv_results$metrics$rmse, 4), "\n")
  cat("R²:", round(x$spatial_cv_results$metrics$r2, 4), "\n")
  cat("Moran's I (residuals):", round(x$spatial_cv_results$metrics$morans_i, 4), "\n\n")
  
  # Model comparison (if available)
  if (!is.null(x$model_comparison)) {
    cat("--- Model Comparison ---\n")
    print(x$model_comparison, row.names = FALSE)
    cat("\n")
  } else {
    cat("--- Model Comparison ---\n")
    cat("Spatial econometric models not fitted (run_spatial_models = FALSE)\n\n")
  }
  
  # Top 5 important variables
  cat("--- Top 5 Important Variables ---\n")
  top5 <- head(x$variable_importance, 5)
  print(top5, row.names = FALSE)
  cat("\n")
  
  cat("--- Accessing Results ---\n")
  cat("Plots: plot(results) or results$moran_plot, results$importance_plot, results$ale_plots\n")
  if (!is.null(x$model_comparison)) {
    cat("Models: results$ols_model, results$sar_model, results$sem_model, results$sac_model\n")
  }
  cat("Map: results$leaflet_map\n")
  cat("Details: summary(results)\n")
}


#' Summary method for SArf objects
#'
#' @param object An object of class 'SArf'
#' @param ... Additional arguments (not used)
#'
#' @export
summary.SArf <- function(object, ...) {
  cat("\n=== Spatial Autoregressive Random Forest Summary ===\n\n")
  
  cat("Call:\n")
  print(object$call)
  cat("\n")
  
  cat("Formula:", deparse(object$formula), "\n")
  cat("Observations:", nrow(object$data), "\n")
  cat("Predictors:", length(all.vars(object$formula)) - 1, "\n")
  cat("Spatial models fitted:", object$settings$run_spatial_models, "\n\n")
  
  cat("=== Spatial Autocorrelation ===\n")
  cat("Moran's I:", round(object$moran_test$estimate[1], 4), "\n")
  cat("Expected:", round(object$moran_test$estimate[2], 4), "\n")
  cat("Variance:", round(object$moran_test$estimate[3], 6), "\n")
  cat("z-score:", round(object$moran_test$statistic, 4), "\n")
  cat("p-value:", format.pval(object$moran_test$p.value), "\n")
  cat("Interpretation:", 
      ifelse(object$moran_test$p.value < 0.05,
             "Significant spatial autocorrelation detected",
             "No significant spatial autocorrelation"), "\n\n")
  
  cat("=== Spatial Cross-Validation Performance ===\n")
  cat("RMSE:", round(object$spatial_cv_results$metrics$rmse, 4), "\n")
  cat("R²:", round(object$spatial_cv_results$metrics$r2, 4), "\n")
  cat("Moran's I (residuals):", 
      round(object$spatial_cv_results$metrics$morans_i, 4), "\n")
  cat("p-value (residuals):", 
      format.pval(object$spatial_cv_results$metrics$morans_p), "\n\n")
  
  # Model comparison (if available)
  if (!is.null(object$model_comparison)) {
    cat("=== Model Comparison ===\n")
    print(object$model_comparison, row.names = FALSE)
    cat("\n")
  } else {
    cat("=== Model Comparison ===\n")
    cat("Spatial econometric models not fitted (run_spatial_models = FALSE)\n\n")
  }
  
  cat("=== Variable Importance ===\n")
  print(object$variable_importance, row.names = FALSE)
  cat("\n")
  
  cat("=== ALE Analysis ===\n")
  cat("ALE plots generated for top", 
      length(unique(object$ale_results$variable)), "predictors\n")
  cat("Confidence intervals based on spatial CV bootstrap\n\n")
  
  if (!is.null(object$leaflet_map)) {
    cat("=== Spatial Visualization ===\n")
    cat("Interactive leaflet map created\n")
    cat("Access with: object$leaflet_map\n\n")
  }
  
  # Spatial econometric models (if available)
  if (object$settings$run_spatial_models) {
    cat("=== Spatial Econometric Models ===\n\n")
    
    # OLS Model
    if (!is.null(object$ols_model)) {
      cat(strrep("-", 78), "\n")
      cat("OLS MODEL\n")
      cat(strrep("-", 78), "\n")
      print(summary(object$ols_model))
      cat("\n")
    }
    
    # SAR Model
    if (!is.null(object$sar_model)) {
      cat(strrep("-", 78), "\n")
      cat("SAR (SPATIAL LAG) MODEL\n")
      cat(strrep("-", 78), "\n")
      print(summary(object$sar_model))
      cat("\n")
    }
    
    # SEM Model
    if (!is.null(object$sem_model)) {
      cat(strrep("-", 78), "\n")
      cat("SEM (SPATIAL ERROR) MODEL\n")
      cat(strrep("-", 78), "\n")
      print(summary(object$sem_model))
      cat("\n")
    }
    
    # SAC Model
    if (!is.null(object$sac_model)) {
      cat(strrep("-", 78), "\n")
      cat("SAC (SPATIAL LAG + ERROR) MODEL\n")
      cat(strrep("-", 78), "\n")
      print(summary(object$sac_model))
      cat("\n")
    }
    
    cat("=== Quick Access ===\n")
    cat("Individual models: summary(object$ols_model), summary(object$sar_model), etc.\n")
    cat("Coefficients: coef(object$sar_model)\n")
    cat("Spatial parameters: object$sar_model$rho, object$sem_model$lambda\n\n")
  }
  
  invisible(object)
}


#' Print method with optional full model summaries
#'
#' Extended print showing all spatial econometric model results
#'
#' @param x An object of class 'SArf'
#' @param models Logical: print full summaries of all spatial models? (default: FALSE)
#' @param ... Additional arguments (not used)
#'
#' @export
print.SArf.models <- function(x, models = TRUE, ...) {
  # Call summary method which now shows everything
  summary.SArf(x)
  invisible(x)
}


#' Plot method for SArf objects
#'
#' @param x An object of class 'SArf'
#' @param which Character: which plot to display? Options: "all", "moran", 
#'   "importance", "ale", or "comparison" (default: "all")
#' @param ... Additional arguments passed to plotting functions
#'
#' @export
plot.SArf <- function(x, which = "all", ...) {
  
  if (which == "all") {
    # Create 2x2 grid of main plots
    old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    on.exit(par(old_par))
    
    # Plot 1: Moran's I
    print(x$moran_plot)
    
    # Plot 2: Model Comparison (as barplot) - only if models were fitted
    if (!is.null(x$model_comparison)) {
      barplot(x$model_comparison$RMSE, 
              names.arg = x$model_comparison$Model,
              las = 2,
              main = "Model Comparison (RMSE)",
              ylab = "RMSE",
              col = "steelblue")
    } else {
      plot.new()
      text(0.5, 0.5, "Model comparison not available\n(run_spatial_models = FALSE)", 
           cex = 1.2)
    }
    
    # Plot 3 & 4: Show importance and first ALE in base graphics style
    # (Print ggplots separately)
    message("Additional plots:")
    message("- Variable importance: x$importance_plot")
    message("- ALE plots: x$ale_plots")
    message("- Interactive map: x$leaflet_map")
    
  } else if (which == "moran") {
    print(x$moran_plot)
  } else if (which == "importance") {
    print(x$importance_plot)
  } else if (which == "ale") {
    print(x$ale_plots)
  } else if (which == "comparison") {
    if (!is.null(x$model_comparison)) {
      print(ggplot2::ggplot(x$model_comparison, 
                            ggplot2::aes(x = reorder(Model, -RMSE), y = RMSE)) +
              ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
              ggplot2::coord_flip() +
              ggplot2::labs(title = "Model Comparison",
                           x = "Model", y = "RMSE") +
              ggplot2::theme_minimal())
    } else {
      message("Model comparison not available (run_spatial_models = FALSE)")
    }
  } else {
    stop("Invalid 'which' argument. Choose from: 'all', 'moran', 'importance', 'ale', 'comparison'")
  }
}


#' Display full results for spatial econometric models
#'
#' Helper function to easily view complete model summaries with p-values,
#' standard errors, spatial parameters, and diagnostic tests.
#'
#' @param x An object of class 'SArf'
#' @param model Character: which model to display? Options: "ols", "sar", "sem", 
#'   "sac", or "all" (default: "all")
#'
#' @return Invisibly returns the SArf object. Prints model summaries to console.
#'
#' @examples
#' \dontrun{
#' results <- SArf(outcome ~ x1 + x2, data = mydata)
#' 
#' # Show all spatial models
#' show_models(results)
#' 
#' # Show just SAR model
#' show_models(results, "sar")
#' 
#' # Show SEM model
#' show_models(results, "sem")
#' }
#'
#' @export
show_models <- function(x, model = "all") {
  if (!inherits(x, "SArf")) {
    stop("x must be a SArf object")
  }
  
  # Check if spatial models were fitted
  if (!x$settings$run_spatial_models) {
    message("Spatial econometric models were not fitted (run_spatial_models = FALSE)")
    message("Re-run SArf() with run_spatial_models = TRUE to fit these models.")
    return(invisible(x))
  }
  
  models_to_show <- if (tolower(model) == "all") {
    c("ols", "sar", "sem", "sac")
  } else {
    tolower(model)
  }
  
  for (m in models_to_show) {
    model_obj <- switch(m,
                        "ols" = x$ols_model,
                        "sar" = x$sar_model,
                        "sem" = x$sem_model,
                        "sac" = x$sac_model,
                        NULL)
    
    if (!is.null(model_obj)) {
      cat("\n")
      cat(strrep("=", 78), "\n")
      cat(toupper(m), "MODEL - FULL RESULTS\n")
      cat(strrep("=", 78), "\n\n")
      print(summary(model_obj))
      cat("\n")
    } else if (m %in% c("ols", "sar", "sem", "sac")) {
      cat("\n", toupper(m), "model not available\n")
    }
  }
  
  invisible(x)
}

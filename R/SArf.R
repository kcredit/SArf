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
#' @param compare_models Vector of models to compare: "OLS", "SAR", "SEM", "SAC" (default: all)
#' @param create_map Logical: create leaflet map of dependent variable? (default: TRUE)
#' @param seed Random seed for reproducibility (default: 1111)
#' @param verbose Logical: print progress messages? (default: TRUE)
#'
#' @return A list of class 'SArf' containing:
#' \item{moran_test}{Moran's I test results for dependent variable}
#' \item{moran_plot}{ggplot object of Moran's I scatter plot}
#' \item{spatial_cv_results}{Spatial CV predictions and metrics}
#' \item{model_comparison}{Comparison table of RF vs spatial econometric models}
#' \item{variable_importance}{Variable importance with 95% confidence intervals}
#' \item{importance_plot}{ggplot object of variable importance}
#' \item{ale_results}{ALE results for top predictors with confidence intervals}
#' \item{ale_plots}{Combined ggplot object of ALE plots}
#' \item{leaflet_map}{Interactive leaflet map (if create_map = TRUE)}
#' \item{data}{Original data with predictions and spatial lag}
#' \item{formula}{Model formula used}
#' \item{spatial_weights}{Spatial weights matrix}
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
#' 3. **Model Comparison**: Compares random forest to traditional spatial econometric
#'    models (OLS, SAR, SEM, SAC) using RMSE, R², and Moran's I on residuals.
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
#' # Run SArf analysis
#' results <- SArf(
#'   formula = outcome ~ predictor1 + predictor2 + predictor3,
#'   data = data,
#'   k_neighbors = 20,
#'   n_folds = 5,
#'   n_bootstrap = 20
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
                 compare_models = c("OLS", "SAR", "SEM", "SAC"),
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
  
  # Step 4: Compare to spatial econometric models
  if (verbose) cat("\nStep 4: Comparing to spatial econometric models...\n")
  model_comparison <- compare_spatial_models(
    formula = formula,
    data = data,
    spatial_weights = spatial_weights,
    rf_predictions = cv_results$predictions,
    compare_models = compare_models,
    verbose = verbose
  )
  
  # Step 5: Variable importance with bootstrap CIs
  if (verbose) cat("\nStep 5: Calculating variable importance with confidence intervals...\n")
  importance_results <- calculate_importance_ci(
    cv_models = cv_results$models,
    verbose = verbose
  )
  
  # Step 6: ALE plots with bootstrap CIs
  if (verbose) cat("\nStep 6: Generating ALE plots with confidence intervals...\n")
  ale_results <- calculate_ale_ci(
    data = data,
    formula = formula,
    cv_results = cv_results,
    spatial_weights = spatial_weights,
    importance_table = importance_results$table,  # Pass importance results
    n_top_vars = 6,
    verbose = verbose
  )
  
  # Step 7: Create leaflet map
  leaflet_map <- NULL
  if (create_map) {
    if (verbose) cat("\nStep 7: Creating interactive map...\n")
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
    
    # Model comparison
    model_comparison = model_comparison$table,
    
    # Individual spatial models (easy access)
    ols_model = model_comparison$models$OLS,
    sar_model = model_comparison$models$SAR,
    sem_model = model_comparison$models$SEM,
    sac_model = model_comparison$models$SAC,
    
    # All model details (for advanced users)
    model_details = model_comparison$models,
    
    # Variable importance
    variable_importance = importance_results$table,
    importance_plot = importance_results$plot,
    
    # ALE results
    ale_results = ale_results$data,
    ale_plots = ale_results$plots,
    
    # Mapping
    leaflet_map = leaflet_map,
    
    # Data and metadata
    data = data,
    formula = formula,
    spatial_weights = spatial_weights,
    call = match.call()
  )
  
  class(results) <- "SArf"
  
  if (verbose) cat("\n=== ANALYSIS COMPLETE ===\n")
  
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
  
  cat("Formula:", deparse(x$formula), "\n")
  cat("Observations:", nrow(x$data), "\n\n")
  
  cat("--- Spatial Autocorrelation Test ---\n")
  cat("Moran's I:", round(x$moran_test$estimate[1], 4), "\n")
  cat("p-value:", format.pval(x$moran_test$p.value), "\n\n")
  
  cat("--- Model Comparison ---\n")
  print(x$model_comparison, row.names = FALSE)
  cat("\n")
  
  # Print summary of best spatial econometric model
  cat("--- Best Spatial Econometric Model ---\n")
  spatial_models <- x$model_comparison[x$model_comparison$Model != "RF_Spatial_CV", ]
  best_spatial <- spatial_models[which.max(spatial_models$R2), ]
  cat("Model:", best_spatial$Model, "\n")
  cat("R²:", round(best_spatial$R2, 4), "\n")
  cat("RMSE:", round(best_spatial$RMSE, 4), "\n")
  cat("Moran's I (residuals):", round(best_spatial$Morans_I, 4), 
      "( p =", format.pval(best_spatial$Morans_pval), ")\n\n")
  
  # Print model summary
  best_model_name <- as.character(best_spatial$Model)
  best_model_obj <- switch(best_model_name,
                           "OLS" = x$ols_model,
                           "SAR" = x$sar_model,
                           "SEM" = x$sem_model,
                           "SAC" = x$sac_model,
                           NULL)
  
  if (!is.null(best_model_obj)) {
    cat("Summary:\n")
    print(summary(best_model_obj))
    cat("\n")
  }
  
  cat("--- Variable Importance (Top 5) ---\n")
  top5 <- head(x$variable_importance[order(-x$variable_importance$mean), ], 5)
  print(top5, row.names = FALSE)
  cat("\n")
  
  cat("--- Accessing Results ---\n")
  cat("Plots: plot(results) or results$moran_plot, results$importance_plot, results$ale_plots\n")
  cat("Models: results$ols_model, results$sar_model, results$sem_model, results$sac_model\n")
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
  cat("Predictors:", length(all.vars(object$formula)) - 1, "\n\n")
  
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
  
  cat("=== Model Comparison ===\n")
  print(object$model_comparison, row.names = FALSE)
  cat("\n")
  
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
    
    # Plot 2: Model Comparison (as barplot)
    barplot(x$model_comparison$RMSE, 
            names.arg = x$model_comparison$Model,
            las = 2,
            main = "Model Comparison (RMSE)",
            ylab = "RMSE",
            col = "steelblue")
    
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
    print(ggplot2::ggplot(x$model_comparison, 
                          ggplot2::aes(x = reorder(Model, -RMSE), y = RMSE)) +
            ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
            ggplot2::coord_flip() +
            ggplot2::labs(title = "Model Comparison",
                         x = "Model", y = "RMSE") +
            ggplot2::theme_minimal())
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

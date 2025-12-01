#' Compare Random Forest to Spatial Econometric Models
#'
#' @param formula Model formula
#' @param data Spatial data
#' @param spatial_weights Spatial weights
#' @param rf_predictions RF prediction dataframe
#' @param compare_models Which models to compare
#' @param include_naive_rf Include naive RF with simple train-test split
#' @param naive_test_fraction Fraction of data for test set in naive RF (default 0.2)
#' @param num_trees Number of trees for naive RF (should match spatial CV RF)
#' @param mtry Number of variables for naive RF (should match spatial CV RF)
#' @param seed Random seed for naive RF split
#' @param verbose Print progress
#'
#' @return List with comparison table and model objects
#' @keywords internal
compare_spatial_models <- function(formula, data, spatial_weights, 
                                   rf_predictions,
                                   compare_models = c("OLS", "SAR", "SEM", "SAC"),
                                   include_naive_rf = TRUE,
                                   naive_test_fraction = 0.2,
                                   num_trees = 500,
                                   mtry = NULL,
                                   seed = 1111,
                                   verbose = TRUE) {
  
  # Prepare data
  model_data <- if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)
  } else {
    as.data.frame(data)
  }
  
  response_var <- all.vars(formula)[1]
  n_obs <- nrow(model_data)
  
  # Warn about large datasets
  if (n_obs > 2000 && verbose) {
    cat("\n  Note: Large dataset (n =", n_obs, "). Spatial models may take several minutes...\n")
  }
  
  # Calculate RF metrics (OUT-OF-SAMPLE only)
  rf_summary <- rf_predictions %>%
    dplyr::filter(in_training == FALSE) %>%  # Only test set predictions
    dplyr::group_by(row_id) %>%
    dplyr::summarise(pred_mean = mean(prediction, na.rm = TRUE),
                    observed = dplyr::first(observed),
                    .groups = 'drop')
  
  rf_rmse <- sqrt(mean((rf_summary$pred_mean - rf_summary$observed)^2, na.rm = TRUE))
  rf_r2 <- cor(rf_summary$pred_mean, rf_summary$observed, use = "complete.obs")^2
  
  # Create FULL residuals vector for Moran's I (aligned with spatial_weights)
  # Use average prediction across all folds for each observation
  rf_full_predictions <- rf_predictions %>%
    dplyr::filter(in_training == FALSE) %>%
    dplyr::group_by(row_id) %>%
    dplyr::summarise(pred_mean = mean(prediction, na.rm = TRUE),
                    observed = dplyr::first(observed),
                    .groups = 'drop') %>%
    dplyr::arrange(row_id)  # Ensure order matches data
  
  # Create vector of residuals in correct order for ALL observations
  rf_residuals_full <- numeric(n_obs)
  rf_residuals_full[rf_full_predictions$row_id] <- 
    rf_full_predictions$pred_mean - rf_full_predictions$observed
  
  # Initialize results
  results <- list()
  model_objects <- list()
  
  # Helper function to calculate metrics
  calc_metrics <- function(observed, predicted) {
    residuals <- observed - predicted
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    r2 <- cor(observed, predicted, use = "complete.obs")^2
    return(c(RMSE = rmse, R2 = r2))
  }
  
  # ========================================
  # NAIVE RF (if requested)
  # ========================================
  if (include_naive_rf) {
    if (verbose) {
      cat("  Fitting Naive RF (simple train-test split)...")
      flush.console()
    }
    start_time <- Sys.time()
    
    # Create global spatial lag (using ALL observations)
    coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(data)))
    k_neighbors <- round(mean(sapply(spatial_weights$neighbours, length)))
    nb_all <- spdep::knn2nb(spdep::knearneigh(coords, k = k_neighbors))
    lw_all <- spdep::nb2listw(nb_all, style = "W", zero.policy = TRUE)
    
    model_data$spatial_lag <- spdep::lag.listw(
      lw_all,
      model_data[[response_var]],
      zero.policy = TRUE
    )
    
    # Simple random train-test split
    set.seed(seed)
    n_test <- floor(n_obs * naive_test_fraction)
    test_idx <- sample(1:n_obs, n_test)
    train_idx <- setdiff(1:n_obs, test_idx)
    
    train_data <- model_data[train_idx, ]
    test_data <- model_data[test_idx, ]
    
    # Update formula to include spatial_lag
    formula_with_lag <- update(formula, ~ . + spatial_lag)
    
    # Fit RF on training data
    naive_rf <- tryCatch({
      ranger::ranger(
        formula = formula_with_lag,
        data = train_data,
        num.trees = num_trees,
        mtry = mtry,
        importance = "permutation",
        seed = seed,
        verbose = FALSE
      )
    }, error = function(e) {
      warning("Naive RF failed: ", e$message)
      NULL
    })
    
    if (!is.null(naive_rf)) {
      # Predict on test set
      naive_pred <- predict(naive_rf, data = test_data)$predictions
      naive_metrics <- calc_metrics(test_data[[response_var]], naive_pred)
      
      # Calculate Moran's I on residuals
      naive_residuals <- test_data[[response_var]] - naive_pred
      
      # Create spatial weights for test set only (for Moran's I)
      test_coords <- coords[test_idx, , drop = FALSE]
      test_nb <- tryCatch({
        spdep::knn2nb(spdep::knearneigh(test_coords, k = min(k_neighbors, nrow(test_coords) - 1)))
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(test_nb)) {
        test_lw <- spdep::nb2listw(test_nb, style = "W", zero.policy = TRUE)
        
        naive_moran <- tryCatch({
          moran_test <- spdep::moran.test(naive_residuals, test_lw, zero.policy = TRUE)
          c(statistic = as.numeric(moran_test$estimate[1]), p_value = as.numeric(moran_test$p.value))
        }, error = function(e) {
          if (verbose) cat("\n    Warning: Could not calculate Moran's I for Naive RF:", e$message, "\n")
          c(statistic = NA_real_, p_value = NA_real_)
        })
      } else {
        naive_moran <- c(statistic = NA_real_, p_value = NA_real_)
      }
      
      results$Naive_RF <- c(naive_metrics,
                           Morans_I = unname(naive_moran["statistic"]),
                           Morans_pval = unname(naive_moran["p_value"]))
      model_objects$Naive_RF <- naive_rf
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat(" Done! (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat(" Failed!\n")
    }
  }
  
  # ========================================
  # OLS
  # ========================================
  if ("OLS" %in% compare_models) {
    if (verbose) {
      cat("  Fitting OLS...")
      flush.console()
    }
    start_time <- Sys.time()
    
    ols_model <- lm(formula, data = model_data)
    ols_pred <- predict(ols_model)
    ols_metrics <- calc_metrics(model_data[[response_var]], ols_pred)
    
    # Calculate Moran's I on residuals
    ols_resid <- residuals(ols_model)
    ols_moran <- tryCatch({
      moran_test <- spdep::moran.test(ols_resid, spatial_weights, zero.policy = TRUE)
      c(statistic = as.numeric(moran_test$estimate[1]), 
        p_value = as.numeric(moran_test$p.value))
    }, error = function(e) {
      if (verbose) cat("    Warning: Could not calculate Moran's I for OLS:", e$message, "\n")
      c(statistic = NA_real_, p_value = NA_real_)
    })
    
    results$OLS <- c(ols_metrics, 
                     Morans_I = unname(ols_moran["statistic"]),
                     Morans_pval = unname(ols_moran["p_value"]))
    model_objects$OLS <- ols_model
    
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    if (verbose) cat(" Done! (", elapsed, "s)\n", sep = "")
  }
  
  # ========================================
  # SAR (Spatial Lag)
  # ========================================
  if ("SAR" %in% compare_models) {
    if (verbose) {
      cat("  Fitting SAR (Spatial Lag)...")
      if (n_obs > 1000) cat(" [May take 1-3 minutes]")
      cat("\n")
      flush.console()
    }
    start_time <- Sys.time()
    
    sar_model <- tryCatch({
      spatialreg::lagsarlm(formula, data = model_data, listw = spatial_weights,
                          zero.policy = TRUE)
    }, error = function(e) {
      warning("SAR model failed: ", e$message)
      NULL
    })
    
    if (!is.null(sar_model)) {
      sar_pred <- sar_model$fitted.values
      sar_metrics <- calc_metrics(model_data[[response_var]], sar_pred)
      
      # Calculate Moran's I on residuals
      sar_resid <- residuals(sar_model)
      sar_moran <- tryCatch({
        moran_test <- spdep::moran.test(sar_resid, spatial_weights, zero.policy = TRUE)
        c(statistic = as.numeric(moran_test$estimate[1]), p_value = as.numeric(moran_test$p.value))
      }, error = function(e) {
        warning("Could not calculate Moran's I for SAR residuals")
        c(statistic = NA, p_value = NA)
      })
      
      results$SAR <- c(sar_metrics,
                      Morans_I = unname(sar_moran["statistic"]),
                      Morans_pval = unname(sar_moran["p_value"]))
      model_objects$SAR <- sar_model
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat("    ✓ SAR complete (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat("    ✗ SAR failed\n")
    }
  }
  
  # ========================================
  # SEM (Spatial Error)
  # ========================================
  if ("SEM" %in% compare_models) {
    if (verbose) {
      cat("  Fitting SEM (Spatial Error)...")
      if (n_obs > 1000) cat(" [May take 1-3 minutes]")
      cat("\n")
      flush.console()
    }
    start_time <- Sys.time()
    
    sem_model <- tryCatch({
      spatialreg::errorsarlm(formula, data = model_data, listw = spatial_weights,
                            zero.policy = TRUE)
    }, error = function(e) {
      warning("SEM model failed: ", e$message)
      NULL
    })
    
    if (!is.null(sem_model)) {
      sem_pred <- predict(sem_model)
      sem_metrics <- calc_metrics(model_data[[response_var]], sem_pred)
      
      # Calculate Moran's I on residuals
      sem_resid <- residuals(sem_model)
      sem_moran <- tryCatch({
        moran_test <- spdep::moran.test(sem_resid, spatial_weights, zero.policy = TRUE)
        c(statistic = as.numeric(moran_test$estimate[1]), p_value = as.numeric(moran_test$p.value))
      }, error = function(e) {
        warning("Could not calculate Moran's I for SEM residuals")
        c(statistic = NA, p_value = NA)
      })
      
      results$SEM <- c(sem_metrics,
                      Morans_I = unname(sem_moran["statistic"]),
                      Morans_pval = unname(sem_moran["p_value"]))
      model_objects$SEM <- sem_model
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat("    ✓ SEM complete (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat("    ✗ SEM failed\n")
    }
  }
  
  # ========================================
  # SAC (SARAR - Spatial Lag + Error)
  # ========================================
  if ("SAC" %in% compare_models) {
    if (verbose) {
      cat("  Fitting SAC (Spatial Lag + Error)...")
      if (n_obs > 1000) cat(" [May take 2-5 minutes]")
      cat("\n")
      flush.console()
    }
    start_time <- Sys.time()
    
    sac_model <- tryCatch({
      spatialreg::sacsarlm(formula, data = model_data, listw = spatial_weights,
                          type = "sac", zero.policy = TRUE)
    }, error = function(e) {
      warning("SAC model failed: ", e$message)
      NULL
    })
    
    if (!is.null(sac_model)) {
      sac_pred <- sac_model$fitted.values
      sac_metrics <- calc_metrics(model_data[[response_var]], sac_pred)
      
      # Calculate Moran's I on residuals
      sac_resid <- residuals(sac_model)
      sac_moran <- tryCatch({
        moran_test <- spdep::moran.test(sac_resid, spatial_weights, zero.policy = TRUE)
        c(statistic = as.numeric(moran_test$estimate[1]), p_value = as.numeric(moran_test$p.value))
      }, error = function(e) {
        warning("Could not calculate Moran's I for SAC residuals")
        c(statistic = NA, p_value = NA)
      })
      
      results$SAC <- c(sac_metrics,
                      Morans_I = unname(sac_moran["statistic"]),
                      Morans_pval = unname(sac_moran["p_value"]))
      model_objects$SAC <- sac_model
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat("    ✓ SAC complete (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat("    ✗ SAC failed\n")
    }
  }
  
  # ========================================
  # Add RF Spatial CV results
  # ========================================
  if (verbose) cat("  Calculating Moran's I for RF residuals...\n")
  
  rf_moran <- tryCatch({
    # RF residuals are only for OUT-OF-SAMPLE observations
    test_row_ids <- rf_full_predictions$row_id
    
    if (length(test_row_ids) < 4) {
      if (verbose) cat("    Too few test observations for Moran's I\n")
      c(statistic = NA_real_, p_value = NA_real_)
    } else {
      # Extract residuals for test observations
      test_residuals <- rf_residuals_full[test_row_ids]
      
      # Subset spatial_weights to test observations
      test_nb <- spatial_weights$neighbours[test_row_ids]
      
      # Remap neighbor indices to subset
      test_nb_remapped <- lapply(test_nb, function(neighbors) {
        # Keep only neighbors that are in test set
        valid <- neighbors[neighbors %in% test_row_ids]
        # Remap to positions in test_row_ids vector
        match(valid, test_row_ids)
      })
      
      # Remove any NULL entries
      test_nb_remapped <- lapply(test_nb_remapped, function(x) x[!is.na(x)])
      
      # Create proper nb object
      class(test_nb_remapped) <- "nb"
      attr(test_nb_remapped, "region.id") <- as.character(1:length(test_nb_remapped))
      
      test_lw <- spdep::nb2listw(test_nb_remapped, style = "W", zero.policy = TRUE)
      
      moran_test <- spdep::moran.test(test_residuals, test_lw, zero.policy = TRUE)
      
      if (verbose) cat("    ✓ Moran's I =", round(moran_test$estimate[1], 4), "\n")
      c(statistic = as.numeric(moran_test$estimate[1]), p_value = as.numeric(moran_test$p.value))
    }
  }, error = function(e) {
    if (verbose) cat("    ✗ Could not calculate Moran's I:", e$message, "\n")
    c(statistic = NA_real_, p_value = NA_real_)
  })
  
  results$RF_Spatial_CV <- c(
    RMSE = rf_rmse,
    R2 = rf_r2,
    Morans_I = unname(rf_moran["statistic"]),
    Morans_pval = unname(rf_moran["p_value"])
  )
  
  # ========================================
  # Create comparison table with proper ordering
  # ========================================
  
  # Define desired order: Naive RF first, then other models, then RF Spatial CV last
  model_order <- c("Naive_RF", "OLS", "SAR", "SEM", "SAC", "RF_Spatial_CV")
  
  # Keep only models that were actually fit
  model_order <- model_order[model_order %in% names(results)]
  
  # Reorder results
  results <- results[model_order]
  
  comparison_df <- data.frame(
    Model = names(results),
    RMSE = sapply(results, function(x) as.numeric(x["RMSE"])),
    R2 = sapply(results, function(x) as.numeric(x["R2"])),
    Morans_I = sapply(results, function(x) as.numeric(x["Morans_I"])),
    Morans_pval = sapply(results, function(x) as.numeric(x["Morans_pval"])),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  # Round for display
  comparison_df <- comparison_df %>%
    dplyr::mutate(
      RMSE = round(RMSE, 4),
      R2 = round(R2, 4),
      Morans_I = round(Morans_I, 4),
      Morans_pval = round(Morans_pval, 4)
    )
  
  if (verbose) {
    cat("\n  Model Comparison:\n")
    print(comparison_df, row.names = FALSE)
    
    if (include_naive_rf) {
      cat("\n  Note: Naive_RF uses simple train-test split with spatial lag calculated from ALL observations.\n")
      cat("        RF_Spatial_CV uses spatial cross-validation with within-fold spatial lag calculation.\n")
    }
  }
  
  return(list(
    table = comparison_df,
    models = model_objects,
    rf_predictions = rf_summary
  ))
}

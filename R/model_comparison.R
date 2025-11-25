#' Compare Random Forest to Spatial Econometric Models
#'
#' @param formula Model formula
#' @param data Spatial data
#' @param spatial_weights Spatial weights
#' @param rf_predictions RF prediction dataframe
#' @param compare_models Which models to compare
#' @param verbose Print progress
#'
#' @return List with comparison table and model objects
#' @keywords internal
compare_spatial_models <- function(formula, data, spatial_weights, 
                                   rf_predictions, 
                                   compare_models = c("OLS", "SAR", "SEM", "SAC"),
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
  rf_residuals <- rf_summary$pred_mean - rf_summary$observed
  
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
  
  # OLS
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
      list(Morans_I = as.numeric(moran_test$estimate[1]), 
           Morans_pval = as.numeric(moran_test$p.value))
    }, error = function(e) {
      if (verbose) cat("    Warning: Could not calculate Moran's I for OLS:", e$message, "\n")
      list(Morans_I = NA_real_, Morans_pval = NA_real_)
    })
    
    results$OLS <- c(ols_metrics, 
                     Morans_I = ols_moran$Morans_I,
                     Morans_pval = ols_moran$Morans_pval)
    model_objects$OLS <- ols_model
    
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    if (verbose) cat(" Done! (", elapsed, "s)\n", sep = "")
  }
  
  # SAR (Spatial Lag)
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
      sar_pred <- predict(sar_model)
      sar_metrics <- calc_metrics(model_data[[response_var]], sar_pred)
      
      # Calculate Moran's I on residuals
      sar_resid <- residuals(sar_model)
      sar_moran <- tryCatch({
        moran_test <- spdep::moran.test(sar_resid, spatial_weights, zero.policy = TRUE)
        list(Morans_I = as.numeric(moran_test$estimate[1]), Morans_pval = as.numeric(moran_test$p.value))
      }, error = function(e) {
        warning("Could not calculate Moran's I for SAR residuals")
        list(Morans_I = NA_real_, Morans_pval = NA_real_)
      })
      
      results$SAR <- c(sar_metrics,
                      Morans_I = sar_moran$Morans_I,
                      Morans_pval = sar_moran$Morans_pval)
      model_objects$SAR <- sar_model
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat("    ✓ SAR complete (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat("    ✗ SAR failed\n")
    }
  }
  
  # SEM (Spatial Error)
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
        list(Morans_I = as.numeric(moran_test$estimate[1]), Morans_pval = as.numeric(moran_test$p.value))
      }, error = function(e) {
        warning("Could not calculate Moran's I for SEM residuals")
        list(Morans_I = NA_real_, Morans_pval = NA_real_)
      })
      
      results$SEM <- c(sem_metrics,
                      Morans_I = sem_moran$Morans_I,
                      Morans_pval = sem_moran$Morans_pval)
      model_objects$SEM <- sem_model
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat("    ✓ SEM complete (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat("    ✗ SEM failed\n")
    }
  }
  
  # SAC (SARAR - Spatial Lag + Error)
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
        list(Morans_I = as.numeric(moran_test$estimate[1]), Morans_pval = as.numeric(moran_test$p.value))
      }, error = function(e) {
        warning("Could not calculate Moran's I for SAC residuals")
        list(Morans_I = NA_real_, Morans_pval = NA_real_)
      })
      
      results$SAC <- c(sac_metrics,
                      Morans_I = sac_moran$Morans_I,
                      Morans_pval = sac_moran$Morans_pval)
      model_objects$SAC <- sac_model
      
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (verbose) cat("    ✓ SAC complete (", elapsed, "s)\n", sep = "")
    } else {
      if (verbose) cat("    ✗ SAC failed\n")
    }
  }
  
  # Add RF results
  rf_moran <- tryCatch({
    moran_test <- spdep::moran.test(rf_residuals, spatial_weights, zero.policy = TRUE)
    list(Morans_I = as.numeric(moran_test$estimate[1]), Morans_pval = as.numeric(moran_test$p.value))
  }, error = function(e) {
    warning("Could not calculate Moran's I for RF residuals")
    list(Morans_I = NA_real_, Morans_pval = NA_real_)
  })
  
  results$RF_Spatial_CV <- c(
    RMSE = rf_rmse,
    R2 = rf_r2,
    Morans_I = rf_moran$Morans_I,
    Morans_pval = rf_moran$Morans_pval
  )
  
  # Create comparison table with proper structure
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
  }
  
  return(list(
    table = comparison_df,
    models = model_objects,
    rf_predictions = rf_summary
  ))
}

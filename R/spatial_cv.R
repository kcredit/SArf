#' Spatial Cross-Validation Random Forest with Within-Fold Spatial Lag
#'
#' @param formula Model formula
#' @param data Spatial data
#' @param spatial_weights Spatial weights listw object
#' @param n_folds Number of CV folds
#' @param n_bootstrap Number of bootstrap iterations
#' @param block_range Spatial blocking range in meters
#' @param num_trees Number of trees in RF
#' @param mtry Number of variables to split on
#' @param seed Random seed
#' @param verbose Print progress
#'
#' @return List with CV results
#' @keywords internal
spatial_cv_rf <- function(formula, data, spatial_weights, n_folds = 5,
                          n_bootstrap = 20, block_range = 1000, num_trees = 500,
                          mtry = NULL, seed = 1111, verbose = TRUE) {
  
  set.seed(seed)
  
  # Check data size and warn if large
  n_obs <- nrow(data)
  total_models <- n_folds * n_bootstrap
  
  if (verbose) {
    cat("  Dataset size:", n_obs, "observations\n")
    cat("  Total models to fit:", total_models, "\n")
  }
  
  # Memory warning
  if (n_obs > 2000 && total_models > 50) {
    warning("Large dataset (", n_obs, " obs) with many models (", total_models, 
            "). Consider reducing n_bootstrap or n_folds to avoid memory issues.")
  }
  
  # Extract response variable
  response_var <- all.vars(formula)[1]
  predictor_vars <- all.vars(formula)[-1]
  
  # Convert to non-spatial for blockCV
  if (inherits(data, "sf")) {
    data_spatial <- data
    # Ensure correct CRS for blocking
    if (sf::st_is_longlat(data_spatial)) {
      data_spatial <- sf::st_transform(data_spatial, 3857)  # Web Mercator
    }
  } else {
    stop("Data must be sf object for spatial blocking")
  }
  
  # Create spatial blocks with robust error handling
  if (verbose) cat("  Creating spatial blocks...\n")
  
  # Use k-means directly for large datasets to save memory
  coords_mat <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(data_spatial)))
  
  if (n_obs > 5000) {
    if (verbose) cat("  Large dataset detected, using k-means clustering...\n")
    set.seed(seed)
    km <- kmeans(coords_mat, centers = n_folds, nstart = 10)  # Fewer starts for speed
    
    folds_list <- lapply(1:n_folds, function(i) {
      test_idx <- which(km$cluster == i)
      train_idx <- which(km$cluster != i)
      list(train_idx, test_idx)
    })
    
    sb <- list(folds_ids = folds_list)
    
  } else {
    # Try blockCV for smaller datasets
    sb <- tryCatch({
      sb_temp <- blockCV::cv_spatial(
        x = data_spatial,
        column = NULL,
        k = n_folds,
        size = block_range,
        selection = "random",
        iteration = 50,  # Fewer iterations
        progress = FALSE
      )
      
      # Validate folds
      valid_folds <- TRUE
      for (i in 1:n_folds) {
        if (length(sb_temp$folds_ids[[i]]) < 2 ||
            length(sb_temp$folds_ids[[i]][[1]]) == 0 ||
            length(sb_temp$folds_ids[[i]][[2]]) == 0) {
          valid_folds <- FALSE
          break
        }
      }
      
      if (!valid_folds) stop("Invalid folds")
      sb_temp
      
    }, error = function(e) {
      if (verbose) cat("  blockCV failed, using k-means...\n")
      set.seed(seed)
      km <- kmeans(coords_mat, centers = n_folds, nstart = 10)
      
      folds_list <- lapply(1:n_folds, function(i) {
        test_idx <- which(km$cluster == i)
        train_idx <- which(km$cluster != i)
        list(train_idx, test_idx)
      })
      
      list(folds_ids = folds_list)
    })
  }
  
  # Verify folds
  if (verbose) {
    cat("  Fold sizes:\n")
    for (i in 1:n_folds) {
      train_n <- length(sb$folds_ids[[i]][[1]])
      test_n <- length(sb$folds_ids[[i]][[2]])
      cat(sprintf("    Fold %d: %d train, %d test\n", i, train_n, test_n))
    }
  }
  
  # Storage for results - more memory efficient
  all_predictions <- vector("list", total_models)
  all_importances <- vector("list", total_models)
  all_models <- vector("list", total_models)
  result_counter <- 1
  
  # Run spatial CV bootstrap
  if (verbose) cat("  Running spatial CV bootstrap...\n")
  
  for (iter in 1:n_bootstrap) {
    if (verbose && (iter == 1 || iter %% 5 == 0)) {
      cat("    Iteration", iter, "of", n_bootstrap, "\n")
    }
    
    for (fold in 1:n_folds) {
      
      # Get train/test indices
      train_idx <- sb$folds_ids[[fold]][[1]]
      test_idx <- sb$folds_ids[[fold]][[2]]
      
      # Subset data
      train_data <- data[train_idx, ]
      
      # Calculate spatial lag WITHIN training fold
      train_coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(train_data)))
      train_nb <- spdep::knn2nb(spdep::knearneigh(train_coords, k = 20))
      train_lw <- spdep::nb2listw(train_nb, style = "W", zero.policy = TRUE)
      
      # Get train data as data.frame
      train_df <- if (inherits(train_data, "sf")) {
        sf::st_drop_geometry(train_data)
      } else {
        as.data.frame(train_data)
      }
      
      # Add spatial lag
      train_df$spatial_lag <- spdep::lag.listw(
        train_lw, 
        train_df[[response_var]], 
        zero.policy = TRUE
      )
      
      # Update formula
      formula_with_lag <- update(formula, ~ . + spatial_lag)
      
      # Fit RF
      rf_model <- ranger::ranger(
        formula = formula_with_lag,
        data = train_df,
        num.trees = num_trees,
        mtry = mtry,
        importance = "permutation",
        seed = seed + iter * 10 + fold,
        verbose = FALSE  # Suppress ranger output
      )
      
      # Predict on FULL dataset
      full_df <- if (inherits(data, "sf")) {
        sf::st_drop_geometry(data)
      } else {
        as.data.frame(data)
      }
      
      full_df$spatial_lag <- spdep::lag.listw(
        spatial_weights,
        full_df[[response_var]],
        zero.policy = TRUE
      )
      
      predictions <- predict(rf_model, full_df)$predictions
      
      # Store results (more memory efficient)
      all_predictions[[result_counter]] <- data.frame(
        cv_iter = result_counter,
        fold = fold,
        iteration = iter,
        row_id = 1:nrow(data),
        prediction = predictions,
        observed = full_df[[response_var]],
        in_training = 1:nrow(data) %in% train_idx
      )
      
      all_importances[[result_counter]] <- rf_model$variable.importance
      all_models[[result_counter]] <- list(
        model = rf_model,
        train_data = train_df,
        fold = fold,
        iteration = iter
      )
      
      result_counter <- result_counter + 1
      
      # Garbage collection every 10 models
      if (result_counter %% 10 == 0) gc(verbose = FALSE)
    }
  }
  
  # Combine predictions
  predictions_df <- dplyr::bind_rows(all_predictions)
  
  # Calculate metrics (OUT-OF-SAMPLE only)
  predictions_summary <- predictions_df %>%
    dplyr::filter(in_training == FALSE) %>%  # Only test set predictions
    dplyr::group_by(row_id) %>%
    dplyr::summarise(
      pred_mean = mean(prediction, na.rm = TRUE),
      pred_sd = sd(prediction, na.rm = TRUE),
      observed = dplyr::first(observed),
      .groups = 'drop'
    )
  
  rmse <- sqrt(mean((predictions_summary$pred_mean - predictions_summary$observed)^2, 
                   na.rm = TRUE))
  r2 <- cor(predictions_summary$pred_mean, predictions_summary$observed, 
           use = "complete.obs")^2
  
  # Moran's I on residuals
  residuals <- predictions_summary$pred_mean - predictions_summary$observed
  moran_resid <- spdep::moran.test(residuals, spatial_weights, zero.policy = TRUE)
  
  if (verbose) {
    cat("  Spatial CV Results:\n")
    cat("    RMSE:", round(rmse, 4), "\n")
    cat("    R²:", round(r2, 4), "\n")
    cat("    Moran's I (residuals):", round(moran_resid$estimate[1], 4), 
        "( p =", format.pval(moran_resid$p.value), ")\n")
  }
  
  return(list(
    predictions = predictions_df,
    predictions_summary = predictions_summary,
    importances = all_importances,
    models = all_models,
    metrics = list(
      rmse = rmse,
      r2 = r2,
      morans_i = moran_resid$estimate[1],
      morans_p = moran_resid$p.value
    )
  ))
}

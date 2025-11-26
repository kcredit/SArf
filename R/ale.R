#' Calculate ALE Plots with Bootstrap Confidence Intervals
#'
#' @param data Spatial data
#' @param formula Model formula
#' @param cv_results CV results object
#' @param spatial_weights Spatial weights
#' @param importance_table Pre-calculated importance table (optional)
#' @param n_top_vars Number of top variables to plot
#' @param verbose Print progress
#'
#' @return List with ALE data and plots
#' @keywords internal
calculate_ale_ci <- function(data, formula, cv_results, spatial_weights,
                             importance_table = NULL, n_top_vars = 6, 
                             verbose = TRUE) {
  
  # Get response variable name
  response_var <- all.vars(formula)[1]
  spatial_lag_name <- "spatial_lag"
  
  # Get top variables from importance table if provided
  if (!is.null(importance_table)) {
    # Separate spatial_lag from other variables
    other_vars <- importance_table %>%
      dplyr::arrange(desc(mean)) %>%
      dplyr::filter(variable != spatial_lag_name) %>%
      dplyr::pull(variable)
    
    # Check if spatial_lag is in importance table
    has_spatial_lag <- spatial_lag_name %in% importance_table$variable
    
    # Select top variables: ensure spatial lag is included if present
    if (has_spatial_lag) {
      # Take top (n_top_vars - 1) other variables plus spatial lag
      top_vars <- c(head(other_vars, n_top_vars - 1), spatial_lag_name)
    } else {
      # No spatial lag, just take top n_top_vars
      top_vars <- head(other_vars, n_top_vars)
    }
    
    if (verbose) {
      cat("  Using top", length(top_vars), "variables from importance ranking:\n")
      cat("   ", paste(top_vars, collapse = ", "), "\n")
      if (has_spatial_lag) {
        cat("  (Spatial lag of dependent variable included)\n")
      }
    }
    
  } else {
    # Fallback: Calculate from CV results (old method)
    all_importances <- cv_results$importances
    
    importance_summary <- dplyr::bind_rows(
      lapply(seq_along(all_importances), function(i) {
        imp <- all_importances[[i]]
        data.frame(
          variable = names(imp),
          importance = as.numeric(imp),
          stringsAsFactors = FALSE
        )
      })
    ) %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(mean_imp = mean(importance, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(desc(mean_imp))
    
    # Separate spatial lag
    other_vars <- importance_summary %>%
      dplyr::filter(variable != spatial_lag_name) %>%
      dplyr::pull(variable)
    
    has_spatial_lag <- spatial_lag_name %in% importance_summary$variable
    
    if (has_spatial_lag) {
      top_vars <- c(head(other_vars, n_top_vars - 1), spatial_lag_name)
    } else {
      top_vars <- head(other_vars, n_top_vars)
    }
    
    if (verbose) {
      cat("  Calculating ALE for top", length(top_vars), "variables:\n")
      cat("   ", paste(top_vars, collapse = ", "), "\n")
    }
  }
  
  # Prepare data
  model_data <- if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)
  } else {
    as.data.frame(data)
  }
  
  # Ensure spatial lag is in the data
  if (!spatial_lag_name %in% names(model_data)) {
    model_data$spatial_lag <- spdep::lag.listw(
      spatial_weights,
      model_data[[response_var]],
      zero.policy = TRUE
    )
  }
  
  # Calculate ALE for each variable across all bootstrap models
  ale_results <- list()
  
  for (var in top_vars) {
    if (verbose) cat("    Processing", var, "...\n")
    
    var_ales <- list()
    
    for (i in seq_along(cv_results$models)) {
      model_obj <- cv_results$models[[i]]
      
      tryCatch({
        # Check that variable exists in training data
        if (var %in% names(model_obj$train_data)) {
          # Calculate ALE using ALEPlot package
          ale_obj <- ALEPlot::ALEPlot(
            X = model_obj$train_data,
            X.model = model_obj$model,
            pred.fun = function(X.model, newdata) {
              predict(X.model, newdata)$predictions
            },
            J = which(names(model_obj$train_data) == var),
            K = 50
          )
          
          var_ales[[i]] <- data.frame(
            variable = var,
            iteration = i,
            x = ale_obj$x.values,
            ale = ale_obj$f.values,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        if (verbose) {
          cat("      Warning: ALE calculation failed for", var, "in iteration", i, "\n")
        }
        NULL
      })
    }
    
    # Combine and calculate CIs
    var_ales_df <- dplyr::bind_rows(var_ales)
    
    if (nrow(var_ales_df) > 0) {
      # Smooth and interpolate to common grid
      x_range <- range(var_ales_df$x, na.rm = TRUE)
      x_grid <- seq(x_range[1], x_range[2], length.out = 100)
      
      smoothed_ales <- var_ales_df %>%
        dplyr::group_by(iteration) %>%
        dplyr::summarise(
          x_smooth = list(x_grid),
          ale_smooth = list(stats::approx(x, ale, xout = x_grid, rule = 2)$y),
          .groups = 'drop'
        ) %>%
        tidyr::unnest(cols = c(x_smooth, ale_smooth))
      
      # Calculate summary statistics at each x
      ale_summary <- smoothed_ales %>%
        dplyr::group_by(x_smooth) %>%
        dplyr::summarise(
          ale_mean = mean(ale_smooth, na.rm = TRUE),
          ale_lower = quantile(ale_smooth, 0.025, na.rm = TRUE),
          ale_upper = quantile(ale_smooth, 0.975, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(variable = var)
      
      ale_results[[var]] <- ale_summary
    } else {
      if (verbose) {
        cat("      Warning: No valid ALE results for", var, "\n")
      }
    }
  }
  
  # Combine all ALE results
  ale_data <- dplyr::bind_rows(ale_results)
  
  # Create combined ALE plot (3x2 grid)
  if (nrow(ale_data) > 0) {
    # Create clean variable labels for facets
    ale_data <- ale_data %>%
      dplyr::mutate(
        variable_label = dplyr::case_when(
          variable == spatial_lag_name ~ paste0(response_var, " (Spatial Lag)"),
          TRUE ~ variable
        )
      )
    
    ale_plots <- ggplot2::ggplot(
      ale_data,
      ggplot2::aes(x = x_smooth, y = ale_mean)
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ale_lower, ymax = ale_upper),
        fill = "lightblue",
        alpha = 0.4
      ) +
      ggplot2::geom_line(color = "darkblue", linewidth = 1) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::facet_wrap(~ variable_label, scales = "free", ncol = 3) +
      ggplot2::labs(
        title = "Accumulated Local Effects with 95% Confidence Intervals",
        subtitle = "Based on Spatial Cross-Validation Bootstrap",
        x = "Variable Value",
        y = "ALE (Effect on Outcome)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 11),
        strip.text = ggplot2::element_text(face = "bold", size = 10),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  } else {
    ale_plots <- NULL
    warning("No ALE plots could be generated")
  }
  
  return(list(
    data = ale_data,
    plots = ale_plots,
    variables = top_vars
  ))
}

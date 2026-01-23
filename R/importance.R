#' Calculate Variable Importance with Bootstrap Confidence Intervals
#'
#' @param cv_models List of CV model objects
#' @param verbose Print progress
#'
#' @return List with importance table and plot
#' @keywords internal
calculate_importance_ci <- function(cv_models, verbose = TRUE) {
  
  # Extract importances from all models
  all_importances <- lapply(cv_models, function(m) {
    imp <- m$model$variable.importance
    # Validate: must be non-null, named, and numeric
    if (!is.null(imp) && !is.null(names(imp)) && length(imp) > 0 && is.numeric(imp)) {
      imp
    } else {
      NULL
    }
  })

  # Remove NULLs
  all_importances <- all_importances[!sapply(all_importances, is.null)]

  # Convert to data frame
  if (length(all_importances) > 0) {
    importance_df <- dplyr::bind_rows(
      lapply(seq_along(all_importances), function(i) {
        imp <- all_importances[[i]]
        data.frame(
          iteration = i,
          variable = names(imp),
          importance = as.numeric(imp),
          stringsAsFactors = FALSE
        )
      })
    )
  } else {
    importance_df <- data.frame(
      iteration = integer(0),
      variable = character(0),
      importance = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  

  # Handle empty importance_df
  if (nrow(importance_df) == 0) {
    importance_summary <- data.frame(
      variable = character(0),
      mean = numeric(0),
      sd = numeric(0),
      lower = numeric(0),
      upper = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    # Calculate summary statistics
    importance_summary <- importance_df %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(
        mean = mean(importance, na.rm = TRUE),
        sd = sd(importance, na.rm = TRUE),
        lower = quantile(importance, 0.025, na.rm = TRUE),
        upper = quantile(importance, 0.975, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::arrange(dplyr::desc(mean))

    # Fix non-finite values (must be done separately to preserve numeric type)
    importance_summary$mean[!is.finite(importance_summary$mean)] <- 0
    importance_summary$sd[!is.finite(importance_summary$sd)] <- 0
    importance_summary$lower[!is.finite(importance_summary$lower)] <- 0
    importance_summary$upper[!is.finite(importance_summary$upper)] <- 0

    # Ensure numeric types
    importance_summary$mean <- as.numeric(importance_summary$mean)
    importance_summary$sd <- as.numeric(importance_summary$sd)
    importance_summary$lower <- as.numeric(importance_summary$lower)
    importance_summary$upper <- as.numeric(importance_summary$upper)
  }
  
  if (verbose) {
    if (nrow(importance_summary) > 0) {
      cat("  Variable Importance (Top 5):\n")
      top5 <- head(importance_summary, 5)
      for (i in 1:nrow(top5)) {
        cat(sprintf("    %s: %.4f [%.4f, %.4f]\n",
                    top5$variable[i], top5$mean[i],
                    top5$lower[i], top5$upper[i]))
      }
    } else {
      cat("  No variable importance data available.\n")
    }
  }
  
  # Create importance plot (only if we have data)
  if (nrow(importance_summary) > 0) {
    importance_plot <- ggplot2::ggplot(
      importance_summary,
      ggplot2::aes(x = reorder(variable, mean), y = mean)
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = 0.2
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Variable Importance with 95% Confidence Intervals",
        subtitle = "Based on Spatial Cross-Validation Bootstrap",
        x = "Variable",
        y = "Importance (Permutation)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  } else {
    importance_plot <- NULL
    if (verbose) {
      cat("  Note: No variable importance data available to plot.\n")
    }
  }
  
  return(list(
    table = importance_summary,
    plot = importance_plot,
    raw_data = importance_df
  ))
}

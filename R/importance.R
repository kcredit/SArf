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
    if (!is.null(m$model$variable.importance)) {
      m$model$variable.importance
    } else {
      NULL
    }
  })
  
  # Remove NULLs
  all_importances <- all_importances[!sapply(all_importances, is.null)]
  
  # Convert to data frame
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
    dplyr::mutate(
      mean = ifelse(is.infinite(mean) | is.nan(mean), 0, mean),
      sd = ifelse(is.infinite(sd) | is.nan(sd), 0, sd),
      lower = ifelse(is.infinite(lower) | is.nan(lower), 0, lower),
      upper = ifelse(is.infinite(upper) | is.nan(upper), 0, upper)
    ) %>%
    dplyr::arrange(desc(mean))
  
  if (verbose) {
    cat("  Variable Importance (Top 5):\n")
    top5 <- head(importance_summary, 5)
    for (i in 1:nrow(top5)) {
      cat(sprintf("    %s: %.4f [%.4f, %.4f]\n",
                  top5$variable[i], top5$mean[i], 
                  top5$lower[i], top5$upper[i]))
    }
  }
  
  # Create importance plot
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
  
  return(list(
    table = importance_summary,
    plot = importance_plot,
    raw_data = importance_df
  ))
}

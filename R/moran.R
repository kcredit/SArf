#' Test for Spatial Autocorrelation using Moran's I
#'
#' @param data Spatial data (sf object or data.frame)
#' @param variable Name of variable to test
#' @param coords Optional coordinate matrix
#' @param k_neighbors Number of neighbors for spatial weights
#' @param alpha Significance level
#' @param verbose Print progress messages
#'
#' @return List with test results and plot
#' @keywords internal
test_morans_i <- function(data, variable, coords = NULL, k_neighbors = 20,
                          alpha = 0.05, verbose = TRUE) {
  
  # Extract variable
  y <- if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)[[variable]]
  } else {
    data[[variable]]
  }
  
  # Create spatial weights
  if (inherits(data, "sf")) {
    coords_mat <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(data)))
  } else if (!is.null(coords)) {
    coords_mat <- coords
  } else {
    stop("coords must be provided if data is not sf object")
  }
  
  nb <- spdep::knn2nb(spdep::knearneigh(coords_mat, k = k_neighbors))
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Moran's I test
  moran_test <- spdep::moran.test(y, lw, zero.policy = TRUE)
  
  if (verbose) {
    cat("  Moran's I:", round(moran_test$estimate[1], 4), "\n")
    cat("  p-value:", format.pval(moran_test$p.value), "\n")
    if (moran_test$p.value < alpha) {
      cat("  Result: Significant spatial autocorrelation detected\n")
    } else {
      cat("  Result: No significant spatial autocorrelation\n")
    }
  }
  
  # Create Moran scatter plot
  lag_y <- spdep::lag.listw(lw, y, zero.policy = TRUE)
  
  plot_data <- data.frame(
    value = y,
    lagged_value = lag_y
  )
  
  # Create Moran plot using standard syntax to avoid namespace conflicts
  moran_plot <- tryCatch({
    ggplot2::ggplot(plot_data, ggplot2::aes(x = value, y = lagged_value)) +
      ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
      ggplot2::geom_smooth(method = "lm", color = "darkblue", se = TRUE) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = "Moran's I Scatter Plot",
        subtitle = paste0("Moran's I = ", round(moran_test$estimate[1], 4),
                         ", p-value = ", format.pval(moran_test$p.value)),
        x = variable,
        y = paste("Spatially Lagged", variable)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  }, error = function(e) {
    warning("Could not create Moran plot: ", e$message)
    NULL
  })
  
  return(list(
    test = moran_test,
    plot = moran_plot,
    p_value = moran_test$p.value,
    statistic = moran_test$estimate[1]
  ))
}


#' Create Spatial Weights Matrix
#'
#' @param data Spatial data
#' @param coords Optional coordinate matrix
#' @param k_neighbors Number of neighbors
#'
#' @return Spatial weights listw object
#' @keywords internal
create_spatial_weights <- function(data, coords = NULL, k_neighbors = 20) {
  
  if (inherits(data, "sf")) {
    coords_mat <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(data)))
  } else if (!is.null(coords)) {
    coords_mat <- coords
  } else {
    stop("coords must be provided if data is not sf object")
  }
  
  nb <- spdep::knn2nb(spdep::knearneigh(coords_mat, k = k_neighbors))
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  return(lw)
}

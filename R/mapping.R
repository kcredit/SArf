#' Create Interactive Leaflet Map
#'
#' @param data Spatial data (must be sf object)
#' @param variable Variable name to map
#' @param title Map title
#' @param palette Color palette (default: "viridis")
#' @param reverse_palette Reverse color palette? (default: FALSE for viridis)
#'
#' @return Leaflet map object
#' @keywords internal
create_leaflet_map <- function(data, variable, title = NULL,
                               palette = "viridis", reverse_palette = FALSE) {
  
  if (!inherits(data, "sf")) {
    stop("Data must be sf object for mapping")
  }
  
  # Transform to WGS84 for leaflet
  data_map <- sf::st_transform(data, 4326)
  
  # Extract variable
  values <- sf::st_drop_geometry(data_map)[[variable]]
  
  # Create quantile bins
  breaks <- quantile(values, probs = seq(0, 1, 0.2), na.rm = TRUE)
  
  # Create color palette
  pal <- leaflet::colorBin(
    palette = palette,
    domain = values,
    bins = breaks,
    reverse = reverse_palette,
    na.color = "gray"
  )
  
  # Create labels more safely
  # Get ID column (try common names)
  id_col <- NULL
  possible_ids <- c("SA_PUB2022", "GEOID", "ID", "id", "objectid", "OBJECTID")
  for (col in possible_ids) {
    if (col %in% names(data_map)) {
      id_col <- col
      break
    }
  }
  
  # Create labels
  if (!is.null(id_col)) {
    label_text <- sprintf(
      "<strong>%s</strong><br/>%s: %.2f",
      as.character(data_map[[id_col]]),
      variable,
      values
    )
  } else {
    # Fallback: just use row number
    label_text <- sprintf(
      "<strong>Area %d</strong><br/>%s: %.2f",
      1:nrow(data_map),
      variable,
      values
    )
  }
  
  # Convert to HTML safely
  labels <- lapply(label_text, htmltools::HTML)
  
  # Create map
  map <- leaflet::leaflet(data_map) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      fillColor = ~pal(values),
      weight = 0.5,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 0.7,
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = ~values,
      opacity = 0.7,
      title = variable,
      position = "bottomright"
    )
  
  # Add title if provided
  if (!is.null(title)) {
    map <- map %>%
      leaflet::addControl(
        html = paste0("<div style='background: white; padding: 5px; border-radius: 5px;'>",
                     "<h4>", title, "</h4></div>"),
        position = "topright"
      )
  }
  
  return(map)
}

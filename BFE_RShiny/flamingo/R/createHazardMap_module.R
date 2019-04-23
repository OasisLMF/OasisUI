# Create Hazard Map module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' createHazardMap
#'
#' @rdname createHazardMap
#'
#' @description Creates a hazard map using leaflet.
#'
#' @export
createHazardMapUI <- function(id) {
  ns <- NS(id)

  tagList(
    leafletOutput(ns("hazardmap"))
    # fluidRow(
    #   sliderInput(ns("contrast_colors"), "Adjust color contrast", min = 0.1, max = 1, value = 0.5)
    # )
  )
}

# Server -----------------------------------------------------------------------
#' Create Hazard Map
#'
#' @rdname createHazardMap
#'
#' @description Creates a hazard map using leaflet.
#'
#' @template params-module-ui
#' @param file_map File to plot as map.
#' @param file_pins File to plot pins.
#'
#' @return Leaflet map.
#'
#' @importFrom geojsonio geojson_read
#' @importFrom leaflet leaflet
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet colorNumeric
#' @importFrom leaflet awesomeIcons
#' @importFrom leaflet addPolygons
#' @importFrom leaflet addLegend
#' @importFrom leaflet addAwesomeMarkers
#'
#' @export
createHazardMap <- function(input, output, session,
                            file_map,
                            file_pins) {

  ns <- session$ns

  # Plot leaflet
  output$hazardmap <- renderLeaflet({
    .buildHazardMap(file_map, file_pins)
  })

  # Create map
  .buildHazardMap <- function(file_map, file_pins) {

    # Create map color palette
    pal <- colorNumeric("Reds", NULL)

    # Create custom icons
    icon_map <- awesomeIcons(
      icon = 'map-marker-alt',
      library = 'fa',
      iconColor ='green',
      markerColor =  'blue'
    )

    popupData <- tagList(
      strong("Location ID: "), file_pins$LocNumber,
      br(), strong("Latitude: "), file_pins$Latitude,
      br(), strong("Longitude: "), file_pins$Longitude)

    withModalSpinner(
      hazardmap <- leaflet(file_map) %>%
        addTiles() %>%
        addPolygons(color = "transparent",
                    fillColor = ~pal(file_map$ReturnLevel),
                    fillOpacity = 1) %>%
        addLegend(position = "topright",
                  pal = pal,
                  values = file_map$ReturnLevel) %>%
        addAwesomeMarkers(lng = file_pins$Longitude,
                          lat = file_pins$Latitude,
                          icon = icon_map,
                          clusterOptions = TRUE,
                          popup = popupData),
      "Rendering map...",
      size = "s"
    )

    return(hazardmap)
  }
}

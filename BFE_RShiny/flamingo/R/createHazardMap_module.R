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
#' @param file_map File to plot as map.
#' @param file_pins File to plot pins.
#'
#' @return Leaflet map.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet colorNumeric
#' @importFrom leaflet awesomeIcons
#' @importFrom leaflet addPolygons
#' @importFrom leaflet addGeoJSON
#' @importFrom leaflet addLegend
#' @importFrom leaflet addAwesomeMarkers
#'
#' @export
createHazardMap <- function(input, output, session,
                            file_map,
                            file_pins) {

  ns <- session$ns

  # Isolate coordinates, Return Level, lng and lat
  i <- 1:max(file_map$features$id)
  coor <- sapply(i, function(x) {file_map$features$geometry$coordinates[[x]]})
  col <- sapply(i, function(x) {file_map$features$properties[1][[1]][x]})
  pol_lng <- sapply(i, function(x) {file_map$features$geometry$coordinates[[x]][2:5]})
  pol_lat <- sapply(i, function(x) {file_map$features$geometry$coordinates[[x]][7:10]})

  # Plot leaflet
  output$hazardmap <- renderLeaflet({
    .buildHazardMap(file_map, file_pins)
  })

  # Create map
  .buildHazardMap <- function(df_map, file_pins) {

    # Create map color palette
    pal <- colorNumeric("Reds", col)

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

    leaflet() %>%
      addTiles() %>%
      addGeoJSON(df_map) %>%
      addPolygons(lng = pol_lng,
                  lat = pol_lat,
                  color = "red",
                  # fillColor = pal(col),
                  fillOpacity = 1) %>%
      addLegend(position = "topright",
                pal = pal,
                values = col) %>%
      addAwesomeMarkers(lng = file_pins$Longitude,
                        lat = file_pins$Latitude,
                        icon = icon_map,
                        clusterOptions = TRUE,
                        popup = toString(popupData))
  }
}

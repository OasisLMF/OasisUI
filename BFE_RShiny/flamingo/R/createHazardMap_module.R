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
#' @param portfolioID ID of the portfolio.
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
                            portfolioID = "") {

  ns <- session$ns

  # Isolate coordinates and Return Level
  i <- 1:max(file_map$features$id)
  coor <- sapply(i, function(x) {file_map$features$geometry$coordinates[[x]]})
  col <- sapply(i, function(x) {file_map$features$properties[1][[1]][x]})
  pol_lng <- sapply(i, function(x) {file_map$features$geometry$coordinates[[x]][1:4]})
  pol_lat <- sapply(i, function(x) {file_map$features$geometry$coordinates[[x]][6:9]})

  # Find maximum negative (lng) number and minimum (lat) values
  # pos_min <- function(x) {min(x[x > 0])}
  # neg_max <- function(x) {max(x[x < 0])}

  # Plot leaflet
  output$hazardmap <- renderLeaflet({
    .buildHazardMap(file_map, portfolioID())
  })

  # Create map
  .buildHazardMap <- function(df_map, portfolioID) {

    uploaded_locs <- return_file_df(api_get_portfolios_location_file, portfolioID())

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
      strong("Location ID: "), uploaded_locs$LocNumber,
      br(), strong("Latitude: "), uploaded_locs$Latitude,
      br(), strong("Longitude: "), uploaded_locs$Longitude)

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
      addAwesomeMarkers(lng = uploaded_locs$Longitude,
                        lat = uploaded_locs$Latitude,
                        icon = icon_map,
                        clusterOptions = TRUE,
                        popup = toString(popupData))
  }
}

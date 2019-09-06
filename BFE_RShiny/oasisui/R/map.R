#' Create Plain Map
#'
#' @rdname createPlainMap
#'
#' @description Creates a plain map using leaflet.
#'
#' @param df df to plot as map
#'
#' @return Leaflet map.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addMarkers
#' @importFrom leaflet markerClusterOptions
#'
#' @export
createPlainMap <- function(df) {

  df <- build_marker_data(df)

  # Create custom icons
  icon_map <- awesomeIcons(
    icon = 'map-marker-alt',
    library = 'fa',
    iconColor = 'green',
    markerColor = 'blue'
  )

  leaflet(df) %>%
    addTiles() %>%
    addMarkers(lng = ~longitude,
               lat = ~latitude,
               icon = icon_map,
               clusterOptions = markerClusterOptions(maxClusterRadius = 50),
               popup = ~popup)
}


#' build_marker_data
#'
#' @rdname createPlainMap
#'
#' @description Builds markers data to be used in a map rendered with leaflet.
#'
#' @param data dataframe containing location id and coordinates.
#'
#' @return dataframe with popup information under "popup".
#'
#' @export
build_marker_data <- function(data) {
  names(data) <- tolower(names(data))
  # Popup data, must be a character vector of html code
  data$popup <- mapply(
    function(id, lat, lng) {
      as.character(div(
        strong("Location ID: "), id,
        br(), strong("Latitude: "), lat,
        br(), strong("Longitude: "), lng
      ))
    },
    data$locnumber, data$latitude, data$longitude
  )
  data
}

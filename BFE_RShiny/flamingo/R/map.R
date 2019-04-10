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

  popupData <- tagList(
    strong("Location ID: "), df$locnum,
    br(), strong("Latitude: "), df$latitude,
    br(), strong("Longitude: "), df$longitude)

  leaflet() %>%
    addTiles() %>%
    addMarkers(data = df,
               clusterOptions= markerClusterOptions(maxClusterRadius = 50),
               popup = toString(popupData))
}

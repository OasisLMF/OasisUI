#' Create Plain Map
#'
#' @rdname createPlainMap
#'
#' @description Creates a plain map using leaflet.
#'
#' @param fileName name of file to plot as map
#'
#' @return Leaflet map.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addMarkers
#' @importFrom leaflet markerClusterOptions
#'
#' @export
createPlainMap <- function(fileName) {

  markerData <- read.csv(fileName, header = TRUE, sep = ",",
      quote = "\"", dec = ".", fill = TRUE, comment.char = "")

  popupData <- tagList(
      strong("Location ID: "), markerData$LOCNUM,
      br(), strong("Latitude: "), markerData$LATITUDE,
      br(), strong("Longitude: "), markerData$LONGITUDE)

  leaflet() %>%
      addTiles() %>%
      addMarkers(data = markerData,
          clusterOptions= markerClusterOptions(maxClusterRadius = 50),
          popup = toString(popupData))
}

#' Create Footprint Map
#'
#' @rdname createFootprintMap
#'
#' @description creates a footprint map using leaflet based on exposure data
#' stored in the flamingo database.
#'
#' @param dbSettings as returned from \link{flamingoDB}
#' @param fileId File id for exposure data.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addMarkers
#' @importFrom leaflet markerClusterOptions
#'
#' @export
createFootprintMap <- function(dbSettings, fileId) {

  stmt <- buildDbQuery("getFileDataForFile", fileId)
  exposureData <- executeDbQuery(dbSettings, stmt)

  popupData <- tagList(
      strong("Latitude: "), exposureData$latitude,
      br(), strong("Longitude: "), exposureData$longitude,
      br(), strong("Intensity: "), exposureData$intensity,
      br(), strong("Area Peril ID: "), exposureData$AreaPeril)

  leaflet() %>%
      addTiles() %>%
      addMarkers(
          data = exposureData,
          clusterOptions = markerClusterOptions(maxClusterRadius = 30),
          popup = toString(popupData)
      )

}

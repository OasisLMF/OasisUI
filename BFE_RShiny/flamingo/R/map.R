#' Create Plain Map
#' @rdname createPlainMap
#' @description creates a plain map using leaflet
#' @param fileName full file path to marker data
#' @import leaflet
#' @importFrom utils read.csv
#' @importFrom htmltools tagList
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
#' @rdname createFootprintMap
#' @description creates a footprint map using leaflet based on exposure data
#' stored in the flamingo database.
#' @inheritParams executeDbQuery
#' @param fileId file id for exposure data
#' @import leaflet
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

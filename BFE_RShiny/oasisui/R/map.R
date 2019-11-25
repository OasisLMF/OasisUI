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
#' @param session Current session.
#' @param analysisID Chosen analysis ID.
#'
#' @return dataframe with popup information under "popup".
#'
#' @export
build_marker_data <- function(data, session, analysisID) {
  names(data) <- tolower(names(data))

  # extract error messages in case status is "Fail"
  ns <- session$ns
  keys_errors <- session$userData$data_hub$get_ana_dataset_content(id = analysisID,
                                                                   dataset_identifier = "keys-errors.csv",
                                                                   type = "input") %>% filter(PerilID == "ORF")
  error_msg <- data.frame(message = 1:length(data$bitiv))
  for (i in seq(1, length(data$bitiv))) {
    if (length(keys_errors$LocID[which(keys_errors$LocID == i)]) == 0) {
      error_msg$message[i] <- NA
    } else {
      error_msg$message[i] <- keys_errors$Message[which(keys_errors$LocID == i)]
    }
  }

  # Popup data, must be a character vector of html code
  data$popup <- mapply(
    function(id, bitiv, streetaddress, postalcode, message) {
      as.character(div(
        strong("Location ID: "), id,
        # br(), strong("Latitude: "), lat,
        # br(), strong("Longitude: "), lng
        strong("TIV: "), bitiv,
        br(), strong("Street Address: "), streetaddress,
        br(), strong("Postal code: "), postalcode,
        br(), strong("Error message: "), message
      ))
    },
    data$locnumber, data$bitiv, data$streetaddress, data$postalcode, error_msg$message
  )
  data
}

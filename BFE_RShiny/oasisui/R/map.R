#' Create Plain Map
#'
#' @rdname createPlainMap
#'
#' @description Creates a plain map using leaflet.
#'
#' @param df df to plot as map
#' @param session Current session.
#' @param paramID Chosen parameter ID.
#' @param step Only important if user is in Validation Map. NULL by default.
#'
#' @return Leaflet map.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addMarkers
#' @importFrom leaflet markerClusterOptions
#' @importFrom leaflet.extras addFullscreenControl
#'
#' @export
createPlainMap <- function(df, session, paramID, step = NULL) {

  df <- build_marker_data(df, session, paramID, step)

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
               clusterOptions = markerClusterOptions(maxClusterRadius = 50)#,
               # popup = popup
               ) %>% # make map full screen
    addFullscreenControl(pseudoFullscreen = TRUE)
}


#' build_marker_data
#'
#' @rdname build_marker_data
#'
#' @description Builds markers data to be used in a map rendered with leaflet.
#'
#' @param data dataframe containing location id and coordinates.
#' @param session Current session.
#' @param paramID Chosen parameter ID.
#' @param step Only important if user is in Validation Map. NULL by default.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
#' @return dataframe with popup information under "popup".
#'
#' @export
build_marker_data <- function(data, session, paramID, step = NULL) {

  names(data) <- tolower(names(data))

  # extract error messages in case status is "Fail"
  error_msg <- .keys_errors_msg(data, session, paramID)

  # In case streetaddress and postalcode are not entries of the data frame, create vector of NAs
  if (is.null(data$streetaddress)) {
    data$streetaddress <- rep_len(NA, nrow(data))
  }
  if (is.null(data$postalcode)) {
    data$postalcode <- rep_len(NA, nrow(data))
  }

  # sum over all TIVs
  tiv <- data.frame(total = rep_len(0, nrow(data)))
  tiv_var <- 0
  for (i in grep("tiv", names(data))) {
    if (length(data[[i]]) > 0 && !all(is.na(data[[i]]))) {
      # should we check for individual NAs and set to zero for the summation below?
      # doesn't seem to be an issue atm.
      tiv_var <- tiv_var + data[[i]]
      tiv$total <- add_commas(tiv_var)
    }
  }

  data <- data %>% mutate("tiv_tot" = tiv$total)

  # Popup data, must be a character vector of html code
  if (!is.null(step)) {
    # Include error message only if validation map
    data$popup <- paste0(
      "<div>\n  <strong>Location ID: </strong>\n  ",
      data$locnumber, "\n  <br/>\n  <strong>TIV: </strong>\n    ",
      tiv$total,"\n  <br/>\n  <strong>Street Address: </strong>\n  ",
      data$streetaddress, "\n  <br/>\n  <strong>Postal code: </strong>\n  ",
      data$postalcode, "\n  <br/>\n  <strong>Error message: </strong>\n  ",
      error_msg, "\n</div>")
  } else {
    data$popup <- paste0(
      "<div>\n  <strong>Location ID: </strong>\n  ",
      data$locnumber, "\n  <br/>\n  <strong>TIV: </strong>\n    ",
      tiv$total,"\n  <br/>\n  <strong>Street Address: </strong>\n  ",
      data$streetaddress, "\n  <br/>\n  <strong>Postal code: </strong>\n  ",
      data$postalcode, "\n</div>"
    )
  }
  data
}


# Extract error messages
.keys_errors_msg <- function(data, session, paramID) {
  keys_success <- session$userData$data_hub$get_ana_success_summary_content(paramID)
  keys_errors <- session$userData$data_hub$get_ana_errors_summary_content(id = paramID)
  # unify names cases for merging
  names(keys_success) <- tolower(names(keys_success))
  names(keys_errors) <- tolower(names(keys_errors))

  # note below may throw a "Warning: Unknown or uninitialised column: `detail`."
  if (!is.null(keys_errors) && is.null(keys_errors$detail) && !is.null(keys_success)) {
    if (length(as.numeric(keys_errors$locid)) > 0) {
      key_chain <- left_join(keys_success, keys_errors, by = c("loc_id" = "locid"))
      message <- group_by(key_chain, loc_id) %>%
        summarize(message = paste(paste(perilid, ":", message), collapse = " / "))
      if (grepl("locnumber.x", names(data))) {
        names(data) <- gsub(".x", "", names(data))
      }
      data$locnumber <- as.character(data$locnumber)
      message$loc_id <- as.character(message$loc_id)
      errmessage <- left_join(data, message, by = c("locnumber" = "loc_id"))$message
    } else {
      errmessage <- rep_len(NA, nrow(data))
    }
  } else {
    errmessage <- rep_len(NA, nrow(data))
  }
  errmessage
}

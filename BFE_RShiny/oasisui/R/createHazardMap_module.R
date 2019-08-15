# Mock onclick() with plain observeEvent(). We only use it on buttons and
# shinyjs::onclick() leads to performance issues for the version we use (fixed
# in daattali/shinyjs PR #190)
onclick <- function(id, expr) {
  parentFrame <- parent.frame(1)
  session <- shinyjs:::getSession()
  expr <- substitute(expr)
  shiny::observeEvent(session$input[[id]], {
    eval(expr, envir = parentFrame)
  })
  invisible(NULL)
}

# Create Hazard Map module -----------------------------------------------------------

# Shared Module documentation --------------------------------------------------
#' Hazard Map Module
#'
#' Shiny Module for rendering a hazard map using Leaflet.
#'
#' @template params-module
#'
#' @examples # TODO
#'
#' @name createHazardMap
NULL

# UI ---------------------------------------------------------------------------
#' @describeIn createHazardMap Returns the UI elements of the module.
#'
#' @export
createHazardMapUI <- function(id) {
  ns <- NS(id)

  tagList(
    shinycssloaders::withSpinner(
      leafletOutput(ns("hazardmap")),
      # style and color can be set as options used by all spinners
      color = "#bb252c"
    )
    # fluidRow(
    #   sliderInput(ns("contrast_colors"), "Adjust color contrast", min = 0.1, max = 1, value = 0.5)
    # )
  )
}

# Server -----------------------------------------------------------------------
#' @param map_data Reactive expression yielding Leaflet map data.
#' @param pins Reactive expression yielding location pins as a \code{data.frame}
#'   with variables \code{locnumber}, \code{longitude}, \code{latitude}
#'   (case-insensitive).
#'
#' @describeIn createHazardMap Defines the server logic of the module.
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
                            map_data,
                            pins) {

  marker_data <- reactive({
    .build_marker_data(pins())
  })

  # Plot leaflet
  output$hazardmap <- renderLeaflet({
    # prevent rendering when map_data is unavailable ("falsy"), we would see an
    # error otherwise (if we would not have the spinner).
    req(map_data())
    logMessage("building the map...")
    map <- .buildHazardMap(map_data(), marker_data())
    logMessage("map built")
    map
  })

  # Create marker data
  .build_marker_data <- function(pins) {
    data <- pins
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

  # Create map
  .buildHazardMap <- function(map_data, marker_data) {

    # Create map color palette
    pal <- colorNumeric("Reds", NULL)

    # Create custom icons
    icon_map <- awesomeIcons(
      icon = 'map-marker-alt',
      library = 'fa',
      iconColor = 'green',
      markerColor = 'blue'
    )

    hazardmap <- leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(color = ~pal(ReturnLevel),
                  fillColor = ~pal(ReturnLevel),
                  opacity = 0.8,
                  fillOpacity = 0.5) %>%
      addLegend(position = "topright",
                pal = pal,
                values = ~ReturnLevel) %>%
      addAwesomeMarkers(lng = marker_data$longitude,
                        lat = marker_data$latitude,
                        icon = icon_map,
                        popup = marker_data$popup,
                        clusterOptions = TRUE)
    hazardmap
  }

}

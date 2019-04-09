# Create Hazard Map module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' createHazardMap
#'
#' @rdname createHazardMap
#'
#' @description Creates a hazard map using leaflet.
#'
#' @param file File to plot as map.
#'
#' @export
createHazardMapUI <- function(id) {
  ns <- NS(id)

  tagList(
    leafletOutput(ns("hazardmap")),
    fluidRow(
      column(5,
             radioButtons(ns("legend"), "Move legend", choices = c("top right",
                                                                   "top left",
                                                                   "bottom right",
                                                                   "bottom left")),
             radioButtons(ns("map_colors"), "Choose map colors", choices = c("Reds",
                                                                             "Blues",
                                                                             "Greens"))
      ),
      column(5,
             radioButtons(ns("map_style"), "Choose map style", choices = c("Street view",
                                                                           "Portrait",
                                                                           "Black and white",
                                                                           "Basic")),
             radioButtons(ns("pin_colors"), "Choose pin color", choices = c("Red",
                                                                            "Blue",
                                                                            "Green"))
      ),
      sliderInput(ns("contrast_colors"), "Adjust color contrast", min = 0.1, max = 1, value = 0.5)
    )
  )
}

# Server -----------------------------------------------------------------------
#' Create Hazard Map
#'
#' @rdname createHazardMap
#'
#' @description Creates a hazard map using leaflet.
#'
#' @return Leaflet map.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet colorNumeric
#' @importFrom leaflet awesomeIcons
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet addGeoJSON
#' @importFrom leaflet addRectangles
#' @importFrom leaflet addLegend
#' @importFrom leaflet addAwesomeMarkers
#'
#' @export
createHazardMap <- function(input, output, session,
                           file) {

  ns <- session$ns

  # Isolate coordinates and Return Level
  i <- 1:max(file$features$id)
  coor <- sapply(i, function(x) {file$features$geometry$coordinates[[x]]})
  col <- sapply(i, function(x) {file$features$properties[1][[1]][x]})

  # Find maximum negative (lng) number and minimum (lat) values
  pos_min <- function(x) {min(x[x > 0])}
  neg_max <- function(x) {max(x[x < 0])}

  # Set up lng and lat for pins
  pin_lng <- c(file$features$geometry$coordinates[[78]][2],
               file$features$geometry$coordinates[[1000]][2],
               file$features$geometry$coordinates[[3000]][2],
               file$features$geometry$coordinates[[6000]][2]
  )

  pin_lat <- c(file$features$geometry$coordinates[[78]][7],
               file$features$geometry$coordinates[[1000]][7],
               file$features$geometry$coordinates[[3000]][7],
               file$features$geometry$coordinates[[6000]][7]
  )

  # Plot leaflet
  output$hazardmap <- renderLeaflet({
    .buildHazardMap(file)
  })

  # Create map
  .buildHazardMap <- function(df) {

    # Create map color palette
    pal <- colorNumeric(input$map_colors, col)

    # Create custom icons
    icon <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = tolower(input$pin_colors)
    )

    # Customize map
    if (input$map_style == "Street view") {
      style <- leaflet::providers$OpenStreetMap
    } else if (input$map_style == "Portrait") {
      style <- leaflet::providers$Esri.NatGeoWorldMap
    } else if (input$map_style == "Black and white") {
      style <- leaflet::providers$Stamen.Toner
    } else if (input$map_style == "Basic") {
      style <- leaflet::providers$Esri.WorldShadedRelief
    }

    leaflet(df) %>%
      addProviderTiles(style) %>%
      addGeoJSON(df) %>%
      addRectangles(lng1 = apply(coor, 2, min),
                    lat1 = apply(coor, 2, max),
                    lng2 = apply(coor, 2, neg_max),
                    lat2 = apply(coor, 2, pos_min),
                    color = "transparent",
                    fillColor = pal(col),
                    fillOpacity = input$contrast_colors) %>%
      addLegend(position = gsub("\\s+", "", input$legend),
                pal = pal,
                values = col) %>%
      addAwesomeMarkers(lng = pin_lng,
                        lat = pin_lat,
                        icon = icon)
  }
}

# Exposure Validation Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' exposurevalidationmapUI
#' @rdname exposurevalidationmap
#'
#' @description UI/View for exposure validation of an analysis.
#'
#' @return List of tags.
#'
#' @importFrom leaflet leafletOutput
#' @importFrom shinyjs hidden
#'
#' @export
exposurevalidationmapUI <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(div(flamingoRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;")),
    fluidRow(
      leafletOutput(ns("exposure_map")),
      hidden(div(id = ns("div_abuttonviewtbl"), flamingoButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;")))
    )
  )
}


# Server -----------------------------------------------------------------------

#' exposurevalidationmap
#'
#' @rdname exposurevalidationmap
#'
#' @description Server logic for exposure validation of an analysis.
#'
#' @template params-module
#' @param analysisID Selected analysis id.
#' @param portfolioID selected portfolio ID.
#' @param counter Reactive value storing actionButton status.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom DT formatStyle
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom DT styleEqual
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addAwesomeMarkers
#' @importFrom leaflet markerClusterOptions
#' @importFrom leaflet awesomeIcons
#' @importFrom leaflet renderLeaflet
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#'
#' @export
exposurevalidationmap <- function(input,
                               output,
                               session,
                               analysisID = "",
                               portfolioID = "",
                               counter = NULL,
                               active = reactive(TRUE)) {

  ns <- session$ns

  # Params and Reactive Values -------------------------------------------------
  result <- reactiveValues(
    #dataframe of checked exposures
    uploaded_locs_check = NULL
  )

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active()) {
        .reloadExposureValidation()
    }
  })

  # Show/Hide table button -----------------------------------------------------

  observe({
    hide("div_abuttonviewtbl")
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      show("div_abuttonviewtbl")
    }
  })


  # Modal for tabular view  ----------------------------------------------------
  # Modal Panel
  FileContent <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      fluidRow(
        h4("Validated Exposure")),
      fluidRow(
        DTOutput(ns("dt_output_uploaded_lock_check")),
        downloadButton(ns("exp_downloadexcel"), label = "Export to csv"),
        style = "display: inline")
    )
  )

  onclick("abuttonviewtbl", {
    showModal(FileContent)
  })

  # Export to .csv -------------------------------------------------------------

  output$exp_downloadexcel <- downloadHandler(
    # Filename to download
    filename2download <- paste0("exposure_validation_", analysisID(), ".csv"),
    filename = function() {filename2download},
    content = function(file) {
      fwrite(result$uploaded_locs_check, file, row.names = TRUE, quote = TRUE)}
  )

  # Tabular data ---------------------------------------------------------------
  output$dt_output_uploaded_lock_check <- renderDT(
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      datatable(
        result$uploaded_locs_check %>% capitalize_names_df(),
        class = "flamingo-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = .getFLTableOptions()
      ) %>% formatStyle(
        'Modeled',
        target = 'row',
        backgroundColor = styleEqual(levels = c("TRUE", "FALSE"), c('#D4EFDF', '#FADBD8')) # #D4EFDF - limegreen; #FADBD8 - red
      )
    } else {
      .nothingToShowTable("Generated inputs not found.")
    }
  )

  # Map ------------------------------------------------------------------------
  output$exposure_map <- renderLeaflet({
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      .createExposureValMap(result$uploaded_locs_check)
    } else {
      NULL
    }

  })

  # Refresh button -------------------------------------------------------------
  onclick("abuttonexposurerefresh", {
    # Get modeled locations
    withModalSpinner(
      api_get_analyses_input_file(analysisID()),
      "Refreshing...",
      size = "s"
    )
    .reloadExposureValidation()
  })

  # Utils functions ------------------------------------------------------------

  #dummy for exposure location comparison
  .reloadExposureValidation <- function(){

    logMessage(".reloadExposureValidation called")

    uploaded_locs_check <- check_loc(analysisID(), portfolioID())

    #updating reactive only when needed
    if (!identical(uploaded_locs_check,result$uploaded_locs_check)) {
      result$uploaded_locs_check <- uploaded_locs_check
    }

  }

  # default table options
  .getFLTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      processing = 0,
      scrollX = TRUE,
      pageLength = 5
    )
    return(options)
  }

  #empty table
  .nothingToShowTable <- function(contentMessage){
    datatable(
      data.frame(content = contentMessage),
      class = "flamingo-table display",
      selection = "none",
      rownames = FALSE,
      #filter = 'bottom',
      colnames = c(""),
      escape = FALSE,
      options = list(searchHighlight = TRUE)
    )
  }


  # Exposure validation map
  .createExposureValMap <- function(df) {

    marker_colors <- c('green', 'red')

    df <- df %>%
      mutate(modeled = case_when(
        modeled == "TRUE" ~ 1,
        TRUE ~ 2
      ))

    popupData <- tagList(
      strong("Location ID: "), df$LocNumber,
      br(), strong("Latitude: "), df$Latitude,
      br(), strong("Longitude: "), df$Longitude)

    icon_map <- awesomeIcons(
      icon = 'map-marker-alt',
      library = 'fa',
      iconColor = marker_colors[df$modeled],
      markerColor =  marker_colors[df$modeled]
    )

    leaflet(df) %>%
      addTiles() %>%
      addAwesomeMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        icon = icon_map,
        clusterOptions = TRUE,
        popup = toString(popupData))
  }

}

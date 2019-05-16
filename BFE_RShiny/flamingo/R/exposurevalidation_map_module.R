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
      column(1,
             checkboxGroupInput(inputId =ns("chkgrp_perils"), label = "Pick peril", choices = c("WTC"))
             ),
      column(11,
             leafletOutput(ns("exposure_map")),
             hidden(div(id = ns("div_abuttonviewtbl"), flamingoButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;")))
             )
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
#' @template params-active
#' @param analysisID Selected analysis id.
#' @param counter Reactive value to trigger inputs download.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
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
                                  counter = reactive(NULL),
                                  active = reactive(TRUE)) {

  ns <- session$ns

  # Params and Reactive Values -------------------------------------------------
  result <- reactiveValues(
    #dataframe of checked exposures
    uploaded_locs_check = NULL,
    #dataframe of checked exposures by perils
    uploaded_locs_check_peril = NULL
  )

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    counter()
    active()
  }, {
    if (length(active()) > 0 && active() && counter() > 0) {
      .reloadExposureValidation()
      perils <- result$uploaded_locs_check$peril[!is.na(result$uploaded_locs_check$peril)] %>%
        unique()
      logMessage("Updating input$chkgrp_perils")
      updateCheckboxGroupInput(session, inputId = "chkgrp_perils", choices = perils, selected = perils)
    }
  })

  observeEvent(input$chkgrp_perils, ignoreNULL = FALSE, {
    if (any(input$chkgrp_perils != "")) {
      result$uploaded_locs_check_peril <- result$uploaded_locs_check %>%
        left_join(
          data.frame(
            LocNumber = unique( result$uploaded_locs_check$LocNumber),
            modelled = sapply(unique( result$uploaded_locs_check$LocNumber), function(x){
              curr_loc <- result$uploaded_locs_check %>%
                filter(LocNumber == x)
              any(curr_loc$peril_id %in% "WSS")#input$chkgrp_perils)
            })
          )
        ) %>%
      select(-peril_id) %>%
        distinct()
    } else {
      result$uploaded_locs_check_peril <- NULL
    }
  })

  # Show/Hide table button -----------------------------------------------------

  observeEvent(result$uploaded_locs_check_peril, ignoreNULL = FALSE, {
    if (!is.null(result$uploaded_locs_check_peril) && nrow(result$uploaded_locs_check_peril) > 0) {
      show("div_abuttonviewtbl")
    } else {
      hide("div_abuttonviewtbl")
    }
  })


  # Modal for tabular view  ----------------------------------------------------
  # Modal Panel
  FileContent <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      fluidRow(
        h4("Validated Exposure by Peril")),
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
      fwrite(result$uploaded_locs_check_peril, file, row.names = TRUE, quote = TRUE)}
  )

  # Tabular data ---------------------------------------------------------------
  output$dt_output_uploaded_lock_check <- renderDT(
    if (!is.null(result$uploaded_locs_check_peril) && nrow(result$uploaded_locs_check_peril) > 0) {
      datatable(
        result$uploaded_locs_check_peril %>%
          capitalize_names_df(),
        class = "flamingo-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = getTableOptions()
      ) %>% formatStyle(
        'Modeled',
        target = 'row',
        backgroundColor = styleEqual(levels = c("TRUE", "FALSE"), c('#D4EFDF', '#FADBD8')) # #D4EFDF - limegreen; #FADBD8 - red
      )
    } else {
      nothingToShowTable("Generated inputs not found")
    }
  )

  # Map ------------------------------------------------------------------------
  output$exposure_map <- renderLeaflet({
    if (!is.null(result$uploaded_locs_check_peril) && nrow(result$uploaded_locs_check_peril) > 0) {
      .createExposureValMap(result$uploaded_locs_check_peril)
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

    uploaded_locs_check <- check_loc(analysisID()) %>%
      distinct()

    #updating reactive only when needed
    if (!identical(uploaded_locs_check,result$uploaded_locs_check)) {
      result$uploaded_locs_check <- uploaded_locs_check
    }

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

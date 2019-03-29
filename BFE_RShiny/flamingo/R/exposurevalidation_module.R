# Exposure Validation Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' exposurevalidationUI
#' @rdname exposurevalidation
#'
#' @description UI/View for exposure validation of an analysis.
#'
#' @return List of tags.
#'
#' @export
exposurevalidationUI <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(div(flamingoRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;")),
    fluidRow(
      column(6,
             DTOutput(ns("dt_summary_validation"))
      ),
      column(6,
             leafletOutput(ns("exposure_map")),
             hidden(div(id = ns("div_abuttonviewtbl"), flamingoButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;")))
      )
    )
  )
}


# Server -----------------------------------------------------------------------

#' exposurevalidation
#'
#' @rdname exposurevalidation
#'
#' @description Server logic for exposure validation of an analysis.
#'
#' @importFrom data.table fread
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
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
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#'
#' @export
exposurevalidation <- function(input,
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
    uploaded_locs_check = NULL,
    #dataframe summary
    summary_validation_tbl = NULL,
    #filename for export
    filename2download = ""
  )

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if ((!is.null(portfolioID()) && !is.na(portfolioID()) && portfolioID() != "") &&
        (!is.null(analysisID()) && !is.na(analysisID()) && analysisID() != "")) {
      # Get modeled locations
      extractFolder <- set_extractFolder(analysisID(), label = "_inputs/")
      if (!file.exists(extractFolder) && is.na(file.size(extractFolder))) {
        api_get_analyses_input_file(analysisID())
      }
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

  # Summary table --------------------------------------------------------------

  observeEvent(result$uploaded_locs_check,{
    .reloadSummary()
  })

  output$dt_summary_validation <- renderDT(
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl) > 0) {
      datatable(
        result$summary_validation_tbl,
        class = "flamingo-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = .getFLTableOptions()
      )
    } else {
      .nothingToShowTable("Exposure validation summary not found.")
    }
  )


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
    filename = function() {result$filename2download},
    content = function(file) {
      fwrite(result$uploaded_locs_check, file, row.names = TRUE, quote = TRUE)}
  )

  # Tabular data ---------------------------------------------------------------
  output$dt_output_uploaded_lock_check <- renderDT(
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      datatable(
        result$uploaded_locs_check,
        class = "flamingo-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = .getFLTableOptions()
      ) %>% formatStyle(
        'modeled',
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
    api_get_analyses_input_file(analysisID())
    .reloadExposureValidation()
  })

  # Utils functions ------------------------------------------------------------

  # reload dataframes

  #dummy for summary table
  .reloadSummary <- function(.reloadSummary){

    logMessage(".reloadExposureValidation called")

    # Clean up df
    result$summary_validation_tbl <- NULL

    #Build df
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      modelled_location <- result$uploaded_locs_check %>%
        filter(modeled == TRUE) %>%
        nrow()
      dropped_location <- result$uploaded_locs_check %>%
        filter(modeled == FALSE) %>%
        nrow()
      uploaded_location <- nrow(result$uploaded_locs_check)
      result$summary_validation_tbl <- data.frame(
        param = c("num location"),
        input = c(uploaded_location),
        modeled = c(modelled_location),
        dropped = c(dropped_location)
      )
    } else {
      result$summary_validation_tbl <- NULL
    }


  }

  #dummy for exposure location comparison
  .reloadExposureValidation <- function(){

    logMessage(".reloadExposureValidation called")

    # Filename to download
    result$filename2download <- paste0("exposure_validation_", analysisID(), ".csv")

    # Clean up df
    result$uploaded_locs_check <- NULL

    # Get uploaded locations
    uploaded_locs <- return_file_df(api_get_portfolios_location_file, portfolioID())

    extractFolder <- set_extractFolder(analysisID(), label = "_inputs/")
    fileslist <- list.files(extractFolder)
    modeled_loc_filename <- fileslist[order(nchar(fileslist), fileslist, decreasing = TRUE)][1]
    if (!is.na(modeled_loc_filename)) {
      currfilepath <- paste0(extractFolder, modeled_loc_filename)
      modeled_locs <- fread(currfilepath)
      # Hack: drop LocName as it seems to cause issues in the inner_join
      modeled_locs <- modeled_locs %>%
        select(-LocName)

      # compare uploaded locations with modeled locations
      idx_in <- inner_join(uploaded_locs, modeled_locs) %>%
        select(LocNumber)

      uploaded_locs_check <- uploaded_locs %>%
        mutate(modeled = case_when(LocNumber %in% idx_in$LocNumber ~ "TRUE",
                                   TRUE ~ "FALSE"))

      #updating reactive only when needed
      if (!identical(uploaded_locs_check,result$uploaded_locs_check)) {
        result$uploaded_locs_check <- uploaded_locs_check
      }
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
        #clusterOptions = TRUE,
        popup = toString(popupData))
  }

}

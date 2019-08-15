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
    fluidRow(div(oasisuiRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;")),
    fluidRow(
      column(1,
             checkboxGroupInput(inputId = ns("chkgrp_perils"), label = "Pick peril")
      ),
      column(11,
             leafletOutput(ns("exposure_map")),
             hidden(div(id = ns("div_abuttonviewtbl"), oasisuiButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;")))
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
#' @param portfolioID Selected portfolio ID.
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
#' @importFrom htmlwidgets onRender
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
                                  analysisID = reactive(NULL),
                                  portfolioID = reactive(""),
                                  counter = reactive(NULL),
                                  active = reactive(TRUE)) {

  ns <- session$ns

  # Params and Reactive Values -------------------------------------------------
  result <- reactiveValues(
    #dataframe of checked exposures
    uploaded_locs_check = NULL,
    #dataframe of checked exposures by perils
    uploaded_locs_check_peril = NULL,
    # peril codes
    perils_codes = NULL,
    #perils names
    perils_names = NULL
  )

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    counter()
    active()
  }, {
    if (length(active()) > 0 && active() && !is.null(analysisID())) {
      .reloadExposureValidation()
      perils <- result$uploaded_locs_check$peril[!is.na(result$uploaded_locs_check$peril)] %>%
        unique()
      mapping <- session$userData$data_hub$get_oed_peril_codes_mapping()
      mapped_perils <- lapply(mapping[perils], function(x) {x[["desc"]]})
      result$perils_names <- unlist(mapped_perils,  use.names = FALSE)
      result$peril_codes <- names(mapped_perils)
      logMessage("Updating input$chkgrp_perils")
      updateCheckboxGroupInput(session, inputId = "chkgrp_perils", choices = result$perils_names, selected = result$perils_names)
    }
  })

  observeEvent(input$chkgrp_perils, ignoreNULL = FALSE, {
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      result$uploaded_locs_check_peril <- result$uploaded_locs_check %>%
        left_join(
          data.frame(
            LocNumber = unique( result$uploaded_locs_check$LocNumber),
            modeled = sapply(unique( result$uploaded_locs_check$LocNumber), function(x){
              curr_loc <- result$uploaded_locs_check %>%
                filter(LocNumber == x)
              any(curr_loc$peril_id %in% result$peril_codes[which(result$perils_names == input$chkgrp_perils)])
            })
          )
        ) %>%
        select(-peril_id) %>%
        distinct()
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
      fwrite(result$uploaded_locs_check_peril, file, row.names = FALSE, quote = TRUE)}
  )

  # Tabular data ---------------------------------------------------------------
  output$dt_output_uploaded_lock_check <- renderDT(
    if (!is.null(result$uploaded_locs_check_peril) && nrow(result$uploaded_locs_check_peril) > 0) {
      datatable(
        result$uploaded_locs_check_peril %>%
          capitalize_names_df() %>%
          mutate(Modeled = as.character(Modeled)),
        class = "oasisui-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = getTableOptions()
      ) %>% formatStyle(
        'Modeled',
        target = 'row',
        backgroundColor = styleEqual(levels = c("TRUE", "FALSE"), values = c('#D4EFDF', '#FADBD8')) # #D4EFDF - limegreen; #FADBD8 - red
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
      .reloadExposureValidation(),
      "Refreshing...",
      size = "s"
    )
  })

  # Utils functions ------------------------------------------------------------

  #dummy for exposure location comparison
  .reloadExposureValidation <- function(){

    logMessage(".reloadExposureValidation called")

    uploaded_locs_check <- check_loc(analysisID(), portfolioID(), data_hub = session$userData$data_hub)

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

    # popupData <- tagList(
    #   strong("Location ID: "), df$LocNumber,
    #   br(), strong("Latitude: "), df$Latitude,
    #   br(), strong("Longitude: "), df$Longitude)

    df <- build_marker_data(df)

    icon_map <- awesomeIcons(
      icon = 'map-marker-alt',
      library = 'fa',
      iconColor = marker_colors[df$modeled],
      markerColor =  marker_colors[df$modeled]
    )

    # color clusters red if any mark is red, and green in fall marks are green.
    # Reference https://stackoverflow.com/questions/47507854/coloring-clusters-by-markers-inside
    leaflet(df) %>%
      addTiles() %>%
      addAwesomeMarkers(
        lng = df$longitude,
        lat = df$latitude,
        icon = icon_map,
        clusterOptions = markerClusterOptions(),
        group = "clustered",
        clusterId = "cluster",
        popup = ~df$popup) %>%
      onRender("function(el,x) {
                            map = this;

                            var style = document.createElement('style');
                            style.type = 'text/css';
                            style.innerHTML = '.red, .red div { background-color: rgba(255,0,0,0.6); }'; // set both at the same time
                            document.getElementsByTagName('head')[0].appendChild(style);


                            var cluster = map.layerManager.getLayer('cluster','cluster');
                            cluster.options.iconCreateFunction = function(c) {
                            var markers = c.getAllChildMarkers();
                            var priority = {
                            'green': 0,
                            'red': 1,
                            'red': 2
                            };
                            var highestRank = 0; // defaults to the lowest level to start

                            markers.forEach(function(m) {
                            var color = m.options.icon.options.markerColor;

                            // check each marker to see if it is the highest value
                            if(priority[color] > highestRank) {
                            highestRank = priority[color];
                            }
                            })

                            var styles = [
                            'marker-cluster-small', // green
                            'marker-cluster-large',  // red
                            'red' // red
                            ]

                            var style = styles[highestRank];
                            var count = markers.length;

                            return L.divIcon({ html: '<div><span>'+count+'</span></div>', className: 'marker-cluster ' + style, iconSize: new L.Point(40, 40) });
                            }
  }")

  }

}

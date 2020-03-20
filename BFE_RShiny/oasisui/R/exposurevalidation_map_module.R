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
    fluidRow(#column(3, numericInput(ns("insert_radius"), "Radius (m)", value = NULL)),
             column(12,
                    div(oasisuiRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;"))),
    fluidRow(
      column(1,
             checkboxGroupInput(inputId = ns("chkgrp_perils"), label = "Pick peril")
      ),
      column(10,
             leafletOutput(ns("exposure_map")),
             hidden(div(id = ns("div_abuttonviewtbl"), oasisuiButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;")))
      )
    ),
    fluidRow(
      column(2, hidden(tableOutput(ns("radius_circle")))),
      column(5, hidden(tableOutput(ns("exposure_circle"))))
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
#' @importFrom dplyr between
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
#' @importFrom leaflet addLayersControl
#' @importFrom leaflet layersControlOptions
#' @importFrom leaflet.extras addFullscreenControl
#' @importFrom leaflet.extras addDrawToolbar
#' @importFrom leaflet.extras editToolbarOptions
#' @importFrom leaflet.extras selectedPathOptions
#' @importFrom leaflet.extras drawRectangleOptions
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom geosphere destPoint
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
    # dataframe of checked exposures
    uploaded_locs_check = NULL,
    # dataframe of checked exposures by perils
    uploaded_locs_check_peril = NULL,
    # peril codes
    perils_codes = NULL,
    # perils names
    perils_names = NULL,
    # radius of drawn circle
    circle_radius = NULL
  )

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    counter()
    active()
  }, {
    if (length(active()) > 0 && active() && counter() > 0 && !is.null(analysisID())) {
      .reloadExposureValidation()
      perils <- result$uploaded_locs_check$peril[!is.na(result$uploaded_locs_check$peril)] %>%
        unique()
      mapping <- session$userData$data_hub$get_oed_peril_codes_mapping()
      mapped_perils <- lapply(mapping[perils], function(x) {x[["desc"]]})
      result$perils_names <- unlist(mapped_perils,  use.names = FALSE)
      result$peril_codes <- names(mapped_perils)
      logMessage("Updating input$chkgrp_perils")
      updateCheckboxGroupInput(session, inputId = "chkgrp_perils", choices = as.list(perils), selected = as.list(perils))
    }
  })

  observeEvent(input$chkgrp_perils, ignoreNULL = FALSE, {
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      if (is.null(input$chkgrp_perils)) {
        result$uploaded_locs_check_peril <- result$uploaded_locs_check %>%
          mutate(modeled = NA)
      } else {
        result$uploaded_locs_check_peril <- result$uploaded_locs_check %>%
          filter(peril_id %in% input$chkgrp_perils) %>%
          mutate(modeled = case_when(
            is.na(peril_id) ~ FALSE,
            peril_id %in% input$chkgrp_perils ~ TRUE,
            TRUE ~ NA)
          ) %>%
          filter(!is.na(modeled)) %>%
          select(-peril_id) %>%
          distinct()
      }
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

  observeEvent(input$abuttonviewtbl, {
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

  # observeEvent(input$exposure_map_click, {
  #   show("exposure_circle")
  #   show("radius_circle")
  # })

  observeEvent(input$exposure_map_click$lat, {
    # find distance between cliked point and closest radius, use that radius to output table
    p_1 <- cbind(input$exposure_map_click$lng, input$exposure_map_click$lat)
    dist_click <- unlist(lapply(seq(1, length(input$exposure_map_draw_all_features$features)), function(x) {
      p_2 <- cbind(input$exposure_map_draw_all_features$features[[x]]$geometry$coordinates[[1]],
                   input$exposure_map_draw_all_features$features[[x]]$geometry$coordinates[[2]])
      geosphere::distm(p_1, p_2)
    }))
    min_dist <- grep(min(dist_click), dist_click)
    radius <- input$exposure_map_draw_all_features$features[[min_dist]]$properties$radius
    lat_click <- input$exposure_map_draw_all_features$features[[min_dist]]$geometry$coordinates[[2]]
    long_click <- input$exposure_map_draw_all_features$features[[min_dist]]$geometry$coordinates[[1]]
    output$exposure_circle <- renderTable({.DrawnCircles(radius, lat_click, long_click)})
    output$radius_circle <- renderTable({.showRadius(radius)})
    show("exposure_circle")
    show("radius_circle")
  })

  # Refresh button -------------------------------------------------------------
  observeEvent(input$abuttonexposurerefresh, {
    # Get modeled locations
    withModalSpinner(
      .reloadExposureValidation(),
      "Refreshing...",
      size = "s", t = 0.5
    )
  })

  # Insert new radius for circle -----------------------------------------------
  observeEvent(input$insert_radius, {
    result$circle_radius <- input$insert_radius
  })

  # Utils functions ------------------------------------------------------------
  # dummy for exposure location comparison
  .reloadExposureValidation <- function() {
    logMessage(".reloadExposureValidation called")
    uploaded_locs_check <- check_loc(analysisID(), portfolioID(), data_hub = session$userData$data_hub)
    # updating reactive only when needed
    if (!identical(uploaded_locs_check,result$uploaded_locs_check)) {
      result$uploaded_locs_check <- uploaded_locs_check
    }
    invisible()
  }

  # Exposure validation map
  .createExposureValMap <- function(df) {
    marker_colors <- c('green', 'red')

    if (is.null(input$chkgrp_perils)) {
      icon_map <- NULL
      df <- df
      leaflet(df) %>%
        addTiles() %>%
        leaflet::setView(mean(df$Longitude), mean(df$Latitude), zoom = 20)
    } else {
      df <- df %>%
        mutate(modeled = case_when(
          modeled == "TRUE" ~ 1,
          TRUE ~ 2
        )) %>%
        build_marker_data(session = session, paramID = analysisID(), step = "Validation Map")

      icon_map <- awesomeIcons(
        icon = 'map-marker-alt',
        library = 'fa',
        iconColor = marker_colors[df$modeled],
        markerColor = marker_colors[df$modeled]
      )

      # color clusters red if any mark is red, and green if all marks are green.
      # Reference https://stackoverflow.com/questions/47507854/coloring-clusters-by-markers-inside
      leaflet(df) %>%
        addTiles() %>%
        addAwesomeMarkers(
          lng = ~longitude,
          lat = ~latitude,
          icon = icon_map,
          clusterOptions = markerClusterOptions(),
          group = "clustered",
          clusterId = "cluster",
          popup = ~popup) %>%
        addDrawToolbar(
          targetGroup = 'draw',
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          rectangleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          circleOptions = TRUE,
          singleFeature = FALSE,
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        )  %>%
        addLayersControl(overlayGroups = c('draw'),
                         options = layersControlOptions(collapsed = FALSE)) %>%
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
      }") %>% # make map full screen
        addFullscreenControl(pseudoFullscreen = TRUE)
    }
  }

  # Drawn circles infos, outputs and radius
  .DrawnCircles <- function(radius = NULL, lat_click = NULL, long_click = NULL) {

    #get pins coordinates
    long <- result$uploaded_locs_check_peril$Longitude
    lat <- result$uploaded_locs_check_peril$Latitude

    p <- cbind(long_click, lat_click)

    # #get coordinates of the radius
    # if(is.null(input$exposure_map_draw_edited_features)) {
    #   p <- cbind(input$exposure_map_draw_new_feature$geometry$coordinates[[1]],
    #              input$exposure_map_draw_new_feature$geometry$coordinates[[2]])
    #   radius <- input$exposure_map_draw_new_feature$properties$radius
    # } else {
    #   p <- cbind(input$exposure_map_draw_edited_features$features[[1]]$geometry$coordinates[[1]],
    #              input$exposure_map_draw_edited_features$features[[1]]$geometry$coordinates[[2]])
    #   radius <- input$exposure_map_draw_edited_features$features[[1]]$properties$radius
    # }

    # if (is.null(radius) || is.na(radius)) {
    #   radius <- input$exposure_map_draw_new_feature$properties$radius
    # }

    # calculate two rectangles within circle area
    degrees_dist_1 <- c(90, 180, 270, 360)
    circle_bounds_1 <- geosphere::destPoint(p, degrees_dist_1, radius)

    degrees_dist_2 <- c(135, 225, 315, 405)
    circle_bounds_2 <- geosphere::destPoint(p, degrees_dist_2, radius)

    # calculate LocID and TIV for pins inside areas
    locID_list <- unlist(lapply(seq_len(length(result$uploaded_locs_check_peril$Longitude)), function(x) {
      if ((between(long[x], min(circle_bounds_1[ ,1]), max(circle_bounds_1[ ,1])) &&
           between(lat[x], min(circle_bounds_1[ ,2]), max(circle_bounds_1[ ,2]))) ||
          (between(long[x], min(circle_bounds_2[ ,1]), max(circle_bounds_2[ ,1])) &&
           between(lat[x], min(circle_bounds_2[ ,2]), max(circle_bounds_2[ ,2])))) {
        result$uploaded_locs_check_peril$LocNumber[x]
      }
    }))

    tiv_list <- unlist(lapply(seq_len(length(result$uploaded_locs_check_peril$Longitude)), function(x) {
      if ((between(long[x], min(circle_bounds_1[ ,1]), max(circle_bounds_1[ ,1])) &&
           between(lat[x], min(circle_bounds_1[ ,2]), max(circle_bounds_1[ ,2]))) ||
          (between(long[x], min(circle_bounds_2[ ,1]), max(circle_bounds_2[ ,1])) &&
           between(lat[x], min(circle_bounds_2[ ,2]), max(circle_bounds_2[ ,2])))) {
        add_commas(result$uploaded_locs_check_peril$BuildingTIV[x])
      }
    }))

    street_address <- unlist(lapply(seq_len(length(result$uploaded_locs_check_peril$Longitude)), function(x) {
      if ((between(long[x], min(circle_bounds_1[ ,1]), max(circle_bounds_1[ ,1])) &&
           between(lat[x], min(circle_bounds_1[ ,2]), max(circle_bounds_1[ ,2]))) ||
          (between(long[x], min(circle_bounds_2[ ,1]), max(circle_bounds_2[ ,1])) &&
           between(lat[x], min(circle_bounds_2[ ,2]), max(circle_bounds_2[ ,2])))) {
        result$uploaded_locs_check_peril$StreetAddress[x]
      }
    }))

      data.frame(LodID = locID_list,
                 TIV = tiv_list,
                 `Street Address` = street_address)
  }

  .showRadius <- function(radius = NULL) {
    # if(is.null(input$exposure_map_draw_edited_features)) {
    #   p <- cbind(input$exposure_map_draw_new_feature$geometry$coordinates[[1]],
    #              input$exposure_map_draw_new_feature$geometry$coordinates[[2]])
    #   radius <- input$exposure_map_draw_new_feature$properties$radius
    # } else {
    #   p <- cbind(input$exposure_map_draw_edited_features$features[[1]]$geometry$coordinates[[1]],
    #              input$exposure_map_draw_edited_features$features[[1]]$geometry$coordinates[[2]])
    #   radius <- input$exposure_map_draw_edited_features$features[[1]]$properties$radius
    # }

    data.frame(`radius (m)` = add_commas(round(radius, 3)))
  }

  invisible()
}

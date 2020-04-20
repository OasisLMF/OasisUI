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
    fluidRow(
      column(3,
             selectInput(ns("tot_tiv_param"), label = "Choose TIV by", choices = c("Circles", "Countries"))),
      column(9,
             div(oasisuiRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;"))),
    fluidRow(
      column(1,
             checkboxGroupInput(inputId = ns("chkgrp_perils"), label = "Pick peril")
      ),
      column(11,
             leafletOutput(ns("exposure_map")),
             hidden(div(id = ns("div_abuttonviewtbl"), oasisuiButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;")))
      )
    ),
    fluidRow(
      column(4,
             hidden(tableOutput(ns("radius_circle")))
      ),
      column(8,
             hidden(numericInput(ns("damage_ratio"), "Damage ratio (%)", value = 100, min = 0, max = 100)),
             hidden(DT::dataTableOutput(ns("exposure_circle")))
      )
    ),
    fluidRow(column(2, hidden(downloadButton(ns("exp_tivs"), label = "Export to csv"))))
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
#' @importFrom leaflet leafletProxy
#' @importFrom leaflet highlightOptions
#' @importFrom leaflet removeShape
#' @importFrom leaflet setView
#' @importFrom leaflet.extras addFullscreenControl
#' @importFrom leaflet.extras addDrawToolbar
#' @importFrom leaflet.extras editToolbarOptions
#' @importFrom leaflet.extras selectedPathOptions
#' @importFrom leaflet.extras drawRectangleOptions
#' @importFrom revgeo revgeo
#' @importFrom jsonlite fromJSON
#' @importFrom rgdal readOGR
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom geosphere destPoint
#' @importFrom geosphere distm
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
    circle_radius = NULL,
    # damage TIV
    damage = NULL,
    # Total country TIV
    tot_country_tiv = NULL,
    # locations under circles
    circle_locs = NULL
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

  observeEvent(input$damage_ratio, {
    result$damage <- input$damage_ratio
  })

  observeEvent(input$tot_tiv_param, {
    if (input$tot_tiv_param == "Countries") {
      hide("damage_ratio")
      hide("radius_circle")
      hide("exposure_circle")
      hide("exp_tivs")
    }
  })

  observeEvent(input$exposure_map_click, {
    if (input$tot_tiv_param == "Circles") {
      hide("exposure_circle")
      circles_features <- input$exposure_map_draw_all_features$features

      # distance between click and closest isocenter
      if (length(circles_features) > 0) {
        p_1 <- cbind(input$exposure_map_click$lng, input$exposure_map_click$lat)
        dist_click <- unlist(lapply(seq_len(length(circles_features)), function(x) {
          p_2 <- cbind(circles_features[[x]]$geometry$coordinates[[1]],
                       circles_features[[x]]$geometry$coordinates[[2]])
          distm(p_1, p_2)
        }))
        # highlight the circle that is the closest to the point of click
        min_dist <- grep(min(dist_click), dist_click)
        radius <- circles_features[[min_dist]]$properties$radius
        lat_click <- circles_features[[min_dist]]$geometry$coordinates[[2]]
        long_click <- circles_features[[min_dist]]$geometry$coordinates[[1]]

        #re-set damage ratio everytime the user re-clicks on the map
        updateNumericInput(session, "damage_ratio", value = 100)

        if (is.null(input$damage_ratio)) {
          result$damage <- 100
        }

        circles_pins_tbl <- .showPinsInfo(radius = radius,
                                          lat_click = lat_click,
                                          long_click = long_click,
                                          ratio = result$damage)

        output$exposure_circle <- DT::renderDataTable({
          .showPinsInfo(radius = radius,
                         lat_click = lat_click,
                         long_click = long_click,
                         ratio = result$damage)
        })

        output$radius_circle <- renderTable({
          .showRadius(radius = radius,
                      lat_click = lat_click,
                      long_click = long_click,
                      ratio = result$damage)
        })

        # update file with only locations under circles
        info_circles <- .DrawnCircles(radius, lat_click, long_click, result$damage)
        result$circle_locs <- do.call(rbind, lapply(seq_len(length(info_circles$locID_list)), function(x) {
          result$uploaded_locs_check %>% filter(LocNumber == info_circles$locID_list[x])
        }))

        leafletProxy("exposure_map") %>% addCircles(lng = long_click,
                                                    lat = lat_click,
                                                    radius = radius,
                                                    layerId = "hilightArea",
                                                    highlightOptions = highlightOptions(color = "green",
                                                                                        weight = 5,
                                                                                        bringToFront = F,
                                                                                        opacity = 1))
        show("damage_ratio")
        show("radius_circle")
        show("exposure_circle")
        show("exp_tivs")

      } else {
        hide("damage_ratio")
        hide("radius_circle")
        hide("exposure_circle")
        hide("exp_tivs")
      }
    } else {
      hide("exposure_circle")
      p_long <- input$exposure_map_click$lng
      p_lat <- input$exposure_map_click$lat
      h <- revgeo(longitude = p_long, latitude = p_lat)
      js_lite <- fromJSON("~/RStudioProjects/OasisLMFUI/BFE_RShiny/oasisui/inst/app/geojson_data/europe_geojson.json")
      js <- readOGR("~/RStudioProjects/OasisLMFUI/BFE_RShiny/oasisui/inst/app/geojson_data/europe_geojson.json")
      country_num <- unlist(lapply(seq_len(length(js_lite$features[[2]][["name"]])), function(x) {
        if(grepl(js_lite$features[[2]][["name"]][[x]], h)) {
          x
        }
      }))

      if (is.null(country_num)) {

      } else {

        #get set of coordinates for selected country
        if (class(js_lite$features$geometry$coordinates[[country_num]]) == "list") {
          lati <- na.omit(unlist(lapply(seq_len(length(js_lite$features$geometry$coordinates[[country_num]])), function(x) {
            coord_len <- length(js_lite$features$geometry$coordinates[[country_num]][[x]])
            half_len_1 <- (coord_len/2)+1
            js_lite$features$geometry$coordinates[[country_num]][[x]][half_len_1:coord_len]
          })))

          long <- na.omit(unlist(lapply(seq_len(length(js_lite$features$geometry$coordinates[[country_num]])), function(x) {
            coord_len <- length(js_lite$features$geometry$coordinates[[country_num]][[x]])
            half_len <- coord_len/2
            js_lite$features$geometry$coordinates[[country_num]][[1]][1:half_len]
          })))
        } else {
          coord_len <- length(js_lite$features$geometry$coordinates[[country_num]])
          half_len_1 <- (coord_len/2)+1
          lati <- na.omit(js_lite$features$geometry$coordinates[[country_num]][half_len_1:coord_len])

          coord_len <- length(js_lite$features$geometry$coordinates[[country_num]])
          half_len <- coord_len/2
          long <- na.omit(js_lite$features$geometry$coordinates[[country_num]][1:half_len])
        }

        # check for pins within country borders
        match_lat <- grep(TRUE, between(result$uploaded_locs_check_peril$Latitude, min(unlist(lati)), max(unlist(lati))))
        match_long <- grep(TRUE, between(result$uploaded_locs_check_peril$Longitude, min(unlist(long)), max(unlist(long))))

        # adjust TIV wrt damage ratio
        if (is.null(input$damage_ratio)) {
          result$damage <- 100
        }

        output$exposure_circle <- DT::renderDataTable({
          .showCountryInfo(js_lite, match_long, match_lat, result$damage, country_num)
        })

        country_selected <- subset(js, js@data$filename == js@data$filename[country_num])

        leafletProxy("exposure_map") %>% addPolygons(data = country_selected, weight = 2,
                                                     fillColor = "blue",
                                                     layerId = "hilightArea")

        show("damage_ratio")
        show("exposure_circle")
      }
    }
  })

  observeEvent(input$exposure_map_draw_all_features, {
    leafletProxy("exposure_map") %>% removeShape(layerId = "hilightArea")
    hide("damage_ratio")
    hide("radius_circle")
    hide("exposure_circle")
    hide("exp_tivs")
  })

  # Export new TIV table to csv ------------------------------------------------
  output$exp_tivs <- downloadHandler(
    # Filename to download
    filename =  function() {
      paste0("circled_locations_", analysisID(), ".csv")
    },
    content = function(file) {
      fwrite(result$circle_locs,
             file,
             row.names = FALSE,
             quote = FALSE)
    }
  )

  # Refresh button -------------------------------------------------------------
  observeEvent(input$abuttonexposurerefresh, {
    # Get modeled locations
    withModalSpinner(
      .reloadExposureValidation(),
      "Refreshing...",
      size = "s", t = 0.5
    )
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
      if (input$tot_tiv_param == "Circles") {
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
      } else {
        js <- readOGR("~/RStudioProjects/OasisLMFUI/BFE_RShiny/oasisui/inst/app/geojson_data/europe_geojson.json")

        leaflet(js) %>%
          addPolygons(color = "black", fill = FALSE, weight = 2) %>%
          addTiles() %>%
          setView(lng = df$longitude[1],
                  lat = df$latitude[1], zoom = 3) %>%
          addAwesomeMarkers(
            lng = df$longitude,
            lat = df$latitude,
            icon = icon_map,
            clusterOptions = markerClusterOptions(),
            group = "clustered",
            clusterId = "cluster",
            popup = df$popup) %>% # make map full screen
          addFullscreenControl(pseudoFullscreen = TRUE)
      }
    }
  }

  # Drawn circles infos, outputs and radius
  .DrawnCircles <- function(radius, lat_click, long_click, ratio) {

    # calculate LocID and TIV for pins inside areas
    locID_list <- .is_within_bounds(format(result$uploaded_locs_check_peril$LocNumber, big.mark = "", scientific = FALSE), radius, lat_click, long_click, ratio)
    tiv_list <- .is_within_bounds(result$uploaded_locs_check_peril$BuildingTIV * (ratio/100), radius, lat_click, long_click, ratio)
    street_address <- .is_within_bounds(result$uploaded_locs_check_peril$StreetAddress, radius, lat_click, long_click, ratio)

    list(locID_list = locID_list, tiv_list = tiv_list, street_address = street_address)
  }

  .between_min_max <- function(circle_bounds, coord) {
    between(coord, min(circle_bounds), max(circle_bounds))
  }

  .is_within_bounds <- function(uploaded_locs_input, radius, lat_click, long_click, ratio) {

    # get pins coordinates
    long <- result$uploaded_locs_check_peril$Longitude
    lat <- result$uploaded_locs_check_peril$Latitude

    coord_df <- cbind(long_click, lat_click)

    # calculate two rectangles within circle area, 22.5 degrees of separation
    degrees_dist_1 <- c(90, 180, 270, 360)
    circle_bounds_1 <- destPoint(coord_df, degrees_dist_1, radius)

    degrees_dist_2 <- c(135, 225, 315, 405)
    circle_bounds_2 <- destPoint(coord_df, degrees_dist_2, radius)

    degrees_dist_3 <- degrees_dist_1 + 22.5
    circle_bounds_3 <- destPoint(coord_df, degrees_dist_3, radius)

    degrees_dist_4 <- degrees_dist_2 + 22.5
    circle_bounds_4 <- destPoint(coord_df, degrees_dist_4, radius)

    unlist(lapply(seq_len(length(result$uploaded_locs_check_peril$Longitude)), function(x) {
      if ((.between_min_max(circle_bounds_1[, 1], long[x]) &&
           .between_min_max(circle_bounds_1[, 2], lat[x])) ||
          (.between_min_max(circle_bounds_2[, 1], long[x]) &&
           .between_min_max(circle_bounds_2[, 2], lat[x])) ||
          (.between_min_max(circle_bounds_3[, 1], long[x]) &&
           .between_min_max(circle_bounds_3[, 2], lat[x])) ||
          (.between_min_max(circle_bounds_4[, 1], long[x]) &&
           .between_min_max(circle_bounds_4[, 2], lat[x]))) {
        uploaded_locs_input[x]
      }
    }))
  }

  # show TIV table for pins under circle
  .showPinsInfo <- function(radius, lat_click, long_click, ratio) {
    info_circles <- .DrawnCircles(radius, lat_click, long_click, ratio)
    if(!is.null(info_circles$locID_list)) {
      DT::datatable(data.frame(LocID = info_circles$locID_list,
                 TIV = add_commas(info_circles$tiv_list),
                 "Street Address" = info_circles$street_address)
      )
    } else {
      hide("damage_ratio")
      invisible()
    }
  }

  # show circle radius
  .showRadius <- function(radius, lat_click, long_click, ratio) {
    info_circles <- .DrawnCircles(radius, lat_click, long_click, ratio)
    data.frame("Radius (m)" = add_commas(round(radius, 3)),
               "Total TIV" = add_commas(sum(info_circles$tiv_list))
    )
  }

  # show TIV table for pins in selected country
  .showCountryInfo <- function(js_lite, match_long, match_lat, ratio, country_num) {
    if(length(match_long) > 0 && length(match_lat) > 0) {
      result$tot_country_tiv <- add_commas(sum(unlist(lapply(seq_len(length(match_long)), function (x) {
        result$uploaded_locs_check_peril$BuildingTIV[x]
      }))) * (ratio/100))
    } else {
      result$tot_country_tiv <- "No locations in this country"
      hide("damage_ratio")
    }

    DT::datatable(
      data.frame("Country" = js_lite$features[[2]][["name"]][[country_num]],
                 "Total TIV" = result$tot_country_tiv), options = list(dom = 't')
    )
  }

  invisible()
}

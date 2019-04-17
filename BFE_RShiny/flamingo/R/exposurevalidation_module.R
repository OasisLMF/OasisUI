# Exposure Validation Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' exposurevalidationUI
#' @rdname exposurevalidation
#'
#' @description UI/View for exposure validation of an analysis.
#'
#' @return List of tags.
#'
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#'
#' @export
exposurevalidationUI <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(div(flamingoRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;")),
    fluidRow(
      column(12,
             tabsetPanel(
               id = ns("tabsExposureValidationDetails"),
               tabPanel(
                 title = "Summary",
                 DTOutput(ns("dt_summary_validation")),
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("outputplot_loc1"))),
                   column(6,
                          plotlyOutput(ns("outputplot_loc2"))
                   )
                 ),
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("outputplot_tiv1"))),
                   column(6,
                          plotlyOutput(ns("outputplot_tiv2"))
                   )
                 ),
                 value = ns("tabvalidation_summary")
               ),
               tabPanel(
                 title = "Details",
                 leafletOutput(ns("exposure_map")),
                 hidden(div(id = ns("div_abuttonviewtbl"), flamingoButton(ns("abuttonviewtbl"), "Table", style = "margin-top:25px;margin-right:25px;float: right;"))),
                 value = ns("tabvalidation_details")
               )
             )
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
#' @template params-module
#' @param analysisID Selected analysis id.
#' @param portfolioID selected portfolio ID.
#' @param counter Reactive value storing actionButton status.
#'
#' @importFrom data.table fread
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr filter
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
#' @importFrom plotly plot_ly
#' @importFrom plotly renderPlotly
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom tidyr spread
#' @importFrom tidyr replace_na
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

  #name of element with location ids
  loc_ids <- "location_ids"

  color_success <- c('rgb(31,119,180)', 'rgb(255,127,14)')
  color_match <- c('rgb(114,147,203)', 'rgb(211,94,96)')

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active()) {
      if ((!is.null(portfolioID()) && !is.na(portfolioID()) && portfolioID() != "") &&
          (!is.null(analysisID()) && !is.na(analysisID()) && analysisID() != "")) {
        extractFolder <- set_extractFolder(analysisID(), label = "_inputs/")
        if (!file.exists(extractFolder) && is.na(file.size(extractFolder))) {
          withModalSpinner(
            api_get_analyses_input_file(analysisID()),
            "Loading...",
            size = "s"
          )
        }
        .reloadExposureValidation()
        .reloadSummary()
      }
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

  output$dt_summary_validation <- renderDT(
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl) > 0) {
      datatable(
        result$summary_validation_tbl %>% capitalize_names_df(),
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

  # Vusualize Summary ----------------------------------------------------------

  observeEvent(result$summary_validation_tbl, {
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl)) {

      df_loc <- .extract_df_pieplot(df = result$summary_validation_tbl, filter_row = loc_ids)
      output$outputplot_loc1 <- renderPlotly({.plotly_pie(df = df_loc$df_sel1, pie_labs =  df_loc$df_sel1$key, pie_vals =  df_loc$df_sel1$value, colors2use = color_success)})
      output$outputplot_loc2 <- renderPlotly({.plotly_pie(df = df_loc$df_sel2, pie_labs = df_loc$df_sel2$key, pie_vals = df_loc$df_sel2$value, colors2use = color_match)})


      df_tiv <- .extract_df_pieplot(df = result$summary_validation_tbl, filter_row = "tiv")
      output$outputplot_tiv1 <- renderPlotly({.plotly_pie(df = df_tiv$df_sel1, pie_labs =  df_tiv$df_sel1$key, pie_vals =  df_tiv$df_sel1$value, colors2use = color_success)})
      output$outputplot_tiv2 <- renderPlotly({.plotly_pie(df = df_tiv$df_sel2, pie_labs = df_tiv$df_sel2$key, pie_vals = df_tiv$df_sel2$value, colors2use = color_match)})

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
    filename = function() {result$filename2download},
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
    withModalSpinner(
      api_get_analyses_input_file(analysisID()),
      "Refreshing...",
      size = "s"
    )
    .reloadExposureValidation()
  })

  # Utils functions ------------------------------------------------------------

  .extract_df_pieplot <- function(df,filter_row){
    tot <- df$all[df$type == filter_row] %>% as.numeric()
    df_sel <- df %>%
      filter(type == filter_row) %>%
      select(-type) %>%
      gather(key, value)
    has_commas <- lapply(seq(nrow(df_sel)),
                         function(x){grepl(",", df_sel$value[x])}) %>%
      unlist() %>%
      any()
    if (!has_commas) {
      df_sel$value <- as.numeric(df_sel$value)
    }
    df_sel <- df_sel %>%
      mutate(value = sapply(seq(nrow(df_sel)),
                            function(x) {
                              if (df_sel$key[x] != "all" && !is.numeric(df_sel$value[x])) {
                                df_sel$value[x] %>%
                                  strsplit(",") %>%
                                  unlist() %>% length()
                              } else {
                                df_sel$value[x]
                              }
                            })) %>%
      mutate(value = as.numeric(value)/tot)

    df_sel1 <- df_sel %>%
      filter(key %in% c("fail", "success"))

    df_sel2 <- df_sel %>%
      rbind(c("match", 1 - df_sel$value[df_sel$key == "nomatch"])) %>%
      filter(key %in% c("nomatch", "match"))

    return(list(
      df_sel1 = df_sel1,
      df_sel2 = df_sel2
    ))
  }

  # visualize exposure validation summary
  .plotly_pie <- function(df, pie_labs, pie_vals, pie_title, colors2use){
    pie_ly <- plot_ly(df, labels = pie_labs, values = as.numeric(pie_vals), type = 'pie', marker = list(colors = colors2use))
    pie_ly
  }

  # dummy read summary.json
  .read_summary <- function(anaID){

    logMessage(".read_summary called")

    forig <- "./www/summary.json"
    json_lst <- jsonlite::read_json(forig)

    reg_expr_dot <- "^([^.]+)[.](.+)$"

    df <- lapply(
      json_lst[sapply(json_lst, function(x) {!is.atomic(x)})],
      function(x) {x[[loc_ids]] <- toString(x[[loc_ids]], width = 20); x}) %>%
      as.data.frame()

    df <- df %>%
      format(scientific = FALSE) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      mutate(key = sub(reg_expr_dot, "\\1", rownames(.)),
             type = sub(reg_expr_dot, "\\2", rownames(.))) %>%
      spread(key, 1, convert = TRUE)
    df
  }

  # reload dataframes

  #reload summary table
  .reloadSummary <- function(){

    logMessage(".reloadSummary called")

    # Clean up df
    result$summary_validation_tbl <- NULL

    #Build df
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      df <- .read_summary(analysisID())
      replace_lst <- lapply(names(df),
                            function(x){toString(nrow(result$uploaded_locs_check))}
      ) %>%
        setNames(names(df))
      result$summary_validation_tbl <- df %>%
        replace_na(replace_lst)
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
        clusterOptions = TRUE,
        popup = toString(popupData))
  }

}

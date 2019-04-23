# Model Details UI -----------------------------------------------

#' modeldetailsUI
#'
#' @rdname modeldetails
#'
#' @description UI side of function wrapping panel to show model details.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
modeldetailsUI <- function(id) {

  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_model_details"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_ModelDetails"), inline = TRUE),
      flamingoRefreshButton(ns("abuttonmodeldetailrfsh")),
      actionButton(inputId = ns("buttonhidemodeldetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),

    tabsetPanel(
      id = ns("tabsModelsDetails"),

      tabPanel(
        title = "Resources",
        h4("Model Settings"),
        DTOutput(ns("dt_model_settings")),
        h4("Lookup Settings"),
        DTOutput(ns("dt_lookup_settings")),
        value = ns("tabresources")
      ),
      tabPanel(
        title = "Hazard Maps",
        selectInput(inputId = ns("hazard_files"),
                    label = "Choose hazard file",
                    choices = list.files("./www/hazard_files")
        ),
        createHazardMapUI(ns("createHazardMap")),
        value = ns("tabmaps")
      )
    )
  )
}


# Model Details Server -------------------------------------------

#' modeldetails
#'
#' @rdname modeldetails
#'
#' @description Server side of function wrapping panel to show analyses details table.
#'
#' @param modelID Selected model ID.
#' @param portfolioID Selected portfolio ID.
#' @template params-module-ui
#'
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom shinyjs onclick
#' @importFrom DT renderDT
#' @importFrom DT datatable
#'
#' @export
modeldetails <- function(input,
                         output,
                         session,
                         modelID,
                         portfolioID,
                         file_pins,
                         counter,
                         active = reactive(TRUE)) {

  ns <- session$ns

  # Params ---------------------------------------------------------------------
  scrollX <- FALSE
  maxrowsperpage <- 5
  filter <- TRUE
  escape <- TRUE


  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    # reactive value for detail of model table
    tbl_modelsDetails = NULL,
    #file for hazard map
    mapfile = NULL,
    # file for pins
    uploaded_locs = NULL
  )


  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_model_details")
    .reloadtbl_modelsDetails()
  })

  # Tab Resources ------------------------------------------------------------
  output$dt_model_settings <- renderDT(
    if (!is.null(result$tbl_modelsDetails[1]) && nrow(result$tbl_modelsDetails[[1]]) > 0) {
      logMessage("re-rendering model settings table")
      datatable(
        result$tbl_modelsDetails[[1]] %>% capitalize_names_df(),
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = "none",
        colnames = c('row number' = 1),
        options = getTableOptions(scrollX, maxrowsperpage, filter, escape)
      )
    } else {
      nothingToShowTable(contentMessage = paste0("no model settings files associated with Model ID ", modelID()))
    }
  )

  output$dt_lookup_settings <- renderDT(
    if (!is.null(result$tbl_modelsDetails[2]) && nrow(result$tbl_modelsDetails[[2]]) > 0) {
      logMessage("re-rendering lookup settings table")
      datatable(
        result$tbl_modelsDetails[[2]],
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = "none",
        colnames = c('row number' = 1),
        options = options
      )
    } else {
      nothingToShowTable(contentMessage = paste0("no lookup settings files associated with Model ID ", modelID()))
    }
  )

  # Tab Hazard Map -----------------------------------------------------------

  # Choose hazard file
  observeEvent(input$hazard_files, {
    if (!is.null(input$hazard_files)) {
      path <- paste0("./www/hazard_files/", input$hazard_files)
      result$mapfile <- geojsonio::geojson_read(path, what = "sp")
      if (is.null(result$mapfile)) {
        hideTab(inputId = "tabsModelsDetails", target = ns("tabmaps"))
      }
    }
  })

  # Draw map
  observeEvent(result$mapfile, ignoreNULL = FALSE, {
    if (!is.null(result$mapfile)) {
      callModule(
        createHazardMap,
        id = "createHazardMap",
        file_map = result$mapfile,
        file_pins = result$uploaded_locs
      )
    }
  })

  # Details Model title
  output$paneltitle_ModelDetails <- renderUI({
    paste0('Resources of model id ', modelID())
  })

  # onclick buttons
  onclick("buttonhidemodeldetails", {
    hide("panel_model_details")
    logMessage("hiding panelModelDetails")
  })

  onclick("abuttonmodeldetailrfsh", {
    .reloadtbl_modelsDetails()
  })


  # Helper functions -----------------------------------------------------------
  # Reload Programme Model Details table
  .reloadtbl_modelsDetails <- function() {
    logMessage(".reloadtbl_modelsDetails called")
    tbl_modelsDetails <- return_models_id_resource_file_df(modelID())
    if (!is.null(tbl_modelsDetails)) {
      result$tbl_modelsDetails <- tbl_modelsDetails
      logMessage("model resources table refreshed")

      result$uploaded_locs <- return_file_df(api_get_portfolios_location_file,
                                             portfolioID())
      logMessage("uploaded_locs refreshed")
      updateSelectInput(session,
                        inputId = ns("hazard_files"),
                        label = "Choose hazard file",
                        choices = list.files("./www/hazard_files"))
      logMessage("map dropdown refreshed")
    } else {
      result$tbl_modelsDetails <- NULL
    }
    result$tbl_modelsDetails
  }

}


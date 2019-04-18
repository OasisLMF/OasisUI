# Model Details UI -----------------------------------------------

#' modeldetailsUI
#'
#' @rdname modeldetails
#'
#' @description UI side of function wrapping panel to show model details.
#'
#' @template params-module-ui
#'
#' @export
modeldetailsUI <- function(id) {

  ns <- NS(id)
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
}


# Model Details Server -------------------------------------------

#' modeldetails
#'
#' @rdname modeldetails
#'
#' @description Server side of function wrapping panel to show analyses details table.
#'
#' @param modelID Selected model ID.
#' @param file_pins Location file to plot pins.
#' @param options Functions displaying options for table.
#' @param nothing_to_show Function used when no table is available.
#' @template params-module-ui
#'
#' @export
modeldetails <- function(input,
                         output,
                         session,
                         modelID,
                         file_pins,
                         options,
                         nothing_to_show,
                         counter,
                         active = reactive(TRUE)) {

  ns <- session$ns

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # reactive value for detail of model table
    tbl_modelsDetails = NULL,
    #file for hazard map
    mapfile = NULL,
    # file for pins
    uploaded_locs = NULL
  )

  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {

    # Tab Resources --------------------------------------------------------------
    result$tbl_modelsDetails <- return_models_id_resource_file_df(modelID())

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
          options = options
        )
      } else {
        nothing_to_show(contentMessage = paste0("no model settings files associated with Model ID ", modelID()))
      })

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
        nothing_to_show(contentMessage = paste0("no lookup settings files associated with Model ID ", modelID()))
      })

    # Tab Hazard Map -------------------------------------------------------------
    updateSelectInput(session,
                      inputId = ns("hazard_files"),
                      label = "Choose hazard file",
                      choices = list.files("./www/hazard_files")
    )

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
          file_pins = file_pins
        )
      }
    })
  })

}


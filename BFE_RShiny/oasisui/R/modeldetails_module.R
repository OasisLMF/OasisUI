# Model Details UI -----------------------------------------------

#' modeldetailsUI
#'
#' @rdname modeldetails
#'
#' @description UI side of function wrapping panel to show model details.
#'
#' @importFrom DT DTOutput
#'
#' @export
modeldetailsUI <- function(id) {

  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    ns("panel_model_details"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_ModelDetails"), inline = TRUE),
      oasisuiRefreshButton(ns("abuttonmodeldetailrfsh")),
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
                    choices = c("Select hazard file")
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
#' @param file_pins file with coordiantes of exposure locations
#' @param counter Reactive value to trigger inputs download.
#' @template params-module
#' @template params-active
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

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    # reactive value for detail of model table
    tbl_modelsDetails = NULL,
    # list of hazard maps associated to model,
    mapfiles_lst = NULL,
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
    if (modelID() != "" && !is.null(modelID())) {
      .reloadtbl_modelsDetails()
    }
  })

  # Tab Resources ------------------------------------------------------------
  output$dt_model_settings <- renderDT(
    if (!is.null(result$tbl_modelsDetails[1]) && nrow(result$tbl_modelsDetails[[1]]) > 0) {
      logMessage("re-rendering model settings table")
      datatable(
        result$tbl_modelsDetails[[1]] %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = FALSE,
        filter = "none",
        escape = FALSE,
        selection = "none",
        options = getTableOptions()
      )
    } else {
      nothingToShowTable(paste0("No model settings files associated with Model ID ", modelID()))
    }
  )

  output$dt_lookup_settings <- renderDT(
    if (!is.null(result$tbl_modelsDetails[2]) && nrow(result$tbl_modelsDetails[[2]]) > 0) {
      logMessage("re-rendering lookup settings table")
      datatable(
        result$tbl_modelsDetails[[2]],
        class = "oasisui-table display",
        rownames = FALSE,
        filter = "none",
        escape = FALSE,
        selection = "none",
        options = getTableOptions()
      )
    } else {
      nothingToShowTable(paste0("No lookup settings files associated with Model ID ", modelID()))
    }
  )

  # Tab Hazard Map -------------------------------------------------------------

  observeEvent(input$tabsModelsDetails, {
    if (input$tabsModelsDetails == ns("tabmaps")) {
      if (!is.null(result$mapfiles_lst)) {
        hazard_choices <- unique(result$mapfiles_lst$filename[!is.na(result$mapfiles_lst$filename)])
        updateSelectInput(session,
                          inputId = "hazard_files",
                          label = "Choose hazard file",
                          choices = hazard_choices,
                          selected = hazard_choices[1])
        logMessage("map dropdown refreshed")
      }
    }
  })

  # Choose hazard file
  observeEvent(input$hazard_files, ignoreInit = TRUE, {
    if (!is.null(input$hazard_files) && input$hazard_files != "" && input$hazard_files != "Select hazard file") {
      # path <- paste0("./www/hazard_files/", input$hazard_files)
      #geojsonio::geojson_read(path, what = "sp")
      mapfile_id <- result$mapfiles_lst[result$mapfiles_lst$filename == input$hazard_files, "id"]
      result$mapfile <- session$userData$data_hub$get_model_hazard_dataset_content(id = mapfile_id, filename = input$hazard_files)
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
    tbl_modelsDetails <- session$userData$data_hub$get_model_resource_dataset_content(modelID())
    if (!is.null(tbl_modelsDetails)) {
      result$tbl_modelsDetails <- tbl_modelsDetails
      logMessage("model resources table refreshed")
      result$uploaded_locs <- session$userData$data_hub$get_pf_location_content(id = portfolioID())
      logMessage("uploaded_locs refreshed")
      result$mapfiles_lst <- session$userData$data_hub$get_model_hazard_data_list(modelID())
      if (is.null(result$mapfiles_lst)) {
        hideTab(inputId = "tabsModelsDetails", target = ns("tabmaps"))
      }
    } else {
      result$tbl_modelsDetails <- NULL
    }
    result$tbl_modelsDetails
  }

}


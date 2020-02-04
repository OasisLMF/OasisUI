# Shared Module documentation --------------------------------------------------
#' Hazard Map Module
#'
#' Shiny Module for showing model details.
#'
#' @template params-module
#'
#' @name modeldetails
NULL


# Model Details UI -----------------------------------------------

#' @describeIn modeldetails Returns the UI elements of the module.
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
        title = "JSON",
        h4("Model Settings JSON file"),
        verbatimTextOutput(ns("json_model_settings"))
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

#' @param analysisID Selected analysis ID.
#' @param modelID Selected model ID.
#' @param portfolioID Selected portfolio ID.
#' @param file_pins file with coordiantes of exposure locations
#' @param counter Reactive value to trigger inputs download.
#' @template params-active
#'
#' @describeIn modeldetails Defines the server logic of the module.
#'
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom DT renderDT
#' @importFrom DT datatable
#'
#' @export
modeldetails <- function(input,
                         output,
                         session,
                         analysisID,
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
    # file for hazard map
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
    if (!is.null(result$tbl_modelsDetails$model_settings) &&
        length(result$tbl_modelsDetails$model_settings) > 0) {
      logMessage("re-rendering model settings table")
      mdl_sets <- result$tbl_modelsDetails$model_settings

      # extract names, description and default values from model settings
      DF <- lapply(seq_len(length(mdl_sets)), function(x) {
        entry <- mdl_sets[[x]]
        if (is.null(entry$name)) {
          SubDF <- lapply(seq_len(length(entry)), function(y) {
            data.frame(
              Type = names(mdl_sets)[x],
              Name = entry[[y]]$name,
              Description = entry[[y]]$desc,
              Default = paste(unlist(entry[[y]]$default), collapse = ", ")
            )
          })
          do.call(rbind, SubDF)
        } else {
          data.frame(
            Type = names(mdl_sets)[x],
            Name = entry$name,
            Description = entry$desc,
            Default = paste(unlist(entry$default), collapse = ", ")
          )
        }
      })
      DF <- do.call(rbind, DF)

      # create model settings data table
      datatable(
        DF,
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
    if (!is.null(result$tbl_modelsDetails$lookup_settings) &&
        length(result$tbl_modelsDetails$lookup_settings) > 0) {
      logMessage("re-rendering lookup settings table")
      lkp_sets <- result$tbl_modelsDetails$lookup_settings

      # extract ID from lookup settings
      ID <- lapply(grep("id", names(unlist(lkp_sets))), function(x) {
        unlist(lkp_sets)[x]
      })

      # extract description from lookup settings
      Description <- lapply(seq_len(length(ID)), function(x) {
        lkp_sets[[1]][[x]][["desc"]]
      })

      datatable(
        cbind(ID, Description),
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

  # Tab JSON file --------------------------------------------------------------
  output$json_model_settings <- renderText({
    if (!is.null(result$tbl_modelsDetails)) {
      logMessage("re-rendering model settings JSON")
      toJSON(result$tbl_modelsDetails, pretty = TRUE)
    } else {
      paste0("No model settings JSON file associated with Model ID", modelID())
    }
  })

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
      withModalSpinner(
        result$mapfile <- session$userData$data_hub$get_model_hazard_dataset_content(id = mapfile_id, filename = input$hazard_files),
        "Loading hazard map data..."
      )
    }
  })

  # Draw map
  callModule(
    createHazardMap,
    id = "createHazardMap",
    reactive(result$mapfile),
    reactive(result$uploaded_locs),
    analysisID = analysisID
  )

  # Details Model title
  output$paneltitle_ModelDetails <- renderUI({
    paste0('Resources of model id ', modelID())
  })

  observeEvent(input$buttonhidemodeldetails, {
    hide("panel_model_details")
    logMessage("hiding panelModelDetails")
  })

  observeEvent(input$abuttonmodeldetailrfsh, {
    .reloadtbl_modelsDetails()
  })


  # Helper functions -----------------------------------------------------------
  # Reload Programme Model Details table
  .reloadtbl_modelsDetails <- function() {
    logMessage(".reloadtbl_modelsDetails called")

    tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
      query_path = paste("models", modelID(), "settings", sep = "/"),
      query_method = "GET"
    )

    if (!is.null(tbl_modelsDetails)) {
      result$tbl_modelsDetails <- tbl_modelsDetails
      logMessage("model resources table refreshed")
      result$uploaded_locs <- session$userData$data_hub$get_pf_location_content(id = portfolioID())
      logMessage("uploaded_locs refreshed")
      result$mapfiles_lst <- session$userData$data_hub$get_model_hazard_data_list(modelID())
      if (is.null(result$mapfiles_lst)) {
        hideTab(inputId = "tabsModelsDetails", target = ns("tabmaps"))
      } else {
        showTab(inputId = "tabsModelsDetails", target = ns("tabmaps"))
      }
    } else {
      result$tbl_modelsDetails <- NULL
    }

    result$tbl_modelsDetails
  }

  invisible()
}

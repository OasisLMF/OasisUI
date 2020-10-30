# Shared Module documentation --------------------------------------------------
#' Hazard Map Module
#'
#' Shiny Module for showing fly details.
#'
#' @template params-module
#'
#' @name buildFly
NULL


# Build Fly UI -----------------------------------------------

#' @describeIn buildFly Returns the UI elements of the module.
#'
#' @importFrom DT DTOutput
#'
#' @export
buildFlyUI <- function(id) {

  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    ns("panel_build_Fly"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_BuildFly"), inline = TRUE),
      actionButton(inputId = ns("buttonhidebuildfly"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_model_settings")),
    oasisuiButton(inputId = ns("abuttonselsettings"), label = "Apply")
  )
}


# Model Details Server -------------------------------------------

#' @param modelID Selected model ID.
#'
#' @describeIn buildFly Allows user to build their own mode on the fly.
#'
#' @importFrom DT renderDT
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#'
#' @export
buildFly <- function(input,
                     output,
                     session,
                     modelID,
                     supplierID,
                     versionID,
                     counter,
                     active = reactive(TRUE)) {

  ns <- session$ns

  result <- reactiveValues(
    # reactive value for detail of model table
    tbl_modelsDetails = NULL,
    # model settings entries names
    settings_names = NULL,
    # model settings entries description
    settings_desc = NULL,
    # model settings entries default value
    settings_default = NULL,
    # new settings selected from table
    filtered_analysis_settings = NULL,
    # changed values in data table
    changed_entry = NULL
  )

  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_build_Fly")
    selectRows(proxy = dataTableProxy("dt_model_settings"), selected = NULL)
    result$tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
      query_path = paste("models", modelID(), "settings", sep = "/"),
      query_method = "GET"
    )

    # get entries for table: name, description and default value(s)
    result$settings_names <- unlist(lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[x]]$name)) {
        lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
          result$tbl_modelsDetails$model_settings[[x]][[y]]$name
        })
      } else {
        result$tbl_modelsDetails$model_settings[[x]]$name
      }
    }))

    result$settings_desc <- unlist(lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[x]]$desc)) {
        lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
          result$tbl_modelsDetails$model_settings[[x]][[y]]$desc
        })
      } else {
        result$tbl_modelsDetails$model_settings[[x]]$desc
      }
    }))

    result$settings_default <- unlist(lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[x]]$default)) {
        lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
          paste(unlist(result$tbl_modelsDetails$model_settings[[x]][[y]]$default), collapse = ", ")
        })
      } else {
        paste(unlist(result$tbl_modelsDetails$model_settings[[x]]$default), collapse = ", ")
      }
    }))
  })

  output$paneltitle_BuildFly <- renderUI({
    "Select all parameters that you would like to include in the model.
    You will be able to modify the default in the next step"
  })

  observeEvent(input$buttonhidebuildfly, {
    hide("panel_build_Fly")
    logMessage("hiding panelBuildFly")
  })

  output$dt_model_settings <- renderDT(server=FALSE, {
    df <- data.frame(names = result$settings_names, descr = result$settings_desc, value = result$settings_default)
    colnames(df) <- c("Model Settings", "Description", "Default")
    datatable(df, editable = list(target = 'cell', disable = list(columns = c(1,2))), selection = "none")
  })

  #extrapolate changed cells
  observeEvent(input$dt_model_settings_cell_edit, {
    result$settings_default[input$dt_model_settings_cell_edit$row] <<- input$dt_model_settings_cell_edit$value

    changed_val <- input$dt_model_settings_cell_edit$row

    new_settings <- result$settings_names[changed_val]

    result$changed_entry <- unlist(lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
        if (any(new_settings %in% result$tbl_modelsDetails$model_settings[[x]][[y]])) {
          result$changed_entry <- c(result$changed_entry, paste(x,y, input$dt_model_settings_cell_edit$value))
        }
      })
    }))
  })

  observeEvent(input$abuttonselsettings, {
    rows_selected <- input$dt_model_settings_rows_selected

    new_settings <- result$settings_names[rows_selected]
    x <- strsplit(result$changed_entry, " ")

    # change edited values in the table
    for (y in seq_len(length(x))) {
      entry_1 <- as.numeric(x[[y]][[1]])
      entry_2 <- as.numeric(x[[y]][[2]])
      entry_val <- x[[y]][[3]]
      result$tbl_modelsDetails$model_settings[[entry_1]][[entry_2]]$default <- entry_val
    }

    result$tbl_modelsDetails$model_settings$event_occurrence_id <- result$tbl_modelsDetails$model_settings$event_occurrence_id$default
    result$tbl_modelsDetails$model_settings$event_set <- result$tbl_modelsDetails$model_settings$event_set$default
    filtered_settings <- c("model_configurable"= TRUE, result$tbl_modelsDetails$model_settings)

    hide("panel_build_Fly")
    logMessage("hiding panelBuildFly")
    oasisuiNotification(type = "message",
                        "Model settings filtered by chosen entries.")
    gul_summaries <- list(
      summarycalc = FALSE,
      eltcalc = FALSE,
      aalcalc = FALSE,
      pltcalc = FALSE,
      id = 1,
      oed_fields = list(),
      lec_output = FALSE,
      leccalc = list(
        return_period_file = FALSE,
        full_uncertainty_aep = FALSE,
        full_uncertainty_oep = FALSE,
        wheatsheaf_aep = FALSE,
        wheatsheaf_oep = FALSE,
        wheatsheaf_mean_aep = FALSE,
        wheatsheaf_mean_oep = FALSE,
        sample_mean_aep = FALSE,
        sample_mean_oep = FALSE
      ))
    result$filtered_analysis_settings <- list(analysis_settings = c(
      list(
        module_supplier_id = supplierID(),
        model_version_id = versionID(),
        number_of_samples = 0,
        model_settings = filtered_settings,
        gul_output = FALSE,
        gul_summaries = list(gul_summaries)
      )
    ))
  })

  moduleOutput <- reactive({result$filtered_analysis_settings})

  moduleOutput
}

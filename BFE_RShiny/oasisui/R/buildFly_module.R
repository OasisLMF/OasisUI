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
#' @importFrom shinyjs disabled
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
    disabled(oasisuiButton(inputId = ns("abuttonselsettings"), label = "Apply Selection"))
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
    # new settings selected from table
    filtered_analysis_settings = NULL
  )

  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_build_Fly")
    if (!is.null(input$dt_model_settings_rows_selected)) {
      selectRows(proxy = dataTableProxy("dt_model_settings"), selected = NULL)
    }
  })

  output$paneltitle_BuildFly <- renderUI({
    "Select all parameters that you would like to include in the model.
    You will be able to modify the default in the next step"
  })

  observeEvent(input$buttonhidebuildfly, {
    hide("panel_build_Fly")
    logMessage("hiding panelBuildFly")
  })

  output$dt_model_settings <- renderDT({

    result$tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
        query_path = paste("models", modelID(), "settings", sep = "/"),
      query_method = "GET"
    )

    # get entries for table: name, description and default value(s)
    result$settings_names <- lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[x]]$name)) {
        lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
          result$tbl_modelsDetails$model_settings[[x]][[y]]$name
        })
      } else {
        result$tbl_modelsDetails$model_settings[[x]]$name
      }
    })

    settings_desc <- lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[x]]$desc)) {
        lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
          result$tbl_modelsDetails$model_settings[[x]][[y]]$desc
        })
      } else {
        result$tbl_modelsDetails$model_settings[[x]]$desc
      }
    })

    settings_default <- lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[x]]$default)) {
        lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
          paste(unlist(result$tbl_modelsDetails$model_settings[[x]][[y]]$default), collapse = ", ")
        })
      } else {
        paste(unlist(result$tbl_modelsDetails$model_settings[[x]]$default), collapse = ", ")
      }
    })

    df <- data.frame(names = unlist(result$settings_names), descr = unlist(settings_desc), value = unlist(settings_default))

    colnames(df) <- c("Model Settings", "Description", "Default")
    df
  })

  observeEvent(input$dt_model_settings_rows_selected, ignoreNULL = FALSE, {
    if (length(input$dt_model_settings_rows_selected) > 0) {
      enable("abuttonselsettings")
    } else {
      disable("abuttonselsettings")
    }
  })

  observeEvent(input$abuttonselsettings, {
    rows_selected <- input$dt_model_settings_rows_selected

    new_settings <- unlist(result$settings_names)[rows_selected]

    # extrapolate which are the corresponding entries of the selected items in table to the model settings
    filtered_entries <- unlist(lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
        if (any(new_settings %in% result$tbl_modelsDetails$model_settings[[x]][[y]])) {
          c(x,y)
        }
      })
    }))

    # create new model settings with only selected entries from the table
    filtered_df <- data.frame(split(filtered_entries, rep(1:2, length = length(filtered_entries))))
    filtered_settings <- lapply(seq_len(nrow(filtered_df)), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[filtered_df$X1[[x]]]]$name)) {
        result$tbl_modelsDetails$model_settings[[filtered_df$X1[[x]]]][[filtered_df$X2[[x]]]]
      } else {
        result$tbl_modelsDetails$model_settings[[filtered_df$X1[[x]]]]
      }
    })

    names(filtered_settings) <- names(result$tbl_modelsDetails$model_settings[filtered_df$X1])
    filtered_settings$event_occurrence_id <- filtered_settings$event_occurrence_id$default
    filtered_settings$event_set <- filtered_settings$event_set$default

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

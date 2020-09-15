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
    oasisuiButton(inputId = ns("abuttonselsettings"), label = "Apply Selection")
  )
}


# Model Details Server -------------------------------------------

#' @param modelID Selected model ID.
#'
#' @describeIn buildFly Allows user to build their own mode on the fly.
#'
#' @importFrom DT renderDT
#'
#' @export
buildFly <- function(input,
                     output,
                     session,
                     modelID,
                     counter,
                     active = reactive(TRUE)) {

  ns <- session$ns

  result <- reactiveValues(
    # reactive value for detail of model table
    tbl_modelsDetails = NULL,
    # model settings entries names
    settings_names = NULL,
    # new settings selected from table
    filtered_settings = NULL
  )

  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_build_Fly")
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
    result$filtered_settings <- lapply(seq_len(nrow(filtered_df)), function(x) {
      if(is.null(result$tbl_modelsDetails$model_settings[[filtered_df$X1[[x]]]]$name)) {
        result$tbl_modelsDetails$model_settings[[filtered_df$X1[[x]]]][[filtered_df$X2[[x]]]]
      } else {
        result$tbl_modelsDetails$model_settings[[filtered_df$X1[[x]]]]
      }
    })

    names(result$filtered_settings) <- names(result$tbl_modelsDetails$model_settings[filtered_df$X1])

    hide("panel_build_Fly")
    logMessage("hiding panelBuildFly")
    oasisuiNotification(type = "message",
                        "Model settings filtered by chosen entries.")
  })

  moduleOutput <- reactive({result$filtered_settings})

  moduleOutput
}

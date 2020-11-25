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
    ns("panel_build_fly_actions"),
    collapsible = FALSE,
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_BuildFly"), inline = TRUE),
      actionButton(inputId = ns("buttonhidebuildfly"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    tabsetPanel(
      id = ns("panel_build_Fly"),
      tabPanel(
        title = "Model values",
        DTOutput(ns("dt_model_values"))),
      tabPanel(
        title = "shp and csv files",
        # DTOutput(ns("dt_model_files")),
        fluidRow(uiOutput(ns("browsers_tables"))),
        #   column(4,
        #          fileInput(inputId = ns("damage_ratio_paths"), label = 'Damage Ratio Paths:', multiple = TRUE,
        #                    accept = c('csv', 'comma-separated-values', '.csv')), DTOutput(ns("dt_damage_ratio"))),
        #   column(4,
        #          fileInput(inputId = ns("damage_ratio_lookup_paths"), label = 'Damage Ratio Lookup Paths:', multiple = TRUE,
        #                    accept = c('csv', 'comma-separated-values', '.csv')), DTOutput(ns("dt_damage_ratio_lookup"))),
        #   column(4,
        #          fileInput(inputId = ns("disagg_locator_descriptions_paths"), label = 'Disagg Locator Desc Paths:', multiple = TRUE,
        #                    accept = c('csv', 'comma-separated-values', '.csv')), DTOutput(ns("dt_disagg_locator_descriptions")))
        # ),
        # fluidRow(
        #   column(4,
        #          fileInput(inputId = ns("schema_file_paths"), label = 'Schema File Paths:', multiple = TRUE,
        #                    accept = c('csv', 'comma-separated-values', '.csv')), DTOutput(ns("dt_schema_file"))),
        #   column(4,
        #          fileInput(inputId = ns("vuln_locator_shapefile_paths"), label = 'Vuln Locator shp Paths:', multiple = TRUE,
        #                    accept = "shp"), DTOutput(ns("dt_vuln_locator_shapefile"))))
        # fluidRow(
        #   column(2, fileInput(inputId = ns("damage_ratio_paths"), label = 'Damage Ratio Paths:', multiple = TRUE,
        #                       accept = c('csv', 'comma-separated-values', '.csv'))),
        #   column(2, fileInput(inputId = ns("damage_ratio_lookup_paths"), label = 'Damage Ratio Lookup Paths:', multiple = TRUE,
        #                       accept = c('csv', 'comma-separated-values', '.csv'))),
        #   column(2, fileInput(inputId = ns("disagg_locator_descriptions_paths"), label = 'Disagg Locator Desc Paths:', multiple = TRUE,
        #                       accept = c('csv', 'comma-separated-values', '.csv'))),
        #   column(2, fileInput(inputId = ns("schema_file_paths"), label = 'Schema File Paths:', multiple = TRUE,
        #                       accept = c('csv', 'comma-separated-values', '.csv'))),
        #   column(2, fileInput(inputId = ns("vuln_locator_shapefile_paths"), label = 'Vuln Locator shp Paths:', multiple = TRUE,
        #                       accept = "shp"))
        # )
      )
    ),
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
                     portfolioID,
                     modelID,
                     supplierID,
                     versionID,
                     analysisID,
                     analysisNAME,
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
    changed_entry = NULL,
    # table with list of data files
    tbl_files = NULL,
    # list of files to write in analysis settings
    list_files = NULL,
    # FileInputs and DTOutputs
    browsers_tables = NULL,
    # FileInputs IDs
    inputID = NULL
  )

  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_build_fly_actions")
    selectRows(proxy = dataTableProxy("dt_model_values"), selected = NULL)
    result$tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
      query_path = paste("models", modelID(), "settings", sep = "/"),
      query_method = "GET"
    )
    result$tbl_files <- session$userData$data_hub$return_tbl_dataFiles(name = "")
    result$list_files <- list()

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
    "Change values in the model"
  })

  observeEvent(input$buttonhidebuildfly, {
    hide("panel_build_fly_actions")
    logMessage("hiding panelBuildFly")
  })

  output$dt_model_values <- renderDT(server=FALSE, {
    df <- data.frame(names = result$settings_names, descr = result$settings_desc, value = result$settings_default)
    colnames(df) <- c("Model Settings", "Description", "Default")
    datatable(df, editable = list(target = 'cell', disable = list(columns = c(1,2))), selection = "none")
  })

  #extrapolate changed cells
  observeEvent(input$dt_model_values_cell_edit, {
    result$settings_default[input$dt_model_values_cell_edit$row] <<- input$dt_model_values_cell_edit$value

    changed_val <- input$dt_model_values_cell_edit$row

    new_settings <- result$settings_names[changed_val]

    result$changed_entry <- unlist(lapply(seq_len(length(names(result$tbl_modelsDetails$model_settings))), function(x) {
      lapply(seq_len(length(result$tbl_modelsDetails$model_settings[[x]])), function(y) {
        if (any(new_settings %in% result$tbl_modelsDetails$model_settings[[x]][[y]])) {
          result$changed_entry <- c(result$changed_entry, paste(x,y, input$dt_model_values_cell_edit$value))
        }
      })
    }))
  })

  # # create tables for each "_paths" entry
  # output$dt_damage_ratio <- renderDT(server=FALSE, {
  #   entry <- grep("damage_ratio_paths", result$tbl_modelsDetails$data_settings$datafile_selectors)
  #   cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[entry]]$allow_multiple
  #   if (cond) {
  #     select = "multiple"
  #   } else {
  #     select = "single"
  #   }
  #   tbl_files_filtered <- result$tbl_files %>% filter(file_description == "damage_ratio_paths")
  #   df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
  #   # show last element of data frame first in table
  #   df <- df[nrow(df):1, ]
  #   colnames(df) <- c("ID", "Name")
  #   datatable(df, selection = select)
  # })
  #
  # # update list for damage ratio paths selection
  # observeEvent(input$dt_damage_ratio_rows_selected, {
  #   for (x in seq_len(length(input$dt_damage_ratio_rows_selected))) {
  #     # because df is reversed, also order of row has to be reversed
  #     entry <- (nrow(result$tbl_files)+1) - x
  #     result$list_files$damage_ratio_paths[x] <- result$tbl_files[entry,]$filename
  #     result$list_files$damage_ratio_paths <- result$list_files$damage_ratio_paths[!is.na(result$list_files$damage_ratio_paths)]
  #   }
  #   result$list_files$damage_ratio_paths <- unlist(result$list_files$damage_ratio_paths)
  #   result$list_files
  # })
  #
  # output$dt_damage_ratio_lookup <- renderDT(server=FALSE, {
  #   entry <- grep("damage_ratio_lookup_paths", result$tbl_modelsDetails$data_settings$datafile_selectors)
  #   cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[entry]]$allow_multiple
  #   if (cond) {
  #     select = "multiple"
  #   } else {
  #     select = "single"
  #   }
  #   tbl_files_filtered <- result$tbl_files %>% filter(file_description == "damage_ratio_lookup_paths")
  #   df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
  #   # show last element of data frame first in table
  #   df <- df[nrow(df):1, ]
  #   colnames(df) <- c("ID", "Name")
  #   datatable(df, selection = select)
  # })
  #
  # observeEvent(input$dt_damage_ratio_lookup_rows_selected, {
  #   for (x in seq_len(length(input$dt_damage_ratio_lookup_rows_selected))) {
  #     # because df is reversed, also order of row has to be reversed
  #     entry <- (nrow(result$tbl_files)+1) - x
  #     result$list_files$damage_ratio_lookup_paths[x] <- result$tbl_files[entry,]$filename
  #     result$list_files$damage_ratio_lookup_paths <-
  #       result$list_files$damage_ratio_lookup_paths[!is.na(result$list_files$damage_ratio_lookup_paths)]
  #   }
  #   result$list_files$damage_ratio_lookup_paths <- unlist(result$list_files$damage_ratio_lookup_paths)
  #   result$list_files
  # })
  #
  # output$dt_disagg_locator_descriptions <- renderDT(server=FALSE, {
  #   entry <- grep("disagg_locator_descriptions", result$tbl_modelsDetails$data_settings$datafile_selectors)
  #   cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[entry]]$allow_multiple
  #   if (cond) {
  #     select = "multiple"
  #   } else {
  #     select = "single"
  #   }
  #   tbl_files_filtered <- result$tbl_files %>% filter(file_description == "disagg_locator_descriptions_paths")
  #   df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
  #   # show last element of data frame first in table
  #   df <- df[nrow(df):1, ]
  #   colnames(df) <- c("ID", "Name")
  #   datatable(df, selection = select)
  # })
  #
  # observeEvent(input$dt_disagg_locator_descriptions_rows_selected, {
  #   for (x in seq_len(length(input$dt_disagg_locator_descriptions_rows_selected))) {
  #     # because df is reversed, also order of row has to be reversed
  #     entry <- (nrow(result$tbl_files)+1) - x
  #     result$list_files$disagg_locator_descriptions_paths[x] <- result$tbl_files[entry,]$filename
  #     result$list_files$disagg_locator_descriptions_paths <-
  #       result$list_files$disagg_locator_descriptions_paths[!is.na(result$list_files$disagg_locator_descriptions_paths)]
  #   }
  #   result$list_files$disagg_locator_descriptions_paths <- unlist(result$list_files$disagg_locator_descriptions_paths)
  #   result$list_files
  # })
  #
  # output$dt_schema_file <- renderDT(server=FALSE, {
  #   entry <- grep("schema_file_paths", result$tbl_modelsDetails$data_settings$datafile_selectors)
  #   cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[entry]]$allow_multiple
  #   if (cond) {
  #     select = "multiple"
  #   } else {
  #     select = "single"
  #   }
  #   tbl_files_filtered <- result$tbl_files %>% filter(file_description == "schema_file_paths")
  #   df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
  #   # show last element of data frame first in table
  #   df <- df[nrow(df):1, ]
  #   colnames(df) <- c("ID", "Name")
  #   datatable(df, selection = select)
  # })
  #
  # observeEvent(input$dt_schema_file_rows_selected, {
  #   for (x in seq_len(length(input$dt_schema_file_rows_selected))) {
  #     # because df is reversed, also order of row has to be reversed
  #     entry <- (nrow(result$tbl_files)+1) - x
  #     result$list_files$schema_file_paths[x] <- result$tbl_files[entry,]$filename
  #     result$list_files$schema_file_paths <- list(result$list_files$schema_file_paths[!is.na(result$list_files$schema_file_paths)])
  #   }
  #   result$list_files$schema_file_paths <- unlist(result$list_files$schema_file_paths)
  #   result$list_files
  # })
  #
  # output$dt_vuln_locator_shapefile <- renderDT(server=FALSE, {
  #   entry <- grep("vuln_locator_shapefile_paths", result$tbl_modelsDetails$data_settings$datafile_selectors)
  #   cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[entry]]$allow_multiple
  #   if (cond) {
  #     select = "multiple"
  #   } else {
  #     select = "single"
  #   }
  #   tbl_files_filtered <- result$tbl_files %>% filter(file_description == "vuln_locator_shapefile_paths")
  #   df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
  #   # show last element of data frame first in table
  #   df <- df[nrow(df):1, ]
  #   colnames(df) <- c("ID", "Name")
  #   datatable(df, selection = select)
  # })
  #
  # observeEvent(input$dt_vuln_locator_shapefile_rows_selected, {
  #   for (x in seq_len(length(input$dt_vuln_locator_shapefile_rows_selected))) {
  #     # because df is reversed, also order of row has to be reversed
  #     entry <- (nrow(result$tbl_files)+1) - x
  #     result$list_files$vuln_locator_shapefile_paths[x] <- result$tbl_files[entry,]$filename
  #     result$list_files$vuln_locator_shapefile_paths <- result$list_files$vuln_locator_shapefile_paths[
  #       !is.na(result$list_files$vuln_locator_shapefile_paths)]
  #   }
  #   result$list_files$vuln_locator_shapefile_paths <- unlist(result$list_files$vuln_locator_shapefile_paths)
  #   result$list_files
  # })

  # dynamically create fileInputs and DTOutputs
  output$browsers_tables <- renderUI({
    if (!is.null(result$tbl_modelsDetails$data_settings$datafile_selectors)) {
      result$inputID <- lapply(seq_len(length(result$tbl_modelsDetails$data_settings$datafile_selectors)), function(i) {
        result$tbl_modelsDetails$data_settings$datafile_selectors[[i]]$name
      })
      label_pre <- lapply(seq_len(length(result$tbl_modelsDetails$data_settings$datafile_selectors)), function(i) {
        paste(strsplit(result$tbl_modelsDetails$data_settings$datafile_selectors[[i]]$name, "_"), " ")
      })
      accept <- lapply(seq_len(length(result$tbl_modelsDetails$data_settings$datafile_selectors)), function(i) {
        paste0(".", unlist(result$tbl_modelsDetails$data_settings$datafile_selectors[[i]]$search_filters))
      })
      result$outputID <- result$inputID
      label <- paste(label_pre, sep = " ")
      result$browsers_tables <- lapply(seq_len(length(result$tbl_modelsDetails$data_settings$datafile_selectors)), function(i) {
        c(column(4, fileInput(inputId = ns(result$inputID[i]), label = label_pre[[i]], multiple = TRUE, accept = accept[i])),
          column(4, DTOutput(ns(result$outputID[i]))))
      })
    } else {
      result$browsers_tables <- NULL
    }
    result$browsers_tables
  })

  observeEvent(result$outputID, {
    lapply((seq_len(length(result$outputID))), function(i) {
      output[[result$outputID[[i]]]] <- renderDT(server = FALSE, {
        cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[i]]$allow_multiple
        if (cond) {
          select = "multiple"
        } else {
          select = "single"
        }

        tbl_files_filtered <- result$tbl_files[c(grep(result$outputID[[i]], result$tbl_files$file_description)),]
        # tbl_files_filtered <- result$tbl_files %>% filter(file_description == result$outputID[[i]])
        df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
        # show last element of data frame first in table
        df <- df[nrow(df):1, ]
        colnames(df) <- c("ID", "Name")
        datatable(df, selection = select)
      })
    })
  })

  # TODO: access observeEvent when input[[result$inputID[[i]]]] is triggered
  # observeEvent(input[[result$inputID[[1]]]], ignoreInit = TRUE, {
  #   for (i in seq_len(nrow(input[[result$inputID[[1]]]]))) {
  #     .uploadDamageFile(file_entry = result$inputID[[1]], file_name = input[[result$inputID[[1]]]][i,])
  #   }
  # })

  #TODO: should work for all output[[result$outputID[[i]]]], not just 1
  # observeEvent(output[[result$outputID[[1]]]], {
  #   browser()
  # })

  # output$dt_model_files <- renderDT(server=FALSE, {
  #   result$tbl_files <- session$userData$data_hub$return_tbl_dataFiles(name = "")
  #   df <- data.frame(id = result$tbl_files$id, names = result$tbl_files$filename, descr = result$tbl_files$file_description)
  #   # show last element of data frame first in table
  #   df <- df[nrow(df):1, ]
  #   colnames(df) <- c("ID", "Name", "Description")
  #   datatable(df, selection = "multiple")
  # })

  # observeEvent(input$dt_model_files_rows_selected, {
  #   for (x in seq_len(length(input$dt_model_files_rows_selected))) {
  #     # because df is reversed, also order of row has to be reversed
  #     entry <- (nrow(result$tbl_files)+1) - x
  #     if (result$tbl_files[entry,]$file_description == "damage_ratio_paths") {
  #       result$list_files$damage_ratio_paths[x] <- result$tbl_files[entry,]$filename
  #       result$list_files$damage_ratio_paths <- result$list_files$damage_ratio_paths[!is.na(result$list_files$damage_ratio_paths)]
  #       if (length(result$list_files$damage_ratio_paths) > 1) {
  #         result$list_files$damage_ratio_paths <- list(result$list_files$damage_ratio_paths)
  #         # remove null elements in list
  #         result$list_files$damage_ratio_paths <- as.list(unlist(result$list_files$damage_ratio_paths))
  #       }
  #     } else if (result$tbl_files[entry,]$file_description == "damage_ratio_lookup_paths") {
  #       result$list_files$damage_ratio_lookup_paths[x] <- result$tbl_files[entry,]$filename
  #       result$list_files$damage_ratio_lookup_paths <-
  #         result$list_files$damage_ratio_lookup_paths[!is.na(result$list_files$damage_ratio_lookup_paths)]
  #       if (length(result$list_files$damage_ratio_lookup_paths) > 1) {
  #         result$list_files$damage_ratio_lookup_paths <- list(result$list_files$damage_ratio_lookup_paths)
  #         # remove null elements in list
  #         result$list_files$damage_ratio_lookup_paths <- as.list(unlist(result$list_files$damage_ratio_lookup_paths))
  #       }
  #     } else if (result$tbl_files[entry,]$file_description == "disagg_locator_descriptions_paths") {
  #       result$list_files$disagg_locator_descriptions_paths[x] <- result$tbl_files[entry,]$filename
  #       result$list_files$disagg_locator_descriptions_paths <-
  #         result$list_files$disagg_locator_descriptions_paths[!is.na(result$list_files$disagg_locator_descriptions_paths)]
  #       if (length(result$list_files$disagg_locator_descriptions_paths) > 1) {
  #         result$list_files$disagg_locator_descriptions_paths <- list(result$list_files$disagg_locator_descriptions_paths)
  #         # remove null elements in list
  #         result$list_files$disagg_locator_descriptions_paths <- as.list(unlist(result$list_files$disagg_locator_descriptions_paths))
  #       }
  #     } else if (result$tbl_files[entry,]$file_description == "schema_file_paths") {
  #       result$list_files$schema_file_paths[x] <- result$tbl_files[entry,]$filename
  #       result$list_files$schema_file_paths <- list(result$list_files$schema_file_paths[!is.na(result$list_files$schema_file_paths)])
  #       if (length(result$list_files$schema_file_paths) > 1) {
  #         result$list_files$schema_file_paths <- list(result$list_files$schema_file_paths)
  #         # remove null elements in list
  #         result$list_files$schema_file_paths <- as.list(unlist(result$list_files$schema_file_paths))
  #       }
  #     }
  #   }
  #   result$list_files
  # })

  # # upload files in fileinputs
  # observeEvent(input$damage_ratio_paths, ignoreInit = TRUE, {
  #   for (i in seq_len(nrow(input$damage_ratio_paths))) {
  #     .uploadDamageFile(file_entry = "damage_ratio_paths", file_name = input$damage_ratio_paths[i,])
  #   }
  # })
  #
  # observeEvent(input$damage_ratio_lookup_paths, ignoreInit = TRUE, {
  #   for (i in seq_len(nrow(input$damage_ratio_lookup_paths))) {
  #     .uploadDamageFile(file_entry = "damage_ratio_lookup_paths", file_name = input$damage_ratio_lookup_paths[i,])
  #   }
  # })
  #
  # observeEvent(input$disagg_locator_descriptions_paths, ignoreInit = TRUE, {
  #   for (i in seq_len(nrow(input$disagg_locator_descriptions_paths))) {
  #     .uploadDamageFile(file_entry = "disagg_locator_descriptions_paths", file_name = input$disagg_locator_descriptions_paths[i,])
  #   }
  # })
  #
  # observeEvent(input$schema_file_paths, ignoreInit = TRUE, {
  #   for (i in seq_len(nrow(input$schema_file_paths))) {
  #     .uploadDamageFile(file_entry = "schema_file_paths", file_name = input$schema_file_paths[i,])
  #   }
  # })
  #
  # observeEvent(input$vuln_locator_shapefile_paths, ignoreInit = TRUE, {
  #   for (i in seq_len(nrow(input$vuln_locator_shapefile_paths))) {
  #     .uploadDamageFile(file_entry = "vuln_locator_shapefile_paths", file_name = input$vuln_locator_shapefile_paths[i,])
  #   }
  # })

  # output new analysis settings with changed values
  observeEvent(input$abuttonselsettings, {
    if (!is.null(result$changed_entry)) {
      rows_selected <- input$dt_model_values_rows_selected

      new_settings <- result$settings_names[rows_selected]
      x <- strsplit(result$changed_entry, " ")

      # change edited values in the table
      for (y in seq_len(length(x))) {
        entry_1 <- as.numeric(x[[y]][[1]])
        entry_2 <- as.numeric(x[[y]][[2]])
        entry_val <- x[[y]][[3]]
        result$tbl_modelsDetails$model_settings[[entry_1]][[entry_2]]$default <- entry_val
      }
    }
    #retrieve all files for analysis and place them under complex_model_data_files
    list_data_files_names <- list(input$damage_ratio_lookup_paths$name,
                                  input$damage_ratio_paths$name,
                                  input$disagg_locator_descriptions_paths$name,
                                  input$schema_file_paths$name,
                                  input$vuln_locator_shapefile_paths$name)
    get_file_ids <- lapply(unlist(list_data_files_names), function(x) {
      file <- result$tbl_files %>% filter(filename == x)
      file$id
    })
    patch_analyses <- session$userData$oasisapi$api_patch_query(query_path = paste("analyses", analysisID(), sep = "/"),
                                                                query_body = list(name = analysisNAME(),
                                                                                  portfolio = portfolioID(),
                                                                                  model = modelID(),
                                                                                  complex_model_data_files = get_file_ids),
                                                                query_method = "PATCH")

    result$tbl_modelsDetails$model_settings$event_occurrence_id <- result$tbl_modelsDetails$model_settings$event_occurrence_id$default
    result$tbl_modelsDetails$model_settings$event_set <- result$tbl_modelsDetails$model_settings$event_set$default
    filtered_settings <- c("model_configurable" = TRUE, result$tbl_modelsDetails$model_settings)

    hide("panel_build_fly_actions")
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
        model_settings = c(filtered_settings, result$list_files),
        gul_output = FALSE,
        gul_summaries = list(gul_summaries)
      ), result$list_files
    ))
  })

  # Reload files table
  .reloadtbl_modelsFiles <- function() {
    logMessage(".reloadtbl_modelsFiles called")
    result$tbl_files <- session$userData$data_hub$return_tbl_dataFiles(name = "")
    invisible()
  }

  # upload files in second tab
  .uploadDamageFile <- function(file_entry, file_name) {
    logMessage(paste0("Uploading file ", file_name))
    if (!is.null(file_name)) {
      post_file <- session$userData$oasisapi$api_body_query(query_path = paste("data_files", sep = "/"),
                                                            query_body = list(file_description = file_entry),
                                                            query_method = "POST")
      get_file_id <- content(post_file$result)$id
      tmp <- unlist(strsplit(file_name$datapath, split = "/"))
      datapath <- paste(c(tmp[-length(tmp)], ""), collapse = "/")
      file_to_upload <- paste0(datapath, file_name$name)
      file.rename(file_name$datapath, file_to_upload)
      withModalSpinner(
        post_file_content <- session$userData$oasisapi$api_post_file_query(paste("data_files", get_file_id, "content", sep = "/"),
                                                                           query_body = file_to_upload),
        "Linking...",
        size = "s"
      )
      if (post_file_content$status == "Success") {
        oasisuiNotification(type = "message",
                            paste("Damage file linked successfully."))
      } else {
        oasisuiNotification(type = "error",
                            paste("Damage file link failed."))
      }
    }
    .reloadtbl_modelsFiles()
    invisible()
  }
  moduleOutput <- reactive({result$filtered_analysis_settings})

  moduleOutput
}

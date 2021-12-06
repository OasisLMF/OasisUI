# Shared Module documentation --------------------------------------------------
#' Hazard Map Module
#'
#' Shiny Module for showing customized details.
#'
#' @template params-module
#'
#' @name buildCustom
NULL


# Build Custom UI -----------------------------------------------

#' @describeIn BuildCustom Returns the UI elements of the module.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
buildCustomUI <- function(id) {

  ns <- NS(id)
  oasisuiPanel(
    ns("panel_build_custom_actions"),
    collapsible = FALSE,
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_BuildCustom"), inline = TRUE),
      actionButton(inputId = ns("buttonhidebuildcustom"), label = NULL, icon = icon("times"), style = "float: right;"),
      oasisuiButton(inputId = ns("abuttonselsettings"), label = "Apply", style = "float: right;") %>%
        bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonselsettings, placement = "right")
    ),
    tabsetPanel(
      id = ns("panel_build_Custom"),
      tabPanel(
        title = "Model Values",
        numericInput(ns("inputnumsamples"), label = "Number of Samples:", value = 9),
        uiOutput(ns("tabs_BuildCustom"), inline = TRUE)
      ),
      tabPanel(
        title = "File Uploads",
        fluidRow(uiOutput(ns("browsers_tables")))
      )
    )
  )
}


# Model Details Server -------------------------------------------

#' buildCustom
#'
#' @param portfolioID Selected portfolio ID.
#' @param modelID Selected model ID.
#' @param supplierID Selected supplier model ID.
#' @param versionID Selected model ID version.
#' @param analysisID Selected analysis ID.
#'
#' @describeIn buildCustom Allows user to build their own mode on the fly.
#'
#' @importFrom DT renderDT
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#'
#' @export
buildCustom <- function(input,
                        output,
                        session,
                        portfolioID,
                        modelID,
                        supplierID,
                        versionID,
                        analysisID,
                        counter,
                        active = reactive(TRUE)) {

  ns <- session$ns

  result <- reactiveValues(
    # reactive value for detail of model table
    tbl_modelsDetails = NULL,
    # model settings tracking
    settings_df = NULL,
    # model settings table
    settings_tbl = NULL,
    # new settings selected from table
    filtered_analysis_settings = NULL,
    # table with list of data files
    tbl_files = NULL,
    # list of files to write in analysis settings
    list_files = NULL,
    # FileInputs IDs
    inputID = NULL,
    # Output IDs tables
    outputID = NULL,
    # extrapolate file ids
    file_ids = list(),
    list_files = list()
  )

  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_build_custom_actions")
    # initialize table selection to NULL every time panel is opened
    result$settings_df <- NULL
    result$tbl_modelsDetails <- NULL
    result$settings_tbl <- NULL
    .reloadtbl_modelsValue()

  })

  output$paneltitle_BuildCustom <- renderUI({
    "Change values in the model"
  })

  observeEvent(input$buttonhidebuildcustom, {
    hide("panel_build_custom_actions")
    logMessage("hiding panelBuildCustom")
  })

  # render dynamically multiple tabs for Model Values section
  output$tabs_BuildCustom <- renderUI({
    nTabs <- length(result$tbl_modelsDetails$model_settings$parameter_groups)
    if (nTabs == 0) {
      # in case parameter_group is not available, create only one tab
      param_group <- as.list(
        c(
          names(result$tbl_modelsDetails$model_settings[1:2]),
          unlist(
            lapply(result$tbl_modelsDetails$model_settings[3:length(result$tbl_modelsDetails$model_settings)], function(x) {
              sapply(x, function(y) {
                y$name
              })
            }))
        )
      )
      names(param_group) <- NULL
      myTabs = lapply(1, function (x) {
        tabPanel(
          title = "Model Parameters",
          Global_funs(session, result$tbl_modelsDetails$model_settings, "generation",
                      param_group)
        )
      })
    } else {
      myTabs = lapply(seq_len(nTabs), function (x) {
        tabPanel(
          title = result$tbl_modelsDetails$model_settings$parameter_groups[[x]]$name,
          Global_funs(session, result$tbl_modelsDetails$model_settings, "generation",
                      result$tbl_modelsDetails$model_settings$parameter_groups[[x]]$presentation_order)
        )
      })
    }

    do.call(tabsetPanel, myTabs)
  })

  # dynamically create fileInputs and DTOutputs
  output$browsers_tables <- renderUI({
    df_selectors <- result$tbl_modelsDetails$data_settings$datafile_selectors
    if (!is.null(df_selectors)) {
      result$inputID <- lapply(seq_len(length(df_selectors)), function(i) {
        df_selectors[[i]]$name
      })
      label_pre <- lapply(seq_len(length(df_selectors)), function(i) {
        paste(strsplit(df_selectors[[i]]$name, "_")[[1]], collapse = " ")
      })
      accept <- lapply(seq_len(length(df_selectors)), function(i) {
        ext <- paste0(".", unlist(df_selectors[[i]]$search_filters))
        ext
      })
      result$outputID <- result$inputID
      ui_content <- list()
      for (i in seq_len(length(df_selectors))) {
        ui_content[[i * 2]] <- fluidRow(column(1), column(10, DTOutput(ns(paste0("dt_", result$outputID[i])))))
        ui_content[[i * 2 - 1]] <- fluidRow(column(1),
                                            column(10, fileInput(inputId = ns(result$inputID[i]),
                                                                 label = label_pre[[i]],
                                                                 multiple = TRUE, accept = accept[i])))
      }
      tagList(ui_content)
    } else {
      paste0("this model has no datafile selectors")
    }
  })

  observeEvent(result$outputID, {
    lapply((seq_len(length(result$outputID))), function(i) {
      dt_output <- paste0("dt_", result$outputID[[i]])
      output[[dt_output]] <- renderDT(server = FALSE, {
        cond <- result$tbl_modelsDetails$data_settings$datafile_selectors[[i]]$allow_multiple
        if (cond) {
          select = "multiple"
        } else {
          select = "single"
        }
        tbl_files_filtered <- result$tbl_files[c(grep(result$outputID[[i]], result$tbl_files$file_description)),]
        df <- data.frame(id = tbl_files_filtered$id, names = tbl_files_filtered$filename)
        # show last element of data frame first in table
        df <- df[nrow(df):1, ]
        if (length(df) > 0) {
          colnames(df) <- c("ID", "Name")
          datatable(df, selection = select, rownames = FALSE)
        } else {
          datatable(data.frame("ID" = character(0), "Name" = character(0)), rownames = FALSE)
        }
      })
    })
  })

  observeEvent(result$inputID, {
    h <- unlist(result$inputID)
    df_selectors <- result$tbl_modelsDetails$data_settings$datafile_selectors
    lapply(seq_len(length(h)), function(i) {
      check_ext <- paste0(".", unlist(df_selectors[[i]]$search_filters))
      observeEvent(input[[h[i]]], {
        for (j in seq_len(nrow(input[[h[i]]]))) {
          if (paste0(".", tools::file_ext(input[[h[i]]][j,]$name)) %in% check_ext) {
            .uploadDamageFile(file_entry = h[i], file_name = input[[h[i]]][j, ])
          } else {
            oasisuiNotification(type = "error", paste("Extension not supported."))
          }
        }
      })
    })
    .reloadtbl_modelsFiles()
  })

  # output new analysis settings with changed values through tabs
  observeEvent(input$abuttonselsettings, {
    fetch_model_settings <- function(model_settings) {

      n_string <- grep("string_parameters", names(model_settings))
      string_input <- lapply(seq_len(length(model_settings[n_string]$string_parameters)), function(x) {
        sapply(seq_len(length(model_settings[n_string]$string_parameters[[x]]$default)), function(z) {
          if (!is.null(input[[paste0("string_parameters", x, z)]])) {
            result$tbl_modelsDetails$model_settings$string_parameters[[x]]$default[z] <-
              input[[paste0("string_parameters", x, z)]]
          }
          result$tbl_modelsDetails$model_settings$string_parameters[[x]]$default[z]
        })
      })

      n_dict <- grep("dictionary_parameters", names(model_settings))
      dict_input <- lapply(seq_len(length(model_settings[n_dict]$dictionary_parameters)), function(x) {
        sapply(seq_len(length(model_settings[n_dict]$dictionary_parameters[[x]]$default)), function(z) {
          if (!is.null(input[[paste0("dictionary_parameters", x, z)]])) {
            result$tbl_modelsDetails$model_settings$dictionary_parameters[[x]]$default[z] <-
              input[[paste0("dictionary_parameters", x, z)]]
          }
          result$tbl_modelsDetails$model_settings$dictionary_parameters[[x]]$default[z]
        })
      })

      n_dropdown <- grep("dropdown_parameters", names(model_settings))
      dropdown_input <- lapply(seq_len(length(model_settings[n_dropdown]$dropdown_parameters)), function(x) {
        sapply(seq_len(length(model_settings[n_dropdown]$dropdown_parameters[[x]]$default)), function(z) {
          if (!is.null(input[[paste0("dropdown_parameters", x)]])) {
            result$tbl_modelsDetails$model_settings$dropdown_parameters[[x]]$default[z] <-
              input[[paste0("dropdown_parameters", x)]]
          }
          result$tbl_modelsDetails$model_settings$dropdown_parameters[[x]]$default[z]
        })
      })

      n_bool <- grep("boolean_parameters", names(model_settings))
      boolean_input <- lapply(seq_len(length(model_settings[n_bool]$boolean_parameters)), function(x) {
        sapply(seq_len(length(model_settings[n_bool]$boolean_parameters[[x]]$default)), function(z) {
          if (!is.null(input[[paste0("boolean_parameters", x)]])) {
            result$tbl_modelsDetails$model_settings$boolean_parameters[[x]]$default[z] <-
              input[[paste0("boolean_parameters", x)]]
          }
          unlist(result$tbl_modelsDetails$model_settings$boolean_parameters[[x]]$default[z])
        })
      })

      n_float <- grep("float_parameters", names(model_settings))
      float_input <- lapply(seq_len(length(model_settings[n_float]$float_parameters)), function(x) {
        sapply(seq_len(length(model_settings[n_float]$float_parameters[[x]]$default)), function(z) {
          if (!is.null(input[[paste0("float_parameters", x)]])) {
            result$tbl_modelsDetails$model_settings$float_parameters[[x]]$default[z] <-
              input[[paste0("float_parameters", x)]]
          }
          result$tbl_modelsDetails$model_settings$float_parameters[[x]]$default[z]
        })
      })

      n_list <- grep("list_parameters", names(model_settings))
      list_input <- lapply(seq_len(length(model_settings[n_list]$list_parameters)), function(x) {
        if (!is.null(input[[paste0("list_parameters", x)]])) {
          result$tbl_modelsDetails$model_settings$list_parameters[[x]]$default <-
            as.list(strsplit(input[[paste0("list_parameters", x)]], ",")[[1]])
          result$tbl_modelsDetails$model_settings$list_parameters[[x]]$default
        }
      })

      inputs_list <- list(string_input,
                          list_input,
                          dict_input,
                          float_input,
                          dropdown_input)

      params_list <- list("string_parameters",
                          "list_parameters",
                          "dictionary_parameters",
                          "float_parameters",
                          "dropdown_parameters")

      # create list of re-ordered and grouped model inputs names
      inputs_name <- list()
      for (param in seq_len(length(params_list))) {
        if (length(inputs_list[[param]]) > 0) {
          n_param <- grep(params_list[[param]], names(model_settings))
          param_name <- unlist(lapply(seq_len(length(model_settings[n_param][[1]])), function(i) {
            # model_settings[i][[1]][[param]][["name"]]
            model_settings[n_param][[1]][[i]][["name"]]
          }))
          inputs_name[[param]] <- param_name
        }
        # if a param is NULL and skipped, it will result in a NULL entry in the inputs_name list that will be removed by the unlist below
      }
      inputs_name <- unlist(inputs_name)
      # find boolean parameters names
      if (length(boolean_input) > 0) {
        boolean_name <- lapply(seq_len(length(boolean_input)), function(i) {
          model_match <- model_settings[grep("boolean_parameters", names(model_settings))]$boolean_parameters[[i]]
          model_match[["name"]]
        })
      } else {
        boolean_name <- NULL
      }

      # create model settings for analysis settings
      model_settings_out <- c(input$event_set_g,
                              input$event_occurrence_g,
                              input$inputnumsamples,
                              # note that boolean_input is a list, making sure that the result of this c() is a flat list!
                              boolean_input,
                              string_input,
                              list_input,
                              dict_input,
                              float_input,
                              dropdown_input)
      # NULL or list() elements won't survive the c() above!
      # create list/vector of names for model settings
      names_full_list <- c("event_set",
                           "event_occurrence_id",
                           "number_of_samples",
                           boolean_name,
                           inputs_name)

      if (any(sapply(names_full_list, is.na))) {
        names(model_settings_out) <- names_full_list[-which(sapply(names_full_list, is.na))]
      } else if(length(model_settings_out) > 0) {
        names(model_settings_out) <- names_full_list
      }

      model_settings_out
    }

    core_model_settings <- fetch_model_settings(result$tbl_modelsDetails$model_settings)

    filtered_settings <- c("model_configurable" = TRUE, core_model_settings)

    hide("panel_build_custom_actions")
    logMessage("hiding panelBuildCustom")
    oasisuiNotification(type = "message",
                        "Model settings filtered by chosen entries.")
    gul_summaries <- summary_template

    h <- unlist(result$inputID)
    lapply(seq_len(length(h)), function(i) {
      input_dt <- paste0("dt_", h[i], "_rows_selected")
      if (!is.null(input[[input_dt]])) {
        tbl <- result$tbl_files %>% filter(file_description == h[i])
        tbl <- tbl[nrow(tbl):1,]
        file_ids <- tbl[input[[input_dt]], "id"]
        result$file_ids[[i]] <- file_ids
        file_names <- tbl[input[[input_dt]], "filename"]
        # because df is reversed, also order of row has to be reversed
        result$list_files[[h[i]]] <- file_names
        result$list_files[[h[i]]] <- result$list_files[[h[i]]][!is.na(result$list_files[[h[i]]])]
        if (length(result$list_files[[h[i]]]) > 1) {
          result$list_files[[h[i]]] <- as.list(result$list_files[[h[i]]])
        }
        result$list_files[[h[i]]]
      }
    })

    result$filtered_analysis_settings <- list(analysis_settings = c(
      list(
        model_supplier_id = supplierID(),
        model_name_id = modelID(),
        number_of_samples = input$inputnumsamples,
        model_settings = c(filtered_settings, result$list_files),
        gul_output = FALSE,
        gul_summaries = list(gul_summaries)
      )
    ))
  })

  .reloadtbl_modelsValue <- function() {
    result$tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
      query_path = paste("models", modelID(), "settings", sep = "/"),
      query_method = "GET"
    )
    result$settings_tbl
  }

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

  moduleOutput <- list(
    fullsettings = reactive({result$filtered_analysis_settings}),
    fileids = reactive({unlist(result$file_ids)}),
    changeddefaults = reactive({result$settings_df})
  )

  moduleOutput
}

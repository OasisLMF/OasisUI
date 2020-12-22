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
#' @importFrom bsplus bs_embed_tooltip
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
      actionButton(inputId = ns("buttonhidebuildfly"), label = NULL, icon = icon("times"), style = "float: right;"),
      oasisuiButton(inputId = ns("abuttonselsettings"), label = "Apply", style = "float: right;") %>%
        bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonselsettings, placement = "right")
    ),
    tabsetPanel(
      id = ns("panel_build_Fly"),
      tabPanel(
        title = "Model Values",
        DTOutput(ns("dt_model_values"))),
      tabPanel(
        title = "File Uploads",
        fluidRow(uiOutput(ns("browsers_tables")))
      )
    )
  )
}


# Model Details Server -------------------------------------------

#' buildFly
#'
#' @param portfolioID Selected portfolio ID.
#' @param modelID Selected model ID.
#' @param supplierID Selected supplier model ID.
#' @param versionID Selected model ID version.
#' @param analysisID Selected analysis ID.
#'
#' @describeIn buildFly Allows user to build their own mode on the fly.
#'
#' @importFrom DT renderDT
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
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
    file_ids = NULL
  )

  # Initialize -----------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    show("panel_build_fly_actions")
    selectRows(proxy = dataTableProxy("dt_model_values"), selected = NULL)
    # initialize table selection to NULL every time panel is opened
    result$settings_df <- NULL
    result$tbl_modelsDetails <- NULL
    result$settings_tbl <- NULL
    .reloadtbl_modelsValue()
  })

  output$paneltitle_BuildFly <- renderUI({
    "Change values in the model"
  })

  observeEvent(input$buttonhidebuildfly, {
    hide("panel_build_fly_actions")
    logMessage("hiding panelBuildFly")
  })

  output$dt_model_values <- renderDT(server = FALSE, {
    datatable(result$settings_tbl, caption = "Double click on Default values to edit",
              editable = list(target = 'cell', disable = list(columns = c(1,2))), selection = "none", rownames = FALSE)
  })

  # extrapolate changed cells
  observeEvent(input$dt_model_values_cell_edit, {
    inp_celledit <- input$dt_model_values_cell_edit
    result$settings_df[inp_celledit$row, "value"] <- inp_celledit$value
    result$settings_df[inp_celledit$row, "changed"] <- TRUE
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
        ui_content[[i * 2]] <- fluidRow(column(1), column(10, DTOutput(ns(paste0("dt_",result$outputID[i])))))
        ui_content[[i * 2 - 1]] <- fluidRow(column(1), column(10, fileInput(inputId = ns(result$inputID[i]), label = label_pre[[i]],
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
        colnames(df) <- c("ID", "Name")
        datatable(df, selection = select, rownames = FALSE)
      })
    })
  })

  observeEvent(result$inputID, {
    h <- unlist(result$inputID)
    lapply(seq_len(length(h)), function(i) {
      observeEvent(input[[h[i]]], {
        for (j in seq_len(nrow(input[[h[i]]]))) {
          .uploadDamageFile(file_entry = h[i], file_name = input[[h[i]]][j, ])
        }
      })
    })

    lapply(seq_len(length(h)), function(i) {
      input_dt <- paste0("dt_", h[i], "_rows_selected")
      observeEvent(input[[input_dt]], {
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
      })
    })
  })

  # output new analysis settings with changed values
  observeEvent(input$abuttonselsettings, {
    # update default choices for edited model settings
    if (any(result$settings_df$changed)) {
      new_settings <- result$settings_df %>% filter(changed)
      res_mdlsettings <- result$tbl_modelsDetails$model_settings

      invisible(lapply(seq_len(length(names(res_mdlsettings))), function(x) {
        if (is.null(res_mdlsettings[[x]]$name)) {
          lapply(seq_len(length(res_mdlsettings[[x]])), function(y) {
            findSetting <- new_settings$names %in% res_mdlsettings[[x]][[y]]
            # (typically new_settings$names will match the name attribute, i.e. res_mdlsettings[[x]][[y]]$name)
            if (any(findSetting)) {
              if (is.list(res_mdlsettings[[x]][[y]]$default) && !is.null(names(res_mdlsettings[[x]][[y]]$default))) {
                # dict parameters case
                result$tbl_modelsDetails$model_settings[[x]][[y]]$default <- jsonlite::fromJSON(new_settings$value[which(findSetting)])
              } else {
                if (is.list(res_mdlsettings[[x]][[y]]$default)) {
                  # list parameters case
                  result$tbl_modelsDetails$model_settings[[x]][[y]]$default <- as.list(strsplit(new_settings$value[which(findSetting)], split = ",  ")[[1]])
                } else {
                  result$tbl_modelsDetails$model_settings[[x]][[y]]$default <- strsplit(new_settings$value[which(findSetting)], split = ",  ")[[1]]
                }
              }
            }
          })
        } else {
          # e.g. Event Occurrence
          findSetting <- new_settings$names %in% res_mdlsettings[[x]]
          if (any(findSetting)) {
            result$tbl_modelsDetails$model_settings[[x]]$default <- strsplit(new_settings$value[which(findSetting)], split = ",  ")[[1]]
          }
        }
      }))
    }

    # TODO: update result$tbl_modelsDetails$model_setting just once, do above and below in memory first (i.e. make a function as outlined above)
    #result$tbl_modelsDetails$model_settings$event_occurrence_id <- result$tbl_modelsDetails$model_settings$event_occurrence_id$default
    #result$tbl_modelsDetails$model_settings$event_set <- result$tbl_modelsDetails$model_settings$event_set$default
    # replace all settings similarly with just default values
    fetch_model_settings <- function(model_settings) {
      model_settings <- model_settings %>% unlist(recursive = FALSE)
      string_input <- unlist(lapply(grep("string_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      }))

      dict_input <- lapply(grep("dictionary_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      })

      dropdown_input <- lapply(grep("dropdown_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      })

      # below is purposedly list() rather than NULL in case there are none (i.e. not doing unlist() on purpose)!
      boolean_input <- lapply(grep("boolean_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$default)) {
          as.logical(model_settings[[x]]$default)
        }
      })

      float_input <- lapply(grep("float_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      })

      list_input <- lapply(grep("list_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
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
          param_name <- unlist(lapply(grep(params_list[[param]], names(model_settings)), function(i) {
            model_settings[[i]][["name"]]
          }))
          inputs_name[[param]] <- param_name
        }
        # if a param is NULL and skipped, it will result in a NULL entry in the inputs_name list that will be removed by the unlist below
      }
      inputs_name <- unlist(inputs_name)

      # find boolean parameters names
      if (length(boolean_input) > 0) {
        boolean_name <- lapply(seq_len(length(boolean_input)), function(i) {
          model_match <- model_settings[grep("boolean_parameters", names(model_settings))][[i]]
          model_match[["name"]]
        })
      } else {
        boolean_name <- NULL
      }

      # create model settings for analysis settings
      model_settings_out <- c(model_settings$event_set.default,
                              model_settings$event_occurrence_id.default,
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

    hide("panel_build_fly_actions")
    logMessage("hiding panelBuildFly")
    oasisuiNotification(type = "message",
                        "Model settings filtered by chosen entries.")
    gul_summaries <- summary_template

    result$filtered_analysis_settings <- list(analysis_settings = c(
      list(
        module_supplier_id = supplierID(),
        model_version_id = versionID(),
        number_of_samples = 0,
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
    result$tbl_files <- session$userData$data_hub$return_tbl_dataFiles(name = "")
    result$list_files <- list()
    result$file_ids <- list()

    # get entries for table: name, description and default value(s)
    tbl_mdl_settings <- result$tbl_modelsDetails$model_settings
    # drop "parameter_groups" (and potentially other unknown entries):
    subset_settings <- names(tbl_mdl_settings) %in% c("event_set",
                                                      "event_occurrence_id",
                                                      "string_parameters",
                                                      "boolean_parameters",
                                                      "float_parameters",
                                                      "list_parameters",
                                                      "dictionary_parameters",
                                                      "dropdown_parameters")
    tbl_mdl_settings <- tbl_mdl_settings[subset_settings]
    order_list <- unlist(lapply(seq_len(length(result$tbl_modelsDetails$model_settings$parameter_groups)), function(x) {
      unlist(lapply(seq_len(length(result$tbl_modelsDetails$model_settings$parameter_groups[[x]]$presentation_order)), function(y) {
        result$tbl_modelsDetails$model_settings$parameter_groups[[x]]$presentation_order[[y]]
      }))
    }))

    settings_names <- unlist(lapply(seq_len(length(names(tbl_mdl_settings))), function(x) {
      if (is.null(tbl_mdl_settings[[x]]$name)) {
        lapply(seq_len(length(tbl_mdl_settings[[x]])), function(y) {
          tbl_mdl_settings[[x]][[y]]$name
        })
      } else {
        tbl_mdl_settings[[x]]$name
      }
    }))

    intersect_n <- intersect(order_list, settings_names)
    if (length(intersect_n) != length(settings_names)) {
      extras <- unlist(lapply(settings_names, function(x) {
        if (x %notin% intersect_n) {
          grep(x, settings_names)
        }
      }))
    } else {
      extras <- NULL
    }
    reorder_nums <- unlist(lapply(intersect_n, function(x) {grep(x, settings_names)}))
    reorder_names <- c(settings_names[extras], settings_names[reorder_nums])

    settings_desc <- unlist(lapply(seq_len(length(names(tbl_mdl_settings))), function(x) {
      if (is.null(tbl_mdl_settings[[x]]$desc)) {
        lapply(seq_len(length(tbl_mdl_settings[[x]])), function(y) {
          tbl_mdl_settings[[x]][[y]]$desc
        })
      } else {
        tbl_mdl_settings[[x]]$desc
      }
    }))
    reorder_desc <- c(settings_desc[extras], settings_desc[reorder_nums])

    settings_tooltip <- unlist(lapply(seq_len(length(names(tbl_mdl_settings))), function(x) {
      if (is.null(tbl_mdl_settings[[x]]$tooltip)) {
        lapply(seq_len(length(tbl_mdl_settings[[x]])), function(y) {
          tbl_mdl_settings[[x]][[y]]$tooltip
        })
      } else {
        tbl_mdl_settings[[x]]$tooltip
      }
    }))
    reorder_tooltip <- c(settings_tooltip[extras], settings_tooltip[reorder_nums])

    settings_default <- unlist(lapply(seq_len(length(names(tbl_mdl_settings))), function(x) {
      if (is.null(tbl_mdl_settings[[x]]$default)) {
        lapply(seq_len(length(tbl_mdl_settings[[x]])), function(y) {
          dflt <- tbl_mdl_settings[[x]][[y]]$default
          if (is.list(dflt) && !is.null(names(dflt))) {
            paste(jsonlite::toJSON(dflt))
          } else {
            # below works on a list same as on a vector
            paste(dflt, collapse = ",  ")
          }
        })
      } else {
        # e.g. event set and event occurrence
        # below works on a list same as on a vector
        paste(tbl_mdl_settings[[x]]$default, collapse = ",  ")
      }
    }))
    reorder_default <- c(settings_default[extras], settings_default[reorder_nums])

    result$settings_df <- data.frame(names = reorder_names, descr = reorder_desc, tooltip = reorder_tooltip, value = reorder_default,
                                     changed = rep(FALSE, times = length(settings_names)), stringsAsFactors = FALSE)
    tmp_df <- result$settings_df[, c("names", "descr", "tooltip", "value")]
    colnames(tmp_df) <- c("Setting Name", "Description", "Tooltip", "Default")
    # output$dt_model_values depends (renders) on this one:
    result$settings_tbl <- tmp_df
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

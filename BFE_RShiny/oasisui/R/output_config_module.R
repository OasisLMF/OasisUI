# Define Output Configuration UI -----------------------------------------------

#' modeldetailsUI
#'
#' @rdname def_out_config
#'
#' @description UI side of function wrapping panel to show oputput configuration.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
def_out_configUI <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    id = ns("panel_anaoutput"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defAnaConfigOutput"), inline = TRUE),
      actionButton(
        inputId = ns("abuttonhidepanelconfigureoutput"),
        label = NULL,
        icon = icon("times"),
        style = "float: right;"
      )
    ),
    fluidRow(
      column(4,
             panelModelParams(id)),
      column(
        8,
        fluidRow(panelOutputParams(id)),
        fluidRow(panelOutputParamsDetails(id))
      )
    ),
    fluidRow(column(
      12,
      oasisuiButton(
        inputId = ns("abuttonexecuteanarun"),
        label = "Execute Run"
      ), align = "right"
    )) %>%
      bs_embed_tooltip(
        title = defineSingleAna_tooltips$abuttonexecuteanarun,
        placement = "right"
      )
  )
}

#' panelModelParams
#'
#' @rdname panelModelParams
#'
#' @description Function wrapping sub-panel to define model params.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelModelParams <- function(id) {
  ns <- NS(id)
  tagList(
    oasisuiPanel(
      collapsible = FALSE,
      id = ns("panel_ModelParams"),
      heading = h4("Model Parameters"),
      div(
        id = ns("basic"),
        style = "width:100%; margin: 0 auto;",
        uiOutput(ns("basic_model_param"))
      ),
      oasisuiButton(inputId = ns("abuttonadvanced"), label = "Advanced"),
      hidden(
        div(
          id = ns("configureAnaParamsAdvanced"),
          align = "left",
          numericInput(ns("tinputnoofsample"), label = "Number of Samples:", value = 10),
          numericInput(ns("tinputthreshold"), label = "Loss Threshold:", value = 0),
          uiOutput(ns("advanced_model_param")),
          oasisuiButton(inputId = ns("abuttonbasic"), label = "Basic")
        )
      )
    )
  )
}


#' panelOutputParams
#'
#' @rdname panelOutputParams
#'
#' @description Function wrapping sub-panel to define output params.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelOutputParams <- function(id) {
  ns <- NS(id)
  tagList(oasisuiPanel(
    collapsible = FALSE,
    id = ns("panel_OutputParams"),
    heading = h4("Output Parameters"),
    fluidRow(
      column(
        8,
        selectInput(
          inputId = ns("sintag"),
          label = "Tag",
          choices = default_tags,
          selected = default_tags[1]
        )
      ),
      column(
        4,
        br(),
        actionButton(
          ns(paste0("abuttonchoosetag")),
          label = NULL,
          icon = icon("list-alt"),
          style = " color: rgb(71, 73, 73);
                    background-color: white;
                    padding: 0px;
                    font-size: 24px;
                    background-image: none;
                    border: none;
          "
        ) %>%
          bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonchoosetag, placement = "right")
      )
    )
  ))
}

#' panelOutputParamsDetails
#'
#' @rdname panelOutputParamsDetails
#'
#' @description Function wrapping sub-panel to define output params details.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelOutputParamsDetails <- function(id) {
  ns <- NS(id)
  tagList(
    oasisuiPanel(
      collapsible = TRUE,
      show = TRUE,
      id = ns("panel_OutputParamsDetails"),
      heading = h4("Output Parameters Details"),
      checkboxGroupInput(
        inputId = ns("chkboxgrplosstypes"),
        label = "Perspective",
        choices = output_options$losstypes,
        inline = TRUE,
        selected = output_options$losstypes[1]
      ),
      # checkboxses for all perspectives; available for all tags
      uiOutput(ns("summary_levels_reports_ui")),
      # combinations of summary levels and reports.
      uiOutput(ns("out_params_review_ui")),
      hidden(
        actionButton(
          inputId = ns("clearselection"),
          label = "Clear",
          style = "float:right;"
        ) %>%
          bs_embed_tooltip(title = defineSingleAna_tooltips$clearselection, placement = "right")
      )
    )
  )
}


# Define Output Configuration Server -------------------------------------------

#' def_out_config
#'
#' @rdname def_out_config
#'
#' @description Server side of function wrapping panel to show oputput configuration.
#'
#' @param analysisID Selected analysis ID.
#' @param analysisName Selected analysis name.
#' @param ana_flag Flag to know if the user is creating a new output configuration or rerunning an analysis.
#' @param counter Counter to decide whether to clear output options and show some panel (?)
#' @template params-module
#' @template params-active
#'
#' @return ana_flag flag to know if the user is creating a new output configuration or rerunning an analysis.
#' @return ana_post_status status of posting the analysis.
#'
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom shinyjs disabled
#' @importFrom shinyjs enable
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom jsonlite write_json
#'
#' @export
def_out_config <- function(input,
                           output,
                           session,
                           analysisID = reactive(NULL),
                           analysisName = reactive(""),
                           ana_flag = reactive("C"),
                           counter = reactive(NULL),
                           active = reactive(TRUE)) {
  ns <- session$ns

  # max number rows for custom output params is 9, but set to 8 when counting from 0
  max_n <- 8

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    # flag to know if the user is creating a new output configuration or rerunning an analysis
    ana_flag = "C",
    # result of posting RUN_analysis
    ana_post_status = "",
    # consecutive number of rows of output parameters - dependent on abuttonadd
    n_add = 0,
    # data for output params review
    out_params_review = data.frame(
      perspective = c(),
      summary_level = c(),
      report = c()
    )
  )

  # inserted fields
  inserted <- reactiveValues(val = 0)

  # Set up ---------------------------------------------------------------------

  # ana_flag
  observeEvent(ana_flag(), {
    if (ana_flag() != result$ana_flag) {
      result$ana_flag <- ana_flag()
    }
  })

  observeEvent(counter(), {
    .clearOutputOptions(result$ana_flag)
    show("panel_anaoutput")
  })

  # Panel infos ----------------------------------------------------------------

  # hide panel
  observeEvent(input$abuttonhidepanelconfigureoutput, {
    hide("panel_anaoutput")
  })

  # configuration title
  output$paneltitle_defAnaConfigOutput <- renderUI({
    analysisName <- ifelse(analysisName() == " ", "", paste0('"', analysisName(), '"'))
    if (result$ana_flag  == "R") {
      paste0('Re-define output configuration for analysis id ',
             analysisID(),
             ' ',
             analysisName)
    } else {
      paste0('Define output configuration for analysis id ',
             analysisID(),
             ' ',
             analysisName)
    }
  })

  # Select Tag from another analysis -------------------------------------------

  # > Modal Dialogue
  AnaList <- modalDialog(
    easyClose = TRUE,
    size = "l",
    selectInput(
      ns("sinoutputoptions"),
      "Select Custom Configuration:",
      choices = ""
    ),
    footer = tagList(
      oasisuiButton(
        ns("abuttonselectconf"),
        label = "Select Configuration",
        align = "left"
      ),
      actionButton(ns("abuttoncancel"),
                   label = "Cancel", align = "right")
    )
  )

  # Choose Tag
  observeEvent(input$abuttonchoosetag, {
    tbl_analysesData <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    # keep all analyses that have been run, i.e. that have an analysis settings associated.
    tbl_analysesData <- tbl_analysesData %>% filter(grepl("run", tolower(status_detailed)))
    namesList <- tbl_analysesData[, tbl_analysesDataNames$name]
    idList <- tbl_analysesData[, tbl_analysesDataNames$id]
    choicesList <- paste(idList, namesList, sep = " / ")
    updateSelectInput(inputId = "sinoutputoptions",
                      choices = choicesList,
                      session = session)
    showModal(AnaList)
  })

  # update tag based on analysis selection
  observeEvent(input$abuttonselectconf, {
    # Using analyses names to select the output configuration of a previously posted analyses
    logMessage(
      paste0(
        "updating output configuration because input$sinoutputoptions changed to ",
        input$sinoutputoptions
      )
    )
    if (length(input$sinoutputoptions) > 0 &&
        input$sinoutputoptions != "") {
      anaName <- strsplit(input$sinoutputoptions, split = " / ")[[1]][2]
      anaID <- strsplit(input$sinoutputoptions, split = " / ")[[1]][1]
      analysis_settings <- session$userData$data_hub$get_ana_settings_content(anaID, oasisapi = session$userData$oasisapi)

      if (!is.null(analysis_settings$detail) &&
          analysis_settings$detail == "Not found.") {
        oasisuiNotification(
          type = "error",
          paste0(
            "No output configuration associated to analysis ",
            anaName,
            " id ",
            anaID,
            "."
          )
        )
      } else {
        logMessage(paste0(
          "appling the output configuration of analysis ",
          anaName,
          " id ",
          anaID
        ))
      }
      # re-set configuration to previous selection
      output$summary_levels_reports_ui <- renderUI({
        dynamicUI_btns(session, anaID, "R", tag = input$sintag, oed_field_react())
      })
      .updateOutputConfig(analysis_settings, result$ana_flag)
    }
    removeModal()
  })

  # > close modal
  observeEvent(input$abuttoncancel, {
    removeModal()
  })

  # Preselected Output Configuration -------------------------------------------

  # log message and potential render UI in case analysis or flag changes
  observeEvent({
    result$ana_flag
    analysisID()
  }, {

    if (length(analysisID()) > 0) {
      if (result$ana_flag == "R") {
        analysis_settings <- session$userData$data_hub$get_ana_settings_content(analysisID(), oasisapi = session$userData$oasisapi)
        if (!is.null(analysis_settings$detail) &&
            analysis_settings$detail == "Not found.") {
          oasisuiNotification(
            type = "error",
            paste0(
              "No output configuration associated to analysis ",
              analysisName(),
              " id ",
              analysisID(),
              "."
            )
          )
        } else {
          logMessage(
            paste0(
              "applying the output configuration of analysis ",
              analysisName(),
              " id ",
              analysisID()
            )
          )
        }
        # re-set configuration to previous selection
        .updateOutputConfig(analysis_settings, result$ana_flag)
      } else {
        # re-set model params config
        .clearOutputOptions("C")
      }
    }
  })

  # >> summary levels and reports ----------------------------------------------
  oed_field_react <- reactive({
    session$userData$data_hub$get_ana_oed_summary_levels(id = analysisID())$oed_field
  })

  observeEvent({
    input$sintag
    analysisID()
  }, {
    if (length(analysisID()) > 0) {
      logMessage(paste(
        "updating output parameters for",
        input$sintag,
        "configuration and cleaning up UI"
      ))

      # clean up UI
      if (any(grepl("sinsummarylevels", input))) {
        removeUI(selector = "div:has(> #sinsummarylevels)",
                 multiple = TRUE,
                 immediate = TRUE)
      }
      if (any(grepl("sinreports", input))) {
        removeUI(selector = "div:has(> #sinreports)",
                 multiple = TRUE,
                 immediate = TRUE)
      }
    }
    # main output configuration panel (perspectives, summary levels, reports)
    output$summary_levels_reports_ui <- renderUI({
      oed_field <- oed_field_react()
      # if oed fields are provided, a vector is returned, otherwise NA
      if (all(is.na(oed_field))) {
        logMessage("No list of summary levels provided")
      } else {
        # below returns NULL for Summary case
        dynamicUI_btns(session, analysisID(), result$ana_flag, tag = input$sintag, oed_field_react())
      }
    })
  })

  # disable removeBtn in case there is just a single set of fields
  observeEvent(result$n_add, {
    if (result$n_add < 1) {
      disable("removeBtn")
    } else {
      enable("removeBtn")
    }
  })

  # insert new summary levels and reports
  observeEvent(input$addBtn, {
    result$n_add <- result$n_add + 1
    id <- length(inserted$val)
    logMessage(paste0("insert ui because addBtn changed to ",  result$n_add))
    add_UI(result$n_add, id, input$sintag)
    # Max number of fields limited to 9 (current limitation by the back-end)
    if (result$n_add >= max_n) {
      disable("addBtn")
    } else if (input$sintag == default_tags[2] && result$n_add >= (max_n - 1)) {
      # drill-down allows one slot less, since one is already pre-occupied by the default "All Risks"
      disable("addBtn")
    }
    inserted$val <- c(inserted$val, id)
  })

  # remove summary levels and reports
  observeEvent(input$removeBtn, {
    result$n_add <- result$n_add - 1
    if (input$sintag == default_tags[2] && result$n_add < (max_n - 1)) {
      enable("addBtn")
    } else if (input$sintag == default_tags[3] && result$n_add < max_n) {
      # custom allows one more since no slot is pre-occupied by a default
      enable("addBtn")
    }
    removeUI(selector = paste0('#', inserted$val[length(inserted$val)]))
    inserted$val <- inserted$val[-length(inserted$val)]
  })

  # clear all items
  observeEvent(input$clearselection, {
    result$n_add <- 0
    output$summary_levels_reports_ui <- renderUI({
      dynamicUI_btns(session, analysisID(), "C", tag = input$sintag, oed_field_react())
    })
  })

  # > Output Params Review -----------------------------------------------------
  # review of output configuration in long format. As a collapsible panel. Available for all tags
  output$out_params_review_ui <- renderUI(tagList(
    oasisuiPanel(
      collapsible = TRUE,
      show = FALSE,
      id = ns("panel_OutputParamsReview"),
      heading = h4("Output Parameters Review"),
      oasisuiTableUI(ns("out_params_review_tbl")),
      downloadButton(ns("download_out_params_review_tbl"), label = "Export to csv") %>%
        bs_embed_tooltip(
          title = defineSingleAna_tooltips$download_out_params_review_tbl,
          placement = "right"
        )
    )
  ))

  observeEvent(result$out_params_review, {
    if (nrow(result$out_params_review) == 0) {
      disable("abuttonexecuteanarun")
    } else {
      enable("abuttonexecuteanarun")
    }
  })

  sinsummarylevels_react_all <- reactive({
    unlist(
      lapply(seq(0, result$n_add), function(x) {
        input[[paste0("sinsummarylevels", x)]]
      })
    )
  })

  sinreports_react_all <- reactive({
    unlist(
      lapply(seq(0, result$n_add), function(x) {
        input[[paste0("sinreports", x)]]
      })
    )
  })

  observeEvent({
    input$sintag
    input$chkboxgrplosstypes
    sinsummarylevels_react_all()
    sinreports_react_all()
    analysisID()
  }, ignoreInit = TRUE, {
    if (length(analysisID()) > 0) {
      .observe_output_param()
    }
  })

  observeEvent(input$sintag, {
    if (input$sintag == default_tags[2] || input$sintag == default_tags[3]) {
      show("clearselection")
    } else {
      hide("clearselection")
    }
  })

  observeEvent({sinsummarylevels_react_all()
    sinreports_react_all()}, ignoreNULL = FALSE, {
      if (any(!is.null(sinsummarylevels_react_all())) || any(!is.null(sinreports_react_all()))) {
        enable("clearselection")
      } else {
        disable("clearselection")
      }
    })

  callModule(
    oasisuiTable,
    id = "out_params_review_tbl",
    data = reactive({result$out_params_review}),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE
  )

  output$download_out_params_review_tbl <- downloadHandler(
    filename = paste0("outputParams_review_analysis_", analysisID(), ".csv"),
    content = function(file) {
      fwrite(
        result$out_params_review %>% capitalize_names_df(),
        file,
        row.names = FALSE,
        quote = TRUE
      )
    }
  )

  # Run analysis ---------------------------------------------------------------
  # Execute analysis
  observeEvent(input$abuttonexecuteanarun, {
    analysis_settingsList <- .gen_analysis_settings()
    # post analysis settings
    post_analysis_settings <- session$userData$oasisapi$api_body_query(
      query_path = paste("analyses", analysisID(), "settings", sep = "/"),
      query_body = analysis_settingsList[[1]]
    )
    result$ana_post_status <- post_analysis_settings$status
  })

  # show advanced view
  observeEvent(input$abuttonadvanced, {
    .advancedview()
  })

  # show basic view
  observeEvent(input$abuttonbasic, {
    .basicview()
  })

  # UI functions ---------------------------------------------------------------

  # Add additional fields to the UI
  add_UI <- function(n_field, id, tag) {
    insertUI(
      selector = '#placeholder',
      where = "beforeBegin",
      immediate = TRUE,
      ui = tags$div(id = id,
                    fluidRow(column(3),
                             column(8,
                                    dynamicUI(session, "C", tag, n_field, oed_field_react())
                             )))
    )
  }

  # Output table function ------------------------------------------------------
  .create_output_params <- function(sum_rep_grid) {
    if (is.null(input$chkboxgrplosstypes)) {
      reports_summary_levels <- data.frame(
        perspective = character(0),
        summary_level = character(0),
        report = character(0)
      )
    } else {
      perspectives <- input$chkboxgrplosstypes
      reports_summary_levels <- distinct(data.frame(
        perspective = rep(perspectives, each = nrow(sum_rep_grid)),
        sum_rep_grid
      ))
    }
    result$out_params_review <- reports_summary_levels
  }

  .observe_output_param <- function() {
    sum_rep_grid_default <- expand.grid(
      summary_level = "All Risks",
      report = sort(output_options$variables[output_options$variables_default])
    )

    if (input$sintag == default_tags[3]) {
      # Custom(3)
      sum_rep_grid_user <- do.call("rbind.data.frame",
                                   lapply(seq(0, result$n_add), function(x) {
                                     expand.grid(summary_level = paste(sort(input[[paste0("sinsummarylevels", x)]]), collapse = ", "),
                                                 report = paste(input[[paste0("sinreports", x)]])
                                     ) %>%
                                       filter(summary_level != "") %>%
                                       filter(report != "")
                                   }))

      sum_rep_grid <- sum_rep_grid_user
    } else if (input$sintag == default_tags[2]) {
      # Drill-down (2)
      sum_rep_grid_user <- do.call("rbind.data.frame",
                                   lapply(seq(0, result$n_add), function(x) {
                                     expand.grid(
                                       summary_level = paste(sort(input[[paste0("sinsummarylevels", x)]]), collapse = ", "),
                                       report = sort(output_options$variables[output_options$variables_default])
                                     ) %>%
                                       filter(summary_level != "")
                                   }))

      sum_rep_grid <- rbind(sum_rep_grid_default, sum_rep_grid_user)
    } else {
      # Summary (1)
      sum_rep_grid <- sum_rep_grid_default
    }

    .create_output_params(sum_rep_grid)
    invisible()
  }

  # Update and Clear Outputs Functions -----------------------------------------
  # re-set Rerun panel to previous selection
  .updateOutputConfig <- function(analysis_settings, ana_flag) {
    logMessage(".updateOutputConfig called")
    if (ana_flag == "R") {
      # In case of Rerun, tag is set to Custom
      chosen_tag <- default_tags[3]
      # update Number of samples and Threshold in model params panel
      if (is.null(analysis_settings$detail) || analysis_settings$detail != "Not found.") {
        .clearOutputOptions(ana_flag)
      }
    } else {
      # In case of Output Configuration, tag is set to Summary
      chosen_tag <- default_tags[1]
    }
    # Update tag
    updateSelectInput(inputId = "sintag",
                      selected = chosen_tag,
                      session = session)
  }

  .clearOutputOptions <- function(ana_flag) {
    # this is actually a function that takes care of the model parameters panel of the output configuration. possibly rename it.
    logMessage(".clearOutputOptions called")

    # Predefined params
    tbl_analysesData <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    # Model params
    modelID <- tbl_analysesData[tbl_analysesData[, tbl_analysesDataNames$id] == analysisID(), tbl_analysesDataNames$model]

    if (length(modelID) != 0) {
      tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
        query_path = paste("models", modelID, "settings", sep = "/"),
        query_method = "GET"
      )
      if (!is.null(tbl_modelsDetails)) {
        model_settings <- tbl_modelsDetails$model_settings %>% unlist(recursive = FALSE)

        if (length(names(model_settings)) > 0) {
          # Basic model params
          output$basic_model_param <- renderUI({
            basicConfig_funs(session, model_settings)
          })

          # Advanced model params
          output$advanced_model_param <- renderUI({
            advancedConfig_funs(session, model_settings)
          })
        }
      }
    }
  }

  # Generate analysis settings -------------------------------------------------
  .gen_analysis_settings <- function() {
    logMessage(".gen_analysis_settings called")
    # Predefined params
    tbl_analysesData <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    modelID <- tbl_analysesData[tbl_analysesData[, tbl_analysesDataNames$id] == analysisID(), tbl_analysesDataNames$model]
    modelData <- session$userData$data_hub$return_tbl_modelData(modelID)

    fetch_model_settings <- function(modelID) {
      tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(
        query_path = paste("models", modelID, "settings", sep = "/"),
        query_method = "GET"
      )
      model_settings <- tbl_modelsDetails$model_settings %>% unlist(recursive = FALSE)
      string_input <- unlist(lapply(grep("string_parameters", names(model_settings)), function(x) {
        if (!is.null(input[[paste0("string_parameters", x)]])) {
          input[[paste0("string_parameters", x)]]
        } else if (!is.null( model_settings[[x]][[y]]$default)) {
          model_settings[[x]][[y]]$default
        }
      }))

      dict_input <- lapply(grep("dictionary_parameters", names(model_settings)), function(x) {
        if (!is.null(input[[paste0("dictionary_parameters", x)]])) {
          setNames(input[[paste0("dictionary_parameters", x)]], names(model_settings[[x]]$default))
        } else if (!is.null(model_settings[[x]]$default)) {
          setNames(model_settings[[x]]$default, names(model_settings[[x]]$default))
        }
      })

      dropdown_input <- lapply(grep("dropdown_parameters", names(model_settings)), function(x) {
        if (!is.null(input[[paste0("dropdown_parameters", x)]])) {
          input[[paste0("dropdown_parameters", x)]]
        } else if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      })
      # below is purposedly list() rather than NULL in case there are none!
      boolean_input <- unlist(lapply(grep("boolean_parameters", names(model_settings)), function(x) {
        if (!is.null(input[[paste0("boolean_parameters", x)]])) {
          input[[paste0("boolean_parameters", x)]]
        } else if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      }))

      float_input <- lapply(grep("float_parameters", names(model_settings)), function(x) {
        if (!is.null(input[[paste0("float_parameters", x)]])) {
          input[[paste0("float_parameters", x)]]
        } else if (!is.null(model_settings[[x]]$default)) {
          model_settings[[x]]$default
        }
      })

      list_input <- lapply(grep("list_parameters", names(model_settings)), function(x) {
        if (!is.null(input[[paste0("list_parameters", x)]])) {
          as.numeric(unlist(strsplit(input[[paste0("list_parameters", x)]], ", ")))
        } else if (!is.null(model_settings[[x]]$default)) {
          as.numeric(unlist(model_settings[[x]]$default))
        }
      })

      inputs_list <- list(string_input,
                          list_input,
                          dict_input,
                          float_input)

      params_list <- list("string_parameters",
                          "list_parameters",
                          "dictionary_parameters",
                          "float_parameters")
      # create list of re-ordered and grouped model inputs names
      inputs_name <- c()
      for (param in seq_len(length(params_list))) {
        if (length(inputs_list[[param]]) > 0) {
          param_name <- unlist(lapply(grep(params_list[param], names(model_settings)), function(i) {
            model_match <- model_settings[i]
            lapply(seq_len(length(model_match)), function(j) {
              model_match[[j]][["name"]]
            })
          }))
          inputs_name[param] <- param_name
        }
        # if a param is NULL and skipped, it will result in an NA in the inputs_name vector
      }

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
      model_settings <- c(input$event_set,
                          input$event_occurrence,
                          # note that boolean_input is a list, making sure that the result of this c() is a flat list!
                          boolean_input,
                          string_input,
                          list_input,
                          dict_input,
                          float_input)

      # NULL or list() elements won't survive the c() above!

      # create list/vector of names for model settings
      names_full_list <- c("event_set",
                           "event_occurrence_id",
                           boolean_name,
                           inputs_name)

      # remove all NA elements
      if (any(sapply(names_full_list, is.na))) {
        names(model_settings) <- names_full_list[-which(sapply(names_full_list, is.na))]
        } else if(length(model_settings) > 0) {
          names(model_settings) <- names_full_list
        }

      model_settings
    }

    inputsettings <- list(
      "analysis_tag" = as.character(analysisID()),
      # category tag
      "ui_config_tag" = input$sintag,
      # potential new tag analysis_id
      "gul_threshold" = as.integer(input$tinputthreshold),
      "model_version_id" = modelData[[tbl_modelsDataNames$model_id]],
      "module_supplier_id" = modelData[[tbl_modelsDataNames$supplier_id]],
      "number_of_samples" = as.integer(input$tinputnoofsample),
      # potential new tag portfolio_id
      "prog_id" = as.integer(session$userData$oasisapi$api_return_query_res(query_path = paste("analyses", analysisID(), sep = "/"),
                                                                            query_method = "GET")[["portfolio"]]),
      # potential new tag environment_tag
      "source_tag" = getOption("oasisui.settings.oasis_environment")
    )

    fetch_summary <- function(prsp, checked) {
      if (prsp %in% checked) {
        summary_template <- list(
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
          )
        )

        p <- which(result$out_params_review$perspective == prsp)
        review_prsp <- result$out_params_review[p, ]
        # convert factors to char
        review_prsp[] <- sapply(review_prsp, as.character)
        # All Risks as default for both Summary and Drill down, optional for Custom
        if (input$sintag == default_tags[3]) {
          review_prsp$summary_level <- sapply(seq(1, length(review_prsp$summary_level)), function(x) {
            if (grepl("All Risks", review_prsp$summary_level[x])) {
              ""
            } else {
              review_prsp$summary_level[x]
            }
          })
          # distinct, in case someone did multiple combinations of "All Risks"
          review_prsp <- distinct(review_prsp)
        } else if (input$sintag == default_tags[2]) {
          # Replace "All Risks" with empty string for Drill-down
          p <- which(review_prsp$summary_level == "All Risks")
          review_prsp$summary_level[p] <- ""
        } else {
          # Replace "All Risks" with empty string for Summary ("All Risks" being the
          # only summary level in case of Summary!)
          review_prsp$summary_level <- ""
        }

        fields_to_add <- unique(review_prsp$summary_level)
        update_item_list <- function(lst, reps) {
          nm <- names(lst)
          # we want to keep names!
          names(nm) <- nm
          ret <- lapply(nm, function(x, y) {
            if (is.list(y[[x]])) {
              update_item_list(y[[x]], reps = reps)
            } else {
              if (isFALSE(y[[x]])) {
                if (x %in% reps) {TRUE} else {NULL}
              } else {
                # skip
                y[[x]]
              }
            }
          }, y = lst)
          idxDrop <- sapply(ret, is.null)
          ret[!idxDrop]
        }

        # list collecting all sub-lists per summary level
        item_list_full <- list()
        # for every item in fields_to_add (== summary level):
        for (item in seq_len(length(fields_to_add))) {
          item_list <- summary_template
          item_list$id <- item
          # update requested reports for summary level that is being iterated
          idx_item <- review_prsp$summary_level == fields_to_add[item]
          keep <- review_prsp[idx_item, "report"]
          lec_list <- c("LEC Full Uncertainty AEP", "LEC Full Uncertainty OEP",
                        "LEC Wheatsheaf AEP", "LEC Wheatsheaf OEP", "LEC Mean Wheatsheaf AEP",
                        "LEC Mean Wheatsheaf OEP", "LEC Sample Mean AEP", "LEC Sample Mean OEP")
          if (length(intersect(lec_list, keep)) > 0) {
            item_list$lec_output <- TRUE
            item_list$leccalc$return_period_file <- TRUE
          }
          corresp_varsdf <- which(varsdf$labels %in% keep)
          item_list_upd <- update_item_list(item_list, varsdf$field[corresp_varsdf])
          # oed_fields should be a list in the json file
          item_list_upd$oed_fields <- lapply(strsplit(fields_to_add[item], split = ", ")[[1]], identity)
          # add final item_list for single summary level to the list collecting all
          item_list_full[[item]] <- item_list_upd
        }
        item_list_full
      } else {
        NULL
      }
    }

    analysis_settings <- list(analysis_settings = c(
      inputsettings,
      list(
        gul_output = "GUL" %in% input$chkboxgrplosstypes,
        gul_summaries = fetch_summary("GUL", input$chkboxgrplosstypes),
        il_output = "IL" %in% input$chkboxgrplosstypes,
        il_summaries = fetch_summary("IL", input$chkboxgrplosstypes),
        ri_output = "RI" %in% input$chkboxgrplosstypes,
        ri_summaries = fetch_summary("RI", input$chkboxgrplosstypes)
      ),
      list(model_settings = fetch_model_settings(modelID = modelID))
    ))

    analysis_settings
  }

  # Output view
  .advancedview <- function() {
    logMessage(".advancedview called")
    show("configureAnaParamsAdvanced")
    show("abuttonbasic")
    hide("abuttonadvanced")
  }

  .basicview <- function() {
    logMessage(".basicview called")
    hide("configureAnaParamsAdvanced")
    hide("abuttonbasic")
    show("abuttonadvanced")
  }

  # Module Output --------------------------------------------------------------
  moduleOutput <- c(list(
    ana_flag = reactive(result$ana_flag),
    ana_post_status = reactive(result$ana_post_status),
    ana_post_update = reactive(input$abuttonexecuteanarun)
  ))

  moduleOutput
}

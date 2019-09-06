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
        title = defineSingleAna$abuttonexecuteanarun,
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
        uiOutput(ns("basic_model_param")),
        uiOutput(ns("chkinputsperils"))
      ),
      oasisuiButton(inputId = ns("abuttonadvanced"), label = "Advanced"),
      hidden(
        div(
          id = ns("configureAnaParamsAdvanced"),
          align = "left",
          textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10"),
          textInput(ns("tinputthreshold"), label = "Loss Threshold:", value = "0"),
          uiOutput(ns("advanced_model_param")),
          # checkboxInput(ns("chkinputsummaryoption"), "Summary Reports", value = TRUE),
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
          bs_embed_tooltip(title = defineSingleAna$abuttonchoosetag, placement = "right")
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
      show = FALSE,
      id = ns("panel_OutputParamsDetails"),
      heading = h4("Output Parameters Details"),
      uiOutput(ns("perspective_ui")),
      # checkboxses for all perspectives; available for all tags
      uiOutput(ns("summary_levels_reports_ui")),
      # combinations of summary levels and reports.
      uiOutput(ns("out_params_review_ui")), # review of output configuration in long format. As a collapsible panel. Available for all tags
      hidden(
        actionButton(
          inputId = ns("clearselection"),
          label = "Clear",
          style = "float:right;"
        ) %>%
          bs_embed_tooltip(title = defineSingleAna$clearselection, placement = "right")
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

  # max number rows for custom output params
  max_n <- 9
  # inserted fields
  inserted <- reactiveValues(val = 0)

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

  # Set up ---------------------------------------------------------------------

  #ana_flag
  observeEvent(ana_flag(), {
    if (ana_flag() != result$ana_flag) {
      result$ana_flag <- ana_flag()
    }
  })

  observeEvent(counter(), {
    .clearOutputOptions()
    show("panel_anaoutput")
  })

  # Panel infos ----------------------------------------------------------------

  # hide panel
  observeEvent(input$abuttonhidepanelconfigureoutput, {
    hide("panel_anaoutput")
  })

  # configuration title
  output$paneltitle_defAnaConfigOutput <- renderUI({
    analysisName <-
      ifelse(analysisName() == " ", "", paste0('"', analysisName(), '"'))
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

  # Choose Tag
  observeEvent(input$abuttonchoosetag, {
    tbl_analysesData  <-
      session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    tbl_analysesData <-
      tbl_analysesData %>% filter(grepl("run", tolower(status_detailed))) #keep all analyses that have been run, i.e. that have an analysis settings associated.
    namesList <- tbl_analysesData[, tbl_analysesDataNames$name]
    idList <- tbl_analysesData[, tbl_analysesDataNames$id]
    choicesList <- paste(idList, namesList, sep = " / ")
    showModal(AnaList)
    updateSelectInput(inputId = "sinoutputoptions",
                      choices = choicesList,
                      session = session)
  })

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
      anaID <-
        strsplit(input$sinoutputoptions, split = " / ")[[1]][1]
      analysis_settings <-
        session$userData$data_hub$get_ana_settings_content(anaID, oasisapi = session$userData$oasisapi)
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
        # Get chosen tag out of the analysis settings
        chosen_tag <- default_tags[1]
        # Update tag
        updateSelectInput(inputId = "sintag",
                          selected = chosen_tag,
                          session = session)
        #Set inputs
        .updateOutputConfig(analysis_settings)
      }
    }
    removeModal()
  })

  # > close modal
  observeEvent(input$abuttoncancel, {
    removeModal()
  })

  # Preselected Output Configuration -------------------------------------------

  # Rerun case
  observeEvent({
    result$ana_flag
    analysisID()
  }, {
    if (!is.null(analysisID()) && result$ana_flag == "R") {
      analysis_settings <-
        session$userData$data_hub$get_ana_settings_content(analysisID(), oasisapi = session$userData$oasisapi)
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
            "appling the output configuration of analysis ",
            analysisName(),
            " id ",
            analysisID()
          )
        )
        #Set inputs
        .updateOutputConfig(analysis_settings)
      }
    }
  })

  # Output Parameters Details --------------------------------------------------

  # checkboxses for all perspectives; available for all tags
  output$perspective_ui <-
    renderUI(
      checkboxGroupInput(
        inputId = ns("chkboxgrplosstypes"),
        label = "Perspective",
        choices = output_options$losstypes,
        inline = TRUE,
        selected = output_options$losstypes[1]
      )
    )


  # >> summary levels and reports ----------------------------------------------

  observeEvent({
    input$sintag
    analysisID()
  }, {
    logMessage(paste0(
      "updating output parameters for ",
      input$sintag,
      " configuration"
    ))

    # clean up ui
    logMessage("clean up UI")
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

    # reset counter
    output$summary_levels_reports_ui <- renderUI({
      oed_field <-
        session$userData$data_hub$get_ana_oed_summary_levels(id = analysisID())$oed_field
      # if oed fields are provided, a vector is returned, otherwise NA
      if (all(is.na(oed_field))) {
        logMessage("No list of summary levels provided")
      } else {
        dynamicUI_btns(tag = input$sintag, n = 0)
      }
    })
  })

  # insert new summary levels and reports
  observeEvent(input$addBtn, {
    result$n_add <- result$n_add + 1
    id <- length(inserted$val) + 1
    logMessage(paste0("insert ui because addBtn changed to ",  result$n_add))
    add_UI(result$n_add, id, input$sintag)
    #Max number of fields limited to 9
    if (result$n_add >= max_n) {
      disable("addBtn")
    } else if (input$sintag == default_tags[2] && result$n_add >= (max_n - 1)) {
      disable("addBtn")
    }
    inserted$val <- c(inserted$val, id)
    enable("removeBtn")
  })

  # remove summary levels and reports
  observeEvent(input$removeBtn, {
    enable("addBtn")
    # browser()
    result$n_add <- result$n_add - 1
    removeUI(selector = paste0('#', inserted$val[length(inserted$val)]))
    inserted$val <- inserted$val[-length(inserted$val)]

    observe_output_param()
    if (result$n_add < 1) {
      disable("removeBtn")
    }
  })

  # clear all items
  observeEvent(input$clearselection, {
    result$n_add <- 0
    output$summary_levels_reports_ui <- renderUI({
      dynamicUI_btns(tag = input$sintag, n = 0)
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
          title = defineSingleAna$download_out_params_review_tbl,
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

  sinsummarylevels_react_all <-
    reactive({
      unlist(
        lapply(seq(0, result$n_add), function(x) {
          input[[paste0("sinsummarylevels", x)]]
        })
      )
    })

  sinreports_react_all <-
    reactive({
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
      observe_output_param()
    }

    if (input$sintag == default_tags[2] || input$sintag == default_tags[3]) {
      show("clearselection")
      if (any(!is.null(sinsummarylevels_react_all())) || any(!is.null(sinreports_react_all()))) {
        enable("clearselection")
      } else {
        disable("clearselection")
      }
    } else {
      hide("clearselection")
    }
  })

  callModule(
    oasisuiTable,
    id = "out_params_review_tbl",
    data = reactive({
      result$out_params_review
    }),
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
    #write out file to be uploades
    currfolder <- session$userData$data_hub$get_user_destdir()
    dest <- file.path(currfolder, "analysis_settings.json")
    write_json(analysis_settingsList,
               dest,
               pretty = TRUE,
               auto_unbox = TRUE)

    #post analysis settings
    post_analysis_settings_file <-
      session$userData$oasisapi$api_post_file_query(
        query_path = paste("analyses", analysisID(), "settings_file", sep = "/"),
        query_body = dest,
        query_encode = "multipart"
      )

    result$ana_post_status <- post_analysis_settings_file$status
  })

  # show advanced view
  observeEvent(input$abuttonadvanced, {
    .advancedview()
  })

  # show basic view
  observeEvent(input$abuttonbasic, {
    .basicview()
  })

  # UI functions -----------------------------------------------------------------

  # Summary Level and Reports fields
  dynamicUI <- function(tag, n) {
    oed_field <- session$userData$data_hub$get_ana_oed_summary_levels(id = analysisID())$oed_field
    if (tag == default_tags[3]) {
      fluidRow(column(
        5,
        selectInput(
          inputId = ns(paste0("sinsummarylevels", n)),
          label = "Summary Levels",
          choices = c("All Risks",
                      oed_field),
          multiple = TRUE
        )
      ),
      column(
        5,
        selectInput(
          inputId = ns(paste0("sinreports", n)),
          label = "Reports",
          choices = output_options$variables,
          multiple = TRUE
        )
      ))
    } else if (tag == default_tags[2]) {
      fluidRow(column(
        5,
        selectInput(
          inputId = ns(paste0("sinsummarylevels", n)),
          label = "Summary Levels",
          choices = oed_field,
          multiple = TRUE
        )
      ))
    }
  }

  # add "+" and "X" buttons to dynamic UI
  dynamicUI_btns <- function(tag, n) {
    if(tag == default_tags[2] || tag == default_tags[3]) {
      tagList(fluidRow(
        column(
          1,
          br(),
          actionButton(ns("addBtn"), label = "", icon = icon("plus")) %>%
            bs_embed_tooltip(title = defineSingleAna$addBtn, placement = "right")
        ),
        column(2,
               br(),
               disabled(
                 actionButton(
                   ns("removeBtn"),
                   label = "",
                   icon = icon("times")
                 ) %>%
                   bs_embed_tooltip(title = defineSingleAna$removeBtn, placement = "right")
               )),
        column(8,
               dynamicUI(tag, n))
      ),
      tags$div(id = 'placeholder'))
    }
  }

  # Add additional fields to the UI
  add_UI <- function(n, id, tag) {
    insertUI(
      selector = '#placeholder',
      where = "beforeBegin",
      immediate = TRUE,
      ui = tags$div(id = id,
                    fluidRow(column(3),
                             column(
                               8,
                               dynamicUI(tag, n)
                             )))
    )
  }

  # Output table function ------------------------------------------------------

  create_output_params <- function(sum_rep_grid) {
    if (is.null(input$chkboxgrplosstypes)) {
      perspectives <- output_options$losstypes[1]
    } else {
      perspectives <- input$chkboxgrplosstypes
    }
    reports_summary_levels <- distinct(data.frame(
      perspective = rep(perspectives, each = nrow(sum_rep_grid)),
      rep(sum_rep_grid, times = length(3))
    ))

    result$out_params_review <- reports_summary_levels
  }

  observe_output_param <- function() {
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

      sum_rep_grid <-
        rbind(sum_rep_grid_default, sum_rep_grid_user)
    } else {
      # Summary (1)
      sum_rep_grid <- sum_rep_grid_default
    }

    create_output_params(sum_rep_grid)
    invisible()
  }

  # Helper Functions -----------------------------------------------------------

  .updateOutputConfig <- function(analysis_settings) {
    logMessage(".updateOutputConfig called")
  }

  .gen_analysis_settings <- function() {
    logMessage(".gen_analysis_settings called")
    # Predefined params
    tbl_analysesData <-
      session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    modelID <-
      tbl_analysesData[tbl_analysesData[, tbl_analysesDataNames$id] == analysisID(), tbl_analysesDataNames$model]
    modelData <-
      session$userData$data_hub$return_tbl_modelData(modelID)

    fetch_model_settings <- function(modelID) {
      # Predefined params
      tbl_analysesData  <-
        session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
      modelID <-
        tbl_analysesData[tbl_analysesData[, tbl_analysesDataNames$id] == analysisID(), tbl_analysesDataNames$model]
      # modelData <- session$userData$data_hub$return_tbl_modelData(modelID)
      tbl_modelsDetails <-
        session$userData$oasisapi$api_return_query_res(
          query_path = paste("models", modelID, "resource_file", sep = "/"),
          query_method = "GET"
        )
      model_settings <- tbl_modelsDetails$model_settings %>%
        unlist(recursive = FALSE)
      model_params_lst <- lapply(names(model_settings), function(i) {
        ifelse(is.null(input[[paste0("model_params_", i)]]), model_settings[[i]]$default, input[[paste0("model_params_", i)]])
      }) %>%
        setNames(names(model_settings))
      model_settings <- list(return_period_file = TRUE,
                             model_settings) # list of 4 entries
    }

    inputsettings <- list(
      "analysis_tag" = as.integer(result$anaID),
      #potential new tag analysis_id
      "gul_threshold" = as.integer(input$tinputthreshold),
      "model_version_id" = modelData[[tbl_modelsDataNames$model_id]],
      # potential new tag model_id
      "module_supplier_id" = modelData[[tbl_modelsDataNames$supplier_id]],
      # potential new tag model_supplier_id
      "number_of_samples" = as.integer(input$tinputnoofsample),
      "prog_id" = as.integer(4),
      # potential new tag `portfolio_id`
      "source_tag" = getOption("oasisui.settings.oasis_environment") # potential new tag environment_tag
    )

    fetch_summary <- function(prsp, checked) {
      summary_template <- list(
        summarycalc = FALSE,
        eltcalc = FALSE,
        aalcalc = FALSE,
        pltcalc = FALSE,
        id = 1,
        oed_fields = "prog",
        lec_output = TRUE,
        leccalc = list(
          return_period_file = TRUE,
          outputs = list(
            full_uncertainty_aep = FALSE,
            full_uncertainty_oep = FALSE
          )
        )
      )

      if (prsp %in% checked) {
        p <- which(result$out_params_review$perspective == prsp)
        review_prsp <- result$out_params_review[p,]
        # All Risks as default for both Summary and Drill down, optional for Custom
        if (input$sintag == default_tags[3]) {
          review_prsp <-
            lapply(seq(1, length(review_prsp$summary_level)), function(x) {
              if (grepl("All Risks", review_prsp$summary_level[x])) {
                df <- data.frame(prsp, "", review_prsp$report)
                names(df) <-
                  c("perspective", "summary_level", "report")
                review_prsp <- df
              } else {
                review_prsp <- result$out_params_review[p,]
              }
            })

        } else if (input$sintag == default_tags[2]) {
          # Add "All Risks" (empty field) to table for Drill-down
          df <- data.frame(prsp, "", review_prsp$report)
          names(df) <- c("perspective", "summary_level", "report")
          review_prsp <- rbind(review_prsp, df)
        } else {
          # Replace "All Risks" with empty field for Summary
          df <- data.frame(prsp, "", review_prsp$report)
          names(df) <- c("perspective", "summary_level", "report")
          review_prsp <- df
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
                if (x %in% reps) {
                  TRUE
                } else {
                  NULL
                }
              } else {
                # skip
                y[[x]]
              }
            }
          }, y = lst)
          idxDrop <- sapply(ret, is.null)
          ret[!idxDrop]
        }

        item_list_full <- list()
        # for every item in fields_to_add (eg bitiv):
        for (item in seq_len(length(fields_to_add))) {
          item_list <- summary_template
          item_list$id <- item
          item_list$oed_fields <- as.character(fields_to_add[item])
          review_prsp$summary_level <- fields_to_add[item]
          idx_lvl <- review_prsp$summary_level
          keep <- review_prsp[item, "report"]
          corresp_varsdf <- which(varsdf$labels %in% keep)
          varsdf_field <- varsdf$field[corresp_varsdf]
          item_list_upd <- update_item_list(item_list, varsdf_field)
          item_list_full[[item]] <- item_list_upd
        }

        item_list_full

      } else {
        NULL
      }
    }

    analysis_settings <- list(analysis_settings = c(
      list(model_settings = fetch_model_settings()),
      inputsettings,
      list(
        # TRUE should be changed to the choice in the checkboxgroup that matches the perspective
        gul_output = TRUE,
        gul_summaries = fetch_summary("GUL", input$chkboxgrplosstypes),
        il_output = TRUE,
        il_summaries = fetch_summary("IL", input$chkboxgrplosstypes),
        ri_output = TRUE,
        ri_summaries = fetch_summary("RI", input$chkboxgrplosstypes)
      )
    ))

    analysis_settings
  }

  .clearOutputOptions <- function() {
    logMessage(".clearOutputOptions called")

    # Predefined params
    tbl_analysesData  <-
      session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)

    # Model Params
    modelID <-
      tbl_analysesData[tbl_analysesData[, tbl_analysesDataNames$id] == analysisID(), tbl_analysesDataNames$model]
    tbl_modelsDetails <-
      session$userData$oasisapi$api_return_query_res(
        query_path = paste("models", modelID, "resource_file", sep = "/"),
        query_method = "GET"
      )
    if (!is.null(modelID) && !is.null(tbl_modelsDetails)) {
      model_settings <-
        tbl_modelsDetails$model_settings %>% unlist(recursive = FALSE)
      names_settings_type <-
        lapply(names(model_settings), function(i) {
          model_settings[[i]][["type"]]
        }) %>%
        setNames(names(model_settings))

      if (length(names(model_settings)) > 0) {
        # Basic model params
        fixed_settings <- c("event_set", "event_occurrence_id")
        basic_model_params <-
          names(model_settings)[names(model_settings) %in% fixed_settings]
        ui_basic_model_param <-
          lapply(basic_model_params, function(p) {
            curr_param_lst <- model_settings[[p]]
            curr_param_name <-
              capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
            if (curr_param_lst$type == "boolean") {
              checkboxInput(
                inputId = ns(paste0("model_params_", p)),
                label = curr_param_name,
                value = curr_param_lst$default
              )
            } else if (curr_param_lst$type == "dictionary") {
              selectInput(
                inputId = ns(paste0("model_params_", p)),
                label = curr_param_name,
                choices = SwapNamesValueInList(curr_param_lst$values),
                selected =  curr_param_lst$default
              )
            } else if (curr_param_lst$type == "float") {
              sliderInput(
                inputId = ns(paste0("model_params_", p)),
                label = curr_param_name,
                min = curr_param_lst$min,
                max = curr_param_lst$max,
                value =  curr_param_lst$default
              )
            }
          })
        output$basic_model_param <- renderUI(ui_basic_model_param)

        # Perils Settings
        model_perils <- names(model_settings)[grepl("peril_", names(model_settings))]
        if (length(model_perils) > 0) {
          ui_perils <- lapply(seq(1, length(model_perils)), function(p) {
            checkboxInput(ns(paste0("model_params_", names(model_perils)[p])),
                          label = model_perils[[p]], # curr_param_lst$name,
                          value = TRUE) #curr_param_lst$default)
          })
          output$chkinputsperils <-
            renderUI(list(h5("Available Perils"), ui_perils))
        }

        # Advanced model params
        advanced_model_param <-
          names(model_settings)[names(model_settings) %notin% c(basic_model_params, model_perils)]
        ui_advanced_model_param <-
          lapply(advanced_model_param, function(p) {
            curr_param_lst <- model_settings[[p]]
            curr_param_name <-
              capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
            if (curr_param_lst$type == "boolean") {
              checkboxInput(
                inputId = ns(paste0("model_params_", p)),
                label = curr_param_name,
                value = curr_param_lst$default
              )
            } else if (curr_param_lst$type == "dictionary") {
              selectInput(
                inputId = ns(paste0("model_params_", p)),
                label = curr_param_name,
                choices = .SwapNamesValueInList(curr_param_lst$values),
                selected =  curr_param_lst$default
              )
            } else if (curr_param_lst$type == "float") {
              sliderInput(
                inputId = ns(paste0("model_params_", p)),
                label = curr_param_name,
                min = curr_param_lst$min,
                max = curr_param_lst$max,
                value =  curr_param_lst$default
              )
            }
          })
        output$advanced_model_param <-
          renderUI(ui_advanced_model_param)
      }
    }
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

  # Module Outout --------------------------------------------------------------

  moduleOutput <- c(list(
    ana_flag = reactive(result$ana_flag),
    ana_post_status = reactive(result$ana_post_status)
  ))


}

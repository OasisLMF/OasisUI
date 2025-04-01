# step3_configureOutput Module -------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step3_configureOutputUI
#'
#' @rdname step3_configureOutput
#'
#' @description UI/View for the step3_configureOutput.
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step3_configureOutputUI <- function(id) {

  ns <- NS(id)

  tagList(
    hidden(div(id = ns("panelAnalysisTable"), panelAnalysisTable(id))),
    hidden(div(id = ns("panelDefineOutputs"), def_out_configUI(ns("def_out_config")))),
    hidden(div(id = ns("panelAnalysisLogs"), panelAnalysisLogs(id)))
  )
}

#' panelAnalysisTable
#'
#' @rdname panelAnalysisTable
#'
#' @description Function wrapping panel to show analysestable.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelAnalysisTable <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysis"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnalysisTable"), inline = TRUE),
      oasisuiRefreshButton(ns("abuttonanarefresh"))
    ),
    div(id = "divAnalysis",
        DTOutput(ns("dt_analyses")),
        fluidRow(column(12,
                        div(id = ns("divAnalysisButtons"),
                            oasisuiTableButton(inputId = ns("abuttoncancelana"), label = "Cancel Analysis Run") %>%
                              bs_embed_tooltip(title = defineSingleAna_tooltips$abuttoncancelana, placement = "right"),
                            oasisuiTableButton(inputId = ns("abuttonshowlog"), label = "Show Run Log") %>%
                              bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonshowlog, placement = "right")
                        ))
        ),
        br(),
        fluidRow(column(12,
                        oasisuiButton(inputId = ns("abuttonrunconfig"), label = "New Output Configuration") %>%
                          bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonrunconfig, placement = "right"),
                        div(
                          actionButton(inputId = ns("abuttondisplayoutput"), label = "Proceed to Dashboard") %>%
                            bs_embed_tooltip(title = defineSingleAna_tooltips$abuttondisplayoutput, placement = "right"),
                          style = "inline: true;float: right;")
        ))
    )
  )
}

#' panelAnalysisLogs
#'
#' @rdname panelAnalysisLogs
#'
#' @description Function wrapping panel to show log table for specific Analysis.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelAnalysisLogs <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    ns("panel_analogs"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnaLogs"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelog"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    div(class = "panel", style = 'overflow-y: scroll; max-height: 200px; min-height: 30px;',
        htmlOutput(ns("text_analysesrunlog"))
    ),
    hidden(downloadButton(ns("download_log"), label = "Download"))
  )
}

# Server -----------------------------------------------------------------------

#' step3_configureOutput server
#'
#' @rdname step3_configureOutput
#'
#' @description Server logic to step3_configureOutput.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#'
#' @param currstep Current selected step.
#' @param portfolioID Selected portfolio ID.
#' @param analysisID Selected analysis ID.
#' @param pfName Name of selected portfolio.
#' @param customModSettings Customizable Model settings.
#'
#' @return dashboardAnaID id of selected run.
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT selectPage
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom dplyr filter
#' @importFrom jsonlite write_json
#' @importFrom jsonlite read_json
#'
#' @export
step3_configureOutput <- function(input, output, session,
                                  active = reactive(TRUE),
                                  currstep = reactive(-1),
                                  portfolioID = reactive(""),
                                  pfName = reactive(""),
                                  analysisID = reactive(NULL),
                                  customModSettings = reactive(NULL)
) {

  ns <- session$ns

  # list of sub-modules
  sub_modules <- list()

  # Reactive Values and parameters ---------------------------------------------

  # number of Rows per Page in a dataable
  pageLength <- 5

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # reactve value for navigation
    navigationstate = NULL,
    # reactive value for Analysis table
    tbl_analysesData = NULL,
    # analysis run logs table
    tbl_analysisrunlog = NULL,
    # flag to know if the user is creating a new output configuration or rerunning an analysis
    ana_flag = "C",
    # Id of the Analysis
    anaID = NULL,
    # anaId for Dashboard
    dashboardAnaID = NULL,
    # analysis_ setting
    analysis_settings = NULL
  )

  # Reset Param
  observe(if (active()) {
    result$navigationstate <- NULL
    result$dashboardAnaID <- NULL
    if (!is.null(analysisID())) {
      result$anaID <- analysisID()
    }
  })


  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 3) {
      .defaultstep3()
      .reloadAnaData()
    }
  })

  # If portfolioID changes, reload analysis table and set view back to default
  observeEvent(portfolioID(), ignoreInit = TRUE, {
    if (active()) {
      .hideDivs()
      show("panelAnalysisTable")
      .reloadAnaData()
    }
  })

  # Enable and disable buttons -------------------------------------------------

  #Enabling based on analysis
  observeEvent({
    result$tbl_analysesData
    portfolioID()
    currstep()
    input$dt_analyses_rows_selected}, ignoreNULL = FALSE, ignoreInit = TRUE, {
      disable("abuttondisplayoutput")
      disable("abuttonshowlog")
      disable("abuttonrunconfig")
      disable("abuttoncancelana")

      if (portfolioID() != "") {
        if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && length(input$dt_analyses_rows_selected) > 0 && max(input$dt_analyses_rows_selected) <= nrow(result$tbl_analysesData)) {
          enable("abuttonshowlog")
          if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] %notin% c(Status$Completed, Status$Failed, Status$Ready)) {
            enable("abuttoncancelana")
          }
          if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] == Status$Completed) {
            enable("abuttondisplayoutput")
          }
          if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] == "ready") {
            enable("abuttonrunconfig")
            updateActionButton(session, inputId = "abuttonrunconfig", label = "Output Configuration")
            result$ana_flag <- "C"
          } else if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] %in%
                     c("run completed", "run error")) {
            enable("abuttonrunconfig")
            updateActionButton(session, inputId = "abuttonrunconfig", label = "Rerun")
            result$ana_flag <- "R"
          } else {
            updateActionButton(session, inputId = "abuttonrunconfig", label = "Output Configuration")
            result$ana_flag <- "C"
          }
        }
      }
    }
  )

  # Analyses Table ------------------------------------------------------------
  output$dt_analyses <- renderDT(
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0) {
      index <- which(result$tbl_analysesData[, tbl_analysesDataNames$id] == analysisID())
      if (length(index) == 0 && is.null(analysisID())) {
        #print("*** analysisID() NULL, set index to 1")
        index <- 1
      } else {
        #print(paste("*** index is", index))
      }
      logMessage("re-rendering analysis table")
      datatable(
        result$tbl_analysesData %>% session$userData$data_hub$return_tbl_analysesData_nice(
          admin_mode = getOption("oasisui.settings.admin.mode"),
          Status = Status, tbl_modelsDataNames = tbl_modelsDataNames,
          tbl_portfoliosDataNames = tbl_portfoliosDataNames, tbl_analysesDataNames = tbl_analysesDataNames
        ),
        class = "oasisui-table display",
        rownames = FALSE,
        selection = list(mode = 'single', selected = index),
        escape = FALSE,
        filter = 'bottom',
        options = getTableOptions(maxrowsperpage = pageLength)
      )
    } else {
      nothingToShowTable(paste0("No analysis available"))
    })

  # Analyses Table Title
  output$paneltitle_AnalysisTable <- renderUI({
    if (portfolioID() != "") {
      pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
      paste0('Analyses associated to portfolio ', pfName, ', id ', portfolioID())
    } else {
      paste0("Analyses")
    }
  })

  # Delete analysis button -----------------------------------------------------
  observeEvent(input$abuttoncancelana, {
    showModal(.cancelAnaModal())
  })

  output$cancelAnaModaltitle <- renderUI({
    AnaId <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    AnaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
    paste0('Cancel analysis ', AnaId, ' ', AnaName)
  })

  observeEvent(input$btnCancelAnaDel, {
    removeModal()
  })

  observeEvent(input$abuttonConfirmDelAna, {
    removeModal()

    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    #should use /v1/analyses/{id}/cancel/
    delete_analyses_id <- session$userData$oasisapi$api_post_query(query_path = paste("analyses", analysisID, "cancel",  sep = "/"))

    if (delete_analyses_id$status == "Success") {
      oasisuiNotification(type = "message",
                          paste0("Analysis id ", analysisID, " cancelled."))
      .reloadAnaData()
      idxSel <- match(analysisID, result$tbl_analysesData[, tbl_analysesDataNames$id])
      pageSel <- ceiling(idxSel/pageLength)
      selectRows(dataTableProxy("dt_analyses"), idxSel)
      selectPage(dataTableProxy("dt_analyses"), pageSel)
    } else {
      oasisuiNotification(type = "error",
                          paste0("Error in cancelling analysis ", result$anaID, ". Analysis is not running."))
    }

  })

  # Configure Output -----------------------------------------------------------
  sub_modules$def_out_config <- callModule(
    def_out_config,
    id = "def_out_config",
    #analysisID = reactive(result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]),
    analysisID = reactive(result$anaID),
    analysisName = reactive(result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]),
    ana_flag = reactive(result$ana_flag),
    counter = reactive({input$abuttonrunconfig}),
    active = active,
    customModSettings = customModSettings
  )

  # update ana flag
  observeEvent(sub_modules$def_out_config$ana_flag(), ignoreInit = TRUE, {
    if (sub_modules$def_out_config$ana_flag() != result$ana_flag) {
      result$ana_flag <- sub_modules$def_out_config$ana_flag()
    }
  })

  # Show Output Configuration Panel and Re-run
  observeEvent(input$abuttonrunconfig, {
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && length(input$dt_analyses_rows_selected) > 0) {
      if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] == Status$Ready) {
        hide("panelAnalysisLogs")
        show("panelDefineOutputs")
        logMessage("showing panelDefineOutputs")
        #result$ana_flag <- "C"
      } else if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] %in% c(Status$Completed, Status$Failed)) {
        hide("panelAnalysisLogs")
        show("panelDefineOutputs")
        logMessage("showing panelDefineOutputs")
        #result$ana_flag <- "R"
      }
    }
  })

  # Run Analyses ---------------------------------------------------------------
  observeEvent(sub_modules$def_out_config$ana_post_update(), {
    if (!is.null(result$anaID)) {
      if (sub_modules$def_out_config$ana_post_status() == "Success") {
        oasisuiNotification(type = "message",
                            paste0("Analysis settings posted to ", result$anaID, "."))
        analyses_run <- session$userData$oasisapi$return_df(paste("analyses", result$anaID, "run", sep = "/"), query_method = "POST")
        if (!is.null(analyses_run) && nrow(analyses_run) == 1) {
          idxSel <- match(result$anaID, result$tbl_analysesData[, tbl_analysesDataNames$id])
          pageSel <- ceiling(idxSel/pageLength)
          .reloadAnaData()
          hide("panelDefineOutputs")
          # need to set selection back after reload above
          selectRows(dataTableProxy("dt_analyses"), idxSel)
          selectPage(dataTableProxy("dt_analyses"), pageSel)
          if (analyses_run[[tbl_analysesDataNames$status]] == "RUN_STARTED") {
            oasisuiNotification(type = "message",
                                paste0("Analysis ", result$anaID ," is executing."))
          }
        } else {
          oasisuiNotification(type = "error",
                              paste0("Run could not be started for analysis ", result$anaID, "."))
        }
      } else {
        oasisuiNotification(type = "error",
                            paste0("Analysis settings not posted to ", result$anaID,
                                   "; error ", sub_modules$def_out_config$ana_post_status(), "."))
      }
    }
  })

  # Logs -----------------------------------------------------------------------
  observeEvent(input$abuttonshowlog, {
    hide("panelDefineOutputs")
    show("panelAnalysisLogs")
    logMessage("showing analysis run log table")
    .reloadAnaRunLog()
  })

  observeEvent(input$abuttonhidelog, {
    hide("panelAnalysisLogs")
  })

  # Export to .csv
  output$download_log <- downloadHandler(
    filename = "analysis_run_log.txt",
    content = function(file) {
      session$userData$data_hub$write_file(
        # go back from the <pre> tag (for rendering) to character for output
        data = result$tbl_analysisrunlog$children[[1]],
        dataset_identifier = "analysis_run_log.txt",
        file_towrite = file)
    }
  )

  observeEvent(result$tbl_analysisrunlog, ignoreNULL = FALSE, {
    if (!is.null(result$tbl_analysisrunlog)) {
      show("download_log")
    } else {
      hide("download_log")
    }
  })

  ### Log Table
  output$text_analysesrunlog <- renderUI({
    if (length(input$dt_analyses_rows_selected) > 0) {
      logMessage("re-rendering analysis log table")
      if (!is.null(result$tbl_analysisrunlog)) {
        # NOTE: for larger log files this is a bit slow in rendering...
        result$tbl_analysisrunlog
      } else {
        paste0("No log files associated with analysis ID ", ifelse(!is.null(result$anaID), result$anaID, "NULL"))
      }
    }
  })

  # run logs title
  output$paneltitle_AnaLogs <- renderUI({
    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    analysisName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
    analysisName <- ifelse(analysisName == " ", "", paste0('"', analysisName, '"'))
    paste0('Run logs for analysis id ', analysisID, ' ', analysisName)
  })

  # Refresh Buttons ------------------------------------------------------------
  observeEvent(input$abuttonanarefresh, {
    idxSel <- match(result$anaID, result$tbl_analysesData[, tbl_analysesDataNames$id])
    pageSel <- ceiling(idxSel/pageLength)
    withModalSpinner(
      .reloadAnaData(),
      "Refreshing...",
      size = "s", t = 0.5
    )
    hide("panelDefineOutputs")
    # keep analysis selection
    selectRows(dataTableProxy("dt_analyses"), idxSel)
    selectPage(dataTableProxy("dt_analyses"), pageSel)
  })

  observeEvent(input$abuttonanarefreshlogs, {
    .reloadAnaRunLog()
  })

  # Updates dependent on changed: dt_analyses_rows_selected --------------------
  # Allow display output option only if run successful. Otherwise default view is logs
  observeEvent({
    input$dt_analyses_rows_selected
    result$tbl_analysesData  # TODO cornercase: the trigger above won't work switching from dt_analyses_rows_selected == 1 to a new portfolio / tbl_analysesData, which will also result in dt_analyses_rows_selected == 1.
    # however, we end up triggering this event twice in cases where both are changing... so if tbl_analysesData changes, and dt_analyses_rows_selected was 1 from before (and usually still is at this point), then skip this trigger (assuming the new tbl_analysesData has rows)
    # --> also consider if tbl_analysesData changes, and dt_analyses_rows_selected was 0 from before (pf without any analysis), in case we switch to another pf without any analysis...
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste("input$dt_analyses_rows_selected is changed to:", input$dt_analyses_rows_selected))
      hide("panelDefineOutputs")
      hide("panelAnalysisLogs")
      if (length(input$dt_analyses_rows_selected) > 0 &&
          !is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 &&
          max(input$dt_analyses_rows_selected) <= nrow(result$tbl_analysesData)) {
        result$anaID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
        logMessage(paste0("analysisId changed to ", result$anaID))
        if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] == Status$Failed) {
          show("panelAnalysisLogs")
          logMessage("showing analysis run log table")
          .reloadAnaRunLog()
        }
      } else {
        result$anaID <- NULL
      }
    }
  })

  # Navigation -----------------------------------------------------------------
  # Go to browse section
  observeEvent(input$abuttondisplayoutput, ignoreInit = TRUE, {
    result$dashboardAnaID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    logMessage(paste0("selected analysis ID is ", result$dashboardAnaID))
    result$navigationstate <- "SBR"
  })


  # Helper functions -------------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs step3 called")
    #Section "Configure Output & Run" = "3"
    hide("panelAnalysisTable")
    hide("panelDefineOutputs")
    hide("panelAnalysisLogs")
    invisible()
  }

  # show default view for Section "Configure Output & Run" = "3"
  .defaultstep3 <- function() {
    logMessage(".defaultstep3 called")
    show("panelAnalysisTable")
    # disable("chkgulpolicy")
    invisible()
  }

  # Reload Analyses table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData step3 called")
    if (portfolioID() != "") {
      tbl_analysesData <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
      if (!is.null(tbl_analysesData) && nrow(tbl_analysesData) > 0) {
        tbl_analysesData <- tbl_analysesData %>% filter(!! sym(tbl_analysesDataNames$portfolio) == portfolioID())
        result$tbl_analysesData <- tbl_analysesData
        logMessage(paste("analyses table refreshed with", nrow(tbl_analysesData), "rows"))
      }
    } else {
      result$tbl_analysesData <- NULL
    }
    invisible()
  }

  # Reload Analysis Run Log table
  .reloadAnaRunLog <- function() {
    logMessage(".reloadAnaRunLog called")
    if (!is.null(result$anaID)) {
        result$tbl_analysisrunlog <- session$userData$oasisapi$return_df(
          paste("analyses", result$anaID, "run_traceback_file", sep = "/")
        )
        result$tbl_analysisrunlog <- pre(HTML(result$tbl_analysisrunlog[[1]]))
    } else {
      result$tbl_analysisrunlog <-  NULL
    }
    invisible()
  }

  .cancelAnaModal <- function() {
    ns <- session$ns
    modalDialog(label = "cancelAnaModal",
                title = uiOutput(ns("cancelAnaModaltitle"), inline = TRUE),
                paste0("Are you sure that you want to cancel this analysis?"),
                footer = tagList(
                  oasisuiButton(ns("abuttonConfirmDelAna"),
                                label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonConfirmDel, placement = "right"),
                  actionButton(ns("btnCancelAnaDel"),
                               label = "Go back", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # Model Outout ---------------------------------------------------------------

  moduleOutput <- c(
    list(
      navigationstate = reactive(result$navigationstate),
      dashboardAnaID = reactive({result$dashboardAnaID})
    )
  )

  moduleOutput

}

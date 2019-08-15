# step2_chooseAnalysis Module -----------------------------------------------------

# UI ---------------------------------------------------------------------------

#' step2_chooseAnalysis UI
#'
#' @rdname step2_chooseAnalysis
#'
#' @description UI/View for the step2_chooseAnalysis.
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step2_chooseAnalysisUI <- function(id) {

  ns <- NS(id)

  tagList(
    hidden(div(id = ns("panelCreateAnalysesTable"), panelCreateAnalysesTable(id))),
    hidden(div(id = ns("panelAnalysisDetails"), panelAnalysisDetails(id))),
    hidden(div(id = ns("panelAnalysisLog"), panelAnalysisLog(id))),
    hidden(div(id = ns("panelModelTable"), panelModelTable(id))),
    hidden(div(id = ns("panelModelDetails"), modeldetailsUI(ns("modeldetails"))))
  )
}

#' panelCreateAnalysesTable
#'
#' @rdname panelCreateAnalysesTable
#'
#' @description Function wrapping panel to show analyses table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelCreateAnalysesTable <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_createanalyses"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_CreateAnalysesTable"), inline = TRUE),
      oasisuiRefreshButton(ns("abuttonanarefresh"))
    ),
    DTOutput(ns("dt_analyses")),
    fluidRow(
      column(12,
             oasisuiTableButton(inputId = ns("abuttonstartcancIG"), label = "Generate Inputs") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonstartcancIG, placement = "right"),
             oasisuiTableButton(inputId = ns("abuttonshowlog"), label = "Show Log") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonshowlog, placement = "right"),
             oasisuiTableButton(inputId = ns("abuttonshowanadetails"), label = "Show Details") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonshowanadetails, placement = "right")
      )
    ),
    br(),
    fluidRow(
      column(12,
             oasisuiButton(inputId = ns("abuttoncreateana"), label = "Create Analysis") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttoncreateana, placement = "right"),
             actionButton(ns("abuttonpgotonextstep"), "Proceed to Configure Output & Run", style = "float:right")
      ),
      style = "margin-top: 10px;"
    )
  )
}

#' panelAnalysisDetails
#'
#' @rdname panelAnalysisDetails
#'
#' @description Function wrapping panel to show analyses details table.
#'
#' @template params-module-ui
#'
#' @export
panelAnalysisDetails <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysisdetails"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_analysis_details"), inline = TRUE),
      actionButton(inputId = ns("buttonhideanadetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    analysis_detailsUI(ns("analysis_details"))
  )
}

#' panelAnalysisLog
#'
#' @rdname panelAnalysisLog
#'
#' @description Function wrapping panel to show analyses logs table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelAnalysisLog <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysislog"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnalysisLog"), inline = TRUE),
      oasisuiRefreshButton(ns("abuttonanalogrefresh")),
      actionButton(inputId = ns("buttonhideanalog"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_analysislog")),
    downloadButton(ns("download_log"), label = "Download")
  )
}

#' panelModelTable
#'
#' @rdname panelModelTable
#'
#' @description Function wrapping panel to show list of models table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelModelTable <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_model"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_ModelTable"), inline = TRUE),
      oasisuiRefreshButton(ns("abuttonmodelrefresh")),
      actionButton(inputId = ns("buttonhidemodel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_models")),
    fluidRow(
      column(4,
             oasisuiButton(ns("abuttonmodeldetails"), "Show Model Details", style = "float:left") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonmodeldetails, placement = "right")),
      column(6,
             br(),
             div(textInput(inputId = ns("anaName"), label = "Analysis Name"), style = "float:right;")),
      column(2,
             br(),
             oasisuiButton(ns("abuttonsubmit"), "Submit", style = "float:right; margin-top:25px;")
      )
    )
  )
}

# Server -----------------------------------------------------------------------

#' step2_chooseAnalysis Server
#'
#' @rdname step2_chooseAnalysis
#'
#' @description Server logic to step2_chooseAnalysis.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#'
#' @param currstep current selected step.
#' @param portfolioID selected portfolio ID.
#' @param pfName Name of selected portfolio
#' @param pfstatus Status of selected portfolio
#'
#' @return newstep navigation step
#' @return analysisID analysis ID
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs onclick
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT selectPage
#' @importFrom dplyr filter
#' @importFrom dplyr sym
#'
#' @export
step2_chooseAnalysis <- function(input, output, session,
                                 active = reactive(TRUE),
                                 currstep = reactive(-1),
                                 portfolioID = reactive({""}),
                                 pfName = reactive({""}),
                                 pfstatus = reactive({""})

) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  # list of sub-modules
  sub_modules <- list()

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # reactive for modelID
    modelID = "",
    # reactive value for model table
    tbl_modelsData = NULL,
    # analyses table
    tbl_analysesData = NULL,
    #analysis log
    tbl_analysislog = NULL,
    #analysis ID
    analysisID = NULL,
    #exposure_counter
    exposure_counter = NULL
  )

  # Panels Visualization -------------------------------------------------------
  observeEvent({
    currstep()
    portfolioID()}, {
      .hideDivs()
      if (currstep() == 2) {
        .defaultAssociateModel()
        .reloadAnaData()
        .defaultAnalysisDetails()
        .reloadtbl_modelsData()
      }
    })

  observeEvent(input$dt_analyses_rows_selected, ignoreNULL = FALSE, {
    hide("panelAnalysisLog")
    hide("panelModelTable")
    hide("panelAnalysisGenInputs")
  })

  # Analyses  Table ------------------------------------------------------------

  output$dt_analyses <- renderDT(
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0) {
      index <- 1
      logMessage("re-rendering analysis table")
      datatable(
        result$tbl_analysesData %>%session$userData$data_hub$return_tbl_analysesData_nice(admin_mode = getOption("oasisui.settings.admin.mode"), Status = Status, tbl_modelsDataNames = tbl_modelsDataNames, tbl_portfoliosDataNames = tbl_portfoliosDataNames, tbl_analysesDataNames = tbl_analysesDataNames),
        class = "oasisui-table display",
        rownames = FALSE,
        selection = list(mode = 'single',
                         selected = index),
        escape = FALSE,
        filter = 'bottom',
        options = getTableOptions(maxrowsperpage = pageLength)
      )
    } else {
      nothingToShowTable("No analysis available")
    })

  # Create Analyses Table  Title
  output$paneltitle_CreateAnalysesTable <- renderUI({
    if (!is.null(portfolioID()) && portfolioID() != "") {
      pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
      paste0('Analyses associated with portfolio ', pfName, ', id ', toString(portfolioID()))
    } else {
      paste0('Analyses')
    }
  })

  observeEvent(portfolioID(), {
    .reloadAnaData()
  })


  # Analysis ID ----------------------------------------------------------------

  observeEvent({
    input$dt_analyses_rows_selected
    portfolioID()}, ignoreNULL = FALSE, {
      if (!is.null(input$dt_analyses_rows_selected)) {
        result$analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
      } else {
        result$analysisID <- NULL
      }
    })

  # Model ID -------------------------------------------------------------------

  observeEvent({
    input$dt_models_rows_selected
    portfolioID()}, ignoreNULL = FALSE, {
      if (!is.null(input$dt_models_rows_selected)) {
        result$modelID <- result$tbl_modelsData[input$dt_models_rows_selected, tbl_modelsDataNames$id]
      } else {
        result$modelID <- ""
      }
    })

  # Generate input -------------------------------------------------------------
  onclick("abuttonstartcancIG", {
    if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] == Status_details$input_gen_started) {
      showModal(.cancelIGModal())
    } else {
      hide("panelAnalysisDetails")
      hide("panelAnalysisLog")
      hide("panelModelTable")
      hide("panelAnalysisGenInputs")
      hide("panelModelDetails")
      input_generation_id <- session$userData$oasisapi$api_post_query(query_path = paste("analyses", result$analysisID, "generate_inputs",  sep = "/"))
      if (input_generation_id$status == "Success") {
        oasisuiNotification(type = "message",
                             paste0("Input generation for analysis id ", result$analysisID, " started."))
      } else {
        oasisuiNotification(type = "error",
                             paste0("Input generation for analysis id ", result$analysisID, " could not be started."))
      }
      anaid <- result$analysisID
      .reloadAnaData()
      idxSel <- match(anaid, result$tbl_analysesData[, tbl_analysesDataNames$id])
      pageSel <- ceiling(idxSel/pageLength)
      selectRows(dataTableProxy("dt_analyses"), idxSel)
      selectPage(dataTableProxy("dt_analyses"), pageSel)
    }
  })

  output$cancelIGModaltitle <- renderUI({
    AnaId <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    AnaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
    paste0('Cancel input generation for id ', AnaId, ', ', AnaName)
  })

  .cancelIGModal <- function(){
    ns <- session$ns
    modalDialog(label = "cancelIGModal",
                title = uiOutput(ns("cancelIGModaltitle"), inline = TRUE),
                paste0("Are you sure that you want to cancel this input generation?"),
                footer = tagList(
                  oasisuiButton(ns("abuttonConfirmDelIG"),
                                 label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonConfirmDelIG, placement = "right"),
                  actionButton(ns("btnCancelIGDel"),
                               label = "Go back", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$btnCancelIGDel, {
    removeModal()
    hide("panelAnalysisDetails")
    hide("panelAnalysisLog")
    hide("panelModelTable")
    hide("panelAnalysisGenInputs")
    hide("panelModelDetails")
  })

  observeEvent(input$abuttonConfirmDelIG, {
    removeModal()

    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    delete_analyses_id <- session$userData$oasisapi$api_post_query(query_path = paste("analyses", analysisID, "cancel_generate_inputs",  sep = "/"))

    if (delete_analyses_id$status == "Success") {
      oasisuiNotification(type = "message",
                           paste0("Input Generation for analysis id ", analysisID, "cancelled."))
    } else {
      oasisuiNotification(type = "error",
                           paste0("Input Generation for analysis id ", analysisID, " could not be cancelled."))
    }

    anaid <- result$analysisID
    .reloadAnaData()
    idxSel <- match(anaid, result$tbl_analysesData[, tbl_analysesDataNames$id])
    pageSel <- ceiling(idxSel/pageLength)
    selectRows(dataTableProxy("dt_analyses"), idxSel)
    selectPage(dataTableProxy("dt_analyses"), pageSel)

  })

  # Analysis details ------------------------------------------------------------
  observeEvent (input$abuttonshowanadetails, {
    hide("panelAnalysisLog")
    hide("panelModelTable")
    hide("panelAnalysisGenInputs")
    hide("panelModelDetails")
    logMessage("showing panelAnalysisDetails")
    show("panelAnalysisDetails")
  })

  observeEvent({
    result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed]
    result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]}, {
      if (currstep() == 2) {
        .defaultAnalysisDetails()
      } else {
        hide("panelAnalysisDetails")
      }
    })

  observeEvent(input$abuttonshowanadetails,{
    result$exposure_counter <- result$exposure_counter + input$abuttonshowanadetails
  })

  sub_modules$analysis_details <- callModule(
    analysis_details,
    id = "analysis_details",
    analysisID = reactive({result$analysisID}),
    portfolioID = portfolioID,
    counter = reactive({result$exposure_counter})
  )

  onclick("buttonhideanadetails", {
    hide("panelAnalysisDetails")
  })

  # Analysis Logs --------------------------------------------------------------
  onclick("abuttonshowlog", {
    hide("panelAnalysisDetails")
    hide("panelModelTable")
    hide("panelAnalysisGenInputs")
    hide("panelModelDetails")

    logMessage("showing panelAnalysisLog")
    show("panelAnalysisLog")
    .reloadAnaLog()
  })

  onclick("buttonhideanalog", {
    hide("panelAnalysisLog")
  })

  output$paneltitle_AnalysisLog <- renderUI({
    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    AnaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
    paste0('Logs for analysis ', analysisID, ' ', AnaName)
  })


  # Export to .csv
  output$download_log <- downloadHandler(
    filename = "analysis_inputs_log.txt",
    content = function(file) {
      fwrite(result$tbl_analysislog,
             file,
             row.names = FALSE,
             col.names = FALSE,
             quote = FALSE)
    }
  )

  observeEvent(result$tbl_analysislog, {
    if (!is.null(result$tbl_analysislog) && nrow(result$tbl_analysislog) > 1) {
      show("download_log")
    } else {
      hide("download_log")
    }
  })

  output$dt_analysislog <- renderDT(
    if (!is.null(result$tbl_analysislog) && nrow(result$tbl_analysislog) > 1) {
      logMessage("re-rendering analysis log table")
      datatable(
        result$tbl_analysislog %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'none'),
        options = getTableOptions(maxrowsperpage = pageLength)
      )
    } else {
      nothingToShowTable(paste0("No log files associtated with analysis id ", result$analysisID))
    }
  )

  #  panelAnalysisLog Table title
  output$paneltitle_panelAnalysisLog <- renderUI({
    if (!is.null(result$analysisID)) {
      anaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
      paste0('Input generation Logs of analysis id ', toString(result$analysisID), ' ', anaName)
    } else {
      paste0("Input generation Logs")
    }
  })

  #  analysis_details Table title
  output$paneltitle_analysis_details <- renderUI({
    if (!is.null(result$analysisID)) {
      anaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
      analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
      paste0('Details of analysis id ', toString(analysisID), ' ', anaName)
    }
  })


  # Model Table ----------------------------------------------------------------

  onclick("abuttoncreateana", {
    hide("panelAnalysisDetails")
    hide("panelAnalysisLog")
    hide("panelAnalysisGenInputs")
    hide("panelAnalysisDetails")
    logMessage("showing panelModelTable")
    show("panelModelTable")
    .reloadtbl_modelsData()
    .clearinputanaName()
  })

  onclick("buttonhidemodel", {
    hide("panelModelTable")
  })

  output$dt_models <- renderDT(
    if (!is.null(result$tbl_modelsData) && nrow(result$tbl_modelsData) > 0 ) {
      logMessage("re-rendering model table")
      datatable(
        result$tbl_modelsData %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = FALSE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = 1),
        options = getTableOptions(maxrowsperpage = pageLength)
      )
    } else {
      nothingToShowTable(paste0("No models associated with Portfolio ID ", portfolioID()))
    }
  )

  # Model Table title
  output$paneltitle_ModelTable <- renderUI({
    if (!is.null(portfolioID()) && portfolioID() != "") {
      pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
      paste0('Pick a model and choose an analysis name')
    } else {
      paste0("List of models")
    }
  })

  # Model Details Table --------------------------------------------------------

  # Show/hide Model Details Panel
  onclick("abuttonmodeldetails", {
    hide("panelAnalysisDetails")
    hide("panelAnalysisLog")
    hide("panelAnalysisGenInputs")
    logMessage("showing panelModelDetails")
    show("panelModelDetails")
    logMessage("showing panelModelDetails")
  })

  #Hide panel if model id changes
  observeEvent(input$dt_models_rows_selected, ignoreNULL = FALSE, {
    hide("panelModelDetails")
  })

  callModule(
    modeldetails,
    id = "modeldetails",
    modelID = reactive({result$modelID}),
    portfolioID = portfolioID,
    counter = reactive({input$abuttonmodeldetails}),
    active = reactive(TRUE)
  )

  # Create new Analysis --------------------------------------------------------

  onclick("abuttonsubmit", {
    if (input$anaName != "") {
      post_portfolios_create_analysis <- session$userData$oasisapi$api_body_query(query_path = paste("portfolios", portfolioID(), "create_analysis", sep = "/"), query_body = list(name = input$anaName, model = result$modelID), query_method = "POST")
      logMessage(paste0("Calling api_post_portfolios_create_analysis with id ", portfolioID(),
                        " name ", input$anaName,
                        " model ",  result$modelID))
      if (post_portfolios_create_analysis$status == "Success") {
        oasisuiNotification(type = "message",
                             paste0("Analysis ", input$anaName, " created."))
        .reloadAnaData()
      } else {
        oasisuiNotification(type = "error",
                             paste0("Analysis ", input$anaName, " could not be created."))
      }
    }
    hide("panelModelTable")
    hide("panelModelDetails")
  })

  # Enable and disable buttons -------------------------------------------------

  #Make submit button dependent of analysis name
  observeEvent({
    input$dt_models_rows_selected
    input$anaName}, ignoreInit = TRUE, {
      if (length(input$dt_models_rows_selected) > 0 && !is.null(input$anaName) && input$anaName != "") {
        enable("abuttonsubmit")
      } else {
        disable("abuttonsubmit")
      }
    })

  #note initialization causes the buttons to be enabled on app lounch if tables are empty
  observeEvent({
    result$tbl_analysesData
    input$dt_analyses_rows_selected
    result$tbl_modelsData
    input$dt_models_rows_selected
    result$analysisID
    currstep()}, ignoreNULL = FALSE, ignoreInit = TRUE, {
      disable("abuttonshowlog")
      disable("abuttonshowanadetails")
      disable("abuttondelana")
      disable("abuttonstartcancIG")
      disable("abuttonmodeldetails")
      disable("abuttonpgotonextstep")
      disable("abuttonsubmit")
      if (length(input$dt_models_rows_selected) > 0) {
        enable("abuttonmodeldetails")
      }
      if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && length(input$dt_analyses_rows_selected) > 0 && max(input$dt_analyses_rows_selected) <= nrow(result$tbl_analysesData)) {
        enable("abuttonshowlog")
        enable("abuttondelana")
        enable("abuttonstartcancIG")
        if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] == Status_details$input_gen_started) {
          updateActionButton(session, inputId = "abuttonstartcancIG", label = "Cancel Input Generation")
        } else {
          updateActionButton(session, inputId = "abuttonstartcancIG", label = "Generate Inputs")
        }
        if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] != Status_details$input_gen_failed ||
            result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] != Status_details$input_gen_started) {
          enable("abuttonpgotonextstep")
        }
        if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] == Status_details$ready ||
            result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] == Status_details$run_err ||
            result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed] == Status_details$run_ok) {
          enable("abuttonshowanadetails")
        }
      }
    })

  #Not allowed creation of an analysis for an incomplete portfolio
  observeEvent({
    portfolioID()
    pfstatus()}, {
      if (pfstatus() == "- Status: Completed") {
        enable("abuttoncreateana")
      } else {
        disable("abuttoncreateana")
      }
    })

  # Refresh Buttons ------------------------------------------------------------
  onclick("abuttonanarefresh", {
    .reloadAnaData()
  })

  onclick("abuttonanalogrefresh", {
    .reloadAnaLog()
  })

  onclick("abuttonmodelrefresh", {
    .reloadtbl_modelsData()
  })

  # Help Functions -------------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    #Section "Choose Analysis" = "2"
    hide("panelCreateAnalysesTable")
    hide("panelAnalysisDetails")
    hide("panelAnalysisLog")
    hide("panelModelTable")
    hide("panelAnalysisGenInputs")
    hide("panelModelDetails")
  }

  # show default analysis details
  .defaultAnalysisDetails <- function(){
    curr_status <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status_detailed]
    if (length(curr_status) > 0 && !is.na(curr_status) &&  (curr_status == Status_details$ready ||
                                                            curr_status == Status_details$run_err ||
                                                            curr_status == Status_details$run_ok)) {
      show("panelAnalysisDetails")
      result$exposure_counter <- result$exposure_counter + 1
    } else {
      hide("panelAnalysisDetails")
    }
  }

  #show default view for Section "Choose Analysis" = "2"
  .defaultAssociateModel <- function(){
    logMessage(".defaultAssociateModel called")
    show("panelCreateAnalysesTable")
  }

  # Reload Analysis table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData called")
    if (!is.null(portfolioID()) && portfolioID()  != "") {
      tbl_analysesData  <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
      if (!is.null(tbl_analysesData)  && nrow(tbl_analysesData) > 0) {
        result$tbl_analysesData <- tbl_analysesData %>% filter(!! sym(tbl_analysesDataNames$portfolio) == portfolioID())
      }
      logMessage("analyses table refreshed")
    }  else {
      result$tbl_analysesData <- NULL
    }
    invisible()
  }

  #clear text input
  .clearinputanaName <- function(){
    updateTextInput(session = session, inputId = "anaName", value = "")
  }

  # Reload Analysis Log table
  .reloadAnaLog <- function() {
    logMessage(".reloadAnaLog called")
    if (!is.null(result$analysisID)) {
      result$tbl_analysislog <- session$userData$oasisapi$return_df(paste( "analyses", result$analysisID, "input_generation_traceback_file", sep = "/"))
    } else {
      result$tbl_analysislog <-  NULL
    }
  }

  # Reload Programme Model table
  .reloadtbl_modelsData <- function() {
    logMessage(".reloadtbl_modelsData called")
    if (!is.null(portfolioID()) && portfolioID() != "") {
      result$tbl_modelsData <- session$userData$data_hub$return_tbl_modelsData(tbl_modelsDataNames = tbl_modelsDataNames)
      logMessage("models table refreshed")
    } else {
      result$tbl_modelsData <- NULL
    }
    invisible()
  }

  # Model Outout ---------------------------------------------------------------

  moduleOutput <- c(
    list(
      analysisID = reactive({result$analysisID}),
      newstep = reactive({input$abuttonpgotonextstep})
    )
  )

  moduleOutput

}

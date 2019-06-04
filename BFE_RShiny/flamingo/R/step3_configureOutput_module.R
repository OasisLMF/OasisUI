# step3_configureOutput Module -------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step3_configureOutputUI
#'
#' @rdname step3_configureOutput
#'
#' @description UI/View for the step3_configureOutput.
#'
#' @template params-module-ui
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
    hidden(div(id = ns("panelDefineOutputs"), panelDefineOutputs(id))),
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
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysis"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnalysisTable"), inline = TRUE),
      flamingoRefreshButton(ns("abuttonanarefresh"))
    ),
    div(id = "divAnalysis",
        DTOutput(ns("dt_analyses")),
        fluidRow(column(12,
                        div(id = ns("divAnalysisButtons"),
                            flamingoTableButton(inputId = ns("abuttoncancelana"), label = "Cancel Analysis Run") %>%
                              bs_embed_tooltip(title = defineSingleAna$abuttoncancelana, placement = "right"),
                            flamingoTableButton(inputId = ns("abuttonshowlog"), label = "Show Log") %>%
                              bs_embed_tooltip(title = defineSingleAna$abuttonshowlog, placement = "right")
                        ))
        ),
        br(),
        fluidRow(column(12,
                        flamingoButton(inputId = ns("abuttonrunconfig"), label = "New Output Configuration") %>%
                          bs_embed_tooltip(title = defineSingleAna$abuttonrunconfig, placement = "right"),
                        div(
                          actionButton(inputId = ns("abuttondisplayoutput"), label = "Proceed to Dashboard") %>%
                            bs_embed_tooltip(title = defineSingleAna$abuttondisplayoutput, placement = "right"),
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
#' @importFrom DT DTOutput
#'
#' @export
panelAnalysisLogs <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_analogs"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnaLogs"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelog"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_analysesrunlog")),
    downloadButton(ns("download_log"), label = "Download")
  )
}

#' panelDefineOutputs
#'
#' @rdname panelDefineOutputs
#'
#' @description Function wrapping panel to define outputs
#'
#' @template params-module-ui
#'
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefineOutputs <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_anaoutput"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defAnaConfigOutput"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidepanelconfigureoutput"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(4,
             panelDefineOutputsDetails(id)),
      column(8,
             panelDefOutputConfiguration(id))
    ),
    fluidRow(
      column(12,
             flamingoButton(inputId = ns("abuttonexecuteanarun"), label = "Execute Run"), align = "right")) %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonexecuteanarun, placement = "right")
  )
}

#' panelDefineOutputsDetails
#'
#' @rdname panelDefineOutputsDetails
#'
#' @description Function wrapping sub-panel to define outputs details.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelDefineOutputsDetails <- function(id) {
  ns <- NS(id)
  tagList(
    flamingoPanel(
      collapsible = FALSE,
      ns("panel_ConfigDetails"),
      heading = h4("Configuration details"),
      selectInput(ns("sinoutputoptions"), "Select Custom Configuration:", choices = "")
    ),
    flamingoPanel(
      collapsible = FALSE,
      ns("panel_defAnaOutputDetails"),
      heading = h4("Model parameters"),
      div(id = ns("basic"), style = "width:100%; margin: 0 auto;",
          uiOutput(ns("basic_model_param")),
          uiOutput(ns("chkinputsperils"))
      ),
      hidden(div(id = ns("configureAnaParamsAdvanced"), align = "left",
                 textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10"),
                 textInput(ns("tinputthreshold"), label = "Loss Threshold:", value = "0"),
                 checkboxInput(ns("chkinputsummaryoption"), "Summary Reports", value = TRUE),
                 uiOutput(ns("advanced_model_param"))
      ))
    )
  )
}

#' panelDefOutputConfiguration
#'
#' @rdname panelDefOutputConfiguration
#'
#' @description Function wrapping sub-panel to define outputs configuration.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefOutputConfiguration <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_outconfig"),
    heading = h4("Output configuration"),
    checkboxInput(ns("chkinputGUL"), label = "Ground Up Loss", value = TRUE),
    hidden(div(id = ns("panel_configureAdvancedGUL"), panel_configureAdvancedGUL(id))),
    checkboxInput(ns("chkinputIL"), label = "Insured Loss", value = FALSE),
    hidden(div(id = ns("panel_configureAdvancedIL"), panel_configureAdvancedIL(id))),
    checkboxInput(ns("chkinputRI"), label = "Net RI Loss", value = FALSE),
    hidden(div(id = ns("panel_configureAdvancedRI"), panel_configureAdvancedRI(id))),
    flamingoButton(inputId = ns("abuttonadvanced"), label = "Advanced"),
    hidden(flamingoButton(inputId = ns("abuttonbasic"), label = "Basic")),
    hidden(flamingoButton(inputId = ns("abuttonclroutopt"), label = "Default"))
  )
}

#' panel_configureAdvancedGUL
#'
#' @rdname panel_configureAdvancedGUL
#'
#' @description Function wrapping sub-panel to define outputs advanced configuration GUL.
#'
#' @template params-module-ui
#'
#' @export
panel_configureAdvancedGUL <- function(id) {
  ns <- NS(id)

  fluidRow(
    # Few outputs commented/disabled for the first release. To be enabled for later releases.
    column(4,
           h4("Ground Up Loss", class = "flamingo-loss"),
           h5("Full Sample", class = "flamingo-measure"),
           h5("ELT", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("AEP", class = "flamingo-measure")),
           tags$div(class = "h5-align", h5("OEP", class = "flamingo-measure")),
           tags$div(class = "h5-align", h5("Multi AEP", class = "flamingo-measure")),
           h5("Multi OEP", class = "flamingo-measure"),
           # h5("WS Mean AEP", class = "flamingo-measure"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", class = "flamingo-measure")),
           # tags$div(class = "h5-align", h5("Sample Mean AEP", class = "flamingo-measure")),
           # h5("Sample Mean OEP", class = "flamingo-measure"),
           h5("AAL", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("PLT", class = "flamingo-measure"))),

    tags$div(class = "multicol",
             checkboxGroupInput(ns("chkgulprog"),
                                label = h6("Prog", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = varsdf$vars[varsdf$defaultChoice]),

             checkboxGroupInput(ns("chkgulstate"),
                                label = h6("State", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkgulcounty"),
                                label = h6("County", class = "flamingo-granularity"),
                                choices =LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkgulloc"),
                                label = h6("Location", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkgullob"),
                                label = h6("LOB", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkgulpolicy"),
                                label = h6("Policy", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL)
    )
  )
}

#' panel_configureAdvancedIL
#'
#' @rdname panel_configureAdvancedIL
#'
#' @description Function wrapping sub-panel to define outputs advanced configuration IL.
#'
#' @template params-module-ui
#'
#' @export
panel_configureAdvancedIL <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           h4("Insured Loss", class = "flamingo-loss"),
           h5("Full Sample", class = "flamingo-measure"),
           h5("ELT", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("AEP", class = "flamingo-measure")),
           tags$div(class = "h5-align", h5("OEP", class = "flamingo-measure")),
           tags$div(class = "h5-align", h5("Multi AEP", class = "flamingo-measure")),
           h5("Multi OEP", class = "flamingo-measure"),
           # h5("WS Mean AEP", class = "flamingo-measure"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", class = "flamingo-measure")),
           # tags$div(class = "h5-align", h5("Sample Mean AEP", class = "flamingo-measure")),
           # h5("Sample Mean OEP", class = "flamingo-measure"),
           h5("AAL", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("PLT", class = "flamingo-measure"))),

    tags$div(class = "multicol",
             checkboxGroupInput(ns("chkilprog"),
                                label = h6("Prog", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkilstate"),
                                label = h6("State", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkilcounty"),
                                label = h6("County", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkilloc"),
                                label = h6("Location", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkillob"),
                                label = h6("LOB", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkilpolicy"),
                                label = h6("Policy", class = "flamingo-granularity"),
                                choices = LosstypesChoices,
                                selected = NULL)
    )
  )
}

#' panel_configureAdvancedRI
#'
#' @rdname panel_configureAdvancedRI
#'
#' @description Function wrapping sub-panel to define outputs advanced configuration RI.
#'
#' @template params-module-ui
#'
#' @export
panel_configureAdvancedRI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           h4("Net RI Loss", style = "font-size: 18px; font-weight: bold;"),
           h5("Full Sample", style = "font-size: 16.5px;"),
           h5("ELT", style="font-size: 16.5px;"),
           tags$div(class = "h5-align", h5("AEP", style="font-size: 16.5px;")),
           tags$div(class = "h5-align", h5("OEP", style="font-size: 16.5px;")),
           tags$div(class = "h5-align", h5("Multi AEP", style="font-size: 16.5px;")),
           h5("Multi OEP", style = "font-size: 16.5px;"),
           # h5("WS Mean AEP", style="font-size: 16.5px;"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", style="font-size: 16.5px;")),
           # tags$div(class = "h5-align",h5("Sample Mean AEP", style="font-size: 16.5px;")),
           # h5("Sample Mean OEP", style="font-size: 16.5px;"),
           h5("AAL", style="font-size: 16.5px;"),
           tags$div(class = "h5-align",h5("PLT", style="font-size: 16.5px;"))),

    tags$div(class = "multicol",
             checkboxGroupInput(ns("chkriprog"),
                                label = h5("Prog", style = "font-size: 15.0px;"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkristate"),
                                label = h5("State", style = "font-size: 15.0px;"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkricounty"),
                                label = h5("County", style = "font-size: 15.0px;"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkriloc"),
                                label = h5("Location", style = "font-size: 15.0px;"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkrilob"),
                                label = h5("LOB", style = "font-size: 15.0px;"),
                                choices = LosstypesChoices,
                                selected = NULL),

             checkboxGroupInput(ns("chkripolicy"),
                                label = h5("Policy", style = "font-size: 15.0px;"),
                                choices = LosstypesChoices,
                                selected = NULL)
    )
  )#end of fluidrow RI
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
#'
#' @return dashboardAnaID id of selected run.
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT selectPage
#' @importFrom shinyjs onclick
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
                                  analysisID = reactive(NULL)
) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  # Default checkgroup for  GUL, IL and RI
  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")
  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")
  checkrigrplist <- c("chkriprog", "chkristate", "chkricounty", "chkriloc", "chkrilob", "chkripolicy")

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
          if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] == Status$Ready) {
            enable("abuttonrunconfig")
            updateActionButton(session, inputId = "abuttonrunconfig", label = "Output Configuration")
          } else if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] %in% c(Status$Completed, Status$Failed)) {
            enable("abuttonrunconfig")
            updateActionButton(session, inputId = "abuttonrunconfig", label = "Rerun")
          } else {
            updateActionButton(session, inputId = "abuttonrunconfig", label = "Output Configuration")
          }
        }
      }
    }
  )

  # reactive expression yielding the output options as a list
  outputOptionsList <- reactive({paste(collapse = ",", c(
    input$chkinputGUL, input$chkgulprog, input$chkgulpolicy,
    input$chkgulstate, input$chkgulcounty, input$chkgulloc,
    input$chkgullob,
    input$chkinputIL, input$chkilprog, input$chkilpolicy,
    input$chkilstate, input$chkilcounty, input$chkilloc,
    input$chkillob,
    input$chkinputRI, input$chkriprog, input$chkripolicy,
    input$chkristate, input$chkricounty, input$chkriloc,
    input$chkrilob
  ))})

  # Enable and disable buttons based on output confifig
  observeEvent(outputOptionsList(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (outputOptionsList() != "") {
      enable("abuttonexecuteanarun")
    } else {
      disable("abuttonexecuteanarun")
    }
  })

  # Analyses  Table ------------------------------------------------------------

  output$dt_analyses <- renderDT(
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0) {
      index <- which(result$tbl_analysesData[ ,tbl_analysesDataNames$id] == analysisID())
      if (length(index) == 0 && is.null(analysisID())) {
        index <- 1
      }
      logMessage("re-rendering analysis table")
      datatable(
        result$tbl_analysesData %>% return_tbl_analysesData_nice(),
        class = "flamingo-table display",
        rownames = FALSE,
        selection = list(mode = 'single',
                         selected = index),
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
      paste0('Analyses associated with portfolio ', pfName, ', id ', portfolioID())
    } else {
      paste0("Analyses")
    }
  })

  # Delete analysis button -----------------------------------------------------
  onclick("abuttoncancelana", {
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
    delete_analyses_id <- api_post_analyses_cancel(analysisID)

    if (delete_analyses_id$status == "Success") {
      flamingoNotification(type = "message",
                           paste0("Analysis id ", analysisID, " cancelled."))
      .reloadAnaData()
      idxSel <- match(analysisID, result$tbl_analysesData[, tbl_analysesDataNames$id])
      pageSel <- ceiling(idxSel/pageLength)
      selectRows(dataTableProxy("dt_analyses"), idxSel)
      selectPage(dataTableProxy("dt_analyses"), pageSel)
    } else {
      flamingoNotification(type = "error",
                           paste0("Error in cancelling analysis ", result$anaID, ". Analysis is not running."))
    }

  })

  # Configure Output -----------------------------------------------------------
  # hide panel
  onclick("abuttonhidepanelconfigureoutput", {
    hide("panelDefineOutputs")
  })

  # configuration title
  output$paneltitle_defAnaConfigOutput <- renderUI({
    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
    analysisName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
    analysisName <- ifelse(analysisName == " ", "", paste0('"', analysisName, '"'))
    if (result$ana_flag  == "R") {
      paste0('Re-define output configuration for analysis id ', analysisID, ' ', analysisName)
    } else {
      paste0('Define output configuration for analysis id ', analysisID, ' ', analysisName)
    }
  })

  #Show Output Configuration Panel and Re-run
  onclick("abuttonrunconfig", {
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && length(input$dt_analyses_rows_selected) > 0) {
      if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] == Status$Ready) {
        .defaultview()
        hide("panelAnalysisLogs")
        show("panelDefineOutputs")
        logMessage("showing panelDefineOutputs")
        result$ana_flag <- "C"

      } else if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] %in% c(Status$Completed, Status$Failed)) {
        .defaultview()
        hide("panelAnalysisLogs")
        show("panelDefineOutputs")
        logMessage("showing panelDefineOutputs")
        result$ana_flag <- "R"
        analysis_settings <- return_analyses_settings_file_list(result$anaID)
        analysisName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$name]
        if (!is.null(analysis_settings$detail) && analysis_settings$detail == "Not found.") {
          flamingoNotification(type = "error",
                               paste0("No output configuration associated to analysis ", analysisName," id ", result$anaID, "."))
        } else {
          logMessage(paste0("appling the output configuration of analysis ", analysisName, " id ", result$anaID))
          #Set inputs
          .updateOutputConfig(analysis_settings)
        }
      }
    }
  })

  # Hide Output Configuration panel
  onclick("abuttonehidepanelconfigureoutput", {
    hide("panelDefineOutputs")
    result$ana_flag <- "C"
  })

  # simplified view selection
  observe({
    gullistlength <- lapply(checkgulgrplist, function(i){
      length(input[[i]])
    }) %>% unlist() %>% sum()
    if (gullistlength > 0) {
      updateCheckboxInput(session, "chkinputGUL", value = TRUE)
      disable("chkgulpolicy")
    }
    illistlength <- lapply(checkilgrplist, function(i){
      length(input[[i]])
    }) %>% unlist() %>% sum()
    if (illistlength > 0) {
      updateCheckboxInput(session, "chkinputIL", value = TRUE)
    }
    rilistlength <- lapply(checkrigrplist, function(i){
      length(input[[i]])
    }) %>% unlist() %>% sum()
    if (rilistlength > 0) {
      updateCheckboxInput(session, "chkinputRI", value = TRUE)
    }
  })


  # Select/deselect GUL
  observeEvent(input$chkinputGUL, ignoreInit = TRUE,  {
    if (active()) {
      if (!input$chkinputGUL) {
        .clearchkboxgrp(checkgulgrplist)
      }  else {
        disable("chkgulpolicy")
        gullistlength <- lapply(checkgulgrplist, function(i){
          length(input[[i]])
        }) %>% unlist() %>% sum()
        if (gullistlength == 0) {
          .defaultchkboxGULgrp()
        }
      }
    }
  })

  # Select/deselect IL
  # Note: the ignoreInit = TRUE does not prevent the trigger once logged in
  observeEvent(input$chkinputIL, ignoreInit = TRUE, {
    if (active()) {
      if (!input$chkinputIL) {
        .clearchkboxgrp(checkilgrplist)
      } else {
        illistlength <- lapply(checkilgrplist, function(i){
          length(input[[i]])
        }) %>% unlist() %>% sum()
        if (illistlength == 0) {
          .defaultchkboxILgrp()
        }
      }
    }
  })

  # Select/deselect RI
  observeEvent(input$chkinputRI, ignoreInit = TRUE, {
    if (active()) {
      if (!input$chkinputRI) {
        .clearchkboxgrp(checkrigrplist)
      } else {
        rilistlength <- lapply(checkrigrplist, function(i){
          length(input[[i]])
        }) %>% unlist() %>% sum()
        if (rilistlength == 0) {
          .defaultchkboxRIgrp()
        }
      }
    }
  })

  # Update button in sidebar panel to update checkboxes for pre-populated values
  #To-Do update output configuration based on analysis setting
  observeEvent(input$sinoutputoptions, {
    # Using analyses names to select the output configuration of a previously posted analyses
    logMessage(paste0("input$sinoutputoptions changed to ",input$sinoutputoptions))
    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "") {
      anaName <- strsplit(input$sinoutputoptions, split = " / ")[[1]][2]
      anaID <- strsplit(input$sinoutputoptions, split = " / ")[[1]][1]
      analysis_settings <-  return_analyses_settings_file_list(anaID)
      if (!is.null(analysis_settings$detail) && analysis_settings$detail == "Not found.") {
        flamingoNotification(type = "error",
                             paste0("No output configuration associated to analysis ", anaName," id ", anaID, "."))
      } else {
        logMessage(paste0("appling the output configuration of analysis ", anaName," id ", anaID))
        #Set inputs
        .updateOutputConfig(analysis_settings)
      }
    }
  })

  # Clear the checkbox groups and preset dropdown - Set back to default
  onclick("abuttonclroutopt", {
    .defaultview()
  })

  # show advanced view
  onclick("abuttonadvanced", {
    .advancedview()
  })

  # show basic view
  onclick("abuttonbasic", {
    .basicview()
  })

  # Run Analyses ---------------------------------------------------------------

  # Execute analysis
  onclick("abuttonexecuteanarun", {
    analysis_settingsList <- .gen_analysis_settings()
    #write out file to be uploades
    currfolder <- getOption("flamingo.settings.api.share_filepath")
    dest <- file.path(currfolder, "analysis_settings.json")
    write_json(analysis_settingsList, dest, pretty = TRUE, auto_unbox = TRUE)

    #post analysis settings
    post_analysis_settings_file <- api_post_analyses_settings_file(result$anaID, dest)

    if (post_analysis_settings_file$status == "Success") {
      flamingoNotification(type = "message",
                           paste0("Analysis settings posted to ", result$anaID ,"."))

      analyses_run <- return_df(api_post_analyses_run,result$anaID)


      if (!is.null(analyses_run) && nrow(analyses_run) == 1) {

        idxSel <- match(result$anaID, analyses_run[, tbl_analysesDataNames$id])
        pageSel <- ceiling(idxSel/pageLength)
        .reloadAnaData()
        hide("panelDefineOutputs")
        .defaultview()
        selectRows(dataTableProxy("dt_analyses"), idxSel)
        selectPage(dataTableProxy("dt_analyses"), pageSel)

        if (analyses_run[[tbl_analysesDataNames$status]] == "RUN_STARTED") {
          flamingoNotification(type = "message",
                               paste0("Analysis ", result$anaID ," is executing."))
        }
      } else {
        flamingoNotification(type = "error",
                             paste0("Run could not be started for analysis ", result$anaID, "."))
      }

    } else {
      flamingoNotification(type = "error",
                           paste0("Analysis settings not posted to ", result$anaID ,
                                  "; error ", post_analysis_settings_file$status, "."))
    }

  })

  # Logs -----------------------------------------------------------------------
  onclick("abuttonshowlog", {
    hide("panelDefineOutputs")
    show("panelAnalysisLogs")
    logMessage("showing analysis run log table")
    .reloadAnaRunLog()
  })

  onclick("abuttonhidelog", {
    hide("panelAnalysisLogs")
  })

  # Export to .csv
  output$download_log <- downloadHandler(
    filename = "analysis_run_log.txt",
    content = function(file) {
      fwrite(result$tbl_analysisrunlog,
             file,
             row.names = FALSE,
             col.names = FALSE,
             quote = FALSE)
    }
  )

  observeEvent(result$tbl_analysisrunlog, {
    if (!is.null(result$tbl_analysisrunlog) && nrow(result$tbl_analysisrunlog) > 1) {
      show("download_log")
    } else {
      hide("download_log")
    }
  })

  ### Log Table
  output$dt_analysesrunlog <- renderDT({
    if (length(input$dt_analyses_rows_selected) > 0) {
      logMessage("re-rendering analysis log table")
      if (!is.null(result$tbl_analysisrunlog) && nrow(result$tbl_analysisrunlog) > 1) {
        datatable(
          result$tbl_analysisrunlog %>% capitalize_names_df(),
          class = "flamingo-table display",
          rownames = TRUE,
          selection = "none",
          escape = FALSE,
          filter = 'bottom',
          options = getTableOptions(maxrowsperpage = pageLength)
        )
      } else {
        nothingToShowTable(paste0("No log files associated with analysis ID ", ifelse(!is.null(result$anaID), result$anaID, "NULL")))
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
  onclick("abuttonanarefresh", {
    .reloadAnaData()
  } )

  onclick("abuttonanarefreshlogs", {
    .reloadAnaRunLog()
  })

  # Updates dependent on changed: dt_analyses_rows_selected --------------------
  # Allow display output option only if run successful. Otherwise default view is logs
  observeEvent({
    input$dt_analyses_rows_selected
    result$tbl_analysesData
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste("input$dt_analyses_rows_selected is changed to:", input$dt_analyses_rows_selected))
      hide("panelDefineOutputs")
      hide("panelAnalysisLogs")
      if (length(input$dt_analyses_rows_selected) > 0 && !is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && max(input$dt_analyses_rows_selected) <= nrow(result$tbl_analysesData)) {
        result$anaID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$id]
        logMessage(paste0("analysisId changed to ", result$anaID))
        if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$status] == Status$Failed) {
          show("panelAnalysisLogs")
          logMessage("showing analysis run log table")
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


  # Help Functions -------------------------------------------------------------

  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    #Section "Configure Output & Run" = "3"
    hide("panelAnalysisTable")
    hide("panelDefineOutputs")
    hide("panelAnalysisLogs")
  }

  #show default view for Section "Configure Output & Run" = "3"
  .defaultstep3 <- function(){
    logMessage(".defaultstep3 called")
    show("panelAnalysisTable")
    disable("chkgulpolicy")
  }

  # Reload Analyses table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData called")
    if (portfolioID()  != "") {
      tbl_analysesData  <- return_tbl_analysesData()
      if (!is.null(tbl_analysesData)  && nrow(tbl_analysesData) > 0) {
        tbl_analysesData <- tbl_analysesData %>% filter(!! sym(tbl_analysesDataNames$portfolio) == portfolioID())
        result$tbl_analysesData <- tbl_analysesData
      }
      logMessage("analyses table refreshed")
    }  else {
      result$tbl_analysesData <- NULL
    }
    invisible()
  }

  # Reload Analysis Run Log table
  .reloadAnaRunLog <- function() {
    logMessage(".reloadAnaRunLog called")
    if (!is.null(result$anaID)) {
      result$tbl_analysisrunlog <- return_file_df(api_get_analyses_run_traceback_file, result$anaID)
    } else {
      result$tbl_analysisrunlog <-  NULL
    }
  }

  .cancelAnaModal <- function(){
    ns <- session$ns
    modalDialog(label = "cancelAnaModal",
                title = uiOutput(ns("cancelAnaModaltitle"), inline = TRUE),
                paste0("Are you sure that you want to cancel this analysis?"),
                footer = tagList(
                  flamingoButton(ns("abuttonConfirmDelAna"),
                                 label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = defineSingleAna$abuttonConfirmDel, placement = "right"),
                  actionButton(ns("btnCancelAnaDel"),
                               label = "Go back", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # Clear Custom Configuration option
  .clearOutputOptions <- function() {
    logMessage(".clearOutputOptions called")
    tbl_analysesData  <- return_tbl_analysesData()
    tbl_analysesData <- tbl_analysesData %>% filter(status != Status$Processing & status != Status$Ready)
    namesList <- tbl_analysesData[,tbl_analysesDataNames$name]
    idList <- tbl_analysesData[,tbl_analysesDataNames$id]
    choicesList <- paste(idList, namesList, sep = " / ")
    updateSelectInput(session, "sinoutputoptions",
                      choices = choicesList,
                      selected = character(0))
  }

  #utility function to swap names and values of a list
  .SwapNamesValueInList <- function(List) {
    L <- setNames(names(List), unlist(List) %>% as.vector() ) %>% as.list()
    return(L)
  }

  # Clear other runtime params
  .clearotherparams <- function() {
    logMessage(".clearotherparams called")
    .clearOutputOptions()
    modelID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$model]
    modelID <- ifelse(modelID == "", -1, modelID)
    tbl_modelsDetails <- return_response(api_get_models_id_resource_file, modelID)
    if (modelID != -1 && !is.null(tbl_modelsDetails)) {
      model_settings <- tbl_modelsDetails$model_settings %>%
        unlist(recursive = FALSE)
      names_settings_type <- lapply(names(model_settings), function(i) {model_settings[[i]][["type"]]}) %>%
        setNames(names(model_settings))

      if (length(names(model_settings)) > 0 ) {

        fixed_settings <- c("event_set", "event_occurrence_id")

        basic_model_params <- names(model_settings)[names(model_settings) %in% fixed_settings]
        ui_basic_model_param <- lapply(basic_model_params, function(p){
          curr_param_lst <- model_settings[[p]]
          curr_param_name <- capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
          if (curr_param_lst$type == "boolean") {
            checkboxInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name, value = curr_param_lst$default)
          } else if (curr_param_lst$type == "dictionary") {
            selectInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        choices = .SwapNamesValueInList(curr_param_lst$values), selected =  curr_param_lst$default)
          } else if (curr_param_lst$type == "float") {
            sliderInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        min = curr_param_lst$min, max = curr_param_lst$max, value =  curr_param_lst$default)
          }
        })
        output$basic_model_param <- renderUI(ui_basic_model_param)

        model_perils <- names(model_settings)[grepl("peril_", names(model_settings))]
        if (length(model_perils) > 0 ) {
          ui_perils <- lapply(model_perils, function(p){
            curr_param_lst <- model_settings[[p]]
            curr_param_name <- capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
            checkboxInput(ns(paste0("model_params_", p)), label = curr_param_lst$name, value = curr_param_lst$default)
          })
          output$chkinputsperils <- renderUI(list(h5("Available Perils"),ui_perils))
        }

        advanced_model_param <- names(model_settings)[ names(model_settings) %notin% c(basic_model_params, model_perils)]
        ui_advanced_model_param <- lapply(advanced_model_param, function(p){
          curr_param_lst <- model_settings[[p]]
          curr_param_name <- capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
          if (curr_param_lst$type == "boolean") {
            checkboxInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name, value = curr_param_lst$default)
          } else if (curr_param_lst$type == "dictionary") {
            selectInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        choices = .SwapNamesValueInList(curr_param_lst$values), selected =  curr_param_lst$default)
          } else if (curr_param_lst$type == "float") {
            sliderInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        min = curr_param_lst$min, max = curr_param_lst$max, value =  curr_param_lst$default)
          }
        })
        output$advanced_model_param <- renderUI(ui_advanced_model_param)
      }

    }
  }

  # Clear checkboxgroups
  .clearchkboxgrp <- function(checkgrplist) {
    logMessage(".clearchkboxgrp called")
    for (i in checkgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "")
    }
  }

  # Default GUL output configuration options
  .defaultchkboxGULgrp <- function() {
    logMessage(".defaultchkboxGULgrp called")
    for (i in checkgulgrplist) {
      if (i == "chkgulprog") {
        defaultSelectChoices <- varsdf$vars[varsdf$defaultChoice]
      } else {
        defaultSelectChoices <- ""
      }
      updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoices)
    }
  }

  # Default output configuration options
  .defaultchkboxILgrp <- function() {
    logMessage(".defaultchkboxILgrp called")
    for (i in checkilgrplist) {
      if (i == "chkilprog" | i == "chkilpolicy") {
        defaultSelectChoices <- varsdf$vars[varsdf$defaultChoice]
      } else {
        defaultSelectChoices <- ""
      }
      updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoices)
    }
  }

  # Default output configuration options
  .defaultchkboxRIgrp <- function() {
    for (i in checkrigrplist) {
      if (i == "chkriprog" | i == "chkripolicy") {
        defaultSelectChoices <- varsdf$vars[varsdf$defaultChoice]
      } else {
        defaultSelectChoices <- ""
      }
      updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoices)
    }
  }

  # Output view
  .advancedview <- function() {
    logMessage(".advancedview called")
    show("panel_configureAdvancedGUL")
    show("panel_configureAdvancedIL")
    show("panel_configureAdvancedRI")
    show("configureAnaParamsAdvanced")
    show("abuttonbasic")
    hide("abuttonadvanced")
    show("abuttonclroutopt")
  }

  .basicview <- function() {
    logMessage(".basicview called")
    hide("panel_configureAdvancedGUL")
    hide("panel_configureAdvancedIL")
    hide("panel_configureAdvancedRI")
    hide("configureAnaParamsAdvanced")
    hide("abuttonbasic")
    show("abuttonadvanced")
    hide("abuttonclroutopt")
  }

  .defaultview <- function() {
    logMessage(".defaultview called")
    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp()
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .clearchkboxgrp(checkilgrplist)
    updateCheckboxInput(session, "chkinputRI", value = FALSE)
    .clearchkboxgrp(checkrigrplist)
    .clearotherparams()
    .basicview()
  }


  #Generate Analysis settings file
  .gen_analysis_settings <- function(){

    logMessage(".gen_analysis_settings called")

    modelID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesDataNames$model]
    modelData <- return_tbl_modelData(modelID)
    tbl_modelsDetails <- return_response(api_get_models_id_resource_file, modelID)
    model_settings <- tbl_modelsDetails$model_settings %>%
      unlist(recursive = FALSE)
    model_params_lst <- lapply(names(model_settings), function(i){
      ifelse( is.null(input[[paste0("model_params_", i)]]),model_settings[[i]]$default,input[[paste0("model_params_", i)]])
      }) %>%
      setNames(names(model_settings))


    inputsettings <- list(
      #analysisSettingsMapping
      "analysis_tag" = as.integer(result$anaID), #potential new tag analysis_id
      "gul_threshold" = as.integer(input$tinputthreshold),
      "model_version_id" = modelData[[tbl_modelsDataNames$model_id]], # potential new tag model_id
      "module_supplier_id" = modelData[[tbl_modelsDataNames$supplier_id]], # potential new tag model_supplier_id
      "number_of_samples" = as.integer(input$tinputnoofsample),
      "prog_id" = as.integer(portfolioID()), # potential new tag `portfolio_id`
      "source_tag" = getOption("flamingo.settings.oasis_environment"), # potential new tag environment_tag,
      #outoutSettingsMappings
      "gul_output" = input$chkinputGUL,
      "il_output" = input$chkinputIL,
      "ri_output" = input$chkinputRI,
      "return_period_file" = TRUE  # currenlty hardcoded
    ) %>%
      c(model_params_lst)

    outputsLossTypes <- list(
      "gul_output" = list("prog" = input$chkgulprog, "policy" = input$chkgulpolicy, "state" = input$chkgulstate, "county" = input$chkgulcounty, "location" = input$chkgulloc, "lob" = input$chkgullob),
      "il_output" =  list("prog" = input$chkilprog, "policy" = input$chkilpolicy, "state" = input$chkilstate, "county" = input$chkilcounty, "location" =  input$chkilloc, "lob" = input$chkillob),
      "ri_output"  = list("prog" = input$chkriprog, "policy" = input$chkripolicy, "state" = input$chkristate, "county" = input$chkricounty, "location" =  input$chkriloc, "lob" = input$chkrilob)
    )

    #add summaries
    ReportChoices <- c('FullUncAEP', 'FullUncOEP', 'AAL')
    if (input$chkinputsummaryoption) {
      for (l in names(outputsLossTypes)) {
        outputsLossTypes[[l]][["prog"]] <- unique(c(outputsLossTypes[[l]][["prog"]], ReportChoices))
      }
    }
    analysis_settings <- construct_analysis_settings(inputsettings, outputsLossTypes)
    return(analysis_settings)
  }

  #update analyses settings
  .updateOutputConfig <- function(analysis_settings){
    logMessage(".updateOutputConfig called")

    #clean checkboxes
    .clearchkboxgrp(checkgulgrplist)
    .clearchkboxgrp(checkilgrplist)
    .clearchkboxgrp(checkrigrplist)

    #reduced list
    settings <- analysis_settings[["analysis_settings"]]
    SettingsMapping <- list(
      "threshold"  = list(
        "inputId" = "tinputthreshold",
        "UpdateWidget" = "updateTextInput",
        "SettingElement" = settings[["gul_threshold"]]
      )
    )

    .updateWidget("threshold", SettingsMapping)

    for (L in tolower(output_options$losstypes)) { #L <- "GUL"
      l <- tolower(L)
      summary_settings <- settings[[paste0(l, "_summaries")]]
      sel_losstype <- settings[[paste0(l, "_output")]]
      if (!is.null(sel_losstype)) {
        SummaryMapping <- list(
          "chkinput" = list(
            "inputId" = paste0("chkinput", L),
            "UpdateWidget" = "updateCheckboxInput",
            "SettingElement" = settings[[paste0(l, "_output")]]
          )
        )
        .updateWidget("chkinput", SummaryMapping)

        for (g in seq(length(summary_settings))) { #g <- 1
          curr_gran <- summary_settings[[g]]
          oed_gran <- granToOed$outputlosstype[granToOed$oed == curr_gran$oed_fields]
          chkgroup_name <- paste0("chk", l, oed_gran)
          nolec_output <- varsdf$fields[!varsdf$lec_output]
          lec_output <- varsdf$fields[varsdf$lec_output]
          selection <- c(curr_gran[nolec_output], curr_gran$leccalc$outputs[lec_output])
          if (!is.null(selection)) {
            selected_fields <- names(selection)[which(unlist(selection))]
            selected_choices <- varsdf$vars[which(varsdf$fields %in%  selected_fields)]
            if (!is.null(selected_choices)) {
              currchkGroupMapping <- list(
                "chkgroup" = list(
                  "inputId" = chkgroup_name,
                  "UpdateWidget" = "updateCheckboxGroupInput",
                  "SettingElement" = selected_choices
                )
              )
              .updateWidget("chkgroup", currchkGroupMapping)
            }
          }
        }
      }
    }
  }

  .updateWidget <- function(inp, MappingList) {
    logMessage(".updateWidget called")
    curr_setting <- MappingList[[inp]]
    if (curr_setting$UpdateWidget == "updateSelectInput" | curr_setting$UpdateWidget == "updateCheckboxGroupInput") {
      get(curr_setting$UpdateWidget)(session = session, inputId = curr_setting$inputId, selected = curr_setting$SettingElement)
    } else {
      get(curr_setting$UpdateWidget)(session = session, inputId = curr_setting$inputId, value = curr_setting$SettingElement)
    }
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

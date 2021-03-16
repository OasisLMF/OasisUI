#' singleAna
#'
#' @rdname singleAna
#'
#' @description Server logic to define an analysis
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#'
#' @param preselPanel selectedstep to visualize as returned from either
#'  \link{visualizationSBR}, \link{visualizationCBR} or \link{visualizationBBR}
#' @param selectAnaID id of selected analysis as returned from \link{visualizationSBR}
#' @param selectPortfolioID portfolio id of selected analysis as returned from \link{visualizationSBR}
#'
#' @return anaID  selected analysis ID
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#'
#' @export
singleAna <- function(input, output, session,
                      active = reactive(TRUE),
                      selectAnaID = reactive(NULL),
                      selectPortfolioID = reactive(""),
                      preselPanel = reactive(1)) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------
  # Navigation State
  navigation_state <- reactiveNavigation()

  # Submodules list
  submodulesList <- list()

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # Id of the analysis to use in dashboard
    dashboardanaID = NULL,
    # Id of analysis
    anaID = NULL,
    # Id of the portfolio
    portfolioID = "",
    # Portfolio table
    tbl_portfoliosData = NULL,
    # Portfolio table row selected
    tbl_portfoliosData_rowselected = NULL,
    # Portfolio Name
    pfName = "",
    # List of Portfolio IDs
    pfChoices = NULL,
    # Portfolio status
    pfstatus = "",
    # modified default model settings values in case of customizable (configurable) models
    customModSettings = NULL
  )

  # Panels switch --------------------------------------------------------------
  # Module to control colors of radio buttons in the singleAnaWorkflowSteps
  workflowSteps <- callModule(singleAnaWorkflowSteps, "workflowsteps")

  # Make sure the view is reset:
  # to first panel if accessing from landing page
  # and to panel 3 if coming from Browse
  observe(if (active()) {
    workflowSteps$update(analysisWorkflowSteps[[preselPanel()]])
    result$dashboardanaID <- NULL
  })

  observeEvent(workflowSteps$step(), ignoreInit = TRUE, {
    if (active()) {
      switch(
        workflowSteps$step(),
        "1" = {
          logMessage("showing Section 'Choose Portfolio' = '1'")
          hide("panelDefineIDs")
        },
        "2" = {
          logMessage("showing Section 'Choose Model' = '2'")
          show("panelDefineIDs")
        },
        "3" = {
          logMessage("showing Section 'Configure Output & Run' = '3'")
          show("panelDefineIDs")
        }
      )
    }
  })

  # Sub-Modules ----------------------------------------------------------------
  submodulesList$step1_choosePortfolio <- callModule(
    step1_choosePortfolio,
    id = "step1_choosePortfolio",
    active = reactive({active() && workflowSteps$step() == 1}),
    currstep = reactive(workflowSteps$step()),
    portfolioID = reactive(result$portfolioID)
  )

  submodulesList$step2_chooseAnalysis <- callModule(
    step2_chooseAnalysis,
    id = "step2_chooseAnalysis",
    active = reactive({active() && workflowSteps$step() == 2}),
    currstep = reactive(workflowSteps$step()),
    portfolioID = reactive(input$portfolioID),
    pfName = reactive({result$pfName}),
    pfstatus = reactive({result$pfstatus})
  )

  submodulesList$step3_configureOutput <- callModule(
    step3_configureOutput,
    id = "step3_configureOutput",
    active = reactive({active() && workflowSteps$step() == 3}),
    currstep = reactive(workflowSteps$step()),
    portfolioID =  reactive({input$portfolioID}),
    pfName = reactive({result$pfName}),
    analysisID = reactive({result$anaID}),
    customModSettings = reactive({result$customModSettings})
  )

  # Sub-Modules output ---------------------------------------------------------
  # > Customizable settings -------------------------------------------------------------
  observeEvent(submodulesList$step2_chooseAnalysis$customsettings(), {
    result$customModSettings <- submodulesList$step2_chooseAnalysis$customsettings()
    logMessage(paste0("output of step2: updating result$customModSettings"))
  })

  # > Navigation ---------------------------------------------------------------
  observeEvent(submodulesList$step3_configureOutput$navigationstate(), ignoreInit = TRUE, {
    if (!is.null(submodulesList$step3_configureOutput$navigationstate())) {
      updateNavigation(navigation_state, submodulesList$step3_configureOutput$navigationstate())
    }
  })

  observeEvent(submodulesList$step1_choosePortfolio$newstep(), ignoreInit = TRUE, {
    workflowSteps$update(analysisWorkflowSteps[[2]])
  })

  observeEvent(submodulesList$step2_chooseAnalysis$newstep(), ignoreInit = TRUE, {
    workflowSteps$update(analysisWorkflowSteps[[3]])
  })

  # > AnaId --------------------------------------------------------------------
  observeEvent(submodulesList$step3_configureOutput$dashboardAnaID(), {
    result$dashboardanaID <- submodulesList$step3_configureOutput$dashboardAnaID()
    logMessage(paste0("updating result$dashboardanaID because submodulesList$step3_configureOutput$dashboardAnaID() changed to: ", result$dashboardanaID))
  })

  observeEvent(submodulesList$step2_chooseAnalysis$analysisID(), {
    result$anaID <- submodulesList$step2_chooseAnalysis$analysisID()
    logMessage(paste0("updating result$anaID because submodulesList$step2_chooseAnalysis$analysisID() changed to: ", result$anaID))
  })

  observeEvent(selectAnaID(), {
    result$anaID <- selectAnaID()
    logMessage(paste0("updating result$anaID because selectAnaID() changed to: ", result$anaID))
  })

  # > portfolioID --------------------------------------------------------------
  observeEvent(submodulesList$step1_choosePortfolio$portfolioID(), ignoreInit = TRUE, {
    portfolioID <- submodulesList$step1_choosePortfolio$portfolioID()
    #Avoid updating input if not necessary
    if (!is.na(portfolioID) && result$portfolioID != portfolioID) {
      logMessage(paste0("updating result$portfolioID because submodulesList$step1_choosePortfolio$portfolioID() changed to: ", portfolioID ))
      result$portfolioID <- portfolioID
    }
  })

  observeEvent(selectPortfolioID(),  ignoreInit = TRUE, {
    portfolioID <- selectPortfolioID()
    #Avoid updating input if not necessary
    if (!is.na(portfolioID) && portfolioID != "" && result$portfolioID != portfolioID) {
      logMessage(paste0("updating result$portfolioID becauseselectPortfolioID() changed to: ", portfolioID ))
      result$portfolioID <- portfolioID
    }
  })

  observeEvent(input$portfolioID, ignoreInit = TRUE,{
    #Avoid updating input if not necessary
    if (input$portfolioID != result$portfolioID) {
      logMessage(paste0("updating result$portfolioID because input$portfolioID changed to: ", input$portfolioID ))
      result$portfolioID <- input$portfolioID
    }
  })

  observeEvent(result$portfolioID, ignoreInit = TRUE,{
    #Avoid updating input if not necessary
    if (input$portfolioID != result$portfolioID) {
      logMessage(paste0("updating input$portfolioID because result$portfolioID changed to: ", result$portfolioID ))
      updateSelectizeInput(session, inputId = "portfolioID", selected = result$portfolioID, choices = result$pfChoices)
    }
  })

  observeEvent({
    workflowSteps$step()
  }, {if (workflowSteps$step() != 1) {
    #Avoid updating input if not necessary
    if (input$portfolioID != result$portfolioID) {
      logMessage(paste0("updating input$portfolioID because result$portfolioID changed to: ", result$portfolioID ))
      updateSelectizeInput(session, inputId = "portfolioID", selected = result$portfolioID, choices = result$pfChoices)
    }  else if (input$portfolioID == "" && input$portfolioID == result$portfolioID) {
      logMessage(paste0("updating input$portfolioID choices"))
      updateSelectizeInput(session, inputId = "portfolioID", selected = character(0), choices = result$pfChoices)
    }
  }
  })

  # > prog Table reactives -----------------------------------------------------
  observeEvent(submodulesList$step1_choosePortfolio$tbl_portfoliosData(), ignoreInit = TRUE,{
    if (is.null(submodulesList$step1_choosePortfolio$tbl_portfoliosData()) || nrow(submodulesList$step1_choosePortfolio$tbl_portfoliosData()) == 0) {
      result$tbl_portfoliosData <- session$userData$data_hub$return_tbl_portfoliosData(Status = Status,
                                                                                       tbl_portfoliosDataNames = tbl_portfoliosDataNames)
    } else {
      result$tbl_portfoliosData <- submodulesList$step1_choosePortfolio$tbl_portfoliosData()
    }
    if (!is.null(result$tbl_portfoliosData) && nrow(result$tbl_portfoliosData) > 0) {
      result$pfChoices <- result$tbl_portfoliosData[, tbl_portfoliosDataNames$id]
    } else {
      result$pfChoices <- NULL
    }
  })

  observeEvent({
    result$portfolioID
    result$pfChoices
    result$tbl_portfoliosData
  }, ignoreInit = TRUE, {
    result$tbl_portfoliosData_rowselected <- match(result$portfolioID, result$pfChoices)
    result$pfName <- result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosDataNames$name]
    pfstatus <- ""
    if (!is.na(result$tbl_portfoliosData_rowselected) && !is.na(result$tbl_portfoliosData) && length(result$tbl_portfoliosData_rowselected) > 0) {
      if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosDataNames$status] == Status$Completed) {
        pfstatus <- "- Status: Completed"
      } else if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosDataNames$status] == Status$Processing) {
        pfstatus <- "- Status: in Progress"
      } else if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosDataNames$status] == Status$Failed) {
        pfstatus <- "- Status: Failed"
      }
    }
    result$pfstatus <- pfstatus
  })

  # Model Outout ---------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      anaID  = reactive(result$dashboardanaID)
    )
  )

  moduleOutput

}

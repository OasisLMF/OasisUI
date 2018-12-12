#' singleAna
#'
#' @rdname singleAna
#'
#' @description Server logic to define an analysis
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-logMessage
#' @template params-active
#' 
#' @param preselRunId selected run id as returned from \link{landingPage}
#' @param preselProcId selected progOasis id as returned from \link{landingPage}
#' @param preselPanel selectedstep to visualize as returned from either
#'  \link{visualizationSBR}, \link{visualizationCBR} or \link{visualizationBBR}
#'
#' @return anaID  selected analysis ID
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#'
#' @export
singleAna <- function(input, output, session,
                     active = reactive(TRUE), logMessage = message,
                     preselRunId = reactive(-1),
                     preselProcId = reactive(-1),
                     preselPanel = reactive(1)) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------
  # Navigation State
  navigation_state <- reactiveNavigation()

  # Submodules list
  submodulesList <- list()

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # Id of the analysis
    anaID = -1,
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
    pfstatus = ""
  )

  # Panels switch --------------------------------------------------------------
  # Module to control colors of radio buttons in the singleAnaWorkflowSteps
  workflowSteps <- callModule(singleAnaWorkflowSteps, "workflowsteps")

  # Make sure the view is reset:
  # to first panel if accessing from landing page
  # and to panel 3 if coming from Browse
  observe(if (active()) {
    workflowSteps$update(analysisWorkflowSteps[[preselPanel()]])
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
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    portfolioID = reactive(result$portfolioID)
  )

  submodulesList$step2_chooseAnalysis <- callModule(
    step2_chooseAnalysis,
    id = "step2_chooseAnalysis",
    active = reactive({active() && workflowSteps$step() == 2}),
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    portfolioID = reactive(input$portfolioID),
    pfName = reactive({result$pfName}),
    pfstatus = reactive({result$pfstatus})
  )

  submodulesList$step3_configureOutput <- callModule(
    step3_configureOutput,
    id = "step3_configureOutput",
    active = reactive({active() && workflowSteps$step() == 3}),
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    portfolioID =  reactive(input$portfolioID),
    analysisID = reactive(submodulesList$step2_chooseAnalysis$analysisID())
  )

  # Sub-Modules output ---------------------------------------------------------
  # > Navigation ---------------------------------------------------------------
  observeEvent(submodulesList$step3_configureOutput$navigationstate(), ignoreInit = TRUE, {
    if (submodulesList$step3_configureOutput$navigationstate() == "SBR") {
      updateNavigation(navigation_state, "SBR")
    }
  })

  observeEvent(submodulesList$step1_choosePortfolio$newstep(), ignoreInit = TRUE, {
    workflowSteps$update(analysisWorkflowSteps[[2]])
  })

  observeEvent(submodulesList$step2_chooseAnalysis$newstep(), ignoreInit = TRUE, {
    workflowSteps$update(analysisWorkflowSteps[[3]])
  })

  # > RunId --------------------------------------------------------------------
  observeEvent(submodulesList$step3_configureOutput$anaID(), ignoreInit = TRUE, {
    result$anaID <- submodulesList$step3_configureOutput$anaID()
  })

  # > portfolioID --------------------------------------------------------------
  observeEvent(submodulesList$step1_choosePortfolio$portfolioID(), ignoreInit = TRUE, {
    portfolioID <- submodulesList$step1_choosePortfolio$portfolioID()
    #Avoid updating input if not necessary
    if (!is.na(portfolioID) &&  result$portfolioID != portfolioID) {
      logMessage(paste0("updating result$portfolioID because submodulesList$step1_choosePortfolio$portfolioID() changed to: ", portfolioID ))
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
      result$tbl_portfoliosData <- return_tbl_portfoliosData()
    } else {
      result$tbl_portfoliosData <- submodulesList$step1_choosePortfolio$tbl_portfoliosData()
    }
    if (!is.null(result$tbl_portfoliosData) && nrow(result$tbl_portfoliosData) > 0) {
      result$pfChoices <- result$tbl_portfoliosData[, tbl_portfoliosData.PortfolioID]
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
    result$pfName <- result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.PortfolioName]
    pfstatus <- ""
    if (!is.na(result$tbl_portfoliosData_rowselected) && !is.na(result$tbl_portfoliosData) && length(result$tbl_portfoliosData_rowselected) > 0) {
      if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.Status] == StatusCompleted) {
        pfstatus <- "- Status: Completed"
      } else if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.Status] == StatusProcessing) {
        pfstatus <- "- Status: in Progress"
      } else if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.Status] == StatusFailed) {
        pfstatus <- "- Status: Failed"
      }
      print 
    }
    result$pfstatus <- pfstatus
  })
  
  # Model Outout ---------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      anaID  = reactive(result$anaID)
    )
  )

  moduleOutput

}

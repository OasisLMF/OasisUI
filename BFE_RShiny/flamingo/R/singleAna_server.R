#' singleAna
#'
#' @rdname singleAna
#'
#' @description Server logic to define an analysis
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#' 
#' @param preselRunId selected run id as returned from \link{landingpage}
#' @param preselProcId selected progOasis id as returned from \link{landingpage}
#' @param preselPanel selectedstep to visualize as returned from either
#'  \link{visualizationSBR}, \link{visualizationCBR} or \link{visualizationBBR}
#'
#' @return processRunId selected process run ID
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#'
#' @export
singleAna <- function(input, output, session, dbSettings,
                                      apiSettings, user, active = reactive(TRUE), logMessage = message,
                                      preselRunId = reactive(-1),
                                      preselProcId = reactive(-1),
                                      preselPanel = reactive(1),
                                      reloadMillis = 10000) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------
  # Navigation State
  navigation_state <- reactiveNavigation()

  # Submodules list
  submodulesList <- list()

  #values to stop ping pong effect
  stop_selPfID <- check_selPfID <- 0
  stop_selProgOasisID <- check_selProgOasisID <- 0

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # Id of the Process Run
    prrunid = -1,
    # Id of the programme
    modelID = "",
    # Id of the model
    portfolioID = "",
    # Prog table
    tbl_portfoliosData = NULL,
    # Prog table row selected
    tbl_portfoliosData_rowselected = NULL,
    # Prog Name
    pfName = "",
    # List of Prog IDs
    progChoices = NULL,
    # Prog status
    progStatus = "",
    # Model table
    POData = NULL,
    # Model table row selected
    POData_rowselected = NULL,
    # Model Name
    progOasisName = "",
    # List of Model IDs
    progOasisChoices = NULL,
    # Model status
    progOasisStatus = ""
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
          hide("divmodelID")
        },
        "3" = {
          logMessage("showing Section 'Configure Output & Run' = '3'")
          show("panelDefineIDs")
          show("divmodelID")
        }
      )
    }
  })

  # Sub-Modules ----------------------------------------------------------------
  submodulesList$step1_choosePortfolio <- callModule(
    step1_choosePortfolio,
    id = "step1_choosePortfolio",
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    user = user,
    active = reactive({active() && workflowSteps$step() == 1}),
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    portfolioID = reactive(result$portfolioID)
  )

  submodulesList$step2_chooseModel <- callModule(
    step2_chooseModel,
    id = "step2_chooseModel",
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    active = reactive({active() && workflowSteps$step() == 2}),
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    selectprogrammeID = reactive(input$portfolioID),
    selectprogOasisID = reactive(input$modelID),
    progName = reactive({result$pfName}),
    progStatus = reactive({result$progStatus}),
    DPProgData = reactive({result$tbl_portfoliosData})
  )

  submodulesList$step3_configureOutput <- callModule(
    step3_configureOutput,
    id = "step3_configureOutput",
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    active = reactive({active() && workflowSteps$step() == 3}),
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    selectprogrammeID = reactive(input$portfolioID),
    selectprogOasisID = reactive(input$modelID),
    progOasisName = reactive({result$progOasisName}),
    progOasisStatus = reactive({result$progOasisStatus})
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

  observeEvent(submodulesList$step2_chooseModel$newstep(), ignoreInit = TRUE, {
    workflowSteps$update(analysisWorkflowSteps[[3]])
  })

  # > RunId --------------------------------------------------------------------
  observeEvent(submodulesList$step3_configureOutput$prrunid(), ignoreInit = TRUE, {
    result$prrunid <- submodulesList$step3_configureOutput$prrunid()
  })

  # > portfolioID --------------------------------------------------------
  observeEvent(submodulesList$step1_choosePortfolio$portfolioID(), ignoreInit = TRUE, {
    prgId <- submodulesList$step1_choosePortfolio$portfolioID()
    #Avoid updating input if not necessary
    if (!is.na(prgId) &&  result$portfolioID != prgId) {
      logMessage(paste0("updating result$portfolioID because submodulesList$step1_choosePortfolio$portfolioID() changed to: ", prgId ))
      result$portfolioID <- prgId
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
      updateSelectizeInput(session, inputId = "portfolioID", selected = result$portfolioID, choices = result$progChoices)
    }  else if (input$portfolioID == "" && input$portfolioID == result$portfolioID) {
      logMessage(paste0("updating input$portfolioID choices"))
      updateSelectizeInput(session, inputId = "portfolioID", selected = character(0), choices = result$progChoices)
    }
  }
  })

  # > prog Table reactives -----------------------------------------------------
  observeEvent(submodulesList$step1_choosePortfolio$tbl_portfoliosData(), ignoreInit = TRUE,{
    if (is.null(submodulesList$step1_choosePortfolio$tbl_portfoliosData()) || nrow(submodulesList$step1_choosePortfolio$tbl_portfoliosData()) == 0) {
      stmt <- buildDbQuery("getProgData")
      result$tbl_portfoliosData <- executeDbQuery(dbSettings, stmt) %>%
        replaceWithIcons()
    } else {
      result$tbl_portfoliosData <- submodulesList$step1_choosePortfolio$tbl_portfoliosData()
    }
    result$progChoices <- result$tbl_portfoliosData[, tbl_portfoliosData.ProgrammeID]
  })

  observeEvent({
    result$portfolioID
    result$progChoices
    result$tbl_portfoliosData
  }, ignoreInit = TRUE, {
    result$tbl_portfoliosData_rowselected <- match(result$portfolioID, result$progChoices)
    result$pfName <- result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.ProgrammeName]
    progStatus <- ""
    if (!is.na(result$tbl_portfoliosData_rowselected) && !is.na(result$tbl_portfoliosData) && length(result$tbl_portfoliosData_rowselected) > 0) {
      if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.Status] == StatusCompleted) {
        progStatus <- "- Status: Completed"
      } else if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.Status] == StatusProcessing) {
        progStatus <- "- Status: in Progress"
      } else if (result$tbl_portfoliosData[result$tbl_portfoliosData_rowselected, tbl_portfoliosData.Status] == StatusFailed) {
        progStatus <- "- Status: Failed"
      }
    }
    result$progStatus <- progStatus
  })

  # > modelID --------------------------------------------------------
  observeEvent(submodulesList$step2_chooseModel$selectprogOasisID(), ignoreInit = TRUE, {
    progOasisId <- submodulesList$step2_chooseModel$selectprogOasisID()
    if (!is.null(progOasisId) && result$modelID != progOasisId) {
      logMessage(paste0("updating result$modelID because submodulesList$step2_chooseModel$portfolioID() changed to: ", progOasisId ))
      result$modelID <- submodulesList$step2_chooseModel$selectprogOasisID()
    }
  })

  observeEvent({
    input$modelID
    }, ignoreInit = TRUE, {
    #Avoid updating input if not necessary
    if (input$modelID != "" && result$modelID != input$modelID) {
      logMessage(paste0("updating result$modelID because input$modelID changed to: ", input$modelID ))
      result$modelID <- input$modelID
    }
  })

  # If programmeID changes, then we select the first progOasis
  observeEvent({
    result$POData_rowselected
    result$portfolioID
    }, ignoreInit = TRUE, {
    progOasisId <- ""
    if (!is.null(result$POData) && nrow(result$POData) > 0) {
      progOasisId <- result$POData[result$POData_rowselected, POData.ProgOasisId]
    }
    if (!is.null(progOasisId) && !is.na(progOasisId) && progOasisId != result$modelID) {
      logMessage(paste0("updating result$modelID because result$POData_rowselected changed to: ", result$POData_rowselected ))
      result$modelID <- progOasisId
    }
  })

  observeEvent({
    workflowSteps$step()
    result$modelID
  }, ignoreInit = TRUE, {
    if (workflowSteps$step() == 3) {
      #Avoid updating input if not necessary
      if (input$modelID  != result$modelID) {
        updateSelectizeInput(session, inputId = "modelID", selected = result$modelID, choices = result$progOasisChoices)
      }
    }
  })

  # > prog Model Table reactives -----------------------------------------------
  observeEvent({
    submodulesList$step2_chooseModel$POData()
    result$portfolioID
  }, ignoreInit = TRUE, {
    if (result$portfolioID != "" & !is.null(result$portfolioID)) {
      result$POData <- getProgOasisForProgdata(dbSettings, result$portfolioID) %>%
        replaceWithIcons()
      if (nrow(result$POData) != 0) {
        result$progOasisChoices <-  result$POData[, POData.ProgOasisId]
      } else {
        result$progOasisChoices <- c("")
      }

    }
  })

  observeEvent({
    result$portfolioID
    result$modelID
    result$progOasisChoices
    result$POData
  }, ignoreInit = TRUE, {
    prgOasisId <- result$modelID
    rowToSelect <- match(prgOasisId, result$progOasisChoices)
    result$POData_rowselected <- ifelse(is.na(rowToSelect), 1, rowToSelect)
    result$progOasisName <- ifelse(nrow(result$POData) > 0, result$POData[result$POData_rowselected, POData.ProgName], "")
    progOasisStatus <- ""
    if (!is.na(result$POData) && nrow(result$POData) > 0 && length(result$POData_rowselected) > 0) {
      if (result$POData[result$POData_rowselected, POData.Status] == StatusCompleted) {
        progOasisStatus <- "- Status: Completed"
      } else if (result$POData[result$POData_rowselected, POData.Status] == StatusProcessing) {
        progOasisStatus <- "- Status: in Progress"
      } else if (result$POData[result$POData_rowselected, POData.Status] == StatusFailed) {
        progOasisStatus <- "- Status: Failed"
      }
    }
    result$progOasisStatus <- progOasisStatus
  })


  # Model Outout ---------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      processRunId = reactive(result$prrunid)
    )
  )

  moduleOutput

}

#' replaceWithIcons
#'
#' @rdname replaceWithIcons
#'
#' @description Function to replace status with icons in table.
#'
#' @param df \code{data.frame}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @export
replaceWithIcons <- function(df){
  #Status
  StatusGood <- c("success", "completed", "loaded")
  StatusBad <- c("cancelled", "failed",  NA_character_)

  # Help function
  '%notin%' <- Negate('%in%')

  #Replace Status in df
  if (!is.null(df)) {
    logMessage(paste0("replacing icons"))
    df <- df %>%
      mutate(Status = tolower(Status)) %>%
      mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                Status %in% StatusBad ~ StatusFailed,
                                Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
      as.data.frame()
  }

  df
}

#' Single Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{programmeDefinitionSingle()}, list of reactives.
#' @template return-outputNavigation
#' @rdname programmeDefinitionSingle
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDT dataTableProxy selectRows DTOutput selectPage
#' @importFrom dplyr mutate select case_when
#' @importFrom shinyjs onclick js removeClass addClass
#' @export
programmeDefinitionSingle <- function(input, output, session, dbSettings,
                                      apiSettings, userId, active = reactive(TRUE), logMessage = message,
                                      preselRunId = reactive(-1),
                                      preselProcId = reactive(-1),
                                      preselPanel = "1",
                                      reloadMillis = 10000) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ------------------------------------------
  # Navigation State
  navigation_state <- reactiveNavigation()
  
  # Submodules list
  submodulesList <- list()
  
  #values to stop ping pong effect
  stop_selProgID <- check_selProgID <- 0
  stop_selProgOasisID <- check_selProgOasisID <- 0
  
  #Statuses to be replaced
  StatusGood <- "Loaded"
  StatusBad <- c("Failed", "Cancelled", NA_character_)
  
  # Help function
  '%notin%' <- Negate('%in%')
  
  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # Id of the Process Run
    prrunid = -1,
    # Id of the programme
    selectprogOasisID = "",
    # Id of the model
    selectprogrammeID = "",
    # Prog table
    DPProgData = NULL,
    # Prog table row selected
    DPProgData_rowselected = NULL,
    # Prog Name
    progName = "",
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
  
  # Panels switch ------------------------------------------------------------
  # Module to control colors of radio buttons in the singleProgrammeWorkflowSteps
  workflowSteps <- callModule(singleProgrammeWorkflowSteps, "workflowsteps")
  
  # Make sure the view is reset: 
  # to first panel if accessing from landing page
  # and to panel 3 if coming from Browse
  observe(if (active()) {
    workflowSteps$update(preselPanel())
  })
  
  observeEvent(workflowSteps$step(), ignoreInit = TRUE, {
    if (active()) {
      switch(
        workflowSteps$step(),
        "1" = {
          logMessage("showing Section 'Choose Programme' = '1'")
          hide("panelDefineIDs")
        },
        "2" = {
          logMessage("showing Section 'Choose Model' = '2'")
          show("panelDefineIDs")
          hide("divselectprogOasisID")
        },
        "3" = {
          logMessage("showing Section 'Configure Output & Run' = '3'")
          show("panelDefineIDs")
          show("divselectprogOasisID")
        }
      )
    }
  })
  
  # Sub-Modules --------------------------------------------------
  submodulesList$step1_chooseProgramme <- callModule(
    step1_chooseProgramme,
    id = "step1_chooseProgramme",
    dbSettings = dbSettings,
    apiSettings = apiSettings, 
    userId = userId, 
    active = reactive({active() && workflowSteps$step() == 1}), 
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    selectprogrammeID = reactive(result$selectprogrammeID)
  )
  
  submodulesList$step2_chooseModel <- callModule(
    step2_chooseModel,
    id = "step2_chooseModel",
    dbSettings = dbSettings,
    apiSettings = apiSettings, 
    userId = userId, 
    active = reactive({active() && workflowSteps$step() == 2}), 
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    selectprogrammeID = reactive(input$selectprogrammeID),
    selectprogOasisID = reactive(input$selectprogOasisID),
    progName = reactive({result$progName}),
    progStatus = reactive({result$progStatus}),
    DPProgData = reactive({result$DPProgData})
  )
  
  submodulesList$step3_configureOutput <- callModule(
    step3_configureOutput,
    id = "step3_configureOutput",
    dbSettings = dbSettings,
    apiSettings = apiSettings, 
    userId = userId, 
    active = reactive({active() && workflowSteps$step() == 3}), 
    logMessage = logMessage,
    currstep = reactive(workflowSteps$step()),
    selectprogrammeID = reactive(input$selectprogrammeID),
    selectprogOasisID = reactive(input$selectprogOasisID),
    progOasisName = reactive({result$progOasisName}),
    progOasisStatus = reactive({result$progOasisStatus}),
    POData_rowselected = reactive({result$POData_rowselected})
  )
  
  # Sub-Modules output ----------------------------------------------------------
  # > Navigation ----
  observeEvent(submodulesList$step3_configureOutput$navigationstate(), ignoreInit = TRUE, {
    if (submodulesList$step3_configureOutput$navigationstate() == "SBR") {
      updateNavigation(navigation_state, "SBR")
    }
  })
  
  # > RunId ----
  observeEvent(submodulesList$step3_configureOutput$prrunid(), ignoreInit = TRUE, {
    result$prrunid <- submodulesList$step3_configureOutput$prrunid()
  })
  
  # > selectprogrammeID -----
  observeEvent(submodulesList$step1_chooseProgramme$selectprogrammeID(), ignoreInit = TRUE, {
    prgId <- submodulesList$step1_chooseProgramme$selectprogrammeID()
    #Avoid updating input if not necessary
    if (!is.na(prgId) &&  result$selectprogrammeID != prgId) {
      logMessage(paste0("updating result$selectprogrammeID because submodulesList$step1_chooseProgramme$selectprogrammeID() changed to: ", prgId ))
      result$selectprogrammeID <- prgId
    }
  })
  
  observeEvent(submodulesList$step2_chooseModel$selectprogrammeID(), ignoreInit = TRUE, {
    prgId <- submodulesList$step2_chooseModel$selectprogrammeID()
    #Avoid updating input if not necessary
    if (prgId != "" && !is.na(prgId) && result$selectprogrammeID != prgId) {
      logMessage(paste0("updating result$selectprogrammeID because submodulesList$step2_chooseModel$selectprogrammeID() changed to: ", prgId ))
      result$selectprogrammeID <- prgId
    }
  })
  
  observeEvent(input$selectprogrammeID, ignoreInit = TRUE,{
    #Avoid updating input if not necessary
    if (input$selectprogrammeID != result$selectprogrammeID) {
      logMessage(paste0("updating result$selectprogrammeID because input$selectprogrammeID changed to: ", input$selectprogrammeID ))
      result$selectprogrammeID <- input$selectprogrammeID 
    }
  })
  
  observeEvent({
    workflowSteps$step()
  }, {if (workflowSteps$step() != 1) {
    #Avoid updating input if not necessary
    if (input$selectprogrammeID != result$selectprogrammeID) {
      logMessage(paste0("updating input$selectprogrammeID because result$selectprogrammeID changed to: ", result$selectprogrammeID ))
      updateSelectizeInput(session, inputId = "selectprogrammeID", selected = result$selectprogrammeID, choices = result$progChoices)
    }  
  }
  })
  
  # > prog Table reactives ----
  observeEvent(submodulesList$step1_chooseProgramme$DPProgData(), ignoreInit = TRUE,{
    if (is.null(submodulesList$step1_chooseProgramme$DPProgData()) || nrow(submodulesList$step1_chooseProgramme$DPProgData()) == 0) {
      stmt <- buildDbQuery("getProgData")
      result$DPProgData <- executeDbQuery(dbSettings, stmt) %>%
        mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                  Status %in% StatusBad ~ StatusFailed,
                                  Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame()
    } else {
      result$DPProgData <- submodulesList$step1_chooseProgramme$DPProgData()
    }
    result$progChoices <- result$DPProgData[, DPProgData.ProgrammeID]
  })
  
  observeEvent({
    result$selectprogrammeID
    result$progChoices
  }, ignoreInit = TRUE, {
    result$DPProgData_rowselected <- match(result$selectprogrammeID, result$progChoices)
    result$progName <- result$DPProgData[result$DPProgData_rowselected, DPProgData.ProgrammeName]
    progStatus <- ""
    if (!is.na(result$DPProgData_rowselected) && !is.na(result$DPProgData) && length(result$DPProgData_rowselected) > 0) {
      if (result$DPProgData[result$DPProgData_rowselected, DPProgData.Status] == StatusCompleted) {
        progStatus <- "- Status: Completed"
      } else if (result$DPProgData[result$DPProgData_rowselected, DPProgData.Status] == StatusProcessing) {
        progStatus <- "- Status: in Progress"
      } else if (result$DPProgData[result$DPProgData_rowselected, DPProgData.Status] == StatusFailed) {
        progStatus <- "- Status: Failed"
      }
    }
    result$progStatus <- progStatus
  })
  
  # > selectprogOasisID -----
  observeEvent(submodulesList$step2_chooseModel$selectprogOasisID(), ignoreInit = TRUE, {
    progOasisId <- submodulesList$step2_chooseModel$selectprogOasisID()
    if (!is.null(progOasisId) && result$selectprogOasisID != progOasisId) {
      logMessage(paste0("updating result$selectprogOasisID because submodulesList$step2_chooseModel$selectprogrammeID() changed to: ", progOasisId ))
      result$selectprogOasisID <- submodulesList$step2_chooseModel$selectprogOasisID()
    }
  })
  
  observeEvent({
    input$selectprogOasisID
    input$selectprogrammeID
    }, ignoreInit = TRUE, {
    #Avoid updating input if not necessary
    if (input$selectprogOasisID != "" && result$selectprogOasisID != input$selectprogOasisID) {
      logMessage(paste0("updating result$selectprogOasisID because input$selectprogOasisID changed to: ", input$selectprogOasisID ))
      result$selectprogOasisID <- input$selectprogOasisID
    }
  })
  
  #If programmeID changes, then we select the first progOasis 
  observeEvent(result$DPProgData_rowselected, ignoreInit = TRUE, {
    progOasisId <- NULL
    if (!is.null(result$POData) && nrow(result$POData) > 0) {
      progOasisId <- result$POData[result$POData_rowselected, POData.ProgOasisId]
    } 
    if (!is.null(progOasisId) && !is.na(progOasisId) && progOasisId != result$selectprogOasisID) {
      logMessage(paste0("updating result$selectprogOasisID because result$POData_rowselected changed to: ", result$POData_rowselected ))
      result$selectprogOasisID <- progOasisId
    }
  })
  
  observeEvent({
    workflowSteps$step()
    result$selectprogOasisID
  }, ignoreInit = TRUE, {
    if (workflowSteps$step() == 3) {
      #Avoid updating input if not necessary
      if (input$selectprogOasisID  != result$selectprogOasisID) {
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = result$selectprogOasisID, choices = result$progOasisChoices)
      } 
    }
  })
  
  # > prog Model Table reactives ----
  observeEvent({
    submodulesList$step2_chooseModel$POData()
    result$selectprogrammeID
  }, ignoreInit = TRUE, {
    if (result$selectprogrammeID != "" & !is.null(result$selectprogrammeID)) {
      result$POData <- getProgOasisForProgdata(dbSettings, result$selectprogrammeID) %>%
        mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                  Status %in% StatusBad ~ StatusFailed,
                                  Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame() 
      if (nrow(result$POData) != 0) {
        result$progOasisChoices <-  result$POData[, POData.ProgOasisId]
      } else {
        result$progOasisChoices <- c("")
      }
      
    }
  })
  
  observeEvent({
    input$selectprogrammeID
    result$selectprogOasisID
    result$progOasisChoices
  }, ignoreInit = TRUE, {
    prgOasisId <- result$selectprogOasisID
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
  
  
  # Model Outout ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      processRunId = reactive(result$prrunid)
    )
  )
  
  moduleOutput
  
}
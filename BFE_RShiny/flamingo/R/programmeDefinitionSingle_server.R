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
  
  # Observers for debugging -------------------------------------------------
  # 
  # observeEvent({
  #   input$selectprogOasisID
  #   input$selectprogrammeID
  #   result$selectprogOasisID
  #   result$selectprogrammeID
  #   result$POData_rowselected
  #   result$progOasisName
  #   result$progOasisChoices
  #   result$progOasisStatus
  #   result$DPProgData_rowselected
  #   result$progName
  #   result$progChoices
  #   result$progStatus
  # }, ignoreNULL = FALSE, ignoreInit = TRUE, {
  #   if (active()) {
  #     print("############################ current status of reactives")
  #     print(paste0("input$selectprogrammeID: ", input$selectprogrammeID))
  #     print(paste0("input$selectprogOasisID: ", input$selectprogOasisID))
  #     print(paste0("result$selectprogrammeID: ", result$selectprogrammeID))
  #     print(paste0("result$selectprogOasisID: ", result$selectprogOasisID))
  #     print(paste0("submodulesList$step1_chooseProgramme$selectprogrammeID:", submodulesList$step1_chooseProgramme$selectprogrammeID()))
  #     print(paste0("submodulesList$step2_chooseModel$selectprogOasisID: ", submodulesList$step2_chooseModel$selectprogOasisID()))
  #     
  #     print(paste0("result$prrunid: ", result$prrunid))
  #     
  #     #print(paste0("result$POData", head(result$POData)))
  #     print(paste0("POData_rowselected: ", result$POData_rowselected))
  #     print(paste0("progOasisName: ", result$progOasisName))
  #     print(paste0("progOasisChoices: ", result$progOasisChoices))
  #     print(paste0("progOasisStatus: ", result$progOasisStatus))
  #     
  #     #print(paste0("result$DPProgData", head(result$DPProgData)))
  #     print(paste0("DPProgData_rowselected: ", result$DPProgData_rowselected))
  #     print(paste0("progName: ", result$progName))
  #     print(paste0("progChoices: ", result$progChoices))
  #     print(paste0("progStatus: ", result$progStatus))
  #   }
  # })
  
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
    selectprogrammeID = reactive(result$selectprogrammeID),
    selectprogOasisID = reactive(result$selectprogOasisID),
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
    selectprogrammeID = reactive(result$selectprogrammeID),
    selectprogOasisID = reactive(result$selectprogOasisID),
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
    result$selectprogrammeID <- submodulesList$step1_chooseProgramme$selectprogrammeID()
  })
  
  observeEvent(submodulesList$step2_chooseModel$selectprogrammeID(), ignoreInit = TRUE, {
    prgId <- submodulesList$step2_chooseModel$selectprogrammeID()
    if (prgId != "" && !is.na(prgId) && !is.null(prgId) && result$selectprogrammeID != prgId) {
      result$selectprogrammeID <- submodulesList$step2_chooseModel$selectprogrammeID()
    }
  })
  
  observeEvent(input$selectprogrammeID, ignoreInit = TRUE,{
    if (input$selectprogrammeID != result$selectprogrammeID) {
      result$selectprogrammeID <- input$selectprogrammeID 
    }
  })
  
  observeEvent({
    workflowSteps$step()
    input$selectprogrammeID
  }, ignoreInit = TRUE,  {
    if (workflowSteps$step() != 1) {
      prgId <- ifelse(is.null(result$selectprogrammeID), "", result$selectprogrammeID)
      #Avoid updating input if not necessary
      if (input$selectprogrammeID != prgId) {
        updateSelectizeInput(session, inputId = "selectprogrammeID", selected = prgId, choices = result$progChoices)
      }  
    }
  })
  
  # > prog Table reactives ----
  observeEvent(submodulesList$step1_chooseProgramme$DPProgData(), ignoreInit = TRUE,{
    stmt <- buildDbQuery("getProgData")
    result$DPProgData <- executeDbQuery(dbSettings, stmt) %>%
      mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                Status %in% StatusBad ~ StatusFailed,
                                Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
      as.data.frame()
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
    result$selectprogOasisID <- submodulesList$step2_chooseModel$selectprogOasisID()
  })
  
  observeEvent(input$selectprogOasisID, ignoreInit = TRUE, {
    #Avoid updating input if not necessary
    if (!is.null(result$selectprogOasisID) && result$selectprogOasisID != input$selectprogOasisID) {
      result$selectprogOasisID <- input$selectprogOasisID
    }
  })
  
  observeEvent({
    workflowSteps$step()
    result$POData_rowselected
    input$selectprogrammeID
  }, ignoreInit = TRUE, {
    if (workflowSteps$step() == 3) {
      progid <- result$POData[result$POData_rowselected, POData.ProgOasisId]
      progid <- ifelse(is.null(progid), "", progid)
      #Avoid updating input if not necessary
      if (input$selectprogOasisID  != progid) {
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = progid, choices = result$progOasisChoices)
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
      result$progOasisChoices <-  result$POData[, POData.ProgOasisId]
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
    result$progOasisName <- result$POData[result$POData_rowselected, POData.ProgName]
    progOasisStatus <- ""
    if (!is.na(result$POData) && !is.na(result$POData_rowselected) && length(result$POData_rowselected) > 0) {
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
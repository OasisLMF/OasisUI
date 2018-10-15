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
  
  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # Id of the Process Run
    prrunid = -1,
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
  
  observeEvent({
    input$selectprogOasisID
    input$selectprogrammeID
    result$POData_rowselected
    result$progOasisName
    result$progOasisChoices
    result$progOasisStatus
    result$DPProgData_rowselected
    result$progName
    result$progChoices
    result$progStatus
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      print("current status of reactives")
      print(paste0("input$selectprogrammeID: ", input$selectprogrammeID))
      print(paste0("input$selectprogOasisID: ", input$selectprogOasisID))
      print(paste0("result$prrunid: ", result$prrunid))
      print(paste0("POData_rowselected: ", result$POData_rowselected))
      print(paste0("progOasisName: ", result$progOasisName))
      print(paste0("progOasisChoices: ", result$progOasisChoices))
      print(paste0("progOasisStatus: ", result$progOasisStatus))
      print(paste0("DPProgData_rowselected: ", result$DPProgData_rowselected))
      print(paste0("progName: ", result$progName))
      print(paste0("progChoices: ", result$progChoices))
      print(paste0("progStatus: ", result$progStatus))
    }
  })
  
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
    selectprogrammeID = reactive(input$selectprogrammeID)
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
    progStatus = reactive({result$progStatus})
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
  observeEvent(submodulesList$step3_configureOutput$navigationstate(), {
    if (submodulesList$step3_configureOutput$navigationstate() == "SBR") {
      updateNavigation(navigation_state, "SBR")
    }
  })
  
  # > RunId ----
  observeEvent(submodulesList$step3_configureOutput$prrunid(),{
    result$prrunid <- submodulesList$step3_configureOutput$prrunid()
  })
  
  # > prog Table reactives ----
  observeEvent(submodulesList$step1_chooseProgramme$DPProgData(), {
    result$DPProgData <- submodulesList$step1_chooseProgramme$DPProgData()
    result$progChoices <- result$DPProgData[, DPProgData.ProgrammeID]
    result$DPProgData_rowselected <- match(input$selectprogrammeID, result$progChoices)
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

  # > update of input$selectprogrammeID ----
  observeEvent({
    submodulesList$step1_chooseProgramme$selectprogrammeID()
    result$progChoices
    workflowSteps$step()
  }, {
    if (active() && workflowSteps$step() != 1 ) {
      prgId <- submodulesList$step1_chooseProgramme$selectprogrammeID()
      #Avoid updating input if not necessary
      if (!is.null(result$progChoices)) {
        updateSelectizeInput(session, inputId = "selectprogrammeID", selected = character(0), choices = result$progChoices)
      }
      if (prgId != "" && input$selectprogrammeID != prgId) {
        if (prgId != "") {
          updateSelectizeInput(session, inputId = "selectprogrammeID", selected = prgId)
        } else {
          updateSelectizeInput(session, inputId = "selectprogrammeID", selected = character(0))
        }
      }
    }
  })
  
  # > prog Model Table reactives ----
  observeEvent(submodulesList$step2_chooseModel$POData(), {
    result$POData <- submodulesList$step2_chooseModel$POData()
    models = ""
    if (!is.na(result$POData) && !is.null(result$POData)) {
      models <-  result$POData[, POData.ProgOasisId]
    } else {
      models <- getModelList(dbSettings)[, "Model ID"]
    }
    result$progOasisChoices <- models
    result$POData_rowselected <- match(input$selectprogOasisID, result$progOasisChoices)
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

  
  observeEvent( submodulesList$step2_chooseModel$selectprogrammeID(), {
    prgId <- submodulesList$step2_chooseModel$selectprogrammeID()
    #Avoid updating input if not necessary
    if (prgId != "" &&  input$selectprogrammeID != prgId) {
      updateSelectizeInput(session, inputId = "selectprogrammeID", selected = prgId)  
    }
  })
  
  # > update of input$selectprogOasisID ----
  observeEvent({
    submodulesList$step2_chooseModel$selectprogOasisID()
    result$progOasisChoices
    workflowSteps$step()
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active() &&  workflowSteps$step() == 3) {
      prgId <- submodulesList$step2_chooseModel$selectprogOasisID()
      print("I am here")
      if (!is.null(result$progOasisChoices)) {
        print("I am here 1")
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0), choices = result$progOasisChoices)
      }
      if (!is.na(prgId) && prgId != "") {
        print("I am here 2")
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = prgId)
      } else {
        print("I am here 3")
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0))
      }
    }
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
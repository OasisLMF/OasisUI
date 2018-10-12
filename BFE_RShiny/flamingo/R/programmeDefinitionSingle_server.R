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
    prrunid = -1
  )
  
  
  # Observers for debugging -------------------------------------------------
  
  observeEvent({
    input$selectprogOasisID
    input$selectprogrammeID
    POData_rowselected()
    progOasisName()
    progOasisChoices()
    progOasisStatus()
    DPProgData_rowselected()
    progName()
    progChoices()
    progStatus()
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      print("current status of reactives")
      print(paste0("input$selectprogrammeID: ", input$selectprogrammeID))
      print(paste0("input$selectprogOasisID: ", input$selectprogOasisID))
      print(paste0("result$prrunid: ", result$prrunid))
      print(paste0("POData_rowselected(): ", POData_rowselected()))
      print(paste0("progOasisName(): ", progOasisName()))
      print(paste0("progOasisChoices(): ", progOasisChoices()))
      print(paste0("progOasisStatus(): ", progOasisStatus()))
      print(paste0("DPProgData_rowselected(): ", DPProgData_rowselected()))
      print(paste0("progName(): ", progName()))
      print(paste0("progChoices(): ", progChoices()))
      print(paste0("progStatus(): ", progStatus()))
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
    progName = reactive({progName()}),
    progStatus = reactive({progStatus()})
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
    progOasisName = reactive({progOasisName()}),
    progOasisStatus = reactive({progOasisStatus()}),
    POData_rowselected = reactive({POData_rowselected()})
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
  DPProgData <- reactive({submodulesList$step1_chooseProgramme$DPProgData()})
  DPProgData_rowselected <- reactive({match(input$selectprogrammeID, progChoices())})
  progName <- reactive({DPProgData()[DPProgData_rowselected(), DPProgData.ProgrammeName]})
  progChoices <- reactive({DPProgData()[, DPProgData.ProgrammeID]})
  progStatus <- reactive({
    progStatus <- ""
    if (!is.na(DPProgData_rowselected()) && !is.na(DPProgData()) && length(DPProgData_rowselected()) > 0) {
      if (DPProgData()[DPProgData_rowselected(), DPProgData.Status] == StatusCompleted) {
        progStatus <- "- Status: Completed"
      } else if (DPProgData()[DPProgData_rowselected(), DPProgData.Status] == StatusProcessing) {
        progStatus <- "- Status: in Progress"
      } else if (DPProgData()[DPProgData_rowselected(), DPProgData.Status] == StatusFailed) {
        progStatus <- "- Status: Failed"
      }
    }
    progStatus
  })
  
  # > update of input$selectprogrammeID ----
  observeEvent(submodulesList$step1_chooseProgramme$selectprogrammeID(), {
    choices <- progChoices()
    prgId <- submodulesList$step1_chooseProgramme$selectprogrammeID()
    #Avoid updating input if not necessary
    if (!is.null(choices)) {
      updateSelectizeInput(session, inputId = "selectprogrammeID", selected = character(0), choices = progChoices())
    }
    if (prgId != "" && input$selectprogrammeID != prgId) {
      if (prgId != "") {
        updateSelectizeInput(session, inputId = "selectprogrammeID", selected = prgId)
      } else {
        updateSelectizeInput(session, inputId = "selectprogrammeID", selected = character(0))
      }
    }
  })
  
  # > prog Model Table reactives ----
  POData <- reactive({submodulesList$step2_chooseModel$POData()})
  POData_rowselected <- reactive({match(input$selectprogOasisID, progOasisChoices())})
  progOasisName <- reactive({POData()[POData_rowselected(), POData.ProgName]})
  progOasisChoices <- reactive({
    progOasisChoices <- NULL
    if (!is.na(POData()) && !is.null(POData())) {
      progOasisChoices <- POData()[, POData.ProgOasisId]
    } else {
      models <- getModelList(dbSettings)
      progOasisChoices <- createSelectOptions(models, "Model ID")
    }
    progOasisChoices
    })
  progOasisStatus <- reactive({
    progOasisStatus <- ""
    if(!is.na(POData()) && !is.na(POData_rowselected()) && length(POData_rowselected()) > 0){
      if (POData()[POData_rowselected(), POData.Status] == StatusCompleted) {
        progOasisStatus <- "- Status: Completed"
      } else if (POData()[POData_rowselected(), POData.Status] == StatusProcessing) {
        progOasisStatus <- "- Status: in Progress"
      } else if (POData()[POData_rowselected(), POData.Status] == StatusFailed) {
        progOasisStatus <- "- Status: Failed"
      }
    }
    progOasisStatus
  })
  
  observeEvent( submodulesList$step2_chooseModel$selectprogrammeID(), {
    prgId <- submodulesList$step2_chooseModel$selectprogrammeID()
    #Avoid updating input if not necessary
    if (prgId != "" &&  input$selectprogrammeID != prgId) {
      updateSelectizeInput(session, inputId = "selectprogrammeID", selected = prgId)  
    }
  })
  
  # > update of input$selectprogOasisID ----
  observeEvent(submodulesList$step2_chooseModel$selectprogOasisID(), {
    choiches <- progOasisChoices()
    prgId <- submodulesList$step2_chooseModel$selectprogOasisID()
    if (!is.null(choiches)) {
      updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0), choices = choiches)
    }
    if (!is.na(prgId) && prgId != "") {
      updateSelectizeInput(session, inputId = "selectprogOasisID", selected = prgId)
    } else {
      updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0))
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
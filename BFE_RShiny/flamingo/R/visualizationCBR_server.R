#' Compare Runs Definition Module
#' @rdname visualizationCBR
#' @description Server logic to compare runs
#' @inheritParams flamingoModule
#' @return For \code{visualizationCBR()}, list of reactives.
#' @template return-outputNavigation
#' @export
visualizationCBR <- function(input, output, session, dbSettings, apiSettings,
                             userId, active = reactive(TRUE), 
                             runIdList = reactive(c(-1)),
                             preselRunId = reactive(-1),
                             processRunId = reactive(-1),
                             logMessage = message) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  navigation_state <- reactiveNavigation()
  
  # list of sub-modules
  sub_modules <- list()

  result <- reactiveValues(
    #Reactive to know if one of the preselected runIds has changed
    RunIDchanged = -2,
    # preselRunId()
    preselRunId = -1,
    # processRunId()
    processRunId = -1,
    #selected run
    preselectedRunId = NULL,
    selectedRunId = NULL,
    #Panel to select
    preselPanel = "1",
    # output files table
    filesListData = NULL
  )
  
  #number of plot output panels
  n_panels <- 5
  
  #clean value
  observeEvent(active(), {
    result$preselPanel <- "1"
  })
  
  # Run identification ---------------------------------------------------------
  
  #Define reactive value to react if any of the preselected run Ids changes
  observe({
    preselRunId()
    processRunId()
    if (is.null(processRunId())) {
      result$processRunId <- -1
    } else {
      result$processRunId <- processRunId()
    }
    if (is.null(preselRunId())) {
      result$preselRunId <- -1
    } else {
      result$preselRunId <- preselRunId()
    }
    result$RunIDchanged <- result$preselRunId + result$processRunId
  })
  
  #Update selected runID
  observe({
    result$RunIDchanged
    if (result$RunIDchanged == -2 ) {
      result$preselectedRunId = runIdList()$RunID[1]
    } else {
      if (result$preselRunId != -1) {
        result$preselectedRunId = isolate(result$preselRunId)
      }
      if (result$processRunId != -1) {
        result$preselectedRunId = isolate(result$processRunId)
      }
    }
  })
  
  #Update list of options
  observeEvent(result$preselectedRunId, {
    index <- match(c(result$preselectedRunId), runIdList()$RunID)
    if (!is.null(index) & !is.na(index)) {
      updateSelectInput(session, inputId = "selectRunID1", choices = runIdList()$RunID, selected = runIdList()$RunID[index])
    }
  })
  
  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    updateNavigation(navigation_state, "PS")
    result$preselPanel <- "4"
  })
  
  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summary,
    id = "summary",
    selectRunID = reactive(input$selectRunID1),
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    active = reactive({active() && input$tabsSBR == "tabsummary"}),
    logMessage = logMessage)
  
  
  # Extract Output files for given runID----------------------------------------
  
  # Tab Output files -----------------------------------------------------------
  sub_modules$outputfiles <- callModule(
    outputfiles,
    id = "outputfiles",
    filesListDatatoview =  reactive({result$filesListData}),
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    active = reactive({active() && input$tabsSBR == "taboutputfiles"}),
    logMessage = logMessage)
  
  
  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectRunID = reactive(input$selectRunID1),
    filesListData =  reactive({result$filesListData}),
    n_panels = n_panels,
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    active = reactive({active() && input$tabsSBR == "tabplots"}),
    logMessage = logMessage)
  

  # Module Outout --------------------------------------------------------------
  
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive({result$preselPanel})
    )
  )
  
  moduleOutput
}

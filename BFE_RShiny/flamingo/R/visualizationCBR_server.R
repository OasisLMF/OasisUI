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
    preselPanel = 1,
    # output files table
    filesListData = NULL
  )
  
  #number of plot output panels
  n_panels <- 5
  
  #clean value
  observeEvent(active(), {
    if (active()) {
      result$preselPanel <- 1
    }
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
  
  # Selected runID -------------------------------------------------------------
  sub_modules$defineID1 <- callModule(
    defineID,
    id = "defineID-1",
    runIdList = runIdList,
    preselectedRunId = reactive(result$preselectedRunId),
    logMessage = logMessage)
  
  selectRunID1 <- reactive({
    sub_modules$defineID1$selectRunID()
  })
  
  sub_modules$defineID2 <- callModule(
    defineID,
    id = "defineID-2",
    runIdList = runIdList,
    preselectedRunId = reactive(result$preselectedRunId),
    logMessage = logMessage)
  
  selectRunID2 <- reactive({
    sub_modules$defineID2$selectRunID()
  })
  
  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    updateNavigation(navigation_state, "PS")
    result$preselPanel <- 3
  })
  
  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summarytab,
    id = "summarytab",
    selectRunID = reactive(selectRunID1()),
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
    selectRunID = reactive(selectRunID1()),
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

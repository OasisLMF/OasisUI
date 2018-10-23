#' Batch Browse Definition Module
#' @rdname visualizationBBR
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @return For \code{visualizationBBR()}, list of reactives.
#' @template return-outputNavigation
#' @export
visualizationBBR <- function(input, output, session, dbSettings,
                             apiSettings, userId,
                             runIdList = reactive(c(-1)),
                             preselRunId = reactive(-1),
                             processRunId = reactive(-1),
                             active = reactive(TRUE), logMessage = message) {

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

  # Selected runID -------------------------------------------------------------
  sub_modules$defineID <- callModule(
    defineID,
    id = "defineID",
    runIdList = runIdList,
    preselectedRunId = reactive(result$preselectedRunId),
    logMessage = logMessage)
  
  selectRunID <- reactive({
    sub_modules$defineID$selectRunID()
  })

  
  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotobatchconfig, {
    updateNavigation(navigation_state, "PB")
    result$preselPanel <- 3
  })
  

  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summarytab,
    id = "summarytab",
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
    selectRunID = reactive(selectRunID()),
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

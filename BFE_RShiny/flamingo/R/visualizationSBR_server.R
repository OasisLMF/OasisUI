# visualizationSBR Module -----------------------------
#' visualizationSBR Module
#' @rdname visualizationSBR
#' @description Server logic for viewing results of a single run
#' @inheritParams flamingoModule
#' @param runIdList list of runs and their status
#' @param preselRunId reactive string expression for reselected run id from landingpage
#' @param processRunId reactive string expression for reselected run id from defineProgramme
#' @return list of reactives:
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable
#' @importFrom dplyr mutate select contains filter
#' @export
visualizationSBR <- function(input, output, session, dbSettings,
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
  sub_modules$defineID <- callModule(
    defineID,
    id = "defineID",
    dbSettings = dbSettings,
    userId = reactive(userId()),
    runIdList = runIdList,
    preselectedRunId = reactive(result$preselectedRunId),
    logMessage = logMessage)
  
  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    updateNavigation(navigation_state, "PS")
    result$preselPanel <- 3
  })
  
  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summarytab,
    id = "summarytab",
    selectRunID = reactive(sub_modules$defineID$selectRunID()),
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    active = reactive({active() && input$tabsSBR == "tabsummary"}),
    logMessage = logMessage)
  
  
  # Extract Output files for given runID----------------------------------------
  observeEvent( sub_modules$defineID$selectRunID(), {
    if (!is.na(sub_modules$defineID$selectRunID()) && sub_modules$defineID$selectRunID() != "") {
      if (!is.null(runIdList())) {
        index <- match(c(sub_modules$defineID$selectRunID()), runIdList()$RunID)
        status <- runIdList()[index, "Status"]
        if (!is.na(status) && status == StatusCompleted) {
          result$filesListData <- getFileList(dbSettings, sub_modules$defineID$selectRunID())
          result$filesListData <- cbind(result$filesListData,do.call(rbind.data.frame,  lapply(result$filesListData$Description, .splitDescription)))
        } else {
          result$filesListData <- NULL
        }
      } else {
        result$filesListData <- NULL
      }
    }
  })
  
  filesListDatatoview <- reactive({
    if (!is.null(result$filesListData)) {
      result$filesListData %>% select(-c("Variable", "Granularity", "Losstype"))
    } else {
      result$filesListData
    }
  })
  
  # Tab Output files -----------------------------------------------------------
  sub_modules$outputfiles <- callModule(
    outputfiles,
    id = "outputfiles",
    filesListDatatoview =  filesListDatatoview,
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    active = reactive({active() && input$tabsSBR == "taboutputfiles"}),
    logMessage = logMessage)
  
  
  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectRunID = reactive(sub_modules$defineID$selectRunID()),
    filesListData =   reactive({result$filesListData}),
    n_panels = n_panels,
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    active = reactive({active() && input$tabsSBR == "tabplots"}),
    logMessage = logMessage)
  
  
  # Helper functions -----------------------------------------------------------
  #function to split the description field of result$filesListData
  .splitDescription <- function(x){
    y <- unlist(strsplit(x,split = " "))
    z <- data.frame("Granularity" = y[2], "Losstype" = y[4], "Variable" = paste(y[5:length(y)], collapse = " "), stringsAsFactors = FALSE)
    return(z)}
  
  
  # Module Outout --------------------------------------------------------------
  
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive({result$preselPanel})
    )
  )
  
  moduleOutput
}



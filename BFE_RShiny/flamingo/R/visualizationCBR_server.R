#' Compare Runs Definition Module Server ---------------------------------------

#' visualizationCBR
#'
#' @rdname visualizationCBR
#'
#' @description Server logic for comparing runs run page.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param runIdList List of runs and their status.
#' @param preselRunId reactive string expression for reselected run id from \link{landingpage}.
#' @param processRunId reactive string expression for reselected run id from \link{defineProgramme}.
#'
#' @importFrom dplyr select
#'
#' @return preselPanel panel to show in the process session.
#'
#' @export
visualizationCBR <- function(input, output, session, dbSettings, apiSettings,
                             user, 
                            active = reactive(TRUE),
                             preselRunId = reactive(-1),
                             processRunId = reactive(-1),
                             logMessage = message) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  navigation_state <- reactiveNavigation()
  
  # list of sub-modules
  sub_modules <- list()

  result <- reactiveValues(
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
  sub_modules$defineID1 <- callModule(
    defineID,
    id = "defineID-1",
    dbSettings = dbSettings,
    user = reactive(user()),
    preselRunId = preselRunId,
    processRunId =  processRunId,
    logMessage = logMessage)

  sub_modules$defineID2 <- callModule(
    defineID,
    id = "defineID-2",
    dbSettings = dbSettings,
    user = reactive(user()),
    preselRunId = preselRunId,
    processRunId =  processRunId,
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
    selectRunID1 = reactive(sub_modules$defineID1$selectRunID()),
    selectRunID2 = reactive(sub_modules$defineID2$selectRunID()),
    compare = TRUE,
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    active = reactive({active() && input$tabsSBR == "tabsummary"}),
    logMessage = logMessage)
  
  
  # Extract Output files for given runID----------------------------------------
  observeEvent( {
    sub_modules$defineID1$selectRunID()
    sub_modules$defineID2$selectRunID()}, {
    if (!is.na(sub_modules$defineID1$selectRunID()) && sub_modules$defineID1$selectRunID() != "" &&
        !is.na(sub_modules$defineID2$selectRunID()) && sub_modules$defineID2$selectRunID() != "") {
        filesListData1 <- getFileList(dbSettings, sub_modules$defineID1$selectRunID())
        result$filesListData <- cbind(filesListData1,
                                      do.call(rbind.data.frame, 
                                              lapply(filesListData1$Description, .splitDescription)))
        filesListData2 <- getFileList(dbSettings, sub_modules$defineID2$selectRunID())
        result$filesListData <- rbind(result$filesListData,
                                      cbind(filesListData2,
                                            do.call(rbind.data.frame, 
                                                    lapply(filesListData2$Description, .splitDescription))))
    } else {
      result$filesListData <- NULL
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
    active = reactive({active() && input$tabsSBR == "taboutputfiles"}),
    logMessage = logMessage)
  
  
  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectRunID = reactive(sub_modules$defineID1$selectRunID()),
    filesListData =  reactive({result$filesListData}),
    n_panels = n_panels,
    dbSettings = dbSettings,
    apiSettings = apiSettings,
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

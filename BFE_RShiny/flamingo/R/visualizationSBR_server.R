# visualizationSBR Module Server -----------------------------------------------

#' visualizationSBR
#'
#' @rdname visualizationSBR
#'
#' @description Server logic for viewing results of a single analysis.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param anaIdList List of analyses and their status.
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingpage}.
#' @param anaID  reactive string expression for reselected run id from \link{step3_configureOutput}.
#'
#' @return preselPanel panel to show in the model session
#'
#' @importFrom dplyr select
#'
#' @export
visualizationSBR <- function(input, output, session, dbSettings,
                             apiSettings,
                             user,
                             anaIdList = reactive(c(-1)),
                             preselAnaId = reactive(-1),
                             anaID  = reactive(-1),
                             active = reactive(TRUE), logMessage = message) {
  
  ns <- session$ns
  # Reactive Values and parameters ------------------------------------------
  navigation_state <- reactiveNavigation()
  
  # list of sub-modules
  sub_modules <- list()
  
  result <- reactiveValues(
    #Panel to select
    preselPanel = 1,
    # output files table
    filesListData = NULL,
    # df analysis output files
    tbl_filesListDataana = NULL,
    # df portfolio input files
    tbl_filesListDatapf = NULL
  )
  
  #number of plot output panels
  n_panels <- 5
  
  #clean value
  observeEvent(active(), {
    if (active()) {
      result$preselPanel <- 1
    }
  })
  
  # Selected anaID -------------------------------------------------------------
  sub_modules$defineID <- callModule(
    defineID,
    id = "defineID",
    dbSettings = dbSettings,
    user = reactive(user()),
    preselAnaId = preselAnaId,
    anaID =  anaID,
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
    selectAnaID1 = reactive(sub_modules$defineID$selectAnaID()),
    portfolioID1 = reactive(sub_modules$defineID$selectPortfolioID()),
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    active = reactive({active() && input$tabsSBR == "tabsummary"}),
    logMessage = logMessage)
  
  
  # Extract Output files for given anaID----------------------------------------
  observeEvent( sub_modules$defineID$selectAnaID(), {
    if (!is.na(sub_modules$defineID$selectAnaID()) && sub_modules$defineID$selectAnaID() != "") {
      tbl_filesListDataana <- return_analyses_output_file_df(sub_modules$defineID$selectAnaID())
      result$tbl_filesListDataana <- cbind(tbl_filesListDataana,
                                    do.call(rbind.data.frame, lapply(tbl_filesListDataana$files,
                                                                     .addDescription)))
      result$tbl_filesListDatapf <- return_tbl_portfolioDetails(sub_modules$defineID$selectPortfolioID())
      result$filesListData <- getFileList(dbSettings, sub_modules$defineID$selectAnaID())
      if (nrow(result$filesListData) > 0) {
        result$filesListData <- cbind(result$filesListData,
                                      do.call(rbind.data.frame, lapply(result$filesListData$Description,
                                                                       .splitDescription)))
      } else {
        result$filesListData <- NULL
      }
    } else {
      result$filesListData <- NULL
    }
  })
  
  filesListDatatoview <- reactive({
    if (!is.null(result$filesListData) ) {
      result$filesListData %>% select(-c("Variable", "Granularity", "Losstype"))
    } else {
      result$filesListData
    }
  })
  
  
  
  # Tab Output files -----------------------------------------------------------
  sub_modules$outputfiles <- callModule(
    outputfiles,
    id = "outputfiles",
    tbl_filesListDataana =  reactive(result$tbl_filesListDataana),
    tbl_filesListDatapf = reactive(result$tbl_filesListDatapf),
    anaId = sub_modules$defineID$selectAnaID,
    portfolioId = sub_modules$defineID$selectPortfolioID, 
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    active = reactive({active() && input$tabsSBR == "taboutputfiles"}),
    logMessage = logMessage)
  
  
  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectAnaID = reactive(sub_modules$defineID$selectAnaID()),
    filesListData =   reactive({result$filesListData}),
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
  
  #Add descritption fields to output files
  .addDescription <- function(x){
    #x <- as.character(tbl_filesListDataana$files[1])
    x <- as.character(x)
    x <- strsplit(x, split = "[.]")[[1]][1]
    y <- unlist(strsplit(x, split = "_"))
    report <-  paste(y[3:(length(y))], collapse = "_")
    z <- data.frame("Perspective" = y[1], "Summary Level" = y[2], "Report" = reportToVar[[ report ]])
  }
  
  # Module Outout --------------------------------------------------------------
  
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive({result$preselPanel})
    )
  )
  
  moduleOutput
}



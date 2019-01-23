# visualizationSBR Module Server -----------------------------------------------

#' visualizationSBR
#'
#' @rdname visualizationSBR
#'
#' @description Server logic for viewing results of a single analysis.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-logMessage
#' @template params-active
#'
#' @param anaIdList List of analyses and their status.
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingPage}.
#' @param anaID  reactive string expression for reselected run id from \link{step3_configureOutput}.
#'
#' @return preselPanel panel to show in the model session
#'
#' @importFrom dplyr select
#'
#' @export
visualizationSBR <- function(input, output, session,
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
    preselAnaId = preselAnaId,
    anaID =  anaID,
    logMessage = logMessage)
  
  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    updateNavigation(navigation_state, "SA")
    result$preselPanel <- 3
  })
  
  # Extract Output files for given anaID----------------------------------------
  observeEvent( sub_modules$defineID$selectAnaID(), {
    if (!is.na(sub_modules$defineID$selectAnaID()) && sub_modules$defineID$selectAnaID() != "") {
      tbl_filesListDataana <- return_analyses_output_file_df(sub_modules$defineID$selectAnaID())
      analysis_settings <- return_analyses_settings_file_list(sub_modules$defineID$selectAnaID())
      result$tbl_filesListDataana <- cbind(tbl_filesListDataana,
                                    do.call(rbind.data.frame, lapply(tbl_filesListDataana$files,
                                                                     .addDescription, analysis_settings)))
      result$tbl_filesListDatapf <- return_tbl_portfolioDetails(sub_modules$defineID$selectPortfolioID())
    } else {
      result$tbl_filesListDatapf <- NULL
      result$tbl_filesListDataana <- NULL
    }
  })
  
  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summarytab,
    id = "summarytab",
    selectAnaID1 = reactive(sub_modules$defineID$selectAnaID()),
    portfolioID1 = reactive(sub_modules$defineID$selectPortfolioID()),
    tbl_filesListDataana1 = reactive({result$tbl_filesListDataana}),
    active = reactive({active() && input$tabsSBR == "tabsummary"}),
    logMessage = logMessage)
  
  
  # Tab Output files -----------------------------------------------------------
  sub_modules$outputfiles <- callModule(
    outputfiles,
    id = "outputfiles",
    tbl_filesListDataana =  reactive(result$tbl_filesListDataana),
    tbl_filesListDatapf = reactive(result$tbl_filesListDatapf),
    anaId = sub_modules$defineID$selectAnaID,
    portfolioId = sub_modules$defineID$selectPortfolioID, 
    active = reactive({active() && input$tabsSBR == "taboutputfiles"}),
    logMessage = logMessage)
  
  
  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectAnaID = reactive(sub_modules$defineID$selectAnaID()),
    filesListData =   reactive({result$tbl_filesListDataana}),
    n_panels = n_panels,
    active = reactive({active() && input$tabsSBR == "tabplots"}),
    logMessage = logMessage)
  
  
  # Helper functions -----------------------------------------------------------

  #Add descritption fields to output files
  .addDescription <- function(x, analysis_settings){
    x <- as.character(x)
    x <- strsplit(x, split = "[.]")[[1]][1]
    y <- unlist(strsplit(x, split = "_"))
    report <-  paste(y[3:(length(y))], collapse = "_")
    g_idx <- as.integer(gsub("S", "", y[2]))
    g_oed <- analysis_settings[["analysis_settings"]][[paste0(y[1], "_summaries")]][[g_idx]][["oed_fields"]]
    g <- granToOed[granToOed$oed == g_oed, "gran"]
    z <- data.frame("perspective" = y[1], "summary_level" = toString(g), "report" = reportToVar(varsdf)[[ report ]], stringsAsFactors = FALSE)
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



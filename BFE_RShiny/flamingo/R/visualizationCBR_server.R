#' Compare Analyses Definition Module Server ---------------------------------------

#' visualizationCBR
#'
#' @rdname visualizationCBR
#'
#' @description Server logic for comparing runs analyses page.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-logMessage
#' @template params-active
#'
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingPage}.
#' @param anaID  reactive string expression for reselected run id from \link{step3_configureOutput}.
#'
#' @importFrom dplyr select
#'
#' @return preselPanel panel to show in the model session.
#'
#' @export
visualizationCBR <- function(input, output, session,
                             active = reactive(TRUE),
                             preselAnaId = reactive(-1),
                             anaID  = reactive(-1),
                             logMessage = message) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  navigation_state <- reactiveNavigation()

  # list of sub-modules
  sub_modules <- list()

  result <- reactiveValues(
    #Panel to select
    preselPanel = 1,
    #id of selected analysis
    selectAnaID = "",
    #portfolio id of selected analysis
    selectPortfolioID = "",
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
      result$selectAnaID <- ""
      result$selectPortfolioID = ""
    }
  })


  # Selected anaID -------------------------------------------------------------
  sub_modules$defineID1 <- callModule(
    defineID,
    id = "defineID-1",
    preselAnaId = preselAnaId,
    anaID =  anaID,
    logMessage = logMessage)

  sub_modules$defineID2 <- callModule(
    defineID,
    id = "defineID-2",
    preselAnaId = preselAnaId,
    anaID =  anaID,
    logMessage = logMessage)


  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    result$preselPanel <- 3
    result$selectAnaID <- sub_modules$defineID1$selectAnaID()
    result$selectPortfolioID <- sub_modules$defineID1$selectPortfolioID()
    logMessage(paste0("Selected analysis id is ", result$selectAnaID, ". Selected portfolio id is ", result$selectPortfolioID))
    updateNavigation(navigation_state, "SA")
  })


  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summarytab,
    id = "summarytab",
    selectAnaID1 = reactive(sub_modules$defineID1$selectAnaID()),
    selectAnaID2 = reactive(sub_modules$defineID2$selectAnaID()),
    portfolioID1 = reactive(sub_modules$defineID1$selectPortfolioID()),
    portfolioID2 = reactive(sub_modules$defineID2$selectPortfolioID()),
    compare = TRUE,
    active = reactive({active() && input$tabsCBR == ns("tabsummary")}),
    logMessage = logMessage)


  # Extract Output files for given anaID----------------------------------------
  observeEvent( {
    sub_modules$defineID1$selectAnaID()
    sub_modules$defineID2$selectAnaID()}, {
      if (!is.na(sub_modules$defineID1$selectAnaID()) && sub_modules$defineID1$selectAnaID() != "" &&
          !is.na(sub_modules$defineID2$selectAnaID()) && sub_modules$defineID2$selectAnaID() != "") {
        tbl_filesListDataana1 <- return_analyses_output_file_df(sub_modules$defineID1$selectAnaID())
        analysis_settings1 <- return_analyses_settings_file_list(sub_modules$defineID1$selectAnaID())
        result$tbl_filesListDataana <- cbind(tbl_filesListDataana1,
                                             do.call(rbind.data.frame, lapply(tbl_filesListDataana1$files,
                                                                              .addDescription, analysis_settings1)))
        tbl_filesListDataana2 <- return_analyses_output_file_df(sub_modules$defineID2$selectAnaID())
        analysis_settings2 <- return_analyses_settings_file_list(sub_modules$defineID2$selectAnaID())
        bl_filesListDataana2 <- return_analyses_output_file_df(sub_modules$defineID2$selectAnaID())
        analysis_settings2 <- return_analyses_settings_file_list(sub_modules$defineID2$selectAnaID())
        result$tbl_filesListDataana <- rbind(result$tbl_filesListDataana, cbind(tbl_filesListDataana1,
                                                                                do.call(rbind.data.frame, lapply(tbl_filesListDataana1$files,
                                                                                                                 .addDescription, analysis_settings1))))
        tbl_filesListDatapf1 <- return_tbl_portfolioDetails(sub_modules$defineID1$selectPortfolioID())
        tbl_filesListDatapf2 <- return_tbl_portfolioDetails(sub_modules$defineID2$selectPortfolioID())
        result$tbl_filesListDatapf <- rbind(tbl_filesListDatapf1, tbl_filesListDatapf2)
      } else {
        result$tbl_filesListDatapf <- NULL
        result$tbl_filesListDataana <- NULL
      }
    })

  # Tab Output files -----------------------------------------------------------
  sub_modules$outputfiles <- callModule(
    outputfiles,
    id = "outputfiles",
    tbl_filesListDataana =  reactive(result$tbl_filesListDataana),
    anaId = sub_modules$defineID1$selectAnaID,
    active = reactive({active() && input$tabsCBR == ns("taboutputfiles")}))


  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectAnaID = reactive(sub_modules$defineID1$selectAnaID()),
    filesListData =   reactive({result$tbl_filesListDataana}),
    n_panels = n_panels,
    active = reactive({active() && input$tabsCBR == ns("tabplots")}),
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
    z <- data.frame("perspective" = y[1], "summary_level" = g, "report" = reportToVar(varsdf)[[ report ]])
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

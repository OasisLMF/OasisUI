#' Compare Analyses Definition Module Server ---------------------------------------

#' visualizationCBR
#'
#' @rdname visualizationCBR
#'
#' @description Server logic for comparing runs analyses page.
#'
#' @template return-outputNavigation
#' @template params-module
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
                             preselAnaId = reactive(NULL),
                             anaID = reactive(NULL)) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  navigation_state <- reactiveNavigation()

  # list of sub-modules
  sub_modules <- list()

  result <- reactiveValues(
    #Panel to select
    preselPanel = 1,
    #id of selected analysis
    selectAnaID = NULL,
    #portfolio id of selected analysis
    selectPortfolioID = "",
    # df analysis output files
    tbl_filesListDataana = NULL
  )

  #number of plot output panels
  n_panels <- 5

  #clean value
  observeEvent(active(), {
    if (active()) {
      result$preselPanel <- 1
      result$selectAnaID <- NULL
      result$selectPortfolioID = ""
    }
  })


  # Selected anaID -------------------------------------------------------------
  sub_modules$defineID1 <- callModule(
    defineID,
    id = "defineID-1",
    preselAnaId = preselAnaId,
    anaID =  anaID)

  sub_modules$defineID2 <- callModule(
    defineID,
    id = "defineID-2",
    preselAnaId = preselAnaId,
    anaID =  anaID)


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
    active = reactive({active() && input$tabsCBR == ns("tabsummary")}))


  # Extract Output files for given anaID----------------------------------------
  observeEvent( {
    sub_modules$defineID1$selectAnaID()
    sub_modules$defineID2$selectAnaID()}, {
      result$tbl_filesListDataana1 <- session$userData$data_hub$get_ana_outputs_data_list(sub_modules$defineID1$selectAnaID())
      result$tbl_filesListDataana2 <- session$userData$data_hub$get_ana_outputs_data_list(sub_modules$defineID2$selectAnaID())
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
    active = reactive({active() && input$tabsCBR == ns("tabplots")}))


  # Module Outout --------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive({result$preselPanel})
    )
  )

  moduleOutput
}

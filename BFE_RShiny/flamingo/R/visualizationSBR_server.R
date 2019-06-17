# visualizationSBR Module Server -----------------------------------------------

#' visualizationSBR
#'
#' @rdname visualizationSBR
#'
#' @description Server logic for viewing results of a single analysis.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#'
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingPage}.
#' @param anaID  reactive string expression for reselected run id from \link{step3_configureOutput}.
#'
#' @return preselPanel panel to show in the model session
#' @return selectAnaID id of selected analysis
#' @return selectPortfolioID portfolio id of selected analysis
#'
#' @importFrom dplyr select
#'
#' @export
visualizationSBR <- function(input, output, session,
                             preselAnaId = reactive(NULL),
                             anaID  = reactive(NULL),
                             active = reactive(TRUE)) {

  ns <- session$ns
  # Reactive Values and parameters ------------------------------------------
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
  sub_modules$defineID <- callModule(
    defineID,
    id = "defineID",
    preselAnaId = preselAnaId,
    anaID =  anaID,
    active = active)

  # Go to Configure Output button ----------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    result$preselPanel <- 3
    result$selectAnaID <- sub_modules$defineID$selectAnaID()
    result$selectPortfolioID <- sub_modules$defineID$selectPortfolioID()
    logMessage(paste0("Selected analysis id is ", result$selectAnaID, ". Selected portfolio id is ", result$selectPortfolioID))
    updateNavigation(navigation_state, "SA")
  })

  # Extract Output files for given anaID----------------------------------------
  observeEvent(sub_modules$defineID$selectAnaID(), {
    result$tbl_filesListDataana <- session$userData$data_hub$get_ana_outputs_data_list(sub_modules$defineID$selectAnaID(),  oasisapi = session$userData$oasisapi)
  })

  # Tab Summary ----------------------------------------------------------------
  sub_modules$summary <- callModule(
    summarytab,
    id = "summarytab",
    selectAnaID1 = reactive(sub_modules$defineID$selectAnaID()),
    portfolioID1 = reactive(sub_modules$defineID$selectPortfolioID()),
    tbl_filesListDataana1 = reactive({result$tbl_filesListDataana}),
    active = reactive({active() && input$tabsSBR == ns("tabsummary")}))

  # Tab Output files -----------------------------------------------------------
  sub_modules$outputfiles <- callModule(
    outputfiles,
    id = "outputfiles",
    tbl_filesListDataana =  reactive(result$tbl_filesListDataana),
    anaId = reactive(sub_modules$defineID$selectAnaID()),
    active = reactive({active() && input$tabsSBR == ns("taboutputfiles")}))

  # Tab Output Plots -----------------------------------------------------------
  sub_modules$outputplots <- callModule(
    outputplots,
    id = "outputplots",
    selectAnaID = reactive(sub_modules$defineID$selectAnaID()),
    filesListData =   reactive({result$tbl_filesListDataana}),
    n_panels = n_panels,
    active = reactive({active() && input$tabsSBR == ns("tabplots")}))

  # Module Outout --------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive({result$preselPanel}),
      selectAnaID = reactive({result$selectAnaID}),
      selectPortfolioID = reactive({result$selectPortfolioID})
    )
  )

  moduleOutput
}



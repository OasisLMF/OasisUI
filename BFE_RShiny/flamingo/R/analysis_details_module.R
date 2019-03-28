# analysis_details Module UI -----------------------------------------------

#' analysis_detailsUI
#'
#' @rdname analysis_details
#'
#' @description UI side of function wrapping panel to show analyses details table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
analysis_detailsUI <- function(id, button) {

  ns <- NS(id)
  tabsetPanel(
    id = ns("tabsDetails"),
    tabPanel(
      title = "Exposure Validation",
      exposurevalidationUI(ns("exposurevalidation")),
      value = ns("tabvalidation")
    ),
    tabPanel(
      title = "Generated Inputs",
      generatedinputsUI(ns("generatedinputs")),
      value = ns("tabgeneratedinputs")
    ),
    tabPanel(
      title = "Status Detail",
      statusdetailUI(ns("statusdetail")),
      value = ns("tabstatusdetail")
    ),
    tabPanel(
      title = "Uploaded Inputs",
      uploadedinputsUI(ns("uploadedinputs")),
      value = ns("tabuploadedinputs")
    )
  )
}


# analysis_details Module Server -------------------------------------------

#' analysis_details
#'
#' @rdname analysis_details
#'
#' @description Server side of function wrapping panel to show analyses details table.
#'
#' @param analysisID Selected analysis ID.
#' @param tbl_filesListData Dataframe of the output files
#' @param reload_generated Imports function to reload Generated Inputs table.
#' @param anaName Analysis name.
#' @param portfolioID Selected portfolio ID.
#'
#' @importFrom shinyjs hide
#'
#' @template params-module-ui
#'
#' @export
analysis_details <- function(input,
                             output,
                             session,
                             analysisID,
                             anaName,
                             portfolioID) {

  ns <- session$ns

  # Tab Exposure Validation ----------------------------------------------------
  callModule(
    exposurevalidation,
    id = "exposurevalidation",
    active = reactive({input$tabsDetails == "tabvalidation"})
  )

  # Tab Generated Inputs -------------------------------------------------------
  callModule(
    generatedinputs,
    id = "generatedinputs",
    analysisID = analysisID,
    active = reactive({input$tabsDetails == "tabgeneratedinputs"})
  )

  # Tab Status Detail ----------------------------------------------------------
  callModule(
    statusdetail,
    id = "statusdetail",
    analysisID = analysisID,
    active = reactive({input$tabsDetails == "tabstatusdetail"})
  )

  # Tab Uploaded Inputs --------------------------------------------------------
  callModule(
    uploadedinputs,
    id = "uploadedinputs",
    portfolioID = portfolioID,
    active = reactive({input$tabsDetails == "tabuploadedinputs"})
  )

  #  analysis_details Table title
  output$paneltitle_analysis_details <- renderUI({
    paste0('Details of analysis id ', toString(analysisID()), ' ', anaName())
  })

}

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
analysis_detailsUI <- function(id) {

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
      title = "Summary Status",
      summarystatusUI(ns("summarystatus")),
      value = ns("tabsummarystatus")
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
                             portfolioID,
                             counter) {

  ns <- session$ns

  # Tab Exposure Validation ----------------------------------------------------
  callModule(
    exposurevalidation,
    id = "exposurevalidation",
    analysisID = analysisID,
    portfolioID = portfolioID,
    counter = counter,
    active = reactive({input$tabsDetails == ns("tabvalidation")})
  )

  # Tab Generated Inputs -------------------------------------------------------
  callModule(
    generatedinputs,
    id = "generatedinputs",
    analysisID = analysisID,
    active = reactive({input$tabsDetails == ns("tabgeneratedinputs")})
  )

  # Tab Status Detail ----------------------------------------------------------
  callModule(
    summarystatus,
    id = "summarystatus",
    analysisID = analysisID,
    active = reactive({input$tabsDetails == ns("tabsummarystatus")})
  )

  # Tab Uploaded Inputs --------------------------------------------------------
  callModule(
    uploadedinputs,
    id = "uploadedinputs",
    portfolioID = portfolioID,
    active = reactive({input$tabsDetails == ns("tabuploadedinputs")})
  )

}

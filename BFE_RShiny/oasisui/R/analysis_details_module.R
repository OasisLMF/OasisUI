# analysis_details Module UI -----------------------------------------------

#' analysis_detailsUI
#'
#' @rdname analysis_details
#'
#' @description UI side of function wrapping panel to show analyses details table.
#'
#' @importFrom DT DTOutput
#'
#' @export
analysis_detailsUI <- function(id) {

  ns <- NS(id)
  tabsetPanel(
    id = ns("tabsDetails"),
    tabPanel(
      title = "Validation Summary",
      exposurevalidationsummaryUI(ns("exposurevalidationsummary")),
      value = ns("tabvalidationsummary")
    ),
    tabPanel(
      title = "Validation Map",
      exposurevalidationmapUI(ns("exposurevalidationmap")),
      value = ns("tabvalidationmap")
    ),
    tabPanel(
      title = "Inputs",
      anainputsUI(ns("anainputs"), show = TRUE),
      value = ns("tabanainputs")
    )
  )
}


# analysis_details Module Server -------------------------------------------

#' analysis_details
#'
#' @rdname analysis_details
#'
#' @description Server side of function wrapping panel to show analyses details table.
#'i
#' @param analysisID Selected analysis ID.
#' @param portfolioID Selected portfolio ID.
#' @param counter Counter.
#' @template params-module
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
    exposurevalidationsummary,
    id = "exposurevalidationsummary",
    analysisID = analysisID,
    counter = counter,
    active = reactive({input$tabsDetails == ns("tabvalidationsummary")})
  )

  callModule(
    exposurevalidationmap,
    id = "exposurevalidationmap",
    analysisID = analysisID,
    portfolioID = portfolioID,
    counter = counter,
    active = reactive({input$tabsDetails == ns("tabvalidationmap")})
  )

  # Tab Analysis Inputs --------------------------------------------------------
  callModule(
    anainputs,
    id = "anainputs",
    analysisID = analysisID,
    refresh_opt = TRUE,
    counter = counter,
    active = reactive({input$tabsDetails == ns("tabanainputs")})
  )

}

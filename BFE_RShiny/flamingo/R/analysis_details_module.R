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
      anainputsUI(ns("anainputs")),
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
#'
#' @param analysisID Selected analysis ID.
#' @param tbl_filesListData Dataframe of the output files
#' @param reload_generated Imports function to reload Generated Inputs table.
#' @param portfolioID Selected portfolio ID.
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

  # Download inputs ------------------------------------------------------------
  observeEvent({
    counter()
  }, {
    if (length(counter()) > 0 && counter() > 0) {
      if ((!is.null(portfolioID()) && !is.na(portfolioID()) && portfolioID() != "") &&
          (!is.null(analysisID()) && !is.na(analysisID()) && analysisID() != "")) {
        extractFolder <- set_extractFolder(analysisID(), label = "_inputs/")
        if (!file.exists(extractFolder) && is.na(file.size(extractFolder))) {
          withModalSpinner(
            api_get_analyses_input_file(analysisID()),
            "Loading...",
            size = "s"
          )
        }
      }
    }
  })

  # Tab Exposure Validation ----------------------------------------------------
  callModule(
    exposurevalidationsummary,
    id = "exposurevalidationsummary",
    analysisID = analysisID,
    portfolioID = portfolioID,
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

  # Tab Generated Inputs -------------------------------------------------------
  callModule(
    anainputs,
    id = "anainputs",
    analysisID = analysisID,
    counter = counter,
    active = reactive({input$tabsDetails == ns("tabanainputs")})
  )

}

# panelAnalysisDetails Module UI -----------------------------------------------

#' panelAnalysisDetailsUI
#'
#' @rdname panelAnalysisDetails
#'
#' @description UI side of function wrapping panel to show analyses details table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelAnalysisDetailsUI <- function(id, button) {

  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    ns("panel_analysisdetails"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_panelAnalysisDetails"), inline = TRUE),
      button
    ),
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
  )
}


# panelAnalysisDetails Module Server -------------------------------------------

#' panelAnalysisDetails
#'
#' @rdname panelAnalysisDetails
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
panelAnalysisDetails <- function(input,
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

  #  panelAnalysisDetails Table title
  output$paneltitle_panelAnalysisDetails <- renderUI({
    paste0('Details of analysis id ', toString(analysisID()), ' ', anaName())
  })

}

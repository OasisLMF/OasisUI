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
panelAnalysisDetailsUI <- function(id) {

  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    ns("panel_analysisdetails"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_panelAnalysisDetails"), inline = TRUE),
      actionButton(inputId = ns("buttonhideanadetails"), label = NULL, icon = icon("times"), style = "float: right;")
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
                                 anaName) {

  ns <- session$ns

  # list of sub-modules
  sub_modules <- list()

  # Tab Exposure Validation ----------------------------------------------------
  sub_modules$exposurevalidation <- callModule(
    exposurevalidation,
    id = "exposurevalidation"
  )

  # Tab Generated Inputs -------------------------------------------------------
  sub_modules$generatedinputs <- callModule(
    generatedinputs,
    id = "generatedinputs",
    analysisID = analysisID
  )

  # Tab Status Detail --------------------------------------------------------
  sub_modules$statusdetail <- callModule(
    statusdetail,
    id = "statusdetail",
    analysisID = analysisID)

  #  panelAnalysisDetails Table title
  output$paneltitle_panelAnalysisDetails <- renderUI({
      paste0('Details of analysis id ', toString(analysisID()), ' ', anaName())
  })

  onclick("buttonhideanadetails", {
    hide("panel_analysisdetails")
  })

  sub_modules
}

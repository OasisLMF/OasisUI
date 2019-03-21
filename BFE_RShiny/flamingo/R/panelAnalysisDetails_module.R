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
        flamingoRefreshButton(ns("abuttonexposurerefresh")),
        exposurevalidationUI(ns("exposurevalidation")),
        value = ns("tabvalidation")
      ),
      tabPanel(
        title = "Generated Inputs",
        flamingoRefreshButton(ns("abuttongeneratedrefresh")),
        generatedinputsUI(ns("generatedinputs")),
        value = ns("tabgeneratedinputs")
      ),
      tabPanel(
        title = "Uploaded Inputs",
        flamingoRefreshButton(ns("abuttonuploadedrefresh")),
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
#' @template params-module-ui
#'
#' @export
panelAnalysisDetails <- function(input,
                                 output,
                                 session,
                                 analysisID,
                                 tbl_filesListData,
                                 param,
                                 file_column,
                                 folderpath,
                                 reload_generated,
                                 reload_uploaded) {

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
    tbl_filesListData = tbl_filesListData,
    param = param,
    file_column = file_column,
    folderpath = folderpath
  )

  # Tab Uploaded Inputs --------------------------------------------------------
  sub_modules$uploadedinputs <- callModule(
    uploadedinputs,
    id = "uploadedinputs",
    analysisID = analysisID)

  onclick("abuttonexposurerefresh", {

  })

  onclick("abuttongeneratedrefresh", {
    reload_generated
  })

  onclick("abuttonuploadedrefresh", {
    reload_uploaded
  })

  sub_modules
}

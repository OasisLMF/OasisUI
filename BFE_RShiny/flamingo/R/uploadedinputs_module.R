# Uploaded Inputs Module -------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' uploadedinputsUI
#' @rdname uploadedinputs
#'
#' @description UI/View for uploaded inputs of an analysis.
#'
#' @return List of tags.
#'
#' @export
uploadedinputsUI <- function(id) {

  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_portfolio_details"),
    flamingoRefreshButton(ns("abuttonuploadedrefresh")),
    actionButton(inputId = ns("buttonhidepfdetails"), label = NULL, icon = icon("times"), style = "float: right;"),
    ViewFilesInTableUI(id  = ns("portfolioDetails"), includechkbox = TRUE)
  )
}


# Server -----------------------------------------------------------------------

#' uploadedinputs
#'
#' @rdname uploadedinputs
#'
#' @description Server logic for exposure validation of an analysis.
#'
#' @param portfolioID selected portfolio ID.
#'
#' @export
uploadedinputs <- function(input, output, session, portfolioID) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    pfId = NULL,
    data = NULL,
    dt_uploaded = NULL
  )

  # Create table ---------------------------------------------------------------
  result$dt_uploaded <- reactive({
    .reloadtbl_portfolioDetails()
  })

  callModule(
    ViewFilesInTable,
    id = "portfolioDetails",
    tbl_filesListData =  result$dt_uploaded,
    param = portfolioID,
    logMessage = logMessage,
    includechkbox = TRUE)

  # reload Uploaded Inputs table-----------------------------------------------
  onclick("abuttonuploadedrefresh", {
    .reloadtbl_portfolioDetails()
  })

  # Reload uploaded inputs table -----------------------------------------------
  .reloadtbl_portfolioDetails <- function() {
    logMessage(".reloadtbl_portfolioDetails called")
    if (!is.null(portfolioID()) && portfolioID() != "") {
      result$data  <- return_tbl_portfolioDetails(portfolioID())
    } else {
      result$data  <- NULL
    }
    result$data
  }
}

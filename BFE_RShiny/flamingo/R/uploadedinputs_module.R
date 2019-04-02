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
    ns("panel_uploadedinputs"),
    flamingoRefreshButton(ns("abuttonuploadedrefresh")),
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
uploadedinputs <- function(input,
                           output,
                           session,
                           portfolioID,
                           active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    pfId = NULL,
    dt_uploaded = NULL
  )

  # Create table ---------------------------------------------------------------
  observeEvent(active(), {
    if (length(active()) > 0 && active()) {
      withModalSpinner(
        .reloadtbl_portfolioDetails(),
        "Loading...",
        size = "s"
      )
    }
  })

  callModule(
    ViewFilesInTable,
    id = "portfolioDetails",
    tbl_filesListData =  reactive({result$dt_uploaded}),
    param = portfolioID,
    logMessage = logMessage,
    includechkbox = TRUE)

  # reload Uploaded Inputs table-----------------------------------------------
  onclick("abuttonuploadedrefresh", {
    withModalSpinner(
      .reloadtbl_portfolioDetails(),
      "Refreshing...",
      size = "s"
    )
  })

  # Reload uploaded inputs table -----------------------------------------------
  .reloadtbl_portfolioDetails <- function() {
    logMessage(".reloadtbl_portfolioDetails called")
    if (!is.null(portfolioID()) && portfolioID() != "") {
      result$dt_uploaded  <- return_tbl_portfolioDetails(portfolioID())
    } else {
      result$dt_uploaded  <- NULL
    }
  }
}

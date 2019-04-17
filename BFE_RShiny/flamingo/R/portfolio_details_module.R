# Portfolio Details Module -------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' portfolio_detailsUI
#' @rdname portfolio_details
#'
#' @description UI/View for portfolio details.
#'
#' @return List of tags.
#'
#' @export
portfolio_detailsUI <- function(id) {

  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_portfolio_details"),
    flamingoRefreshButton(ns("abuttonuploadedrefresh")),
    ViewFilesInTableUI(id  = ns("portfolioDetails"), includechkbox = TRUE)
  )
}


# Server -----------------------------------------------------------------------

#' portfolio_details
#'
#' @rdname portfolio_details
#'
#' @description Server logic for portfolio details.
#'
#' @param portfolioID selected portfolio ID.
#'
#' @export
portfolio_details <- function(input,
                           output,
                           session,
                           portfolioID,
                           counter = NULL,
                           active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    pfId = NULL,
    dt_uploaded = NULL
  )

  # Create table ---------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, {
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

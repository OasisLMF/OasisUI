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
    heading = tagAppendChildren(
      h4("Source files for portfolio"),
      uiOutput(ns("paneltitle_pfDetails"), inline = TRUE),
      flamingoRefreshButton(ns("abuttondefpfrfsh")),
      actionButton(inputId = ns("buttonhidepfdetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    collapsible = FALSE,
    ns("panel_portfolio_details"),
    div(id = ns("refresh"), flamingoRefreshButton(ns("abuttonuploadedrefresh"))),
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
#' @template params-module
#' @param refresh_opt Option to hide/show refresh button.
#' @param portfolioID selected portfolio ID.
#' @param portfolioName selected portfolio Name.
#' @param counter Reactive value storing actionButton status.
#'
#' @importFrom shinyjs hide
#'
#' @export
portfolio_details <- function(input,
                              output,
                              session,
                              refresh_opt = TRUE,
                              portfolioID,
                              portfolioName,
                              counter = reactive(NULL),
                              active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    dt_uploaded = NULL
  )

  # Create table ---------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active()) {
      if (!refresh_opt) {
        hide("refresh")
      }
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
    includechkbox = TRUE)

  # Title Portfolio Details Panel ----------------------------------------------
  output$paneltitle_pfDetails <- renderUI({
    pfId <- portfolioID()
    paste0(' ', portfolioID(), ' ', portfolioName())
  })

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
    session$userData$data_hub$invalidate_pf_data_list()
    logMessage(".reloadtbl_portfolioDetails called")
          result$dt_uploaded  <- session$userData$data_hub$get_pf_data_list(portfolioID())
  }
}

# uploaded inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' summarystatusUI
#' @rdname summarystatus
#'
#' @description UI/View for summary status of files of an analysis.
#'
#' @return List of tags.
#'
#' @export
summarystatusUI <- function(id) {

  ns <- NS(id)

  tagList(
    flamingoPanel(
      selection = list(mode = 'none'),
      escape = FALSE,
      scrollX = TRUE,
      filter = "none",
      rownames = TRUE,
      colnames = c('row number' = 1),
      id = ns("panel_summarystatus"),
      flamingoRefreshButton(ns("abuttonuploadedrefresh")),
      flamingoTableUI(ns("summaryStatusTable"))
    )
  )
}


# Server -----------------------------------------------------------------------

#' summarystatus
#'
#' @rdname summarystatus
#'
#' @description Server logic for summary status of files of an analysis.
#'
#' @param analysisID Selected analysis id.
#'
#' @export
summarystatus <- function(input,
                          output,
                          session,
                          analysisID,
                          counter = NULL,
                          active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    dt_uploaded = NULL
  )

  # Create flamingoTable -------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active()) {
      withModalSpinner({
        .reloadSummaryStatus()
      },
      "Loading...",
      size = "s"
      )
    }
  })

  callModule(
    flamingoTable,
    id = "summaryStatusTable",
    data = reactive({result$dt_uploaded}),
    rownames = TRUE,
    escape = FALSE,
    colnames = c('row number' = 1)
  )

  # reload Status Details table-------------------------------------------------
  onclick("abuttonuploadedrefresh", {
    withModalSpinner(
      .reloadSummaryStatus(),
      "Refreshing...",
      size = "s"
    )
  })

  # Reload Status Detail table -------------------------------------------------
  .reloadSummaryStatus <- function() {
    logMessage(".reloadSummaryStatus called")
    if (!is.null(analysisID()) && analysisID() != "") {
      result$dt_uploaded <- return_tbl_analysisdetails(analysisID())
    } else {
      result$dt_uploaded <- NULL
    }
  }

}

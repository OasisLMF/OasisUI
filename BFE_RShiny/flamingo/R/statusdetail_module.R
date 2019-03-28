# uploaded inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' statusdetailUI
#' @rdname statusdetail
#'
#' @description UI/View for status detail of files of an analysis.
#'
#' @return List of tags.
#'
#' @export
statusdetailUI <- function(id) {

  ns <- NS(id)

  tagList(
    flamingoPanel(
      selection = list(mode = 'none'),
      escape = FALSE,
      scrollX = TRUE,
      filter = "none",
      rownames = TRUE,
      colnames = c('row number' = 1),
      id = ns("panel_statusdetai"),
      flamingoRefreshButton(ns("abuttonuploadedrefresh")),
      flamingoTableUI(ns("statusDetailTable"))
    )
  )
}


# Server -----------------------------------------------------------------------

#' statusdetail
#'
#' @rdname statusdetail
#'
#' @description Server logic for status detail of files of an analysis.
#'
#' @param analysisID Selected analysis id.
#'
#' @export
statusdetail <- function(input,
                         output,
                         session,
                         analysisID,
                         active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    dt_uploaded = NULL
  )

  # Create flamingoTable -------------------------------------------------------
  observeEvent(active(), {
    .reloadStatusDetails()
  })

  callModule(
    flamingoTable,
    id = "statusDetailTable",
    data = reactive({result$dt_uploaded}),
    rownames = TRUE,
    escape = FALSE,
    colnames = c('row number' = 1)
  )

  # reload Status Details table-------------------------------------------------
  onclick("abuttonuploadedrefresh", {
    .reloadStatusDetails()
  })

  # Reload Status Detail table -------------------------------------------------
  .reloadStatusDetails <- function() {
    logMessage(".reloadStatusDetails called")
    if (!is.null(analysisID()) && analysisID() != "") {
      tbl_uploaded <- return_tbl_analysisdetails(analysisID())
    } else {
      tbl_uploaded <-  NULL
    }

    if (!is.null(tbl_uploaded) && nrow(tbl_uploaded) > 0) {
      result$dt_uploaded <- tbl_uploaded
    } else {
      result$dt_uploaded <- NULL
    }
  }

}

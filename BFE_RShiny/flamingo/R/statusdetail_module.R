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
      ViewFilesInTableUI(ns("statusDetailTable"), includechkbox = TRUE)
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
    ViewFilesInTable,
    id = "statusDetailTable",
    tbl_filesListData = reactive({result$dt_uploaded}),
    param = analysisID,
    file_column = "files",
    folderpath = "_inputs/",
    includechkbox = TRUE
  )

  # reload Status Details table-------------------------------------------------
  onclick("abuttonuploadedrefresh", {
    .reloadStatusDetails()
  })

  # Reload Status Detail table -------------------------------------------------
  .reloadStatusDetails <- function() {
    logMessage(".reloadStatusDetails called")
    if (!is.null(analysisID()) && analysisID() != "") {
    #   tbl_uploaded <- return_tbl_analysisdetails(analysisID())
    # } else {
    #   tbl_uploaded <-  NULL
    # }
    #
    # if (!is.null(tbl_uploaded) && nrow(tbl_uploaded) > 0) {
      result$dt_uploaded <- return_tbl_analysisdetails(analysisID())
    } else {
      result$dt_uploaded <- NULL
    }
  }

}

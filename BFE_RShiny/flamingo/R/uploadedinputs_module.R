# uploaded inputs Module ----------------------------------------------------------

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

  tagList(
    flamingoPanel(
      selection = list(mode = 'none'),
      escape = FALSE,
      scrollX = TRUE,
      filter = "none",
      rownames = TRUE,
      colnames = c('row number' = 1),
      id = ns("panel_analysisdetails"),
      flamingoRefreshButton(ns("abuttonuploadedrefresh")),
      flamingoTableUI(ns("uploadedInputsTable"))
    )
  )
}


# Server -----------------------------------------------------------------------

#' uploadedinputs
#'
#' @rdname uploadedinputs
#'
#' @description Server logic for uploaded inputs of an analysis.
#'
#' @param analysisID Selected analysis id.
#'
#' @export
uploadedinputs <- function(input,
                           output,
                           session,
                           analysisID) {

  ns <- session$ns

  result <- reactiveValues(
    data = NULL,
    dt_uploaded = NULL
  )

  # Reload Uploaded Inputs table
  .reloadUploadedInputs <- function() {
    logMessage(".reloadUploadedInputs called")
    if (!is.null(analysisID()) && analysisID() != "") {
      tbl_uploaded <- return_tbl_analysisdetails(analysisID())
    } else {
      tbl_uploaded <-  NULL
    }

    if (!is.null(tbl_uploaded) && nrow(tbl_uploaded) > 0) {
      result$data <- tbl_uploaded
    } else {
      result$data <- NULL
    }
    result$data
  }

  result$dt_uploaded <- reactive({
    .reloadUploadedInputs()
  })

  sub_modules <- list()
  sub_modules$detailsTable <- callModule(
    flamingoTable,
    id = "uploadedInputsTable",
    data = result$dt_uploaded,
    rownames = TRUE,
    escape = FALSE,
    colnames = c('row number' = 1)
  )

  onclick("abuttonuploadedrefresh", {
    .reloadUploadedInputs()
  })

}

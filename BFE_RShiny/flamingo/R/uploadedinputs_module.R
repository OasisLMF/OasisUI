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

  dt_uploaded <- reactive({

    if (!is.null(analysisID()) && analysisID() != "") {
      tbl_analysisdetails <- return_tbl_analysisdetails(analysisID())
    } else {
      tbl_analysisdetails <-  NULL
    }

    if (!is.null(tbl_analysisdetails) && nrow(tbl_analysisdetails) > 0) {
      data <- tbl_analysisdetails
    } else {
      data <- NULL
    }
    data
  })

  sub_modules <- list()
  sub_modules$detailsTable <- callModule(
    flamingoTable,
    id = "uploadedInputsTable",
    data = dt_uploaded,
    rownames = TRUE,
    escape = FALSE,
    colnames = c('row number' = 1)
  )

  # Reload Uploaded Inputs table
  .reloadUploadedInputs <- function() {
    logMessage(".reloadUploadedInputs called")
    if (!is.null(analysisID()) && analysisID() != "") {
      tbl_analysisdetails <- return_tbl_analysisdetails(analysisID())
    } else {
      tbl_analysisdetails <-  NULL
    }
  }

  onclick("abuttonuploadedrefresh", {
    .reloadUploadedInputs()
  })

}

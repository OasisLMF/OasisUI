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
      flamingoTableUI(ns("uploadedInputsTable"))
    )
  )
}


# Server -----------------------------------------------------------------------

#' uploadedinputs
#'
#' @rdname uploadedinputs
#'
#' @param analysisID Selected analysis id.
#'
#' @description Server logic for uploaded inputs of an analysis.
#'
#' @export
uploadedinputs <- function(input, output, session, analysisID) {

  ns <- session$ns
  result <- reactiveValues(
    # analysis details
    tbl_analysisdetails = NULL
  )

  dt_uploaded <- reactive({

    if (!is.null(analysisID()) && analysisID() != "") {
      result$tbl_analysisdetails <- return_tbl_analysisdetails(analysisID())
    } else {
      result$tbl_analysisdetails <-  NULL
    }

    if (!is.null(result$tbl_analysisdetails) && nrow(result$tbl_analysisdetails) > 0) {
      data <- result$tbl_analysisdetails
    } else {
      data <- NULL
    }
    data
  })

  # DTOutput(ns("dt_analysisdetails"))
  sub_modules <- list()
  sub_modules$detailsTable <- callModule(
    flamingoTable,
    id = "uploadedInputsTable",
    data = dt_uploaded
  )
}

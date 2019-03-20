# View DT table Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' ViewDTTableUI
#'
#' @rdname ViewDTTable
#'
#' @description UI/View to view  files.
#'
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#'
#' @export
ViewDTTableUI <-  function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("dt_analysisdetails")
    )
  )
}

# Server -----------------------------------------------------------------------
#' ViewDTTable
#'
#' @rdname ViewDTTable
#'
#' @description Server logic to view DT table.
#'
#' @export
ViewDTTable <- function(input, output, session) {

  ns <- session$ns

}

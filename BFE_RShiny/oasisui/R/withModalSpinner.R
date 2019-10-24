#' @title Modal window spinner
#'
#' @rdname withModalSpinner
#'
#' @description Display a modal window with a spinning wheel and an information message
#' while a (time-consuming) expression is evaluated.
#'
#' @param expr The `expression` to be evaluated.
#' @param info The information message to be displayed.
#' @param spinner The spinning wheel icon.
#' @inheritParams shiny::modalDialog
#' @param t Time to prolong the appearing of the modalDialog.
#'
#' @export
#'
#' @md
withModalSpinner <- function(expr, info,
                             spinner = icon("spinner", "fa-spin"),
                             size = "m",
                             t = 0) {
  showModal(
    modalDialog(
      h4(spinner, info),
      footer = NULL,
      size = size
    )
  )
  Sys.sleep(t)
  force(expr)
  removeModal()
}

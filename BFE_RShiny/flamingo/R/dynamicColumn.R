# Module server function ----

#' dynamicColumn
#'
#' @rdname dynamicColumn
#'
#' @description Shiny module providing support for dynamic UI [column][shiny::column] width
#' without re-rendering the column content.
#'
#' @templateVar shinyjsdep [shinyjs::addClass()] and [shinyjs::removeClass()]
#' @template note-shinyjs
#' @template params-module
#'
#' @details Dynamic column width via `renderUI()` based on a reactive value forces the
#' re-rendering of the column content. This has the following undesired effects:
#'   * The state of the existing column UI content (including user input) is
#'   lost.
#'   * Depending on the complexity of the content, rendering can slow-down the
#'   re-sizing.
#' The _dynamicColumn_ Shiny module prevents re-rendering based on reactive column
#' width. Instead, it changes the class of the existing column UI element
#' (`"col-sm-<WIDTH>"`) depending on a reactive `width` value.
#'
#' @param width Width of the column, a reactive value for `dynamicColumn()`,
#'   non-reactive (or [isolated][shiny::isolate]) for `dynamicColumnUI()`.
#' @inheritParams shiny::column
#'
#' @example man-roxygen/ex-dynamicColumn.R
#'
#' @export
#'
#' @md
dynamicColumn <- function(input, output, session, width) {
  state <- reactiveValues(
    width = isolate(width())
  )
  observeEvent(
    width(), {
      replace_col_class("column", state$width, width())
      state$width <- width()
    })
}


# Module UI function ----

#' dynamicColumn
#'
#' @rdname dynamicColumn
#'
#' @inheritParams accountDefinitionUI
#' @inheritParams dynamicColumn
#' @param ... Other inputs.
#'
#' @export
dynamicColumnUI <- function(id, width, ...) {
  ns <- NS(id)
  # NOTE that we do not call shiny::column() directly but set an explict
  # col-sm-<width> class, since this is what the module relies on; we also do
  # not support offset.
  if (!is.numeric(width) || (width < 1) || (width > 12)) {
    stop("column width must be between 1 and 12")
  }
  div(id = ns("column"), class = col_class(width), ...)
}



# utilities ----

col_class <- function(width) {
  sprintf("col-sm-%d", width)
}


replace_class <- function(id, old, new) {
  # add the class before removing it to avoid intermediate status w/o class
  shinyjs::addClass(id, new)
  shinyjs::removeClass(id, old)
}


replace_col_class <- function(id, old, new) {
  replace_class(id, col_class(old), col_class(new))
}

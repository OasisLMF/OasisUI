# Module server function ----

#' reactiveConditionalPanels
#'
#' @rdname reactiveConditionalPanels
#'
#' @description Shiny module handling the visibility of a set of panels depending on a
#' reactive input.
#'
#' @template params-module
#' @templateVar shinyjsdep the package \pkg{shinyjs}
#' @template note-shinyjs
#'
#' @details In comparison to [shiny::conditionalPanel()], the
#'   _reactiveConditionalPanels_ module provides a convenient abstraction from the
#'   JavaScript condition details and only relies on the reactive input
#'   `visible` to determine which panels are visible.
#'
#' @param visible Reactive string expression determining the visibility of
#'   panels.
#' @inheritParams flamingoModule
#
#' @seealso Module _[reactiveConditionalPanel]_, providing similar functionality
#'   for a single panel.
#'
#' @example man-roxygen/ex-reactiveConditionalPanels.R
#'
#' @return smth.
#'
#' @export
#'
#' @md
reactiveConditionalPanels <- function(input, output, session, visible) {
  output$visible <- visible
  outputOptions(output, "visible", suspendWhenHidden = FALSE)
  invisible()
}


# Module UI function ----

#' reactiveConditionalPanelsUI
#'
#' @rdname reactiveConditionalPanels
#'
#' @template params-module-ui
#' @param panels Named list of UI content for a set of panels. Only the panel
#'   whose name matches the `visible` reactive input to the module server
#'   function is shown.
#'
#' @return Panels.
#'
#' @export
#'
#' @md
reactiveConditionalPanelsUI <- function(id, panels) {
  ns <- NS(id)
  tagList(
    lapply(seq_along(panels), function(i) {
      conditionalPanel(
        sprintf("output.visible == '%s'", names(panels)[i]),
        panels[[i]],
        ns = ns
      )
    }),
    shinyjs::hidden(div(verbatimTextOutput(ns("visible"))))
  )
}

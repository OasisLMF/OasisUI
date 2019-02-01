# Module server function -------------------------------------------------------

#' reactiveConditionalPanel
#'
#' @rdname reactiveConditionalPanel
#'
#' @description Shiny module handling the visibility of a panel based on a
#'   reactive `condition`.
#'
#' @template params-module
#' @param condition Reactive boolean expression determining the visibility of
#'   the panel.
#'
#' @templateVar module reactiveConditionalPanel
#' @templateVar reactive condition
#' @template details-conditionalPanel-js-abstract
#
#' @return SERVER => TO BE REVIEWED AND HAMRONIZED, CONSIDER A SECTION Module
#'   Outputs INSTEAD
#'
#' @templateVar shinyjsdep the \pkg{shinyjs} package
#' @template note-shinyjs
#'
#' @seealso Module _[reactiveConditionalPanels]_, providing similar
#'   functionality for multiple panels.
#'
#' @example man-roxygen/ex-reactiveConditionalPanel.R
#'
#' @export
#'
#' @md
reactiveConditionalPanel <- function(input, output, session, condition) {
  output$visible <- condition
  outputOptions(output, "visible", suspendWhenHidden = FALSE)
  invisible()
}


# Module UI function -----------------------------------------------------------

#' @rdname reactiveConditionalPanel
#'
#' @inheritParams shiny::conditionalPanel
#'
#' @return UI => TO BE REVIEWED AND HAMRONIZED
#'
#' @export
#'
#' @md
reactiveConditionalPanelUI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "output.visible",
      ...,
      ns = ns
    ),
    shinyjs::hidden(div(verbatimTextOutput(ns("visible"))))
  )
}

# Module server function -------------------------------------------------------

#' reactiveConditionalPanels
#'
#' @rdname reactiveConditionalPanels
#'
#' @description Shiny module handling the visibility of a set of panels
#'   depending on a reactive input.
#'
#' @template params-module
#' @param visible Reactive string expression determining the visibility of
#'   panels.
#'
#' @templateVar module reactiveConditionalPanels
#' @templateVar reactive visible
#' @template details-conditionalPanel-js-abstract
#
#' @return SERVER => TO BE REVIEWED AND HAMRONIZED, CONSIDER A SECTION Module
#'   Outputs INSTEAD
#'
#' @templateVar shinyjsdep the \pkg{shinyjs} package
#' @template note-shinyjs
#'
#' @seealso Module _[reactiveConditionalPanel]_, providing similar functionality
#'   for a single panel.
#'
#' @example man-roxygen/ex-reactiveConditionalPanels.R
#'
#' @export
#'
#' @md
reactiveConditionalPanels <- function(input, output, session, visible) {
  output$visible <- visible
  outputOptions(output, "visible", suspendWhenHidden = FALSE)
  invisible()
}


# Module UI function -----------------------------------------------------------

#' @rdname reactiveConditionalPanels
#'
#' @param panels Named list of UI content for a set of panels. Only the panels
#'   whose name matches the `visible` reactive input to the module server
#'   function are shown. Panels with the same name are supported and shown
#'   together.
#'
#' @return UI => TO BE REVIEWED AND HAMRONIZED
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

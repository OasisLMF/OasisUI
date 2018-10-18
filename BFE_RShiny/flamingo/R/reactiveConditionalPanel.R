# Module server function ----

#' Reactive Conditional Panel
#' @rdname reactiveConditionalPanel
#' @description Shiny module handling the visibility of a panel based on a reactive `condition`.
#' @template params-module
#' @inheritParams flamingoModule
#' @param condition Reactive boolean expression determining the visibility of
#'   the panel.
#' @details In comparison to [shiny::conditionalPanel()], the
#'   _reactiveConditionalPanel_ module provides a convenient abstraction from the
#'   JavaScript condition details, relying on the reactive `condition` instead.
#' @templateVar shinyjsdep the package \pkg{shinyjs}
#' @template note-shinyjs
#' @seealso Module _[reactiveConditionalPanels]_, providing similar
#'   functionality for multiple panels.
#' @example man-roxygen/ex-reactiveConditionalPanel.R
#' @export
#' @md
reactiveConditionalPanel <- function(input, output, session, condition) {
  output$visible <- condition # reactive(FALSE) #
  outputOptions(output, "visible", suspendWhenHidden = FALSE)
  invisible()
}


# Module UI function ----

#' @title reactiveConditionalPanelUI
#' @rdname reactiveConditionalPanelUI
#' @inheritParams shiny::conditionalPanel
#' @export
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

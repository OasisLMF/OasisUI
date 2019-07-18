# Define Output Configuration UI -----------------------------------------------

#' modeldetailsUI
#'
#' @rdname defineOutputs
#'
#' @description UI side of function wrapping panel to show oputput configuration.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
defineOutputsUI <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    ns("panel_anaoutput"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defAnaConfigOutput"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidepanelconfigureoutput"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(4,
             panelModelParams(id)),
      column(8,
             fluidRow(panelOutputParams(id)),
             fluidRow(panelOutputParamsDetails(id))
      )
    ),
    fluidRow(
      column(12,
             oasisuiButton(inputId = ns("abuttonexecuteanarun"), label = "Execute Run"), align = "right")) %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonexecuteanarun, placement = "right")
  )
}

# Define Output Configuration Server -------------------------------------------

#' defineOutputs
#'
#' @rdname defineOutputs
#'
#' @description Server side of function wrapping panel to show oputput configuration.
#'
#' @param modelID Selected model ID.
#' @template params-module
#' @template params-active
#'
#' @importFrom shinyjs hide
#'
#' @export
defineOutputs <- function(input,
                          output,
                          session,
                          counter,
                          active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(

  )

}

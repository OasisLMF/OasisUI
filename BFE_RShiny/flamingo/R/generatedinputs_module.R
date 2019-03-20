# uploaded inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' generatedinputsUI
#' @rdname generatedinputs
#'
#' @description UI/View for uploaded inputs of an analysis.
#'
#' @return List of tags.
#'
#' @export
generatedinputsUI <- function(id) {

  ns <- NS(id)

    flamingoPanel(
      collapsible = TRUE,
      show = TRUE,
      ns("panel_analysisIG"),
      heading = tagAppendChildren(
        h4(""),
        uiOutput(ns("paneltitle_panelAnalysisIG"), inline = TRUE),
        flamingoRefreshButton(ns("abuttonanaIGrefresh")),
        actionButton(inputId = ns("buttonhideanaIG"), label = NULL, icon = icon("times"), style = "float: right;")
      ),
      ViewFilesInTableUI(id  = ns("ViewIGFiles"), includechkbox = TRUE)
    )

}


# Server -----------------------------------------------------------------------

#' generatedinputs
#'
#' @rdname generatedinputs
#'
#' @description  Server logic for uploaded inputs of an analysis.
#'
#' @export
generatedinputs <- function(input, output, session) {

  ns <- session$ns

}

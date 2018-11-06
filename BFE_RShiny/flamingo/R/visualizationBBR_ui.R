# visualization Batch Run Browse Module UI -------------------------------------

#' visualizationBBRUI
#'
#' @rdname visualizationBBR
#'
#' @description UI/View for batchbrowse run page.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#'
#' @export
visualizationBBRUI <- function(id) {

  ns <- NS(id)

  tagList(
    h3("Define Batch Browse", class = "flamingo-page-title")
  )

}

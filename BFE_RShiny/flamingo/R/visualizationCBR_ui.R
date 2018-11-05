# visualization Compare Runs Browse Module UI ----------------------------------

#' visualizationCBRUI
#'
#' @rdname visualizationCBR
#'
#' @description UI/View for comparing runs run page.
#'
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#'
#' @export
visualizationCBRUI <- function(id) {

  ns <- NS(id)

  tagList(
    h3("Compare runs", class = "flamingo-page-title")
  )

}

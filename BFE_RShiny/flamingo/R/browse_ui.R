#' @rdname browseprogrammesUI
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets sliderTextInput
#' @export
browseprogrammesUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    
    h3("Browse Process", class = "flamingo-page-title")
    
  )
}
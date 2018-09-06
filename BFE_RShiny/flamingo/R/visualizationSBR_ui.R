#' @rdname visualizationSBR
#' @description UI/View for single browse run page
#' @inheritParams flamingoModuleUI
#' @importFrom DT dataTableOutput
#' @return list of tags
#' @export
visualizationSBRUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h3("Define Single Browse", class = "flamingo-page-title") 
  )
  
}
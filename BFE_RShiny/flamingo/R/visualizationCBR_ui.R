#' @rdname visualizationCBR
#' @description UI/View for compare runs run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @export
visualizationCBRUI <- function(id) {

  ns <- NS(id)

  tagList(
    h3("Compare runs", class = "flamingo-page-title")
  )

}

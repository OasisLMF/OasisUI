#' @rdname visualizationBBR
#' @description UI/View for batchbrowse run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @export
visualizationBBRUI <- function(id) {

  ns <- NS(id)

  tagList(
    h3("Define Batch Browse", class = "flamingo-page-title")
  )

}

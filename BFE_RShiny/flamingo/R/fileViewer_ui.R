
#' @rdname fileViewer
#' @description UI/View to view files
#' @import leaflet
#' @importFrom DT DTOutput
#' @importFrom shinyBS bsTooltip
#' @export
fileViewerUI <- function(id) {

  ns <- NS(id)

  tagList(

    h3("File Viewer", class = "flamingo-page-title"),

    h4("File List", class = "flamingo-table-title"),

    actionButton(inputId = ns("refreshtable"), label = "Refresh", style = "float:right"),
    ViewFilesModuleUI(id  = ns("ViewFilesModule"), includechkbox = TRUE)

  )

}

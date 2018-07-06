
#' @rdname fileViewer
#' @description UI/View to view files
#' @import leaflet
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs hidden disabled
#' @importFrom shinyBS bsTooltip
#' @export
fileViewerUI <- function(id) {
  
  ns <- NS(id)

  attachDependencies(value = flamingoHtmlDependencies(), tagList(
      
      h3("File Viewer", class = "flamingo-page-title"),

          h4("File List", class = "flamingo-table-title"),

          checkboxInput(inputId = ns("tableFVfileListSelectall"), label = "Select all", value = FALSE),
          dataTableOutput(ns("tableFVfileList")),

          downloadButton(ns("FVfileListdownloadzip"), label = "Export to zip"),
          bsTooltip(ns("FVfileListdownloadzip"), 
                    file_Viewer$FVfileListdownloadzip,
                    placement = "right", 
                    options   = list(container = "body")),
          # downloadButton(ns("FVFLdownloadexcel"), label = "Export to csv"),
          # bsTooltip(ns("FVFLdownloadexcel"), 
          #             file_Viewer$FVFLdownloadexcel, 
          #             placement = "right", 
          #             options   = list(container = "body"))
  
  ))
  
}
#' modelSupplierPageUI
#'
#' @rdname modelSupplierPage
#'
#' @description UI/View for the model supplier page.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
modelSupplierPageUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(12,
             
             div(class = "flamingo-page-division",
                 helpText(h4("Model", class = "flamingo-table-title")),
                 DTOutput(ns("tablemodel")),
                 downloadButton(ns("Modeldownloadexcel"), label="Export to csv")
             ),
             
             hidden(
               div(id = ns("divmr"), class = "flamingo-page-division",
                   h4("Model Resource", class = "flamingo-table-title"),
                   DTOutput(ns("mrtable")),
                   flamingoButton(ns("btnCreate"), "Create",
                                  align = "left"),
                   flamingoButton(ns("btnAmend"), "Amend",
                                  align = "centre") %>%
                     bs_embed_tooltip(title = sys_conf$btnAmend, placement = "right"),
                   flamingoButton(ns("btnDelete"), "Delete",
                                  align = "right") %>%
                     bs_embed_tooltip(title = sys_conf$btnDelete, placement = "right"),
                   # flamingoButton(ns("abuttoncrtudptdelmodres"), "Create/Update/Delete"),
                   downloadButton(ns("MRdownloadexcel"), label="Export to csv")
               )
             )
             
      )
    )
  )
  
}

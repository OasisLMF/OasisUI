
#' @rdname modelSupplierPage
#' @description UI/View for the model supplier page
#' @inheritParams accountDefinitionUI
#' @importFrom shinyjs hidden
#' @importFrom shiny downloadButton
#' @importFrom DT DTOutput
#' @export
modelSupplierPageUI <- function(id) {

  ns <- NS(id)

  tagList(

    h3("Model", class = "flamingo-page-title"),

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
                                align = "centre"),
                   flamingoButton(ns("btnDelete"), "Delete",
                                align = "right"),
                   # flamingoButton(ns("abuttoncrtudptdelmodres"), "Create/Update/Delete"),
                   downloadButton(ns("MRdownloadexcel"), label="Export to csv")
               )
             )

      )
    )
  )

}

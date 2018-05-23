
#' @rdname modelSupplierPage
#' @description UI/View for the model supplier page
#' @inheritParams accountDefinitionUI
#' @importFrom shinyjs hidden
#' @importFrom shiny actionButton
#' @export
modelSupplierPageUI <- function(id) {
  
  ns <- NS(id)
  
  attachDependencies(value = flamingoHtmlDependencies(), tagList(

      h3("Model", class = "flamingo-page-title"),
      
      tagAppendAttributes(class = "modal-footless",
          bsModal(ns("crtAmModal"), "Create/Amend Model Resource",
              trigger = "", size = "medium",
              
              textInput(ns("tinmodelresname"), label = "Model Resource Name:",
                  value = ""),
              selectInput(ns("sinresrctype"), label = "Resource Type:",
                  choices = c("")),
              selectInput(ns("sinoasissysname"), label = "Oasis System Name:",
                  choices = c("")),
              textInput(ns("tinmodelresvalue"), label = "Model Resource Value:",
                  value = ""),
              
              actionButton(ns("btnSubmitCrtAm"), class = "btn btn-primary",
                  label = "Submit", align = "left"),
              actionButton(ns("btnCancelCrtAm"), class = "btn btn-primary",
                  label = "Cancel", align = "right"))),
      
      tagAppendAttributes(class = "modal-footless",
          bsModal(ns("delModal"), "Are you sure you want to delete?",
              trigger = "", size = "medium",
              
              actionButton(ns("btnConfirmDel"), class="btn btn-primary",
                  label = "Confirm", align = "center"),
              actionButton(ns("btnCancelDel"), class = "btn btn-primary",
                  label = "Cancel", align = "right"))),
      
      fluidRow(
          column(12,
          
              div(class = "flamingo-page-division",
                  helpText(h4("Model", class = "flamingo-table-title")),
                  dataTableOutput(ns("tablemodel")),
                  downloadButton(ns("Modeldownloadexcel"), label="Export to Excel")
              ),
        
              hidden(
                  div(id = ns("divmr"), class = "flamingo-page-division",
                      h4("Model Resource", class = "flamingo-table-title"),
                      dataTableOutput(ns("mrtable")),
                      actionButton(ns("btnCreate"), "Create",
                          class="btn btn-primary", align = "left"),
                      actionButton(ns("btnAmend"), "Amend",
                          class="btn btn-primary", align = "centre"),
                      actionButton(ns("btnDelete"), "Delete",
                          class="btn btn-primary", align = "right"),
                      # actionButton(ns("abuttoncrtudptdelmodres"), "Create/Update/Delete", class="btn btn-primary"),
                      downloadButton(ns("MRdownloadexcel"), label="Export to Excel")
                  )
              )
      
          )
      )
  ))
  
}
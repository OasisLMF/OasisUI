
#' @description UI/View to define an account.
#' @param id account id
#' @return list of tags
#' @rdname companyDefinition
#' @importFrom shiny actionButton helpText textInput sidebarLayout sidebarPanel
#' downloadButton
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs hidden
#' @importFrom shinyBS bsModal
#' @export
companyDefinitionUI <- function(id) {
  
  ns <- NS(id)
  
  attachDependencies(value = flamingoHtmlDependencies(), tagList(
      
      h3("Company", class = "flamingo-page-title"),
      
      fluidRow(
          column(12,
              div(class = "flamingo-page-division",
      
                  helpText(h4("Company List", class = "flamingo-table-title")),
                  
                  dataTableOutput(ns("tablecompanylist")),
                  
                  actionButton(ns("abuttoncompcrt"),  class="btn btn-primary",
                      label = "Create", align = "left"),
                  actionButton(ns("abuttoncompupdate"),  class="btn btn-primary", 
                      label = "Update", align = "left"),
                  actionButton(ns("abuttoncompdel"),  class="btn btn-primary",
                      label = "Delete", align = "left")
              )
          )
      ),
      
      tagAppendAttributes(class = "modal-footless",
          bsModal(ns("compcrtupmodal"), "Company Details",
              trigger = "", size = "medium",
              
              textInput(ns("tinputCompName"), "Company Name"),
              textInput(ns("tinputCompDom"), "Company Domicile"),
              textInput(ns("tinputCompLegName"), "Company Legal Name"),
              textInput(ns("tinputCompRegNo"), "Company Registration Number"),
              actionButton(ns("abuttonsubcomp"),  class="btn btn-primary",
                  label = "Submit", align = "left"),
              actionButton(ns("abuttonccancel"), class = "btn btn-primary",
                  label = "Cancel", align = "right"))),
      
      tagAppendAttributes(class = "modal-footless",
          bsModal(ns("compdelmodal"), "Are you sure you want to delete?",
              trigger = "", size = "medium",
              
              actionButton(ns("abuttoncconfirmdel"), class="btn btn-primary",
                  label = "Confirm", align = "center"),
              actionButton(ns("abuttonccanceldel"), class = "btn btn-primary",
                  label = "Cancel", align = "right")))
  
  ))
  
}


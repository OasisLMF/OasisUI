
#' @description UI/View to define an account.
#' @param id shiny module id
#' @return list of tags
#' @rdname accountDefinition
#' @importFrom shiny actionButton helpText textInput sidebarLayout sidebarPanel
#' downloadButton
#' @importFrom DT DTOutput
#' @export
accountDefinitionUI <- function(id) {

  ns <- NS(id)

  tagList(

    # h3("Define Account", class = "flamingo-page-title"),

    fluidRow(
      column(12,
             div(class = "flamingo-page-division",

                 helpText(h4("Account Table", class = "flamingo-table-title")),

                 DTOutput(ns("tableDAAccount")),

                 downloadButton(ns("DAAdownloadexcel"), label= " Export to csv"),

                 actionButton(ns("buttoncreateac"), "Create Account",
                              class="btn btn-primary", align = "left"),
                 actionButton(ns("buttonamendac"), "Amend Account",
                              class="btn btn-primary", align = "centre"),
                 actionButton(ns("buttondeleteac"), "Delete Account",
                              class="btn btn-primary", align = "right")
             )
      )
    )

  )

}

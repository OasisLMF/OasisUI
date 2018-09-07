
#' @description UI/View to define an account.
#' @param id shiny module id
#' @return list of tags
#' @rdname accountDefinition
#' @importFrom shiny actionButton helpText textInput sidebarLayout sidebarPanel
#' downloadButton
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs hidden
#' @importFrom shinyBS bsModal
#' @export
accountDefinitionUI <- function(id) {

  ns <- NS(id)

  tagList(

    h3("Define Account", class = "flamingo-page-title"),

    fluidRow(
      column(12,
             div(class = "flamingo-page-division",

                 helpText(h4("Account Table", class = "flamingo-table-title")),

                 dataTableOutput(ns("tableDAAccount")),

                 downloadButton(ns("DAAdownloadexcel"), label= " Export to csv"),

                 actionButton(ns("buttoncreateac"), "Create Account",
                              class="btn btn-primary", align = "left"),
                 actionButton(ns("buttonamendac"), "Amend Account",
                              class="btn btn-primary", align = "centre"),
                 actionButton(ns("buttondeleteac"), "Delete Account",
                              class="btn btn-primary", align = "right")
             )
      )
    ),

    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("crtupModal"), "Create/Amend Account",
                                trigger = "", size = "medium",

                                textInput(ns("tinputDAAccountName"), "Account Name"),
                                actionButton(ns("abuttonAccSubmit"), class = "btn btn-primary",
                                             label = "Submit", align = "left"),
                                actionButton(ns("abuttonAccCancel"), class = "btn btn-primary",
                                             label = "Cancel", align = "right"))),

    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("delModal"), "Are you sure you want to delete?",
                                trigger = "", size = "medium",

                                actionButton(ns("btnConfirmDel"), class="btn btn-primary",
                                             label = "Confirm", align = "center"),
                                actionButton(ns("btnCancelDel"), class = "btn btn-primary",
                                             label = "Cancel", align = "right")))

  )

}

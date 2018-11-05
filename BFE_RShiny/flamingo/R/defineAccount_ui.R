#' accountDefinitionUI
#'
#' @rdname accountDefinitionUI
#'
#' @description UI/View to define an account.
#'
#' @param id Shiny module id.
#'
#' @return List of tags.
#'
#' @importFrom shiny helpText
#' @importFrom shiny textInput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny downloadButton
#' @importFrom DT DTOutput
#'
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

                 flamingoButton(ns("buttoncreateac"), "Create Account",
                              align = "left"),
                 flamingoButton(ns("buttonamendac"), "Amend Account",
                              align = "centre") %>%
                   bs_embed_tooltip(title = define_account$buttonamendac, placement = "right"),
                 flamingoButton(ns("buttondeleteac"), "Delete Account",
                              align = "right") %>%
                   bs_embed_tooltip(title = define_account$buttondeleteac, placement = "right")
             )
      )
    )

  )

}
